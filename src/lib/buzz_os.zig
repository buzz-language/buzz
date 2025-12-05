const std = @import("std");
const api = @import("buzz_api.zig");
const builtin = @import("builtin");
const io = @import("io.zig");

pub export fn sleep(ctx: *api.NativeCtx) callconv(.c) c_int {
    std.Io.sleep(
        ctx.getIo(),
        .fromMilliseconds(@intFromFloat(ctx.vm.bz_peek(0).double())),
        .awake,
    ) catch {}; // Do we care for this error?

    return 0;
}

pub export fn time(ctx: *api.NativeCtx) callconv(.c) c_int {
    ctx.vm.bz_push(
        .fromDouble(
            @as(api.Double, @floatFromInt(
                std.Io.Clock.real.now(ctx.getIo()).toMilliseconds(),
            )),
        ),
    );

    return 1;
}

pub export fn env(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const key = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        ctx.vm.bz_push(api.Value.Null);

        return 1;
    }

    const key_slice = api.VM.allocator.dupeZ(u8, key.?[0..len]) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };
    defer api.VM.allocator.free(key_slice);

    // FIXME: don't use std.posix directly
    if (ctx.getEnv().get(key_slice)) |value| {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (value.len > 0) @as([*]const u8, @ptrCast(value)) else null,
                value.len,
            ),
        );

        return 1;
    }

    ctx.vm.bz_push(api.Value.Null);

    return 1;
}

fn sysTempDir(process_env: *std.process.Environ.Map) []const u8 {
    return switch (builtin.os.tag) {
        .windows => unreachable, // TODO: GetTempPath
        else => process_env.get("TMPDIR") orelse
            process_env.get("TMP") orelse
            process_env.get("TEMP") orelse
            process_env.get("TEMPDIR") orelse
            "/tmp",
    };
}

pub export fn tmpDir(ctx: *api.NativeCtx) callconv(.c) c_int {
    const tmp_dir: []const u8 = sysTempDir(ctx.getEnv());

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(
            ctx.vm,
            if (tmp_dir.len > 0) @as([*]const u8, @ptrCast(tmp_dir)) else null,
            tmp_dir.len,
        ),
    );

    return 1;
}

// TODO: what if file with same random name exists already?
pub export fn tmpFilename(ctx: *api.NativeCtx) callconv(.c) c_int {
    var prefix_len: usize = 0;
    const prefix = ctx.vm.bz_peek(0).bz_valueToString(&prefix_len);

    const prefix_slice = if (prefix_len == 0) "" else prefix.?[0..prefix_len];

    var random_part = std.Io.Writer.Allocating.init(api.VM.allocator);
    defer random_part.deinit();

    var rnd = std.Random.IoSource{ .io = ctx.getIo() };
    random_part.writer.print(
        "{x}",
        .{
            rnd.interface().int(api.Integer),
        },
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    var random_part_b64 = std.ArrayList(u8).initCapacity(
        api.VM.allocator,
        std.base64.standard.Encoder.calcSize(random_part.written().len),
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };
    random_part_b64.expandToCapacity();
    defer random_part_b64.deinit(api.VM.allocator);

    _ = std.base64.standard.Encoder.encode(random_part_b64.items, random_part.written());

    var final = std.Io.Writer.Allocating.init(api.VM.allocator);

    final.writer.print(
        "{s}{s}-{s}",
        .{
            sysTempDir(ctx.getEnv()),
            prefix_slice,
            random_part_b64.items,
        },
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(
            ctx.vm,
            if (final.written().len > 0)
                @as([*]const u8, @ptrCast(final.written()))
            else
                null,
            final.written().len,
        ),
    );

    return 1;
}

// If it was named `exit` it would be considered by zig as a callback when std.process.exit is called
pub export fn buzzExit(ctx: *api.NativeCtx) callconv(.c) c_int {
    const exitCode: api.Integer = ctx.vm.bz_peek(0).integer();

    std.process.exit(@intCast(exitCode));

    return 0;
}

fn handleSpawnError(ctx: *api.NativeCtx, err: std.process.SpawnError) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.FileBusy,
        error.FileSystem,
        error.InvalidWtf8,
        error.IsDir,
        error.NameTooLong,
        error.NoDevice,
        error.NotDir,
        error.InvalidBatchScriptArg,
        error.ProcessFdQuotaExceeded,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.FileNotFound,
        error.Canceled,
        error.WouldBlock,
        error.FileTooBig,
        error.NoSpaceLeft,
        error.DeviceBusy,
        error.PathAlreadyExists,
        error.ReadOnlyFileSystem,
        error.NetworkNotFound,
        error.PipeBusy,
        error.AntivirusInterference,
        error.FileLocksUnsupported,
        error.OperationUnsupported,
        error.UnrecognizedVolume,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.InvalidExe,
        error.InvalidName,
        error.InvalidUserId,
        error.PermissionDenied,
        error.ResourceLimitReached,
        error.ProcessAlreadyExec,
        error.InvalidProcessGroupId,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        },
        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

fn handleWaitError(ctx: *api.NativeCtx, err: std.process.Child.WaitError) void {
    switch (err) {
        error.AccessDenied,
        error.Canceled,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn execute(ctx: *api.NativeCtx) callconv(.c) c_int {
    var command = std.ArrayList([]const u8).empty;
    defer command.deinit(api.VM.allocator);

    const argv = ctx.vm.bz_peek(0);
    const len = argv.bz_listLen();
    var i: usize = 0;
    while (i < len) : (i += 1) {
        const arg = argv.bz_listGet(
            @intCast(i),
            false,
        );
        var arg_len: usize = 0;
        var arg_str = arg.bz_valueToString(&arg_len);

        std.debug.assert(arg_len > 0);

        command.append(api.VM.allocator, arg_str.?[0..arg_len]) catch {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        };
    }

    var child_process = std.process.spawn(
        ctx.getIo(),
        .{
            .argv = command.items,
            .disable_aslr = builtin.target.os.tag.isDarwin(),
        },
    ) catch |err| {
        handleSpawnError(ctx, err);

        return -1;
    };

    const term = child_process.wait(ctx.getIo()) catch |err| {
        handleWaitError(ctx, err);

        return -1;
    };

    ctx.vm.bz_push(.fromInteger(@intFromEnum(term)));

    return 1;
}

fn handleConnectError(ctx: *api.NativeCtx, err: std.Io.net.HostName.ConnectError) void {
    switch (err) {
        error.AccessDenied,
        error.AddressInUse,
        error.ConnectionPending,
        error.ConnectionRefused,
        error.ConnectionResetByPeer,
        error.NameServerFailure,
        error.NetworkUnreachable,
        error.ProcessFdQuotaExceeded,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.UnknownHostName,
        error.WouldBlock,
        error.Canceled,
        error.Timeout,
        error.NetworkDown,
        error.AddressUnavailable,
        error.AddressFamilyUnsupported,
        error.ProtocolUnsupportedBySystem,
        error.ProtocolUnsupportedByAddressFamily,
        error.SocketModeUnsupported,
        error.OptionUnsupported,
        error.HostUnreachable,
        error.ResolvConfParseFailed,
        error.InvalidDnsARecord,
        error.InvalidDnsAAAARecord,
        error.InvalidDnsCnameRecord,
        error.NoAddressReturned,
        error.DetectingNetworkConfigurationFailed,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

fn handleConnectUnixError(ctx: *api.NativeCtx, err: std.Io.net.UnixAddress.ConnectError) void {
    switch (err) {
        error.AddressFamilyUnsupported,
        error.PermissionDenied,
        error.ProcessFdQuotaExceeded,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.FileNotFound,
        error.WouldBlock,
        error.NetworkDown,
        error.ProtocolUnsupportedBySystem,
        error.SocketModeUnsupported,
        error.Canceled,
        error.SymLinkLoop,
        error.NotDir,
        error.ReadOnlyFileSystem,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.AccessDenied,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn SocketConnect(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(ctx.vm.bz_peek(2), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?api.Integer = ctx.vm.bz_peek(1).integer();
    if (port == null or port.? < 0) {
        ctx.vm.pushError("errors.InvalidArgumentError", null);

        return -1;
    }

    const protocol = ctx.vm.bz_peek(0).integer();

    switch (protocol) {
        0 => {
            const host = std.Io.net.HostName.init(address) catch {
                ctx.vm.pushError("errors.InvalidArgumentError", null);

                return -1;
            };
            const stream = host.connect(
                ctx.getIo(),
                @as(u16, @intCast(port.?)),
                .{
                    .mode = .stream,
                    .protocol = .tcp,
                },
            ) catch |err| {
                handleConnectError(ctx, err);

                return -1;
            };

            ctx.vm.bz_push(
                api.Value.fromInteger(
                    if (builtin.os.tag == .windows)
                        @intCast(@intFromPtr(stream.socket.handle))
                    else
                        @intCast(stream.socket.handle),
                ),
            );

            return 1;
        },
        1, // TODO: UDP
        => {
            ctx.vm.pushError("errors.NotYetImplementedError", null);

            return -1;
        },
        2 => {
            // Unix socket not available on windows
            if (builtin.os.tag == .windows) {
                ctx.vm.pushError("errors.InvalidArgumentError", null);

                return -1;
            }

            const unix_address = std.Io.net.UnixAddress.init(address) catch {
                ctx.vm.pushError("errors.InvalidArgumentError", null);

                return -1;
            };
            const stream = unix_address.connect(ctx.getIo()) catch |err| {
                handleConnectUnixError(ctx, err);

                return -1;
            };

            ctx.vm.bz_push(api.Value.fromInteger(@intCast(stream.socket.handle)));

            return 1;
        },
        else => {
            ctx.vm.pushError("errors.InvalidArgumentError", null);

            return -1;
        },
    }
}

pub export fn SocketClose(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.Io.net.Socket.Handle = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(0).integer())))
    else
        @intCast(
            ctx.vm.bz_peek(0).integer(),
        );

    const stream = std.Io.net.Stream{
        .socket = .{
            .address = .{ .ip4 = .unspecified(0) },
            .handle = handle,
        },
    };

    stream.shutdown(ctx.getIo(), .both) catch |err| {
        switch (err) {
            error.Canceled,
            error.ConnectionAborted,
            error.ConnectionResetByPeer,
            error.NetworkDown,
            error.SocketUnconnected,
            error.SystemResources,
            => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

            error.Unexpected,
            => ctx.vm.pushError("errors.UnexpectedError", null),
        }

        return -1;
    };

    return 0;
}

pub export fn SocketRead(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n: api.Integer = ctx.vm.bz_peek(0).integer();
    if (n < 0) {
        ctx.vm.pushError("errors.InvalidArgumentError", null);

        return -1;
    }

    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
    else
        @intCast(
            ctx.vm.bz_peek(1).integer(),
        );

    const stream = std.Io.net.Stream{
        .socket = .{
            .address = .{ .ip4 = .unspecified(0) },
            .handle = handle,
        },
    };
    var reader_buffer: [1024]u8 = undefined;
    const stream_reader = stream.reader(ctx.getIo(), reader_buffer[0..]);
    var reader = stream_reader.interface;

    var content = std.Io.Writer.Allocating.init(api.VM.allocator);
    defer content.deinit();

    while (content.written().len <= n) {
        const byte = reader.takeByte() catch |err| {
            switch (err) {
                error.EndOfStream => break,
                error.ReadFailed => {
                    ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err));
                    return -1;
                },
            }
        };

        content.writer.writeByte(byte) catch |err| {
            ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err));
            return -1;
        };
    }

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(
            ctx.vm,
            if (content.written().len > 0)
                @as([*]const u8, @ptrCast(content.written()))
            else
                null,
            content.written().len,
        ),
    );

    return 1;
}

pub export fn SocketReadLine(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
    else
        @intCast(
            ctx.vm.bz_peek(1).integer(),
        );
    const max_size = ctx.vm.bz_peek(0);

    const stream = std.Io.net.Stream{
        .socket = .{
            .address = .{ .ip4 = .unspecified(0) },
            .handle = handle,
        },
    };
    var reader_buffer = [_]u8{0};
    var stream_reader = stream.reader(ctx.getIo(), reader_buffer[0..]);
    var reader = io.AllocatedReader.init(
        api.VM.allocator,
        &stream_reader.interface,
        if (max_size.isInteger()) @intCast(max_size.integer()) else null,
    );

    if (reader.readUntilDelimiterOrEof('\n') catch |err| {
        switch (err) {
            error.ReadFailed => {
                ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.WriteFailed, error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    }) |ubuffer| {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (ubuffer.len > 0) @as([*]const u8, @ptrCast(ubuffer)) else null,
                ubuffer.len,
            ),
        );
    } else {
        ctx.vm.bz_push(api.Value.Null);
    }

    return 1;
}

pub export fn SocketReadAll(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
    else
        @intCast(
            ctx.vm.bz_peek(1).integer(),
        );
    const max_size = ctx.vm.bz_peek(0);

    const stream = std.Io.net.Stream{
        .socket = .{
            .address = .{ .ip4 = .unspecified(0) },
            .handle = handle,
        },
    };
    var reader_buffer = [_]u8{0};
    var stream_reader = stream.reader(ctx.getIo(), reader_buffer[0..]);
    var reader = io.AllocatedReader.init(
        api.VM.allocator,
        &stream_reader.interface,
        if (max_size.isInteger()) @intCast(max_size.integer()) else null,
    );

    const content = reader.readAll() catch |err| {
        switch (err) {
            error.ReadFailed => {
                ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.WriteFailed, error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    };

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(
            ctx.vm,
            if (content.len > 0) @as([*]const u8, @ptrCast(content)) else null,
            content.len,
        ),
    );

    return 1;
}

pub export fn SocketWrite(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
    else
        @intCast(
            ctx.vm.bz_peek(1).integer(),
        );

    const stream = std.Io.net.Stream{
        .socket = .{
            .address = .{ .ip4 = .unspecified(0) },
            .handle = handle,
        },
    };

    var len: usize = 0;
    var value = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    var writer = stream.writer(ctx.getIo(), &.{});

    _ = writer.interface.write(value.?[0..len]) catch |err| {
        switch (err) {
            error.WriteFailed,
            => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),
        }

        return -1;
    };

    return 0;
}

pub export fn SocketServerStart(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(ctx.vm.bz_peek(3), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?api.Integer = ctx.vm.bz_peek(2).integer();
    if (port == null or port.? < 0) {
        ctx.vm.pushError("errors.InvalidArgumentError", null);

        return -1;
    }

    const reuse_address: bool = ctx.vm.bz_peek(0).boolean();

    const resolved_address = std.Io.net.IpAddress.parse(
        address,
        @intCast(port.?),
    ) catch {
        ctx.vm.pushError("errors.InvalidArgumentError", null);

        return -1;
    };

    const server = resolved_address.listen(
        ctx.getIo(),
        .{
            .reuse_address = reuse_address,
        },
    ) catch |err| {
        switch (err) {
            error.Canceled,
            error.SystemResources,
            error.ProcessFdQuotaExceeded,
            error.SystemFdQuotaExceeded,
            error.NetworkDown,
            error.AddressInUse,
            error.AddressUnavailable,
            error.ProtocolUnsupportedBySystem,
            error.ProtocolUnsupportedByAddressFamily,
            error.SocketModeUnsupported,
            error.OptionUnsupported,
            error.AddressFamilyUnsupported,
            => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        }

        return -1;
    };

    ctx.vm.bz_push(
        api.Value.fromInteger(
            if (builtin.os.tag == .windows)
                @intCast(@intFromPtr(server.socket.handle))
            else
                @intCast(server.socket.handle),
        ),
    );

    return 1;
}

pub export fn SocketServerAccept(ctx: *api.NativeCtx) callconv(.c) c_int {
    var server = std.Io.net.Server{
        .options = if (builtin.os.tag == .windows) .{
            .mode = .stream,
            .protocol = .tcp,
        } else {},
        .socket = .{
            .address = .{ .ip4 = .unspecified(0) }, // FIXME: we lose this
            .handle = if (builtin.os.tag == .windows)
                @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(0).integer())))
            else
                @intCast(
                    ctx.vm.bz_peek(0).integer(),
                ),
        },
    };

    const connection = server.accept(ctx.getIo()) catch |err| {
        switch (err) {
            error.ConnectionAborted,
            error.ProcessFdQuotaExceeded,
            error.SystemFdQuotaExceeded,
            error.SystemResources,
            error.SocketNotListening,
            error.ProtocolFailure,
            error.BlockedByFirewall,
            error.Canceled,
            error.NetworkDown,
            => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),
            error.WouldBlock => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        }

        return -1;
    };

    ctx.vm.bz_push(
        api.Value.fromInteger(
            if (builtin.os.tag == .windows)
                @intCast(@intFromPtr(connection.socket.handle))
            else
                @intCast(connection.socket.handle),
        ),
    );

    return 1;
}
