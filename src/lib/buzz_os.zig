const std = @import("std");
const api = @import("buzz_api.zig");
const builtin = @import("builtin");
const io = @import("io.zig");

pub export fn sleep(ctx: *api.NativeCtx) callconv(.c) c_int {
    std.Thread.sleep(
        @as(u64, @intFromFloat(api.bz_peek(ctx.vm, 0).double())) * 1_000_000,
    );

    return 0;
}

pub export fn time(ctx: *api.NativeCtx) callconv(.c) c_int {
    api.bz_push(
        ctx.vm,
        .fromDouble(@as(api.Double, @floatFromInt(std.time.milliTimestamp()))),
    );

    return 1;
}

pub export fn env(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const key = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );

    if (len == 0) {
        api.bz_push(ctx.vm, .Null);

        return 1;
    }

    const key_slice = api.VM.allocator.dupeZ(u8, key.?[0..len]) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };
    defer api.VM.allocator.free(key_slice);

    // FIXME: don't use std.posix directly
    if (std.process.getEnvVarOwned(api.VM.allocator, key_slice) catch |err| env: {
        switch (err) {
            error.EnvironmentVariableNotFound, error.InvalidWtf8 => break :env null,
            else => @panic("Out of memory"),
        }
    }) |value| {
        api.bz_push(
            ctx.vm,
            api.bz_stringToValue(
                ctx.vm,
                if (value.len > 0) @as([*]const u8, @ptrCast(value)) else null,
                value.len,
            ),
        );

        api.VM.allocator.free(value);

        return 1;
    }

    api.bz_push(ctx.vm, .Null);

    return 1;
}

fn sysTempDir() []const u8 {
    return switch (builtin.os.tag) {
        .windows => unreachable, // TODO: GetTempPath
        else => std.posix.getenv("TMPDIR") orelse
            std.posix.getenv("TMP") orelse
            std.posix.getenv("TEMP") orelse
            std.posix.getenv("TEMPDIR") orelse
            "/tmp",
    };
}

pub export fn tmpDir(ctx: *api.NativeCtx) callconv(.c) c_int {
    const tmp_dir: []const u8 = sysTempDir();

    api.bz_push(
        ctx.vm,
        api.bz_stringToValue(
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
    const prefix = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &prefix_len,
    );

    const prefix_slice = if (prefix_len == 0) "" else prefix.?[0..prefix_len];

    var random_part = std.Io.Writer.Allocating.init(api.VM.allocator);
    defer random_part.deinit();

    random_part.writer.print("{x}", .{std.crypto.random.int(api.Integer)}) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    var random_part_b64 = std.ArrayList(u8).initCapacity(
        api.VM.allocator,
        std.base64.standard.Encoder.calcSize(random_part.written().len),
    ) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };
    random_part_b64.expandToCapacity();
    defer random_part_b64.deinit(api.VM.allocator);

    _ = std.base64.standard.Encoder.encode(random_part_b64.items, random_part.written());

    var final = std.Io.Writer.Allocating.init(api.VM.allocator);

    final.writer.print(
        "{s}{s}-{s}",
        .{
            sysTempDir(),
            prefix_slice,
            random_part_b64.items,
        },
    ) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    api.bz_push(
        ctx.vm,
        api.bz_stringToValue(
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
    const exitCode: api.Integer = api.bz_peek(ctx.vm, 0).integer();

    std.process.exit(@intCast(exitCode));

    return 0;
}

fn handleSpawnError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.FileBusy,
        error.FileSystem,
        error.InvalidHandle,
        error.InvalidUtf8,
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
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.CurrentWorkingDirectoryUnlinked,
        error.InvalidExe,
        error.InvalidName,
        error.InvalidUserId,
        error.PermissionDenied,
        error.ResourceLimitReached,
        error.WaitAbandoned,
        error.WaitTimeOut,
        error.ProcessAlreadyExec,
        error.InvalidProcessGroupId,
        error.ProcessNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },
        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
    }
}

pub export fn execute(ctx: *api.NativeCtx) callconv(.c) c_int {
    var command = std.ArrayList([]const u8).empty;
    defer command.deinit(api.VM.allocator);

    const argv = api.bz_peek(ctx.vm, 0);
    const len = api.bz_listLen(ctx.vm, argv);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        const arg = api.bz_listGet(
            ctx.vm,
            argv,
            @intCast(i),
            false,
        );
        var arg_len: usize = 0;
        var arg_str = api.bz_valueToString(ctx.vm, arg, &arg_len);

        std.debug.assert(arg_len > 0);

        command.append(api.VM.allocator, arg_str.?[0..arg_len]) catch {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        };
    }

    var child_process = std.process.Child.init(command.items, api.VM.allocator);
    child_process.disable_aslr = builtin.target.os.tag.isDarwin();

    const term = child_process.spawnAndWait() catch |err| {
        handleSpawnError(ctx, err);

        return -1;
    };

    switch (term) {
        .Exited => api.bz_push(ctx.vm, .fromInteger(@intCast(term.Exited))),
        .Signal => api.bz_push(ctx.vm, .fromInteger(@intCast(term.Signal))),
        .Stopped => api.bz_push(ctx.vm, .fromInteger(@intCast(term.Stopped))),
        .Unknown => api.bz_push(ctx.vm, .fromInteger(@intCast(term.Unknown))),
    }

    return 1;
}

fn handleConnectError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.AntivirusInterference,
        error.AddressFamilyNotSupported,
        error.AddressInUse,
        error.AddressNotAvailable,
        error.AlreadyBound,
        error.AlreadyConnected,
        error.ConnectionPending,
        error.ConnectionRefused,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.FileDescriptorNotASocket,
        error.HostLacksNetworkAddresses,
        error.Incomplete,
        error.InterfaceNotFound,
        error.InvalidCharacter,
        error.InvalidEnd,
        error.InvalidIPAddressFormat,
        error.InvalidIpv4Mapping,
        error.InvalidProtocolOption,
        error.NameServerFailure,
        error.NetworkSubsystemFailed,
        error.NetworkUnreachable,
        error.PermissionDenied,
        error.ProcessFdQuotaExceeded,
        error.ProtocolFamilyNotAvailable,
        error.ProtocolNotSupported,
        error.ServiceUnavailable,
        error.SocketNotBound,
        error.SocketTypeNotSupported,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.TemporaryNameServerFailure,
        error.TimeoutTooBig,
        error.UnknownHostName,
        error.WouldBlock,
        error.Canceled,
        error.OperationNotSupported,
        error.ResolveConfParseFailed,
        => api.pushErrorEnum(ctx.vm, "errors.SocketError", @errorName(err)),

        error.BadPathName,
        error.DeviceBusy,
        error.FileBusy,
        error.FileLocksNotSupported,
        error.FileNotFound,
        error.FileSystem,
        error.FileTooBig,
        error.InputOutput,
        error.InvalidUtf8,
        error.IsDir,
        error.NameTooLong,
        error.NoDevice,
        error.NonCanonical,
        error.NoSpaceLeft,
        error.NotDir,
        error.PathAlreadyExists,
        error.PipeBusy,
        error.ReadOnlyFileSystem,
        error.SharingViolation,
        error.SymLinkLoop,
        error.InvalidWtf8,
        error.NetworkNotFound,
        error.SocketNotConnected,
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.BrokenPipe,
        error.NotOpenForReading,
        error.OperationAborted,
        error.LockViolation,
        => api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err)),

        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },
        error.ProcessNotFound => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),
        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
        error.Overflow => api.pushError(ctx.vm, "errors.OverflowError", null),
    }
}

fn handleConnectUnixError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AddressFamilyNotSupported,
        error.AddressInUse,
        error.AddressNotAvailable,
        error.AlreadyConnected,
        error.ConnectionPending,
        error.ConnectionRefused,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.PermissionDenied,
        error.ProcessFdQuotaExceeded,
        error.ProtocolFamilyNotAvailable,
        error.ProtocolNotSupported,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.SocketTypeNotSupported,
        error.FileNotFound,
        error.WouldBlock,
        error.NetworkUnreachable,
        error.NameTooLong,
        => api.pushErrorEnum(ctx.vm, "errors.SocketError", @errorName(err)),

        error.AccessDenied,
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
    }
}

pub export fn SocketConnect(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const address_value = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 2),
        &len,
    );
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?api.Integer = api.bz_peek(ctx.vm, 1).integer();
    if (port == null or port.? < 0) {
        api.pushError(ctx.vm, "errors.InvalidArgumentError", null);

        return -1;
    }

    const protocol = api.bz_peek(ctx.vm, 0).integer();

    switch (protocol) {
        0 => {
            const stream = std.net.tcpConnectToHost(
                api.VM.allocator,
                address,
                @as(u16, @intCast(port.?)),
            ) catch |err| {
                handleConnectError(ctx, err);

                return -1;
            };

            api.bz_push(
                ctx.vm,
                api.Value.fromInteger(
                    if (builtin.os.tag == .windows)
                        @intCast(@intFromPtr(stream.handle))
                    else
                        @intCast(stream.handle),
                ),
            );

            return 1;
        },
        1, // TODO: UDP
        => {
            api.pushError(ctx.vm, "errors.NotYetImplementedError", null);

            return -1;
        },
        2 => {
            // Unix socket not available on windows
            if (builtin.os.tag == .windows) {
                api.pushError(ctx.vm, "errors.InvalidArgumentError", null);

                return -1;
            }

            const stream = std.net.connectUnixSocket(address) catch |err| {
                handleConnectUnixError(ctx, err);

                return -1;
            };

            api.bz_push(ctx.vm, api.Value.fromInteger(@intCast(stream.handle)));

            return 1;
        },
        else => {
            api.pushError(ctx.vm, "errors.InvalidArgumentError", null);

            return -1;
        },
    }
}

pub export fn SocketClose(ctx: *api.NativeCtx) callconv(.c) c_int {
    const socket: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(api.bz_peek(ctx.vm, 0).integer())))
    else
        @intCast(
            api.bz_peek(ctx.vm, 0).integer(),
        );

    std.posix.shutdown(socket, .both) catch @panic("Could not stop socket");

    return 0;
}

pub export fn SocketRead(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n: api.Integer = api.bz_peek(ctx.vm, 0).integer();
    if (n < 0) {
        api.pushError(ctx.vm, "errors.InvalidArgumentError", null);

        return -1;
    }

    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(api.bz_peek(ctx.vm, 1).integer())))
    else
        @intCast(
            api.bz_peek(ctx.vm, 1).integer(),
        );

    const stream: std.net.Stream = .{ .handle = handle };
    var reader_buffer: [1024]u8 = undefined;
    var stream_reader = stream.reader(reader_buffer[0..]);
    var reader = stream_reader.interface();

    var content = std.Io.Writer.Allocating.init(api.VM.allocator);
    defer content.deinit();

    while (content.written().len <= n) {
        const byte = reader.takeByte() catch |err| {
            switch (err) {
                error.EndOfStream => break,
                error.ReadFailed => {
                    api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                    return -1;
                },
            }
        };

        content.writer.writeByte(byte) catch |err| {
            api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
            return -1;
        };
    }

    api.bz_push(
        ctx.vm,
        api.bz_stringToValue(
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

fn handleReadLineError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.InputOutput,
        error.IsDir,
        error.SystemResources,
        error.WouldBlock,
        error.SocketNotConnected,
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.Canceled,
        => api.pushErrorEnum(ctx.vm, "errors.SocketError", @errorName(err)),

        error.BrokenPipe,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.NotOpenForReading,
        error.OperationAborted,
        error.StreamTooLong,
        error.LockViolation,
        => api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err)),

        error.ProcessNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),

        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },

        error.EndOfStream => {},
    }
}

pub export fn SocketReadLine(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(api.bz_peek(ctx.vm, 1).integer())))
    else
        @intCast(
            api.bz_peek(ctx.vm, 1).integer(),
        );
    const max_size = api.bz_peek(ctx.vm, 0);

    const stream: std.net.Stream = .{ .handle = handle };
    var reader_buffer = [_]u8{0};
    var stream_reader = stream.reader(reader_buffer[0..]);
    var reader = io.AllocatedReader.init(
        api.VM.allocator,
        stream_reader.interface(),
        if (max_size.isInteger()) @intCast(max_size.integer()) else null,
    );

    if (reader.readUntilDelimiterOrEof('\n') catch |err| {
        switch (err) {
            error.ReadFailed => {
                api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.WriteFailed, error.OutOfMemory => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    }) |ubuffer| {
        api.bz_push(
            ctx.vm,
            api.bz_stringToValue(
                ctx.vm,
                if (ubuffer.len > 0) @as([*]const u8, @ptrCast(ubuffer)) else null,
                ubuffer.len,
            ),
        );
    } else {
        api.bz_push(ctx.vm, api.Value.Null);
    }

    return 1;
}

pub export fn SocketReadAll(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(api.bz_peek(ctx.vm, 1).integer())))
    else
        @intCast(
            api.bz_peek(ctx.vm, 1).integer(),
        );
    const max_size = api.bz_peek(ctx.vm, 0);

    const stream = std.net.Stream{ .handle = handle };
    var reader_buffer = [_]u8{0};
    var stream_reader = stream.reader(reader_buffer[0..]);
    var reader = io.AllocatedReader.init(
        api.VM.allocator,
        stream_reader.interface(),
        if (max_size.isInteger()) @intCast(max_size.integer()) else null,
    );

    const content = reader.readAll() catch |err| {
        switch (err) {
            error.ReadFailed => {
                api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.WriteFailed, error.OutOfMemory => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    };

    api.bz_push(
        ctx.vm,
        api.bz_stringToValue(
            ctx.vm,
            if (content.len > 0) @as([*]const u8, @ptrCast(content)) else null,
            content.len,
        ),
    );

    return 1;
}

pub export fn SocketWrite(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(api.bz_peek(ctx.vm, 1).integer())))
    else
        @intCast(
            api.bz_peek(ctx.vm, 1).integer(),
        );

    const stream: std.net.Stream = .{ .handle = handle };

    var len: usize = 0;
    var value = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );

    if (len == 0) {
        return 0;
    }

    _ = stream.write(value.?[0..len]) catch |err| {
        switch (err) {
            error.SocketNotConnected,
            error.FileNotFound,
            error.NameTooLong,
            error.SymLinkLoop,
            error.NotDir,
            error.NetworkSubsystemFailed,
            error.FileDescriptorNotASocket,
            error.FastOpenAlreadyInProgress,
            error.AccessDenied,
            error.SystemResources,
            error.WouldBlock,
            => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),
            error.BrokenPipe,
            error.ConnectionResetByPeer,
            => api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err)),
            error.Unexpected,
            => api.pushError(ctx.vm, "errors.UnexpectedError", null),
            error.AddressNotAvailable,
            error.AddressFamilyNotSupported,
            error.MessageTooBig,
            error.SocketNotBound,
            error.ConnectionRefused,
            error.NetworkUnreachable,
            => api.pushErrorEnum(ctx.vm, "errors.SocketError", @errorName(err)),
        }

        return -1;
    };

    return 0;
}

pub export fn SocketServerStart(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const address_value = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 3),
        &len,
    );
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?api.Integer = api.bz_peek(ctx.vm, 2).integer();
    if (port == null or port.? < 0) {
        api.pushError(ctx.vm, "errors.InvalidArgumentError", null);

        return -1;
    }

    const reuse_address: bool = api.bz_peek(ctx.vm, 0).boolean();

    const resolved_address = std.net.Address.parseIp(
        address,
        @intCast(port.?),
    ) catch {
        api.pushError(ctx.vm, "errors.InvalidArgumentError", null);

        return -1;
    };

    const server = resolved_address.listen(
        .{
            .reuse_address = reuse_address,
        },
    ) catch |err| {
        switch (err) {
            error.NoDevice,
            error.SymLinkLoop,
            error.NameTooLong,
            error.FileNotFound,
            error.NotDir,
            error.ReadOnlyFileSystem,
            error.AccessDenied,
            error.SystemResources,
            => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),
            error.AlreadyConnected,
            error.SocketNotBound,
            error.AddressNotAvailable,
            error.AlreadyBound,
            error.InvalidProtocolOption,
            error.TimeoutTooBig,
            error.PermissionDenied,
            error.ProtocolFamilyNotAvailable,
            error.ProcessFdQuotaExceeded,
            error.SystemFdQuotaExceeded,
            error.ProtocolNotSupported,
            error.SocketTypeNotSupported,
            error.AddressInUse,
            error.FileDescriptorNotASocket,
            error.OperationNotSupported,
            error.NetworkSubsystemFailed,
            error.AddressFamilyNotSupported,
            => api.pushErrorEnum(ctx.vm, "errors.SocketError", @errorName(err)),
            error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
        }

        return -1;
    };

    api.bz_push(
        ctx.vm,
        api.Value.fromInteger(
            if (builtin.os.tag == .windows)
                @intCast(@intFromPtr(server.stream.handle))
            else
                @intCast(server.stream.handle),
        ),
    );

    return 1;
}

pub export fn SocketServerAccept(ctx: *api.NativeCtx) callconv(.c) c_int {
    var server = std.net.Server{
        .listen_address = undefined, // FIXME: we lose this
        .stream = std.net.Stream{
            .handle = if (builtin.os.tag == .windows)
                @ptrFromInt(@as(usize, @intCast(api.bz_peek(ctx.vm, 0).integer())))
            else
                @intCast(
                    api.bz_peek(ctx.vm, 0).integer(),
                ),
        },
    };

    const connection = server.accept() catch |err| {
        switch (err) {
            error.ConnectionAborted,
            error.ProcessFdQuotaExceeded,
            error.SystemFdQuotaExceeded,
            error.SystemResources,
            error.SocketNotListening,
            error.ProtocolFailure,
            error.BlockedByFirewall,
            error.FileDescriptorNotASocket,
            error.ConnectionResetByPeer,
            error.NetworkSubsystemFailed,
            error.OperationNotSupported,
            => api.pushErrorEnum(ctx.vm, "errors.SocketError", @errorName(err)),
            error.WouldBlock => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),
            error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
        }

        return -1;
    };

    api.bz_push(
        ctx.vm,
        api.Value.fromInteger(
            if (builtin.os.tag == .windows)
                @intCast(@intFromPtr(connection.stream.handle))
            else
                @intCast(connection.stream.handle),
        ),
    );

    return 1;
}
