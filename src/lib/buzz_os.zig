const std = @import("std");
const api = @import("buzz_api.zig");
const builtin = @import("builtin");

pub export fn sleep(ctx: *api.NativeCtx) callconv(.c) c_int {
    std.time.sleep(@as(u64, @intFromFloat(ctx.vm.bz_peek(0).double())) * 1_000_000);

    return 0;
}

pub export fn time(ctx: *api.NativeCtx) callconv(.c) c_int {
    ctx.vm.bz_push(api.Value.fromDouble(@as(api.Double, @floatFromInt(std.time.milliTimestamp()))));

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
    if (std.process.getEnvVarOwned(api.VM.allocator, key_slice) catch |err| env: {
        switch (err) {
            error.EnvironmentVariableNotFound, error.InvalidWtf8 => break :env null,
            else => @panic("Out of memory"),
        }
    }) |value| {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (value.len > 0) @as([*]const u8, @ptrCast(value)) else null,
                value.len,
            ),
        );

        api.VM.allocator.free(value);

        return 1;
    }

    ctx.vm.bz_push(api.Value.Null);

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

    var random_part = std.ArrayList(u8).init(api.VM.allocator);
    defer random_part.deinit();
    random_part.writer().print("{x}", .{std.crypto.random.int(api.Integer)}) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    var random_part_b64 = std.ArrayList(u8).initCapacity(
        api.VM.allocator,
        std.base64.standard.Encoder.calcSize(random_part.items.len),
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };
    random_part_b64.expandToCapacity();
    defer random_part_b64.deinit();

    _ = std.base64.standard.Encoder.encode(random_part_b64.items, random_part.items);

    var final = std.ArrayList(u8).init(api.VM.allocator);
    defer final.deinit();

    final.writer().print("{s}{s}-{s}", .{ sysTempDir(), prefix_slice, random_part_b64.items }) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(
            ctx.vm,
            if (final.items.len > 0)
                @as([*]const u8, @ptrCast(final.items))
            else
                null,
            final.items.len,
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
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

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
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        },
        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn execute(ctx: *api.NativeCtx) callconv(.c) c_int {
    var command = std.ArrayList([]const u8).init(api.VM.allocator);
    defer command.deinit();

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

        command.append(arg_str.?[0..arg_len]) catch {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
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
        .Exited => ctx.vm.bz_push(api.Value.fromInteger(@intCast(term.Exited))),
        .Signal => ctx.vm.bz_push(api.Value.fromInteger(@intCast(term.Signal))),
        .Stopped => ctx.vm.bz_push(api.Value.fromInteger(@intCast(term.Stopped))),
        .Unknown => ctx.vm.bz_push(api.Value.fromInteger(@intCast(term.Unknown))),
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
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

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
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.BrokenPipe,
        error.NotOpenForReading,
        error.OperationAborted,
        error.LockViolation,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        },
        error.ProcessNotFound => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),
        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        error.Overflow => ctx.vm.pushError("errors.OverflowError", null),
    }
}

fn handleConnectUnixError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AddressFamilyNotSupported,
        error.AddressInUse,
        error.AddressNotAvailable,
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
            const stream = std.net.tcpConnectToHost(
                api.VM.allocator,
                address,
                @as(u16, @intCast(port.?)),
            ) catch |err| {
                handleConnectError(ctx, err);

                return -1;
            };

            ctx.vm.bz_push(
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
            ctx.vm.pushError("errors.NotYetImplementedError", null);

            return -1;
        },
        2 => {
            // Unix socket not available on windows
            if (builtin.os.tag == .windows) {
                ctx.vm.pushError("errors.InvalidArgumentError", null);

                return -1;
            }

            const stream = std.net.connectUnixSocket(address) catch |err| {
                handleConnectUnixError(ctx, err);

                return -1;
            };

            ctx.vm.bz_push(api.Value.fromInteger(@intCast(stream.handle)));

            return 1;
        },
        else => {
            ctx.vm.pushError("errors.InvalidArgumentError", null);

            return -1;
        },
    }
}

pub export fn SocketClose(ctx: *api.NativeCtx) callconv(.c) c_int {
    const socket: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(0).integer())))
    else
        @intCast(
            ctx.vm.bz_peek(0).integer(),
        );

    std.posix.shutdown(socket, .both) catch @panic("Could not stop socket");

    return 0;
}

fn handleReadAllError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.InputOutput,
        error.IsDir,
        error.WouldBlock,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.BrokenPipe,
        error.OperationAborted,
        error.NotOpenForReading,
        error.ConnectionTimedOut,
        // error.StreamTooLong,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.ConnectionResetByPeer,
        error.SystemResources,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        // error.OutOfMemory => {ctx.vm.bz_panic("Out of memory");unreachable;, f memory");unreachable;.len},

        // TODO: bug in zig compiler that complains about StreamTooLong and OutOfMemory errors missing when not there, but complains also if they're there
        else => unreachable,
    }
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

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = api.VM.allocator.alloc(u8, @as(usize, @intCast(n))) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    // bz_stringToValue will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch |err| {
        handleReadAllError(ctx, err);

        return -1;
    };

    if (read == 0) {
        ctx.vm.bz_push(api.Value.Null);
    } else {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (buffer[0..read].len > 0) @as([*]const u8, @ptrCast(buffer[0..read])) else null,
                read,
            ),
        );
    }

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
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Canceled,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.BrokenPipe,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.NotOpenForReading,
        error.OperationAborted,
        error.StreamTooLong,
        error.LockViolation,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.ProcessNotFound,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),

        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        },

        error.EndOfStream => {},
    }
}

pub export fn SocketReadLine(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
    else
        @intCast(
            ctx.vm.bz_peek(1).integer(),
        );
    const max_size = ctx.vm.bz_peek(0);

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    const buffer = reader.readUntilDelimiterAlloc(
        api.VM.allocator,
        '\n',
        if (max_size.isNull())
            std.math.maxInt(usize)
        else
            @intCast(max_size.integer()),
    ) catch |err| {
        if (err == error.StreamTooLong) {
            ctx.vm.pushErrorEnum("errors.ReadWriteError", "StreamTooLong");
        } else {
            handleReadLineError(ctx, err);
        }

        return -1;
    };

    // EOF?
    if (buffer.len == 0) {
        ctx.vm.bz_push(api.Value.Null);
    } else {
        ctx.vm.bz_push(api.VM.bz_stringToValue(
            ctx.vm,
            if (buffer.len > 0)
                @as([*]const u8, @ptrCast(buffer))
            else
                null,
            buffer.len,
        ));
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

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    const buffer = reader.readAllAlloc(
        api.VM.allocator,
        if (max_size.isNull())
            std.math.maxInt(usize)
        else
            @intCast(max_size.integer()),
    ) catch |err| {
        if (err == error.StreamTooLong) {
            ctx.vm.pushErrorEnum("errors.ReadWriteError", "StreamTooLong");
        } else {
            handleReadAllError(ctx, err);
        }

        return -1;
    };

    // EOF?
    if (buffer.len == 0) {
        ctx.vm.bz_push(api.Value.Null);
    } else {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (buffer.len > 0)
                    @as([*]const u8, @ptrCast(buffer))
                else
                    null,
                buffer.len,
            ),
        );
    }

    return 1;
}

pub export fn SocketWrite(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.posix.socket_t = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
    else
        @intCast(
            ctx.vm.bz_peek(1).integer(),
        );

    const stream: std.net.Stream = .{ .handle = handle };

    var len: usize = 0;
    var value = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    _ = stream.write(value.?[0..len]) catch |err| {
        switch (err) {
            error.AccessDenied,
            error.InputOutput,
            error.SystemResources,
            error.WouldBlock,
            error.DiskQuota,
            error.FileTooBig,
            error.NoSpaceLeft,
            error.DeviceBusy,
            error.NoDevice,
            => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),
            error.OperationAborted,
            error.BrokenPipe,
            error.ConnectionResetByPeer,
            error.NotOpenForWriting,
            error.LockViolation,
            => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
            error.InvalidArgument => ctx.vm.pushError("errors.InvalidArgumentError", null),
            error.ProcessNotFound,
            => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

            error.PermissionDenied,
            error.MessageTooBig,
            => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),
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

    const reuse_address: bool = ctx.vm.bz_peek(1).boolean();
    const reuse_port: bool = ctx.vm.bz_peek(0).boolean();

    const resolved_address = std.net.Address.parseIp(
        address,
        @intCast(port.?),
    ) catch {
        ctx.vm.pushError("errors.InvalidArgumentError", null);

        return -1;
    };

    const server = resolved_address.listen(
        .{
            .reuse_address = reuse_address,
            .reuse_port = reuse_port,
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
            => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),
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
            => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        }

        return -1;
    };

    ctx.vm.bz_push(
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
                @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(0).integer())))
            else
                @intCast(
                    ctx.vm.bz_peek(0).integer(),
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
            => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),
            error.WouldBlock => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        }

        return -1;
    };

    ctx.vm.bz_push(
        api.Value.fromInteger(
            if (builtin.os.tag == .windows)
                @intCast(@intFromPtr(connection.stream.handle))
            else
                @intCast(connection.stream.handle),
        ),
    );

    return 1;
}
