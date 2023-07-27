const std = @import("std");
const api = @import("./buzz_api.zig");
const builtin = @import("builtin");

export fn sleep(ctx: *api.NativeCtx) c_int {
    std.time.sleep(@as(u64, @intFromFloat(ctx.vm.bz_peek(0).float())) * 1_000_000);

    return 0;
}

export fn time(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(api.Value.fromFloat(@as(f64, @floatFromInt(std.time.milliTimestamp()))));

    return 1;
}

export fn env(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const key = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        ctx.vm.pushError("errors.InvalidArgumentError");

        return -1;
    }

    const key_slice = api.VM.allocator.dupeZ(u8, key.?[0..len]) catch null;
    defer {
        if (key_slice != null) {
            api.VM.allocator.free(key_slice.?);
        }
    }

    if (key_slice == null) {
        ctx.vm.pushError("errors.InvalidArgumentError");

        return -1;
    }

    if (std.os.getenvZ(key_slice.?)) |value| {
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (value.len > 0) @as([*]const u8, @ptrCast(value)) else null, value.len) orelse {
            ctx.vm.pushError("errors.OutOfMemoryError");

            return -1;
        });

        return 1;
    }

    ctx.vm.bz_pushNull();

    return 1;
}

fn sysTempDir() []const u8 {
    return switch (builtin.os.tag) {
        .windows => unreachable, // TODO: GetTempPath
        else => std.os.getenv("TMPDIR") orelse std.os.getenv("TMP") orelse std.os.getenv("TEMP") orelse std.os.getenv("TEMPDIR") orelse "/tmp",
    };
}

export fn tmpDir(ctx: *api.NativeCtx) c_int {
    const tmp_dir: []const u8 = sysTempDir();

    ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (tmp_dir.len > 0) @as([*]const u8, @ptrCast(tmp_dir)) else null, tmp_dir.len) orelse {
        ctx.vm.pushError("errors.OutOfMemoryError");

        return -1;
    });

    return 1;
}

// TODO: what if file with same random name exists already?
export fn tmpFilename(ctx: *api.NativeCtx) c_int {
    var prefix_len: usize = 0;
    const prefix = ctx.vm.bz_peek(0).bz_valueToString(&prefix_len);

    const prefix_slice = if (prefix_len == 0) "" else prefix.?[0..prefix_len];

    var random_part = std.ArrayList(u8).init(api.VM.allocator);
    defer random_part.deinit();
    random_part.writer().print("{x}", .{std.crypto.random.int(i32)}) catch {
        ctx.vm.pushError("errors.OutOfMemoryError");

        return -1;
    };

    var random_part_b64 = std.ArrayList(u8).initCapacity(api.VM.allocator, std.base64.standard.Encoder.calcSize(random_part.items.len)) catch {
        ctx.vm.pushError("errors.OutOfMemoryError");

        return -1;
    };
    random_part_b64.expandToCapacity();
    defer random_part_b64.deinit();

    _ = std.base64.standard.Encoder.encode(random_part_b64.items, random_part.items);

    var final = std.ArrayList(u8).init(api.VM.allocator);
    defer final.deinit();

    final.writer().print("{s}{s}-{s}", .{ sysTempDir(), prefix_slice, random_part_b64.items }) catch {
        ctx.vm.pushError("errors.OutOfMemoryError");

        return -1;
    };

    ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (final.items.len > 0) @as([*]const u8, @ptrCast(final.items)) else null, final.items.len) orelse {
        ctx.vm.pushError("errors.OutOfMemoryError");

        return -1;
    });

    return 1;
}

// If it was named `exit` it would be considered by zig as a callback when std.os.exit is called
export fn buzzExit(ctx: *api.NativeCtx) c_int {
    const exitCode: i32 = ctx.vm.bz_peek(0).integer();

    std.os.exit(@intCast(exitCode));

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
        error.IsDir,
        error.NameTooLong,
        error.NoDevice,
        error.NotDir,
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
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.OutOfMemory => ctx.vm.pushError("errors.OutOfMemoryError"),
        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
    }
}

export fn execute(ctx: *api.NativeCtx) c_int {
    var command = std.ArrayList([]const u8).init(api.VM.allocator);
    defer command.deinit();

    const argv_value = ctx.vm.bz_peek(0);
    const argv = api.ObjList.bz_valueToList(argv_value);
    const len = argv.bz_listLen();
    var i: usize = 0;
    while (i < len) : (i += 1) {
        const arg = api.ObjList.bz_listGet(argv_value, i);
        var arg_len: usize = 0;
        var arg_str = arg.bz_valueToString(&arg_len);

        std.debug.assert(arg_len > 0);

        command.append(arg_str.?[0..arg_len]) catch {
            ctx.vm.pushError("errors.OutOfMemoryError");

            return -1;
        };
    }

    var child_process = std.ChildProcess.init(command.items, api.VM.allocator);
    child_process.disable_aslr = builtin.target.isDarwin();

    child_process.spawn() catch |err| {
        handleSpawnError(ctx, err);

        return -1;
    };

    ctx.vm.bz_pushInteger(@intCast((child_process.wait() catch |err| {
        handleSpawnError(ctx, err);

        return -1;
    }).Exited));

    return 1;
}

fn handleConnectError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
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
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.BadPathName,
        error.DeviceBusy,
        error.FileBusy,
        error.FileLocksNotSupported,
        error.FileNotFound,
        error.FileSystem,
        error.FileTooBig,
        error.InputOutput,
        error.InvalidHandle,
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
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.BrokenPipe,
        error.NetNameDeleted,
        error.NotOpenForReading,
        error.OperationAborted,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.OutOfMemory => ctx.vm.pushError("errors.OutOfMemoryError"),
        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
        error.Overflow => ctx.vm.pushError("errors.OverflowError"),
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

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
    }
}

export fn SocketConnect(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(ctx.vm.bz_peek(2), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?i32 = ctx.vm.bz_peek(1).integer();
    if (port == null or port.? < 0) {
        ctx.vm.pushError("errors.InvalidArgumentError");

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

            ctx.vm.bz_pushInteger(@intCast(stream.handle));

            return 1;
        },
        1, // TODO: UDP
        => {
            ctx.vm.pushError("errors.NotYetImplementedError");

            return -1;
        },
        2 => {
            const stream = std.net.connectUnixSocket(address) catch |err| {
                handleConnectUnixError(ctx, err);

                return -1;
            };

            ctx.vm.bz_pushInteger(@intCast(stream.handle));

            return 1;
        },
        else => {
            ctx.vm.pushError("errors.InvalidArgumentError");

            return -1;
        },
    }
}

export fn SocketClose(ctx: *api.NativeCtx) c_int {
    const socket: std.os.socket_t = @intCast(
        ctx.vm.bz_peek(0).integer(),
    );

    std.os.closeSocket(socket);

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
        error.NetNameDeleted,
        // error.StreamTooLong,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.ConnectionResetByPeer,
        error.SystemResources,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
        // error.OutOfMemory => ctx.vm.pushError("errors.OutOfMemoryError"),

        // TODO: bug in zig compiler that complains about StreamTooLong and OutOfMemory errors missing when not there, but complains also if they're there
        else => unreachable,
    }
}

export fn SocketRead(ctx: *api.NativeCtx) c_int {
    const n: i32 = ctx.vm.bz_peek(0).integer();
    if (n < 0) {
        ctx.vm.pushError("errors.InvalidArgumentError");

        return -1;
    }

    const handle: std.os.socket_t = @intCast(
        ctx.vm.bz_peek(1).integer(),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = api.VM.allocator.alloc(u8, @as(usize, @intCast(n))) catch {
        ctx.vm.pushError("errors.OutOfMemoryError");

        return -1;
    };

    // bz_string will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch |err| {
        handleReadAllError(ctx, err);

        return -1;
    };

    if (read == 0) {
        ctx.vm.bz_pushNull();
    } else {
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (buffer[0..read].len > 0) @as([*]const u8, @ptrCast(buffer[0..read])) else null, read) orelse {
            ctx.vm.pushError("errors.OutOfMemoryError");

            return -1;
        });
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
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.BrokenPipe,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.NetNameDeleted,
        error.NotOpenForReading,
        error.OperationAborted,
        error.StreamTooLong,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
        error.OutOfMemory => ctx.vm.pushError("errors.OutOfMemoryError"),

        error.EndOfStream => {},
    }
}

export fn SocketReadLine(ctx: *api.NativeCtx) c_int {
    const handle: std.os.socket_t = @intCast(
        ctx.vm.bz_peek(0).integer(),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = reader.readUntilDelimiterAlloc(api.VM.allocator, '\n', 16 * 8 * 64) catch |err| {
        handleReadLineError(ctx, err);

        return -1;
    };

    // EOF?
    if (buffer.len == 0) {
        ctx.vm.bz_pushNull();
    } else {
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (buffer.len > 0) @as([*]const u8, @ptrCast(buffer)) else null, buffer.len) orelse {
            ctx.vm.pushError("errors.OutOfMemoryError");

            return -1;
        });
    }

    return 1;
}

export fn SocketReadAll(ctx: *api.NativeCtx) c_int {
    const handle: std.os.socket_t = @intCast(
        ctx.vm.bz_peek(0).integer(),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = reader.readAllAlloc(api.VM.allocator, 16 * 8 * 64) catch |err| {
        handleReadAllError(ctx, err);

        return -1;
    };

    // EOF?
    if (buffer.len == 0) {
        ctx.vm.bz_pushNull();
    } else {
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (buffer.len > 0) @as([*]const u8, @ptrCast(buffer)) else null, buffer.len) orelse {
            ctx.vm.pushError("errors.OutOfMemoryError");

            return -1;
        });
    }

    return 1;
}

export fn SocketWrite(ctx: *api.NativeCtx) c_int {
    const handle: std.os.socket_t = @intCast(
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
            error.AccessDenied => ctx.vm.pushErrorEnum("errors.FileSystemError", "AccessDenied"),
            error.InputOutput => ctx.vm.pushErrorEnum("errors.FileSystemError", "InputOutput"),
            error.SystemResources => ctx.vm.pushErrorEnum("errors.FileSystemError", "SystemResources"),
            error.WouldBlock => ctx.vm.pushErrorEnum("errors.FileSystemError", "WouldBlock"),
            error.OperationAborted => ctx.vm.pushErrorEnum("errors.ReadWriteError", "OperationAborted"),
            error.BrokenPipe => ctx.vm.pushErrorEnum("errors.ReadWriteError", "BrokenPipe"),
            error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("errors.ReadWriteError", "ConnectionResetByPeer"),
            error.DiskQuota => ctx.vm.pushErrorEnum("errors.FileSystemError", "DiskQuota"),
            error.FileTooBig => ctx.vm.pushErrorEnum("errors.FileSystemError", "FileTooBig"),
            error.NoSpaceLeft => ctx.vm.pushErrorEnum("errors.FileSystemError", "NoSpaceLeft"),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
            error.NotOpenForWriting => ctx.vm.pushErrorEnum("errors.ReadWriteError", "NotOpenForWriting"),
            error.LockViolation => ctx.vm.pushErrorEnum("errors.ReadWriteError", "LockViolation"),
            error.DeviceBusy => ctx.vm.pushErrorEnum("errors.FileSystemError", "DeviceBusy"),
            error.InvalidArgument => ctx.vm.pushError("errors.InvalidArgumentError"),
        }

        return -1;
    };

    return 0;
}

export fn SocketServerStart(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(ctx.vm.bz_peek(3), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?i32 = ctx.vm.bz_peek(2).integer();
    if (port == null or port.? < 0) {
        ctx.vm.pushError("errors.InvalidArgumentError");

        return -1;
    }

    const reuse_address: bool = ctx.vm.bz_peek(1).boolean();
    const reuse_port: bool = ctx.vm.bz_peek(0).boolean();

    var server = std.net.StreamServer.init(.{
        .reuse_address = reuse_address,
        .reuse_port = reuse_port,
    });

    const list = std.net.getAddressList(api.VM.allocator, address, @as(u16, @intCast(port.?))) catch |err| {
        switch (err) {
            error.ServiceUnavailable => ctx.vm.pushErrorEnum("errors.SocketError", "ServiceUnavailable"),
            error.UnknownHostName => ctx.vm.pushErrorEnum("errors.SocketError", "UnknownHostName"),
            error.NameServerFailure => ctx.vm.pushErrorEnum("errors.SocketError", "NameServerFailure"),
            error.TemporaryNameServerFailure => ctx.vm.pushErrorEnum("errors.SocketError", "TemporaryNameServerFailure"),
            error.HostLacksNetworkAddresses => ctx.vm.pushErrorEnum("errors.SocketError", "HostLacksNetworkAddresses"),
            error.AddressFamilyNotSupported => ctx.vm.pushErrorEnum("errors.SocketError", "AddressFamilyNotSupported"),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
            error.OutOfMemory => ctx.vm.pushError("errors.OutOfMemoryError"),
            error.AccessDenied => ctx.vm.pushErrorEnum("errors.FileSystemError", "AccessDenied"),
            error.AddressInUse => ctx.vm.pushErrorEnum("errors.SocketError", "AddressInUse"),
            error.AddressNotAvailable => ctx.vm.pushErrorEnum("errors.SocketError", "AddressNotAvailable"),
            error.AlreadyBound => ctx.vm.pushErrorEnum("errors.SocketError", "AlreadyBound"),
            error.AlreadyConnected => ctx.vm.pushErrorEnum("errors.SocketError", "AlreadyConnected"),
            error.BadPathName => ctx.vm.pushErrorEnum("errors.FileSystemError", "BadPathName"),
            error.BrokenPipe => ctx.vm.pushErrorEnum("errors.ReadWriteError", "BrokenPipe"),
            error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("errors.SocketError", "ConnectionResetByPeer"),
            error.ConnectionTimedOut => ctx.vm.pushErrorEnum("errors.SocketError", "ConnectionTimedOut"),
            error.DeviceBusy => ctx.vm.pushErrorEnum("errors.FileSystemError", "DeviceBusy"),
            error.FileBusy => ctx.vm.pushErrorEnum("errors.FileSystemError", "FileBusy"),
            error.FileDescriptorNotASocket => ctx.vm.pushErrorEnum("errors.SocketError", "FileDescriptorNotASocket"),
            error.FileLocksNotSupported => ctx.vm.pushErrorEnum("errors.FileSystemError", "FileLocksNotSupported"),
            error.FileNotFound => ctx.vm.pushErrorEnum("errors.FileSystemError", "FileNotFound"),
            error.FileSystem => ctx.vm.pushErrorEnum("errors.FileSystemError", "FileSystem"),
            error.FileTooBig => ctx.vm.pushErrorEnum("errors.FileSystemError", "FileTooBig"),
            error.Incomplete => ctx.vm.pushErrorEnum("errors.SocketError", "Incomplete"),
            error.InputOutput => ctx.vm.pushErrorEnum("errors.FileSystemError", "InputOutput"),
            error.InterfaceNotFound => ctx.vm.pushErrorEnum("errors.SocketError", "InterfaceNotFound"),
            error.InvalidCharacter => ctx.vm.pushErrorEnum("errors.SocketError", "InvalidCharacter"),
            error.InvalidEnd => ctx.vm.pushErrorEnum("errors.SocketError", "InvalidEnd"),
            error.InvalidHandle => ctx.vm.pushErrorEnum("errors.FileSystemError", "InvalidHandle"),
            error.InvalidIPAddressFormat => ctx.vm.pushErrorEnum("errors.SocketError", "InvalidIPAddressFormat"),
            error.InvalidIpv4Mapping => ctx.vm.pushErrorEnum("errors.SocketError", "InvalidIpv4Mapping"),
            error.InvalidProtocolOption => ctx.vm.pushErrorEnum("errors.SocketError", "InvalidProtocolOption"),
            error.InvalidUtf8 => ctx.vm.pushErrorEnum("errors.FileSystemError", "InvalidUtf8"),
            error.IsDir => ctx.vm.pushErrorEnum("errors.FileSystemError", "IsDir"),
            error.NameTooLong => ctx.vm.pushErrorEnum("errors.FileSystemError", "NameTooLong"),
            error.NetNameDeleted => ctx.vm.pushErrorEnum("errors.ReadWriteError", "NetNameDeleted"),
            error.NetworkSubsystemFailed => ctx.vm.pushErrorEnum("errors.SocketError", "NetworkSubsystemFailed"),
            error.NoDevice => ctx.vm.pushErrorEnum("errors.FileSystemError", "NoDevice"),
            error.NoSpaceLeft => ctx.vm.pushErrorEnum("errors.FileSystemError", "NoSpaceLeft"),
            error.NonCanonical => ctx.vm.pushErrorEnum("errors.SocketError", "NonCanonical"),
            error.NotDir => ctx.vm.pushErrorEnum("errors.FileSystemError", "NotDir"),
            error.NotOpenForReading => ctx.vm.pushErrorEnum("errors.ReadWriteError", "NotOpenForReading"),
            error.OperationAborted => ctx.vm.pushErrorEnum("errors.ReadWriteError", "OperationAborted"),
            error.Overflow => ctx.vm.pushError("errors.OverflowError"),
            error.PathAlreadyExists => ctx.vm.pushErrorEnum("errors.FileSystemError", "PathAlreadyExists"),
            error.PermissionDenied => ctx.vm.pushErrorEnum("errors.SocketError", "PermissionDenied"),
            error.PipeBusy => ctx.vm.pushErrorEnum("errors.FileSystemError", "PipeBusy"),
            error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("errors.FileSystemError", "ProcessFdQuotaExceeded"),
            error.ProtocolFamilyNotAvailable => ctx.vm.pushErrorEnum("errors.SocketError", "ProtocolFamilyNotAvailable"),
            error.ProtocolNotSupported => ctx.vm.pushErrorEnum("errors.SocketError", "ProtocolNotSupported"),
            error.ReadOnlyFileSystem => ctx.vm.pushErrorEnum("errors.FileSystemError", "ReadOnlyFileSystem"),
            error.SharingViolation => ctx.vm.pushErrorEnum("errors.FileSystemError", "SharingViolation"),
            error.SocketNotBound => ctx.vm.pushErrorEnum("errors.SocketError", "SocketNotBound"),
            error.SocketTypeNotSupported => ctx.vm.pushErrorEnum("errors.SocketError", "SocketTypeNotSupported"),
            error.SymLinkLoop => ctx.vm.pushErrorEnum("errors.FileSystemError", "SymLinkLoop"),
            error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("errors.SocketError", "SystemFdQuotaExceeded"),
            error.SystemResources => ctx.vm.pushErrorEnum("errors.FileSystemError", "SystemResources"),
            error.TimeoutTooBig => ctx.vm.pushErrorEnum("errors.SocketError", "TimeoutTooBig"),
            error.WouldBlock => ctx.vm.pushErrorEnum("errors.FileSystemError", "WouldBlock"),
        }

        return -1;
    };
    defer list.deinit();

    if (list.addrs.len == 0) {
        ctx.vm.pushErrorEnum("errors.SocketError", "AddressNotResolved");

        return -1;
    }

    server.listen(list.addrs[0]) catch |err| {
        switch (err) {
            error.NoDevice => ctx.vm.pushErrorEnum("errors.FileSystemError", "NoDevice"),
            error.AlreadyConnected => ctx.vm.pushErrorEnum("errors.SocketError", "AlreadyConnected"),
            error.SocketNotBound => ctx.vm.pushErrorEnum("errors.SocketError", "SocketNotBound"),
            error.AddressNotAvailable => ctx.vm.pushErrorEnum("errors.SocketError", "AddressNotAvailable"),
            error.SymLinkLoop => ctx.vm.pushErrorEnum("errors.FileSystemError", "SymLinkLoop"),
            error.NameTooLong => ctx.vm.pushErrorEnum("errors.FileSystemError", "NameTooLong"),
            error.FileNotFound => ctx.vm.pushErrorEnum("errors.FileSystemError", "FileNotFound"),
            error.NotDir => ctx.vm.pushErrorEnum("errors.FileSystemError", "NotDir"),
            error.ReadOnlyFileSystem => ctx.vm.pushErrorEnum("errors.FileSystemError", "ReadOnlyFileSystem"),
            error.AlreadyBound => ctx.vm.pushErrorEnum("errors.SocketError", "AlreadyBound"),
            error.InvalidProtocolOption => ctx.vm.pushErrorEnum("errors.SocketError", "InvalidProtocolOption"),
            error.TimeoutTooBig => ctx.vm.pushErrorEnum("errors.SocketError", "TimeoutTooBig"),
            error.PermissionDenied => ctx.vm.pushErrorEnum("errors.SocketError", "PermissionDenied"),
            error.ProtocolFamilyNotAvailable => ctx.vm.pushErrorEnum("errors.SocketError", "ProtocolFamilyNotAvailable"),
            error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("errors.SocketError", "ProcessFdQuotaExceeded"),
            error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("errors.SocketError", "SystemFdQuotaExceeded"),
            error.ProtocolNotSupported => ctx.vm.pushErrorEnum("errors.SocketError", "ProtocolNotSupported"),
            error.SocketTypeNotSupported => ctx.vm.pushErrorEnum("errors.SocketError", "SocketTypeNotSupported"),
            error.AddressInUse => ctx.vm.pushErrorEnum("errors.SocketError", "AddressInUse"),
            error.FileDescriptorNotASocket => ctx.vm.pushErrorEnum("errors.SocketError", "FileDescriptorNotASocket"),
            error.OperationNotSupported => ctx.vm.pushErrorEnum("errors.SocketError", "OperationNotSupported"),
            error.NetworkSubsystemFailed => ctx.vm.pushErrorEnum("errors.SocketError", "NetworkSubsystemFailed"),
            error.AddressFamilyNotSupported => ctx.vm.pushErrorEnum("errors.SocketError", "AddressFamilyNotSupported"),
            error.AccessDenied => ctx.vm.pushErrorEnum("errors.FileSystemError", "AccessDenied"),
            error.SystemResources => ctx.vm.pushErrorEnum("errors.FileSystemError", "SystemResources"),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
        }

        return -1;
    };

    ctx.vm.bz_pushInteger(@intCast(server.sockfd.?));

    return 1;
}

export fn SocketServerAccept(ctx: *api.NativeCtx) c_int {
    const server_socket: std.os.socket_t = @intCast(
        ctx.vm.bz_peek(2).integer(),
    );
    const reuse_address: bool = ctx.vm.bz_peek(1).boolean();
    const reuse_port: bool = ctx.vm.bz_peek(0).boolean();

    const default_options = std.net.StreamServer.Options{};
    var server = std.net.StreamServer{
        .sockfd = server_socket,
        .kernel_backlog = default_options.kernel_backlog,
        .reuse_address = reuse_address,
        .reuse_port = reuse_port,
        .listen_address = undefined,
    };

    const connection = server.accept() catch |err| {
        switch (err) {
            error.ConnectionAborted => ctx.vm.pushErrorEnum("errors.SocketError", "ConnectionAborted"),
            error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("errors.SocketError", "ProcessFdQuotaExceeded"),
            error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("errors.SocketError", "SystemFdQuotaExceeded"),
            error.SystemResources => ctx.vm.pushErrorEnum("errors.SocketError", "SystemResources"),
            error.SocketNotListening => ctx.vm.pushErrorEnum("errors.SocketError", "SocketNotListening"),
            error.ProtocolFailure => ctx.vm.pushErrorEnum("errors.SocketError", "ProtocolFailure"),
            error.BlockedByFirewall => ctx.vm.pushErrorEnum("errors.SocketError", "BlockedByFirewall"),
            error.FileDescriptorNotASocket => ctx.vm.pushErrorEnum("errors.SocketError", "FileDescriptorNotASocket"),
            error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("errors.SocketError", "ConnectionResetByPeer"),
            error.NetworkSubsystemFailed => ctx.vm.pushErrorEnum("errors.SocketError", "NetworkSubsystemFailed"),
            error.OperationNotSupported => ctx.vm.pushErrorEnum("errors.SocketError", "OperationNotSupported"),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
        }

        return -1;
    };

    ctx.vm.bz_pushInteger(@intCast(connection.stream.handle));

    return 1;
}
