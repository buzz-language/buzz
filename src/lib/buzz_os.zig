const std = @import("std");
const api = @import("./buzz_api.zig");
const builtin = @import("builtin");

export fn time(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(api.Value.fromFloat(@intToFloat(f64, std.time.milliTimestamp())));

    return 1;
}

export fn env(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const key = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        ctx.vm.pushError("lib.errors.InvalidArgumentError");

        return -1;
    }

    const key_slice = api.VM.allocator.dupeZ(u8, key.?[0..len]) catch null;
    defer {
        if (key_slice != null) {
            api.VM.allocator.free(key_slice.?);
        }
    }

    if (key_slice == null) {
        ctx.vm.pushError("lib.errors.InvalidArgumentError");

        return -1;
    }

    if (std.os.getenvZ(key_slice.?)) |value| {
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (value.len > 0) @ptrCast([*]const u8, value) else null, value.len) orelse {
            ctx.vm.pushError("lib.errors.OutOfMemoryError");

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

    ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (tmp_dir.len > 0) @ptrCast([*]const u8, tmp_dir) else null, tmp_dir.len) orelse {
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

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
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

        return -1;
    };

    var random_part_b64 = std.ArrayList(u8).initCapacity(api.VM.allocator, std.base64.standard.Encoder.calcSize(random_part.items.len)) catch {
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

        return -1;
    };
    random_part_b64.expandToCapacity();
    defer random_part_b64.deinit();

    _ = std.base64.standard.Encoder.encode(random_part_b64.items, random_part.items);

    var final = std.ArrayList(u8).init(api.VM.allocator);
    defer final.deinit();

    final.writer().print("{s}{s}-{s}", .{ sysTempDir(), prefix_slice, random_part_b64.items }) catch {
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

        return -1;
    };

    ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (final.items.len > 0) @ptrCast([*]const u8, final.items) else null, final.items.len) orelse {
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

        return -1;
    });

    return 1;
}

// If it was named `exit` it would be considered by zig as a callback when std.os.exit is called
export fn buzzExit(ctx: *api.NativeCtx) c_int {
    const exitCode: i32 = ctx.vm.bz_peek(0).integer();

    std.os.exit(@intCast(u8, exitCode));

    return 0;
}

fn handleSpawnError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
        error.CurrentWorkingDirectoryUnlinked => ctx.vm.pushErrorEnum("lib.errors.ExecError", "CurrentWorkingDirectoryUnlinked"),
        error.FileBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileBusy"),
        error.FileNotFound => ctx.vm.pushError("lib.errors.FileNotFoundError"),
        error.FileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileSystem"),
        error.InvalidExe => ctx.vm.pushErrorEnum("lib.errors.ExecError", "InvalidExe"),
        error.InvalidHandle => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidHandle"),
        error.InvalidName => ctx.vm.pushErrorEnum("lib.errors.ExecError", "InvalidName"),
        error.InvalidUserId => ctx.vm.pushErrorEnum("lib.errors.ExecError", "InvalidUserId"),
        error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
        error.IsDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir"),
        error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
        error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
        error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
        error.OutOfMemory => ctx.vm.pushError("lib.errors.OutOfMemoryError"),
        error.PermissionDenied => ctx.vm.pushErrorEnum("lib.errors.ExecError", "PermissionDenied"),
        error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ProcessFdQuotaExceeded"),
        error.ResourceLimitReached => ctx.vm.pushErrorEnum("lib.errors.ExecError", "ResourceLimitReached"),
        error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
        error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemFdQuotaExceeded"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        error.WaitAbandoned => ctx.vm.pushErrorEnum("lib.errors.ExecError", "WaitAbandoned"),
        error.WaitTimeOut => ctx.vm.pushErrorEnum("lib.errors.ExecError", "WaitTimeOut"),
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
            ctx.vm.pushError("lib.errors.OutOfMemoryError");

            return -1;
        };
    }

    var child_process = std.ChildProcess.init(command.items, api.VM.allocator);
    child_process.disable_aslr = builtin.target.isDarwin();

    child_process.spawn() catch |err| {
        handleSpawnError(ctx, err);

        return -1;
    };

    ctx.vm.bz_pushInteger(@intCast(i32, (child_process.wait() catch |err| {
        handleSpawnError(ctx, err);

        return -1;
    }).Exited));

    return 1;
}

fn handleConnectError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AddressFamilyNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressFamilyNotSupported"),
        error.AddressInUse => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressInUse"),
        error.AddressNotAvailable => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressNotAvailable"),
        error.ConnectionPending => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ConnectionPending"),
        error.ConnectionRefused => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ConnectionRefused"),
        error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ConnectionResetByPeer"),
        error.ConnectionTimedOut => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ConnectionTimedOut"),
        error.FileNotFound => ctx.vm.pushErrorEnum("lib.errors.SocketError", "FileNotFound"),
        error.HostLacksNetworkAddresses => ctx.vm.pushErrorEnum("lib.errors.SocketError", "HostLacksNetworkAddresses"),
        error.NameServerFailure => ctx.vm.pushErrorEnum("lib.errors.SocketError", "NameServerFailure"),
        error.NetworkUnreachable => ctx.vm.pushErrorEnum("lib.errors.SocketError", "NetworkUnreachable"),
        error.OutOfMemory => ctx.vm.pushError("lib.errors.OutOfMemoryError"),
        error.PermissionDenied => ctx.vm.pushErrorEnum("lib.errors.SocketError", "PermissionDenied"),
        error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProcessFdQuotaExceeded"),
        error.ProtocolFamilyNotAvailable => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProtocolFamilyNotAvailable"),
        error.ProtocolNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProtocolNotSupported"),
        error.ServiceUnavailable => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ServiceUnavailable"),
        error.SocketTypeNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SocketTypeNotSupported"),
        error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SystemFdQuotaExceeded"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SystemResources"),
        error.TemporaryNameServerFailure => ctx.vm.pushErrorEnum("lib.errors.SocketError", "TemporaryNameServerFailure"),
        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        error.UnknownHostName => ctx.vm.pushErrorEnum("lib.errors.SocketError", "UnknownHostName"),
        error.WouldBlock => ctx.vm.pushErrorEnum("lib.errors.SocketError", "WouldBlock"),
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AccessDenied"),
        error.AlreadyBound => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AlreadyBound"),
        error.AlreadyConnected => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AlreadyConnected"),
        error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
        error.BrokenPipe => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "BrokenPipe"),
        error.DeviceBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DeviceBusy"),
        error.FileBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileBusy"),
        error.FileDescriptorNotASocket => ctx.vm.pushErrorEnum("lib.errors.SocketError", "FileDescriptorNotASocket"),
        error.FileLocksNotSupported => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileLocksNotSupported"),
        error.FileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileSystem"),
        error.FileTooBig => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileTooBig"),
        error.Incomplete => ctx.vm.pushErrorEnum("lib.errors.SocketError", "Incomplete"),
        error.InputOutput => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InputOutput"),
        error.InterfaceNotFound => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InterfaceNotFound"),
        error.InvalidCharacter => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidCharacter"),
        error.InvalidEnd => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidEnd"),
        error.InvalidHandle => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidHandle"),
        error.InvalidIPAddressFormat => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidIPAddressFormat"),
        error.InvalidIpv4Mapping => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidIpv4Mapping"),
        error.InvalidProtocolOption => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidProtocolOption"),
        error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
        error.IsDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir"),
        error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
        error.NetNameDeleted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NetNameDeleted"),
        error.NetworkSubsystemFailed => ctx.vm.pushErrorEnum("lib.errors.SocketError", "NetworkSubsystemFailed"),
        error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
        error.NoSpaceLeft => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoSpaceLeft"),
        error.NonCanonical => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NonCanonical"),
        error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
        error.NotOpenForReading => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NotOpenForReading"),
        error.OperationAborted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "OperationAborted"),
        error.Overflow => ctx.vm.pushError("lib.errors.OverflowError"),
        error.PathAlreadyExists => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PathAlreadyExists"),
        error.PipeBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PipeBusy"),
        error.ReadOnlyFileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ReadOnlyFileSystem"),
        error.SharingViolation => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SharingViolation"),
        error.SocketNotBound => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SocketNotBound"),
        error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
        error.TimeoutTooBig => ctx.vm.pushErrorEnum("lib.errors.SocketError", "TimeoutTooBig"),
    }
}

export fn SocketConnect(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(ctx.vm.bz_peek(2), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?i32 = ctx.vm.bz_peek(1).integer();
    if (port == null or port.? < 0) {
        ctx.vm.pushError("lib.errors.InvalidArgumentError");

        return -1;
    }

    const protocol = ctx.vm.bz_peek(0).integer();

    switch (protocol) {
        0 => {
            const stream = std.net.tcpConnectToHost(api.VM.allocator, address, @intCast(u16, port.?)) catch |err| {
                handleConnectError(ctx, err);

                return -1;
            };

            ctx.vm.bz_pushInteger(@intCast(i32, stream.handle));

            return 1;
        },
        1, // TODO: UDP
        2, // TODO: IPC
        => {
            ctx.vm.pushError("lib.errors.NotYetImplementedError");

            return -1;
        },
        else => {
            ctx.vm.pushError("lib.errors.InvalidArgumentError");

            return -1;
        },
    }
}

export fn SocketClose(ctx: *api.NativeCtx) c_int {
    const socket: std.os.socket_t = @intCast(
        std.os.socket_t,
        ctx.vm.bz_peek(0).integer(),
    );

    std.os.closeSocket(socket);

    return 0;
}

fn handleReadAllError(ctx: *api.NativeCtx, err: anytype) void {
    // FIXME: here a zig bug: if i remove OutOfMemory and StreamTooLong it complains they're missing but if i put them it complains they are not required...
    //        switch -> if
    if (err == error.AccessDenied) {
        ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied");
    } else if (err == error.InputOutput) {
        ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InputOutput");
    } else if (err == error.IsDir) {
        ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir");
    } else if (err == error.SystemResources) {
        ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources");
    } else if (err == error.WouldBlock) {
        ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "WouldBlock");
    } else if (err == error.OperationAborted) {
        ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "OperationAborted");
    } else if (err == error.BrokenPipe) {
        ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "BrokenPipe");
    } else if (err == error.ConnectionResetByPeer) {
        ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "ConnectionResetByPeer");
    } else if (err == error.ConnectionTimedOut) {
        ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "ConnectionTimedOut");
    } else if (err == error.NotOpenForReading) {
        ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NotOpenForReading");
    } else if (err == error.Unexpected) {
        ctx.vm.pushError("lib.errors.UnexpectedError");
    } else {
        ctx.vm.pushError("lib.errors.OutOfMemoryError");
    }
    // error.StreamTooLong => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "StreamTooLong"),
    // error.OutOfMemory => ctx.vm.pushError("lib.errors.OutOfMemoryError"),
}

export fn SocketRead(ctx: *api.NativeCtx) c_int {
    const n: i32 = ctx.vm.bz_peek(0).integer();
    if (n < 0) {
        ctx.vm.pushError("lib.errors.InvalidArgumentError");

        return -1;
    }

    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        ctx.vm.bz_peek(1).integer(),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = api.VM.allocator.alloc(u8, @intCast(usize, n)) catch {
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

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
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (buffer[0..read].len > 0) @ptrCast([*]const u8, buffer[0..read]) else null, read) orelse {
            ctx.vm.pushError("lib.errors.OutOfMemoryError");

            return -1;
        });
    }

    return 1;
}

fn handleReadLineError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.InputOutput => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InputOutput"),
        error.IsDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
        error.WouldBlock => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "WouldBlock"),
        error.OperationAborted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "OperationAborted"),
        error.BrokenPipe => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "BrokenPipe"),
        error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "ConnectionResetByPeer"),
        error.ConnectionTimedOut => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "ConnectionTimedOut"),
        error.NotOpenForReading => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NotOpenForReading"),
        error.StreamTooLong => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "StreamTooLong"),
        error.NetNameDeleted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NetNameDeleted"),

        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        error.OutOfMemory => ctx.vm.pushError("lib.errors.OutOfMemoryError"),

        error.EndOfStream => {},
    }
}

export fn SocketReadLine(ctx: *api.NativeCtx) c_int {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
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
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (buffer.len > 0) @ptrCast([*]const u8, buffer) else null, buffer.len) orelse {
            ctx.vm.pushError("lib.errors.OutOfMemoryError");

            return -1;
        });
    }

    return 1;
}

export fn SocketReadAll(ctx: *api.NativeCtx) c_int {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
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
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (buffer.len > 0) @ptrCast([*]const u8, buffer) else null, buffer.len) orelse {
            ctx.vm.pushError("lib.errors.OutOfMemoryError");

            return -1;
        });
    }

    return 1;
}

export fn SocketWrite(ctx: *api.NativeCtx) c_int {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
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
            error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
            error.InputOutput => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InputOutput"),
            error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
            error.WouldBlock => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "WouldBlock"),
            error.OperationAborted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "OperationAborted"),
            error.BrokenPipe => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "BrokenPipe"),
            error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "ConnectionResetByPeer"),
            error.DiskQuota => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DiskQuota"),
            error.FileTooBig => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileTooBig"),
            error.NoSpaceLeft => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoSpaceLeft"),
            error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
            error.NotOpenForWriting => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NotOpenForWriting"),
            error.LockViolation => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "LockViolation"),
            error.DeviceBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DeviceBusy"),
            error.InvalidArgument => ctx.vm.pushError("lib.errors.InvalidArgumentError"),
        }

        return -1;
    };

    return 0;
}

export fn SocketServerStart(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(ctx.vm.bz_peek(2), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?i32 = ctx.vm.bz_peek(1).integer();
    if (port == null or port.? < 0) {
        ctx.vm.pushError("lib.errors.InvalidArgumentError");

        return -1;
    }

    const reuse_address: bool = ctx.vm.bz_peek(0).boolean();

    var server = std.net.StreamServer.init(.{ .reuse_address = reuse_address });

    const list = std.net.getAddressList(api.VM.allocator, address, @intCast(u16, port.?)) catch |err| {
        switch (err) {
            error.ServiceUnavailable => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ServiceUnavailable"),
            error.UnknownHostName => ctx.vm.pushErrorEnum("lib.errors.SocketError", "UnknownHostName"),
            error.NameServerFailure => ctx.vm.pushErrorEnum("lib.errors.SocketError", "NameServerFailure"),
            error.TemporaryNameServerFailure => ctx.vm.pushErrorEnum("lib.errors.SocketError", "TemporaryNameServerFailure"),
            error.HostLacksNetworkAddresses => ctx.vm.pushErrorEnum("lib.errors.SocketError", "HostLacksNetworkAddresses"),
            error.AddressFamilyNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressFamilyNotSupported"),
            error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
            error.OutOfMemory => ctx.vm.pushError("lib.errors.OutOfMemoryError"),
            error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
            error.AddressInUse => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressInUse"),
            error.AddressNotAvailable => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressNotAvailable"),
            error.AlreadyBound => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AlreadyBound"),
            error.AlreadyConnected => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AlreadyConnected"),
            error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
            error.BrokenPipe => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "BrokenPipe"),
            error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ConnectionResetByPeer"),
            error.ConnectionTimedOut => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ConnectionTimedOut"),
            error.DeviceBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DeviceBusy"),
            error.FileBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileBusy"),
            error.FileDescriptorNotASocket => ctx.vm.pushErrorEnum("lib.errors.SocketError", "FileDescriptorNotASocket"),
            error.FileLocksNotSupported => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileLocksNotSupported"),
            error.FileNotFound => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileNotFound"),
            error.FileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileSystem"),
            error.FileTooBig => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileTooBig"),
            error.Incomplete => ctx.vm.pushErrorEnum("lib.errors.SocketError", "Incomplete"),
            error.InputOutput => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InputOutput"),
            error.InterfaceNotFound => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InterfaceNotFound"),
            error.InvalidCharacter => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidCharacter"),
            error.InvalidEnd => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidEnd"),
            error.InvalidHandle => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidHandle"),
            error.InvalidIPAddressFormat => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidIPAddressFormat"),
            error.InvalidIpv4Mapping => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidIpv4Mapping"),
            error.InvalidProtocolOption => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidProtocolOption"),
            error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
            error.IsDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir"),
            error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
            error.NetNameDeleted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NetNameDeleted"),
            error.NetworkSubsystemFailed => ctx.vm.pushErrorEnum("lib.errors.SocketError", "NetworkSubsystemFailed"),
            error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
            error.NoSpaceLeft => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoSpaceLeft"),
            error.NonCanonical => ctx.vm.pushErrorEnum("lib.errors.SocketError", "NonCanonical"),
            error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
            error.NotOpenForReading => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NotOpenForReading"),
            error.OperationAborted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "OperationAborted"),
            error.Overflow => ctx.vm.pushError("lib.errors.OverflowError"),
            error.PathAlreadyExists => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PathAlreadyExists"),
            error.PermissionDenied => ctx.vm.pushErrorEnum("lib.errors.SocketError", "PermissionDenied"),
            error.PipeBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PipeBusy"),
            error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ProcessFdQuotaExceeded"),
            error.ProtocolFamilyNotAvailable => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProtocolFamilyNotAvailable"),
            error.ProtocolNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProtocolNotSupported"),
            error.ReadOnlyFileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ReadOnlyFileSystem"),
            error.SharingViolation => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SharingViolation"),
            error.SocketNotBound => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SocketNotBound"),
            error.SocketTypeNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SocketTypeNotSupported"),
            error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
            error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SystemFdQuotaExceeded"),
            error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
            error.TimeoutTooBig => ctx.vm.pushErrorEnum("lib.errors.SocketError", "TimeoutTooBig"),
            error.WouldBlock => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "WouldBlock"),
        }

        return -1;
    };
    defer list.deinit();

    if (list.addrs.len == 0) {
        ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressNotResolved");

        return -1;
    }

    server.listen(list.addrs[0]) catch |err| {
        switch (err) {
            error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
            error.AlreadyConnected => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AlreadyConnected"),
            error.SocketNotBound => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SocketNotBound"),
            error.AddressNotAvailable => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressNotAvailable"),
            error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
            error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
            error.FileNotFound => ctx.vm.pushError("lib.errors.FileNotFoundError"),
            error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
            error.ReadOnlyFileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ReadOnlyFileSystem"),
            error.AlreadyBound => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AlreadyBound"),
            error.InvalidProtocolOption => ctx.vm.pushErrorEnum("lib.errors.SocketError", "InvalidProtocolOption"),
            error.TimeoutTooBig => ctx.vm.pushErrorEnum("lib.errors.SocketError", "TimeoutTooBig"),
            error.PermissionDenied => ctx.vm.pushErrorEnum("lib.errors.SocketError", "PermissionDenied"),
            error.ProtocolFamilyNotAvailable => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProtocolFamilyNotAvailable"),
            error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProcessFdQuotaExceeded"),
            error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SystemFdQuotaExceeded"),
            error.ProtocolNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProtocolNotSupported"),
            error.SocketTypeNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SocketTypeNotSupported"),
            error.AddressInUse => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressInUse"),
            error.FileDescriptorNotASocket => ctx.vm.pushErrorEnum("lib.errors.SocketError", "FileDescriptorNotASocket"),
            error.OperationNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "OperationNotSupported"),
            error.NetworkSubsystemFailed => ctx.vm.pushErrorEnum("lib.errors.SocketError", "NetworkSubsystemFailed"),
            error.AddressFamilyNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "AddressFamilyNotSupported"),
            error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
            error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
            error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        }

        return -1;
    };

    ctx.vm.bz_pushInteger(@intCast(i32, server.sockfd.?));

    return 1;
}

export fn SocketServerAccept(ctx: *api.NativeCtx) c_int {
    const server_socket: std.os.socket_t = @intCast(
        std.os.socket_t,
        ctx.vm.bz_peek(1).integer(),
    );
    const reuse_address: bool = ctx.vm.bz_peek(0).boolean();

    const default_options = std.net.StreamServer.Options{};
    var server = std.net.StreamServer{
        .sockfd = server_socket,
        .kernel_backlog = default_options.kernel_backlog,
        .reuse_address = reuse_address,
        .listen_address = undefined,
    };

    const connection = server.accept() catch |err| {
        switch (err) {
            error.ConnectionAborted => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ConnectionAborted"),
            error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProcessFdQuotaExceeded"),
            error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SystemFdQuotaExceeded"),
            error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SystemResources"),
            error.SocketNotListening => ctx.vm.pushErrorEnum("lib.errors.SocketError", "SocketNotListening"),
            error.ProtocolFailure => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ProtocolFailure"),
            error.BlockedByFirewall => ctx.vm.pushErrorEnum("lib.errors.SocketError", "BlockedByFirewall"),
            error.FileDescriptorNotASocket => ctx.vm.pushErrorEnum("lib.errors.SocketError", "FileDescriptorNotASocket"),
            error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("lib.errors.SocketError", "ConnectionResetByPeer"),
            error.NetworkSubsystemFailed => ctx.vm.pushErrorEnum("lib.errors.SocketError", "NetworkSubsystemFailed"),
            error.OperationNotSupported => ctx.vm.pushErrorEnum("lib.errors.SocketError", "OperationNotSupported"),
            error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        }

        return -1;
    };

    ctx.vm.bz_pushInteger(@intCast(i32, connection.stream.handle));

    return 1;
}
