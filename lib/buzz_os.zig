const std = @import("std");
const api = @import("./buzz_api.zig");
const builtin = @import("builtin");

export fn time(vm: *api.VM) c_int {
    vm.bz_pushInteger(std.time.milliTimestamp());

    return 1;
}

export fn env(vm: *api.VM) c_int {
    var len: usize = 0;
    const key = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return -1;
    }

    const key_slice = api.VM.allocator.dupeZ(u8, key.?[0..len]) catch null;
    defer {
        if (key_slice != null) {
            api.VM.allocator.free(key_slice.?);
        }
    }

    if (key_slice == null) {
        vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return -1;
    }

    if (std.os.getenvZ(key_slice.?)) |value| {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (value.len > 0) @ptrCast([*]const u8, value) else null, value.len) orelse {
            vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return -1;
        });

        return 1;
    }

    vm.bz_pushNull();

    return 1;
}

fn sysTempDir() []const u8 {
    return switch (builtin.os.tag) {
        .windows => unreachable, // TODO: GetTempPath
        else => std.os.getenv("TMPDIR") orelse std.os.getenv("TMP") orelse std.os.getenv("TEMP") orelse std.os.getenv("TEMPDIR") orelse "/tmp",
    };
}

export fn tmpDir(vm: *api.VM) c_int {
    const tmp_dir: []const u8 = sysTempDir();

    vm.bz_pushString(api.ObjString.bz_string(vm, if (tmp_dir.len > 0) @ptrCast([*]const u8, tmp_dir) else null, tmp_dir.len) orelse {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    });

    return 1;
}

// TODO: what if file with same random name exists already?
export fn tmpFilename(vm: *api.VM) c_int {
    var prefix_len: usize = 0;
    const prefix = vm.bz_peek(0).bz_valueToString(&prefix_len);

    const prefix_slice = if (prefix_len == 0) "" else prefix.?[0..prefix_len];

    var random_part = std.ArrayList(u8).init(api.VM.allocator);
    defer random_part.deinit();
    random_part.writer().print("{x}", .{std.crypto.random.int(i64)}) catch {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };

    var random_part_b64 = std.ArrayList(u8).initCapacity(api.VM.allocator, std.base64.standard.Encoder.calcSize(random_part.items.len)) catch {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };
    random_part_b64.expandToCapacity();
    defer random_part_b64.deinit();

    _ = std.base64.standard.Encoder.encode(random_part_b64.items, random_part.items);

    var final = std.ArrayList(u8).init(api.VM.allocator);
    defer final.deinit();

    final.writer().print("{s}{s}-{s}", .{ sysTempDir(), prefix_slice, random_part_b64.items }) catch {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };

    vm.bz_pushString(api.ObjString.bz_string(vm, if (final.items.len > 0) @ptrCast([*]const u8, final.items) else null, final.items.len) orelse {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    });

    return 1;
}

// If it was named `exit` it would be considered by zig as a callback when std.os.exit is called
export fn buzzExit(vm: *api.VM) c_int {
    const exitCode: i64 = api.Value.bz_valueToInteger(vm.bz_peek(0));

    std.os.exit(@intCast(u8, exitCode));

    return 0;
}

fn handleSpawnError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.CurrentWorkingDirectoryUnlinked => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "CurrentWorkingDirectoryUnlinked", "CurrentWorkingDirectoryUnlinked".len),
        error.ResourceLimitReached => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "ResourceLimitReached", "ResourceLimitReached".len),
        error.WaitAbandoned => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "WaitAbandoned", "WaitAbandoned".len),
        error.WaitTimeOut => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "WaitTimeOut", "WaitTimeOut".len),
        error.InvalidFileDescriptor => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "InvalidFileDescriptor", "InvalidFileDescriptor".len),
        error.PermissionDenied => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "PermissionDenied", "PermissionDenied".len),
        error.InvalidExe => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "InvalidExe", "InvalidExe".len),
        error.ChildExecFailed => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "ChildExecFailed", "ChildExecFailed".len),
        error.InvalidUserId => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "InvalidUserId", "InvalidUserId".len),
        error.InvalidName => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "InvalidName", "InvalidName".len),
        error.TooBig => vm.bz_pushErrorEnum("lib.errors.ExecError", "lib.errors.ExecError".len, "TooBig", "TooBig".len),

        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.BadPathName => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "BadPathName", "BadPathName".len),
        error.FileBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileBusy", "FileBusy".len),
        error.FileSystem => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileSystem", "FileSystem".len),
        error.InputOutput => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len),
        error.InvalidUtf8 => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidUtf8", "InvalidUtf8".len),
        error.IsDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.NameTooLong => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
        error.NoDevice => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
        error.NotDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
        error.ProcessFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
        error.SymLinkLoop => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
        error.SystemFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),

        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.FileNotFound => vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
        error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
    }
}

export fn execute(vm: *api.VM) c_int {
    var command = std.ArrayList([]const u8).init(api.VM.allocator);
    defer command.deinit();

    const argv = api.ObjList.bz_valueToList(vm.bz_peek(0));
    const len = argv.bz_listLen();
    var i: usize = 0;
    while (i < len) : (i += 1) {
        const arg = argv.bz_listGet(i);
        var arg_len: usize = 0;
        var arg_str = arg.bz_valueToString(&arg_len);

        std.debug.assert(arg_len > 0);

        command.append(arg_str.?[0..arg_len]) catch {
            vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return -1;
        };
    }

    var child_process = std.ChildProcess.init(command.items, api.VM.allocator);
    child_process.disable_aslr = builtin.target.isDarwin();

    child_process.spawn() catch |err| {
        handleSpawnError(vm, err);

        return -1;
    };

    vm.bz_pushInteger(@intCast(i64, (child_process.wait() catch |err| {
        handleSpawnError(vm, err);

        return -1;
    }).Exited));

    return 1;
}

fn handleConnectError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AddressFamilyNotSupported => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressFamilyNotSupported", "AddressFamilyNotSupported".len),
        error.AddressInUse => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressInUse", "AddressInUse".len),
        error.AddressNotAvailable => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressNotAvailable", "AddressNotAvailable".len),
        error.ConnectionPending => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ConnectionPending", "ConnectionPending".len),
        error.ConnectionRefused => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ConnectionRefused", "ConnectionRefused".len),
        error.ConnectionResetByPeer => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
        error.ConnectionTimedOut => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ConnectionTimedOut", "ConnectionTimedOut".len),
        error.FileNotFound => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "FileNotFound", "FileNotFound".len),
        error.NetworkUnreachable => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "NetworkUnreachable", "NetworkUnreachable".len),
        error.PermissionDenied => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "PermissionDenied", "PermissionDenied".len),
        error.ProcessFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
        error.ProtocolFamilyNotAvailable => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProtocolFamilyNotAvailable", "ProtocolFamilyNotAvailable".len),
        error.ProtocolNotSupported => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProtocolNotSupported", "ProtocolNotSupported".len),
        error.SocketTypeNotSupported => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SocketTypeNotSupported", "SocketTypeNotSupported".len),
        error.SystemFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SystemResources", "SystemResources".len),
        error.WouldBlock => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "WouldBlock", "WouldBlock".len),
        error.ServiceUnavailable => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ServiceUnavailable", "ServiceUnavailable".len),
        error.UnknownHostName => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "UnknownHostName", "UnknownHostName".len),
        error.NameServerFailure => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "NameServerFailure", "NameServerFailure".len),
        error.TemporaryNameServerFailure => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "TemporaryNameServerFailure", "TemporaryNameServerFailure".len),
        error.HostLacksNetworkAddresses => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "HostLacksNetworkAddresses", "HostLacksNetworkAddresses".len),

        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
    }
}

export fn SocketConnect(vm: *api.VM) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(vm.bz_peek(2), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?i64 = api.Value.bz_valueToInteger(vm.bz_peek(1));
    if (port == null or port.? < 0) {
        vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return -1;
    }

    const protocol = api.Value.bz_valueToInteger(vm.bz_peek(0));

    switch (protocol) {
        0 => {
            const stream = std.net.tcpConnectToHost(api.VM.allocator, address, @intCast(u16, port.?)) catch |err| {
                handleConnectError(vm, err);

                return -1;
            };

            vm.bz_pushInteger(@intCast(i64, stream.handle));

            return 1;
        },
        1, // TODO: UDP
        2, // TODO: IPC
        => {
            vm.bz_pushError("lib.errors.NotYetImplementedError", "lib.errors.NotYetImplementedError".len);

            return -1;
        },
        else => {
            vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

            return -1;
        },
    }
}

export fn SocketClose(vm: *api.VM) c_int {
    const socket: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(0)),
    );

    std.os.closeSocket(socket);

    return 0;
}

fn handleReadAllError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.InputOutput => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len),
        error.IsDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.WouldBlock => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
        error.OperationAborted => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "OperationAborted", "OperationAborted".len),
        error.BrokenPipe => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "BrokenPipe", "BrokenPipe".len),
        error.ConnectionResetByPeer => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
        error.ConnectionTimedOut => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionTimedOut", "ConnectionTimedOut".len),
        error.NotOpenForReading => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "NotOpenForReading", "NotOpenForReading".len),

        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),

        else => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        // error.StreamTooLong => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "StreamTooLong", "StreamTooLong".len),
        // error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
    }
}

export fn SocketRead(vm: *api.VM) c_int {
    const n: i64 = api.Value.bz_valueToInteger(vm.bz_peek(0));
    if (n < 0) {
        vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return -1;
    }

    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(1)),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = api.VM.allocator.alloc(u8, @intCast(usize, n)) catch {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };

    // bz_string will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch |err| {
        handleReadAllError(vm, err);

        return -1;
    };

    if (read == 0) {
        vm.bz_pushNull();
    } else {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (buffer[0..read].len > 0) @ptrCast([*]const u8, buffer[0..read]) else null, read) orelse {
            vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return -1;
        });
    }

    return 1;
}

fn handleReadLineError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.InputOutput => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len),
        error.IsDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.WouldBlock => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
        error.OperationAborted => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "OperationAborted", "OperationAborted".len),
        error.BrokenPipe => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "BrokenPipe", "BrokenPipe".len),
        error.ConnectionResetByPeer => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
        error.ConnectionTimedOut => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionTimedOut", "ConnectionTimedOut".len),
        error.NotOpenForReading => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "NotOpenForReading", "NotOpenForReading".len),
        error.StreamTooLong => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "StreamTooLong", "StreamTooLong".len),

        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),

        error.EndOfStream => {},
    }
}

export fn SocketReadLine(vm: *api.VM) c_int {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(0)),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = reader.readUntilDelimiterAlloc(api.VM.allocator, '\n', 16 * 8 * 64) catch |err| {
        handleReadLineError(vm, err);

        return -1;
    };

    // EOF?
    if (buffer.len == 0) {
        vm.bz_pushNull();
    } else {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (buffer.len > 0) @ptrCast([*]const u8, buffer) else null, buffer.len) orelse {
            vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return -1;
        });
    }

    return 1;
}

export fn SocketReadAll(vm: *api.VM) c_int {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(0)),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = reader.readAllAlloc(api.VM.allocator, 16 * 8 * 64) catch |err| {
        handleReadAllError(vm, err);

        return -1;
    };

    // EOF?
    if (buffer.len == 0) {
        vm.bz_pushNull();
    } else {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (buffer.len > 0) @ptrCast([*]const u8, buffer) else null, buffer.len) orelse {
            vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return -1;
        });
    }

    return 1;
}

export fn SocketWrite(vm: *api.VM) c_int {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(1)),
    );

    const stream: std.net.Stream = .{ .handle = handle };

    var len: usize = 0;
    var value = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    _ = stream.write(value.?[0..len]) catch |err| {
        switch (err) {
            error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
            error.InputOutput => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len),
            error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
            error.WouldBlock => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
            error.OperationAborted => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "OperationAborted", "OperationAborted".len),
            error.BrokenPipe => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "BrokenPipe", "BrokenPipe".len),
            error.ConnectionResetByPeer => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
            error.DiskQuota => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "DiskQuota", "DiskQuota".len),
            error.FileTooBig => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileTooBig", "FileTooBig".len),
            error.NoSpaceLeft => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoSpaceLeft", "NoSpaceLeft".len),
            error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
            error.NotOpenForWriting => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "NotOpenForWriting", "NotOpenForWriting".len),
            error.LockViolation => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "LockViolation", "LockViolation".len),
        }

        return -1;
    };

    return 0;
}

export fn SocketServerStart(vm: *api.VM) c_int {
    var len: usize = 0;
    const address_value = api.Value.bz_valueToString(vm.bz_peek(2), &len);
    const address = if (len > 0) address_value.?[0..len] else "";
    const port: ?i64 = api.Value.bz_valueToInteger(vm.bz_peek(1));
    if (port == null or port.? < 0) {
        vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return -1;
    }

    const reuse_address: bool = api.Value.bz_valueToBool(vm.bz_peek(0));

    var server = std.net.StreamServer.init(.{ .reuse_address = reuse_address });

    const list = std.net.getAddressList(api.VM.allocator, address, @intCast(u16, port.?)) catch |err| {
        switch (err) {
            error.ServiceUnavailable => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ServiceUnavailable", "ServiceUnavailable".len),
            error.UnknownHostName => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "UnknownHostName", "UnknownHostName".len),
            error.NameServerFailure => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "NameServerFailure", "NameServerFailure".len),
            error.TemporaryNameServerFailure => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "TemporaryNameServerFailure", "TemporaryNameServerFailure".len),
            error.HostLacksNetworkAddresses => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "HostLacksNetworkAddresses", "HostLacksNetworkAddresses".len),
            error.AddressFamilyNotSupported => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressFamilyNotSupported", "AddressFamilyNotSupported".len),
            error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
            error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return -1;
    };
    defer list.deinit();

    if (list.addrs.len == 0) {
        vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressNotResolved", "AddressNotResolved".len);

        return -1;
    }

    server.listen(list.addrs[0]) catch |err| {
        switch (err) {
            error.NoDevice => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
            error.AlreadyConnected => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AlreadyConnected", "AlreadyConnected".len),
            error.SocketNotBound => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SocketNotBound", "SocketNotBound".len),
            error.AddressNotAvailable => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressNotAvailable", "AddressNotAvailable".len),
            error.SymLinkLoop => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
            error.NameTooLong => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
            error.FileNotFound => vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
            error.NotDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
            error.ReadOnlyFileSystem => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ReadOnlyFileSystem", "ReadOnlyFileSystem".len),
            error.AlreadyBound => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AlreadyBound", "AlreadyBound".len),
            error.InvalidProtocolOption => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "InvalidProtocolOption", "InvalidProtocolOption".len),
            error.TimeoutTooBig => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "TimeoutTooBig", "TimeoutTooBig".len),
            error.PermissionDenied => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "PermissionDenied", "PermissionDenied".len),
            error.ProtocolFamilyNotAvailable => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProtocolFamilyNotAvailable", "ProtocolFamilyNotAvailable".len),
            error.ProcessFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
            error.SystemFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
            error.ProtocolNotSupported => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProtocolNotSupported", "ProtocolNotSupported".len),
            error.SocketTypeNotSupported => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SocketTypeNotSupported", "SocketTypeNotSupported".len),
            error.AddressInUse => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressInUse", "AddressInUse".len),
            error.FileDescriptorNotASocket => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "FileDescriptorNotASocket", "FileDescriptorNotASocket".len),
            error.OperationNotSupported => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "OperationNotSupported", "OperationNotSupported".len),
            error.NetworkSubsystemFailed => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "NetworkSubsystemFailed", "NetworkSubsystemFailed".len),
            error.AddressFamilyNotSupported => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressFamilyNotSupported", "AddressFamilyNotSupported".len),
            error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
            error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
            error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        }

        return -1;
    };

    vm.bz_pushInteger(@intCast(i64, server.sockfd.?));

    return 1;
}

export fn SocketServerAccept(vm: *api.VM) c_int {
    const server_socket: std.os.socket_t = @intCast(
        std.os.socket_t,
        api.Value.bz_valueToInteger(vm.bz_peek(1)),
    );
    const reuse_address: bool = api.Value.bz_valueToBool(vm.bz_peek(0));

    const default_options = std.net.StreamServer.Options{};
    var server = std.net.StreamServer{
        .sockfd = server_socket,
        .kernel_backlog = default_options.kernel_backlog,
        .reuse_address = reuse_address,
        .listen_address = undefined,
    };

    const connection = server.accept() catch |err| {
        switch (err) {
            error.ConnectionAborted => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ConnectionAborted", "ConnectionAborted".len),
            error.ProcessFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
            error.SystemFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
            error.SystemResources => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SystemResources", "SystemResources".len),
            error.SocketNotListening => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SocketNotListening", "SocketNotListening".len),
            error.ProtocolFailure => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProtocolFailure", "ProtocolFailure".len),
            error.BlockedByFirewall => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "BlockedByFirewall", "BlockedByFirewall".len),
            error.FileDescriptorNotASocket => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "FileDescriptorNotASocket", "FileDescriptorNotASocket".len),
            error.ConnectionResetByPeer => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
            error.NetworkSubsystemFailed => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "NetworkSubsystemFailed", "NetworkSubsystemFailed".len),
            error.OperationNotSupported => vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "OperationNotSupported", "OperationNotSupported".len),
            error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        }

        return -1;
    };

    vm.bz_pushInteger(@intCast(i64, connection.stream.handle));

    return 1;
}
