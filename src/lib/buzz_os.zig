const std = @import("std");
const api = @import("./buzz_api.zig");
const builtin = @import("builtin");

export fn time_raw(_: *api.NativeCtx) api.Value {
    return api.Value.fromFloat(@intToFloat(f64, std.time.milliTimestamp()));
}

export fn time(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(time_raw(ctx));

    return 1;
}

export fn env_raw(ctx: *api.NativeCtx, key_value: api.Value) api.Value {
    var len: usize = 0;
    const key = key_value.bz_valueToString(&len);

    if (len == 0) {
        ctx.vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return api.Value.Error;
    }

    const key_slice = api.VM.allocator.dupeZ(u8, key.?[0..len]) catch null;
    defer {
        if (key_slice != null) {
            api.VM.allocator.free(key_slice.?);
        }
    }

    if (key_slice == null) {
        ctx.vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return api.Value.Error;
    }

    if (std.os.getenvZ(key_slice.?)) |value| {
        return (api.ObjString.bz_string(ctx.vm, if (value.len > 0) @ptrCast([*]const u8, value) else null, value.len) orelse {
            ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return api.Value.Error;
        }).bz_objStringToValue();
    }

    return api.Value.Null;
}

export fn env(ctx: *api.NativeCtx) c_int {
    const result = env_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

fn sysTempDir() []const u8 {
    return switch (builtin.os.tag) {
        .windows => unreachable, // TODO: GetTempPath
        else => std.os.getenv("TMPDIR") orelse std.os.getenv("TMP") orelse std.os.getenv("TEMP") orelse std.os.getenv("TEMPDIR") orelse "/tmp",
    };
}

export fn tmpDir_raw(ctx: *api.NativeCtx) api.Value {
    const tmp_dir: []const u8 = sysTempDir();

    return (api.ObjString.bz_string(
        ctx.vm,
        if (tmp_dir.len > 0) @ptrCast([*]const u8, tmp_dir) else null,
        tmp_dir.len,
    ) orelse {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    }).bz_objStringToValue();
}

export fn tmpDir(ctx: *api.NativeCtx) c_int {
    const result = tmpDir_raw(ctx);

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

// TODO: what if file with same random name exists already?
export fn tmpFilename_raw(ctx: *api.NativeCtx, prefix_value: api.Value) api.Value {
    var prefix_len: usize = 0;
    const prefix = prefix_value.bz_valueToString(&prefix_len);

    const prefix_slice = if (prefix_len == 0) "" else prefix.?[0..prefix_len];

    var random_part = std.ArrayList(u8).init(api.VM.allocator);
    defer random_part.deinit();
    random_part.writer().print("{x}", .{std.crypto.random.int(i32)}) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    };

    var random_part_b64 = std.ArrayList(u8).initCapacity(api.VM.allocator, std.base64.standard.Encoder.calcSize(random_part.items.len)) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    };
    random_part_b64.expandToCapacity();
    defer random_part_b64.deinit();

    _ = std.base64.standard.Encoder.encode(random_part_b64.items, random_part.items);

    var final = std.ArrayList(u8).init(api.VM.allocator);
    defer final.deinit();

    final.writer().print("{s}{s}-{s}", .{ sysTempDir(), prefix_slice, random_part_b64.items }) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    };

    return (api.ObjString.bz_string(ctx.vm, if (final.items.len > 0) @ptrCast([*]const u8, final.items) else null, final.items.len) orelse {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    }).bz_objStringToValue();
}

export fn tmpFilename(ctx: *api.NativeCtx) c_int {
    const result = tmpFilename_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

// If it was named `exit` it would be considered by zig as a callback when std.os.exit is called
export fn buzzExit_raw(_: *api.NativeCtx, exit_value: api.Value) void {
    const exitCode: i32 = exit_value.integer();

    std.os.exit(@intCast(u8, exitCode));
}

export fn buzzExit(ctx: *api.NativeCtx) c_int {
    buzzExit_raw(ctx, ctx.vm.bz_peek(0));

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

export fn execute_raw(ctx: *api.NativeCtx, argv_value: api.Value) api.Value {
    var command = std.ArrayList([]const u8).init(api.VM.allocator);
    defer command.deinit();

    const argv = api.ObjList.bz_valueToList(argv_value);
    const len = argv.bz_listLen();
    var i: usize = 0;
    while (i < len) : (i += 1) {
        const arg = api.ObjList.bz_listGet(argv_value, i);
        var arg_len: usize = 0;
        var arg_str = arg.bz_valueToString(&arg_len);

        std.debug.assert(arg_len > 0);

        command.append(arg_str.?[0..arg_len]) catch {
            ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return api.Value.Error;
        };
    }

    var child_process = std.ChildProcess.init(command.items, api.VM.allocator);
    child_process.disable_aslr = builtin.target.isDarwin();

    child_process.spawn() catch |err| {
        handleSpawnError(ctx.vm, err);

        return api.Value.Error;
    };

    return api.Value.fromInteger(@intCast(i32, (child_process.wait() catch |err| {
        handleSpawnError(ctx.vm, err);

        return api.Value.Error;
    }).Exited));
}

export fn execute(ctx: *api.NativeCtx) c_int {
    const result = execute_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

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

export fn SocketConnect_raw(ctx: *api.NativeCtx, address_value: api.Value, port_value: api.Value, protocol_value: api.Value) api.Value {
    var len: usize = 0;
    const address_str = api.Value.bz_valueToString(address_value, &len);
    const address = if (len > 0) address_str.?[0..len] else "";
    const port: i32 = port_value.integer();
    if (port < 0) {
        ctx.vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return api.Value.Error;
    }

    switch (protocol_value.integer()) {
        0 => {
            const stream = std.net.tcpConnectToHost(api.VM.allocator, address, @intCast(
                u16,
                port,
            )) catch |err| {
                handleConnectError(ctx.vm, err);

                return api.Value.Error;
            };

            return api.Value.fromInteger(@intCast(i32, stream.handle));
        },
        1, // TODO: UDP
        2, // TODO: IPC
        => {
            ctx.vm.bz_pushError("lib.errors.NotYetImplementedError", "lib.errors.NotYetImplementedError".len);

            return api.Value.Error;
        },
        else => {
            ctx.vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

            return api.Value.Error;
        },
    }
}

export fn SocketConnect(ctx: *api.NativeCtx) c_int {
    const result = SocketConnect_raw(
        ctx,
        ctx.vm.bz_peek(2),
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

export fn SocketClose_raw(_: *api.NativeCtx, handle_value: api.Value) void {
    const socket: std.os.socket_t = @intCast(
        std.os.socket_t,
        handle_value.integer(),
    );

    std.os.closeSocket(socket);
}

export fn SocketClose(ctx: *api.NativeCtx) c_int {
    SocketClose_raw(ctx, ctx.vm.bz_peek(0));

    return 0;
}

fn handleReadAllError(vm: *api.VM, err: anytype) void {
    // FIXME: here a zig bug: if i remove OutOfMemory and StreamTooLong it complains they're missing but if i put them it complains they are not required...
    //        switch -> if
    if (err == error.AccessDenied) {
        vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len);
    } else if (err == error.InputOutput) {
        vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len);
    } else if (err == error.IsDir) {
        vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len);
    } else if (err == error.SystemResources) {
        vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len);
    } else if (err == error.WouldBlock) {
        vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len);
    } else if (err == error.OperationAborted) {
        vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "OperationAborted", "OperationAborted".len);
    } else if (err == error.BrokenPipe) {
        vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "BrokenPipe", "BrokenPipe".len);
    } else if (err == error.ConnectionResetByPeer) {
        vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len);
    } else if (err == error.ConnectionTimedOut) {
        vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionTimedOut", "ConnectionTimedOut".len);
    } else if (err == error.NotOpenForReading) {
        vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "NotOpenForReading", "NotOpenForReading".len);
    } else if (err == error.Unexpected) {
        vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len);
    } else {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);
    }
    // error.StreamTooLong => vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "StreamTooLong", "StreamTooLong".len),
    // error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
}

export fn SocketRead_raw(ctx: *api.NativeCtx, handle_value: api.Value, n_value: api.Value) api.Value {
    const n = n_value.integer();
    if (n < 0) {
        ctx.vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return api.Value.Error;
    }

    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        handle_value.integer(),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = api.VM.allocator.alloc(u8, @intCast(usize, n)) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    };

    // bz_string will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch |err| {
        handleReadAllError(ctx.vm, err);

        return api.Value.Error;
    };

    if (read == 0) {
        return api.Value.Null;
    }
    return (api.ObjString.bz_string(ctx.vm, if (buffer[0..read].len > 0) @ptrCast([*]const u8, buffer[0..read]) else null, read) orelse {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    }).bz_objStringToValue();
}

export fn SocketRead(ctx: *api.NativeCtx) c_int {
    const result = SocketRead_raw(ctx, ctx.vm.bz_peek(1), ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

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

export fn SocketReadLine_raw(ctx: *api.NativeCtx, handle_value: api.Value) api.Value {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        handle_value.integer(),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = reader.readUntilDelimiterAlloc(api.VM.allocator, '\n', 16 * 8 * 64) catch |err| {
        handleReadLineError(ctx.vm, err);

        return api.Value.Error;
    };

    // EOF?
    if (buffer.len == 0) {
        return api.Value.Null;
    }
    return (api.ObjString.bz_string(ctx.vm, if (buffer.len > 0) @ptrCast([*]const u8, buffer) else null, buffer.len) orelse {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    }).bz_objStringToValue();
}

export fn SocketReadLine(ctx: *api.NativeCtx) c_int {
    const result = SocketReadLine_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

export fn SocketReadAll_raw(ctx: *api.NativeCtx, handle_value: api.Value) api.Value {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        handle_value.integer(),
    );

    const stream: std.net.Stream = .{ .handle = handle };
    const reader = stream.reader();

    var buffer = reader.readAllAlloc(api.VM.allocator, 16 * 8 * 64) catch |err| {
        handleReadAllError(ctx.vm, err);

        return api.Value.Error;
    };

    // EOF?
    if (buffer.len == 0) {
        return api.Value.Null;
    }
    return (api.ObjString.bz_string(ctx.vm, if (buffer.len > 0) @ptrCast([*]const u8, buffer) else null, buffer.len) orelse {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    }).bz_objStringToValue();
}

export fn SocketReadAll(ctx: *api.NativeCtx) c_int {
    const result = SocketReadAll_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

export fn SocketWrite_raw(ctx: *api.NativeCtx, handle_value: api.Value, value_value: api.Value) api.Value {
    const handle: std.os.socket_t = @intCast(
        std.os.socket_t,
        handle_value.integer(),
    );

    const stream: std.net.Stream = .{ .handle = handle };

    var len: usize = 0;
    var value = value_value.bz_valueToString(&len);

    if (len == 0) {
        return api.Value.Void;
    }

    _ = stream.write(value.?[0..len]) catch |err| {
        switch (err) {
            error.AccessDenied => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
            error.InputOutput => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len),
            error.SystemResources => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
            error.WouldBlock => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
            error.OperationAborted => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "OperationAborted", "OperationAborted".len),
            error.BrokenPipe => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "BrokenPipe", "BrokenPipe".len),
            error.ConnectionResetByPeer => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
            error.DiskQuota => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "DiskQuota", "DiskQuota".len),
            error.FileTooBig => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileTooBig", "FileTooBig".len),
            error.NoSpaceLeft => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoSpaceLeft", "NoSpaceLeft".len),
            error.Unexpected => ctx.vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
            error.NotOpenForWriting => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "NotOpenForWriting", "NotOpenForWriting".len),
            error.LockViolation => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "LockViolation", "LockViolation".len),
        }

        return api.Value.Error;
    };

    return api.Value.Void;
}

export fn SocketWrite(ctx: *api.NativeCtx) c_int {
    if (SocketWrite_raw(ctx, ctx.vm.bz_peek(1), ctx.vm.bz_peek(0)).isError()) {
        return -1;
    }

    return 0;
}

export fn SocketServerStart_raw(ctx: *api.NativeCtx, address_value: api.Value, port_value: api.Value, reuse_address_value: api.Value) api.Value {
    var len: usize = 0;
    const address_str = api.Value.bz_valueToString(address_value, &len);
    const address = if (len > 0) address_str.?[0..len] else "";
    const port = port_value.integer();
    if (port < 0) {
        ctx.vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return api.Value.Error;
    }

    const reuse_address: bool = reuse_address_value.boolean();

    var server = std.net.StreamServer.init(.{ .reuse_address = reuse_address });

    const list = std.net.getAddressList(api.VM.allocator, address, @intCast(u16, port)) catch |err| {
        switch (err) {
            error.ServiceUnavailable => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ServiceUnavailable", "ServiceUnavailable".len),
            error.UnknownHostName => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "UnknownHostName", "UnknownHostName".len),
            error.NameServerFailure => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "NameServerFailure", "NameServerFailure".len),
            error.TemporaryNameServerFailure => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "TemporaryNameServerFailure", "TemporaryNameServerFailure".len),
            error.HostLacksNetworkAddresses => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "HostLacksNetworkAddresses", "HostLacksNetworkAddresses".len),
            error.AddressFamilyNotSupported => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressFamilyNotSupported", "AddressFamilyNotSupported".len),
            error.Unexpected => ctx.vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
            error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        }

        return api.Value.Error;
    };
    defer list.deinit();

    if (list.addrs.len == 0) {
        ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressNotResolved", "AddressNotResolved".len);

        return api.Value.Error;
    }

    server.listen(list.addrs[0]) catch |err| {
        switch (err) {
            error.NoDevice => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
            error.AlreadyConnected => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AlreadyConnected", "AlreadyConnected".len),
            error.SocketNotBound => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SocketNotBound", "SocketNotBound".len),
            error.AddressNotAvailable => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressNotAvailable", "AddressNotAvailable".len),
            error.SymLinkLoop => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
            error.NameTooLong => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
            error.FileNotFound => ctx.vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
            error.NotDir => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
            error.ReadOnlyFileSystem => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ReadOnlyFileSystem", "ReadOnlyFileSystem".len),
            error.AlreadyBound => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AlreadyBound", "AlreadyBound".len),
            error.InvalidProtocolOption => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "InvalidProtocolOption", "InvalidProtocolOption".len),
            error.TimeoutTooBig => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "TimeoutTooBig", "TimeoutTooBig".len),
            error.PermissionDenied => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "PermissionDenied", "PermissionDenied".len),
            error.ProtocolFamilyNotAvailable => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProtocolFamilyNotAvailable", "ProtocolFamilyNotAvailable".len),
            error.ProcessFdQuotaExceeded => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
            error.SystemFdQuotaExceeded => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
            error.ProtocolNotSupported => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProtocolNotSupported", "ProtocolNotSupported".len),
            error.SocketTypeNotSupported => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SocketTypeNotSupported", "SocketTypeNotSupported".len),
            error.AddressInUse => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressInUse", "AddressInUse".len),
            error.FileDescriptorNotASocket => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "FileDescriptorNotASocket", "FileDescriptorNotASocket".len),
            error.OperationNotSupported => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "OperationNotSupported", "OperationNotSupported".len),
            error.NetworkSubsystemFailed => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "NetworkSubsystemFailed", "NetworkSubsystemFailed".len),
            error.AddressFamilyNotSupported => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "AddressFamilyNotSupported", "AddressFamilyNotSupported".len),
            error.AccessDenied => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
            error.SystemResources => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
            error.Unexpected => ctx.vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        }

        return api.Value.Error;
    };

    return api.Value.fromInteger(@intCast(i32, server.sockfd.?));
}

export fn SocketServerStart(ctx: *api.NativeCtx) c_int {
    const result = SocketServerStart_raw(
        ctx,
        ctx.vm.bz_peek(2),
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

export fn SocketServerAccept_raw(ctx: *api.NativeCtx, handle_value: api.Value, reuse_addresse_value: api.Value) api.Value {
    const server_socket: std.os.socket_t = @intCast(
        std.os.socket_t,
        handle_value.integer(),
    );
    const reuse_address: bool = reuse_addresse_value.boolean();

    const default_options = std.net.StreamServer.Options{};
    var server = std.net.StreamServer{
        .sockfd = server_socket,
        .kernel_backlog = default_options.kernel_backlog,
        .reuse_address = reuse_address,
        .listen_address = undefined,
    };

    const connection = server.accept() catch |err| {
        switch (err) {
            error.ConnectionAborted => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ConnectionAborted", "ConnectionAborted".len),
            error.ProcessFdQuotaExceeded => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
            error.SystemFdQuotaExceeded => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
            error.SystemResources => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SystemResources", "SystemResources".len),
            error.SocketNotListening => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "SocketNotListening", "SocketNotListening".len),
            error.ProtocolFailure => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ProtocolFailure", "ProtocolFailure".len),
            error.BlockedByFirewall => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "BlockedByFirewall", "BlockedByFirewall".len),
            error.FileDescriptorNotASocket => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "FileDescriptorNotASocket", "FileDescriptorNotASocket".len),
            error.ConnectionResetByPeer => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
            error.NetworkSubsystemFailed => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "NetworkSubsystemFailed", "NetworkSubsystemFailed".len),
            error.OperationNotSupported => ctx.vm.bz_pushErrorEnum("lib.errors.SocketError", "lib.errors.SocketError".len, "OperationNotSupported", "OperationNotSupported".len),
            error.Unexpected => ctx.vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        }

        return api.Value.Error;
    };

    return api.Value.fromInteger(@intCast(i32, connection.stream.handle));
}

export fn SocketServerAccept(ctx: *api.NativeCtx) c_int {
    const result = SocketServerAccept_raw(ctx, ctx.vm.bz_peek(1), ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}
