const std = @import("std");
const api = @import("./buzz_api.zig");

export fn getStdIn_raw(_: *api.NativeCtx) api.Value {
    return api.Value.fromInteger(@intCast(i32, std.io.getStdIn().handle));
}

export fn getStdIn(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(getStdIn_raw(ctx));

    return 1;
}

export fn getStdOut_raw(_: *api.NativeCtx) api.Value {
    return api.Value.fromInteger(@intCast(i32, std.io.getStdOut().handle));
}

export fn getStdOut(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(getStdOut_raw(ctx));

    return 1;
}

export fn getStdErr_raw(_: *api.NativeCtx) api.Value {
    return api.Value.fromInteger(@intCast(i32, std.io.getStdErr().handle));
}

export fn getStdErr(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(getStdErr_raw(ctx));

    return 1;
}

fn handleFileOpenError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.BadPathName => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "BadPathName", "BadPathName".len),
        error.DeviceBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "DeviceBusy", "DeviceBusy".len),
        error.FileBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileBusy", "FileBusy".len),
        error.FileTooBig => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileTooBig", "FileTooBig".len),
        error.InvalidHandle => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidHandle", "InvalidHandle".len),
        error.InvalidUtf8 => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidUtf8", "InvalidUtf8".len),
        error.IsDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.NameTooLong => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
        error.NoDevice => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
        error.NoSpaceLeft => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoSpaceLeft", "NoSpaceLeft".len),
        error.NotDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
        error.PathAlreadyExists => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PathAlreadyExists", "PathAlreadyExists".len),
        error.PipeBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PipeBusy", "PipeBusy".len),
        error.ProcessFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
        error.SharingViolation => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SharingViolation", "SharingViolation".len),
        error.SymLinkLoop => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
        error.SystemFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.WouldBlock => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
        error.FileLocksNotSupported => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileLocksNotSupported", "FileLocksNotSupported".len),

        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.FileNotFound => vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
    }
}

export fn FileOpen_raw(ctx: *api.NativeCtx, mode_value: api.Value, filename_value: api.Value) api.Value {
    const mode: u8 = @intCast(u8, mode_value.integer());
    var len: usize = 0;
    const filename = filename_value.bz_valueToString(&len);
    const filename_slice = filename.?[0..len];

    var file: std.fs.File = if (std.fs.path.isAbsolute(filename_slice))
        switch (mode) {
            0 => std.fs.openFileAbsolute(filename_slice, .{ .mode = .read_only }) catch |err| {
                handleFileOpenError(ctx.vm, err);

                return api.Value.Error;
            },
            else => std.fs.createFileAbsolute(filename_slice, .{ .read = mode != 1 }) catch |err| {
                handleFileOpenError(ctx.vm, err);

                return api.Value.Error;
            },
        }
    else switch (mode) {
        0 => std.fs.cwd().openFile(filename_slice, .{ .mode = .read_only }) catch |err| {
            handleFileOpenError(ctx.vm, err);

            return api.Value.Error;
        },
        else => std.fs.cwd().createFile(filename_slice, .{ .read = mode != 1 }) catch |err| {
            handleFileOpenError(ctx.vm, err);

            return api.Value.Error;
        },
    };

    return api.Value.fromInteger(@intCast(i32, file.handle));
}

export fn FileOpen(ctx: *api.NativeCtx) c_int {
    const result = FileOpen_raw(
        ctx,
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

export fn FileClose_raw(_: *api.NativeCtx, handle_value: api.Value) void {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        handle_value.integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    file.close();
}

export fn FileClose(ctx: *api.NativeCtx) c_int {
    FileClose_raw(ctx, ctx.vm.bz_peek(0));

    return 0;
}

fn handleFileReadWriteError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.FileTooBig => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileTooBig", "FileTooBig".len),
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
        error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
    }
}

export fn FileReadAll_raw(ctx: *api.NativeCtx, handle_value: api.Value) api.Value {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        handle_value.integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };

    const content: []u8 = file.readToEndAllocOptions(
        api.VM.allocator,
        std.math.maxInt(u64),
        null,
        @alignOf(u8),
        null,
    ) catch |err| {
        handleFileReadWriteError(ctx.vm, err);

        return api.Value.Error;
    };

    return (api.ObjString.bz_string(
        ctx.vm,
        if (content.len > 0) @ptrCast([*]const u8, content) else null,
        content.len,
    ) orelse {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    }).bz_objStringToValue();
}

export fn FileReadAll(ctx: *api.NativeCtx) c_int {
    const result = FileReadAll_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

fn handleFileReadLineError(vm: *api.VM, err: anytype) void {
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
    }
}

export fn FileReadLine_raw(ctx: *api.NativeCtx, handle_value: api.Value) api.Value {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        handle_value.integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    var buffer = reader.readUntilDelimiterOrEofAlloc(api.VM.allocator, '\n', 16 * 8 * 64) catch |err| {
        handleFileReadLineError(ctx.vm, err);

        return api.Value.Error;
    };

    if (buffer) |ubuffer| {
        return (api.ObjString.bz_string(
            ctx.vm,
            if (ubuffer.len > 0) @ptrCast([*]const u8, ubuffer) else null,
            ubuffer.len,
        ) orelse {
            ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return api.Value.Error;
        }).bz_objStringToValue();
    }
    return api.Value.Null;
}

export fn FileReadLine(ctx: *api.NativeCtx) c_int {
    const result = FileReadLine_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

fn handleFileReadAllError(vm: *api.VM, err: anytype) void {
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
    }
}

export fn FileRead_raw(ctx: *api.NativeCtx, handle_value: api.Value, n_value: api.Value) api.Value {
    const n = n_value.integer();
    if (n < 0) {
        ctx.vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return api.Value.Error;
    }

    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        handle_value.integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    var buffer = api.VM.allocator.alloc(u8, @intCast(usize, n)) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    };

    // bz_string will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch |err| {
        handleFileReadAllError(ctx.vm, err);

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

export fn FileRead(ctx: *api.NativeCtx) c_int {
    const result = FileRead_raw(
        ctx,
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}

export fn FileWrite_raw(ctx: *api.NativeCtx, handle_value: api.Value, value_value: api.Value) api.Value {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        handle_value.integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };

    var len: usize = 0;
    var value = value_value.bz_valueToString(&len);

    if (len == 0) {
        return api.Value.Error;
    }

    _ = file.write(value.?[0..len]) catch |err| {
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

// extern fun File_write(int fd, [int] bytes) > void;
export fn FileWrite(ctx: *api.NativeCtx) c_int {
    const result = FileWrite_raw(
        ctx,
        ctx.vm.bz_peek(1),
        ctx.vm.bz_peek(0),
    );

    if (result.isError()) {
        return -1;
    }

    return 0;
}

export fn runFile_raw(ctx: *api.NativeCtx, filename_value: api.Value) api.Value {
    // Read file
    var len: usize = 0;
    const filename_string = filename_value.bz_valueToString(&len);

    const filename: []const u8 = filename_string.?[0..len];
    const filename_slice: []const u8 = std.mem.sliceTo(filename, 0);

    var file: std.fs.File = (if (std.fs.path.isAbsolute(filename_slice))
        std.fs.openFileAbsolute(filename_slice, .{})
    else
        std.fs.cwd().openFile(filename_slice, .{})) catch |err| {
        handleFileOpenError(ctx.vm, err);

        return api.Value.Error;
    };
    defer file.close();

    const source = api.VM.allocator.alloc(u8, (file.stat() catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    }).size) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return api.Value.Error;
    };

    _ = file.readAll(source) catch |err| {
        handleFileReadAllError(ctx.vm, err);

        return api.Value.Error;
    };
    defer api.VM.allocator.free(source);

    // Init new VM
    var vm = ctx.vm.bz_newVM();
    defer vm.bz_deinitVM();

    // Compile
    var function = vm.bz_compile(
        if (source.len > 0) @ptrCast([*]const u8, source) else null,
        source.len,
        if (filename.len > 0) @ptrCast([*]const u8, filename) else null,
        filename.len,
    ) orelse {
        ctx.vm.bz_pushError("lib.errors.CompileError", "lib.errors.CompileError".len);

        return api.Value.Error;
    };

    // Run
    if (!vm.bz_interpret(function)) {
        ctx.vm.bz_pushError("lib.errors.InterpretError", "lib.errors.InterpretError".len);

        return api.Value.Error;
    }

    return api.Value.Void;
}

export fn runFile(ctx: *api.NativeCtx) c_int {
    const result = runFile_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    return 0;
}
