const std = @import("std");
const api = @import("./buzz_api.zig");

export fn getStdIn(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushInteger(@intCast(i32, std.io.getStdIn().handle));

    return 1;
}

export fn getStdOut(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushInteger(@intCast(i32, std.io.getStdOut().handle));

    return 1;
}

export fn getStdErr(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushInteger(@intCast(i32, std.io.getStdErr().handle));

    return 1;
}

fn handleFileOpenError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.BadPathName => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "BadPathName", "BadPathName".len),
        error.DeviceBusy => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "DeviceBusy", "DeviceBusy".len),
        error.FileBusy => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileBusy", "FileBusy".len),
        error.FileTooBig => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileTooBig", "FileTooBig".len),
        error.InvalidHandle => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidHandle", "InvalidHandle".len),
        error.InvalidUtf8 => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidUtf8", "InvalidUtf8".len),
        error.IsDir => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.NameTooLong => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
        error.NoDevice => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
        error.NoSpaceLeft => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoSpaceLeft", "NoSpaceLeft".len),
        error.NotDir => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
        error.PathAlreadyExists => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PathAlreadyExists", "PathAlreadyExists".len),
        error.PipeBusy => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PipeBusy", "PipeBusy".len),
        error.ProcessFdQuotaExceeded => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
        error.SharingViolation => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SharingViolation", "SharingViolation".len),
        error.SymLinkLoop => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
        error.SystemFdQuotaExceeded => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
        error.SystemResources => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.WouldBlock => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
        error.FileLocksNotSupported => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileLocksNotSupported", "FileLocksNotSupported".len),

        error.Unexpected => ctx.vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.FileNotFound => ctx.vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
    }
}

export fn FileOpen(ctx: *api.NativeCtx) c_int {
    const mode: u8 = @intCast(u8, ctx.vm.bz_peek(0).integer());
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(1).bz_valueToString(&len);
    const filename_slice = filename.?[0..len];

    var file: std.fs.File = if (std.fs.path.isAbsolute(filename_slice))
        switch (mode) {
            0 => std.fs.openFileAbsolute(filename_slice, .{ .mode = .read_only }) catch |err| {
                handleFileOpenError(ctx, err);

                return -1;
            },
            else => std.fs.createFileAbsolute(filename_slice, .{ .read = mode != 1 }) catch |err| {
                handleFileOpenError(ctx, err);

                return -1;
            },
        }
    else switch (mode) {
        0 => std.fs.cwd().openFile(filename_slice, .{ .mode = .read_only }) catch |err| {
            handleFileOpenError(ctx, err);

            return -1;
        },
        else => std.fs.cwd().createFile(filename_slice, .{ .read = mode != 1 }) catch |err| {
            handleFileOpenError(ctx, err);

            return -1;
        },
    };

    ctx.vm.bz_pushInteger(@intCast(i32, file.handle));

    return 1;
}

export fn FileClose(ctx: *api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        ctx.vm.bz_peek(0).integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    file.close();

    return 0;
}

fn handleFileReadWriteError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.FileTooBig => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileTooBig", "FileTooBig".len),
        error.InputOutput => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len),
        error.IsDir => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.SystemResources => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.WouldBlock => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
        error.OperationAborted => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "OperationAborted", "OperationAborted".len),
        error.BrokenPipe => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "BrokenPipe", "BrokenPipe".len),
        error.ConnectionResetByPeer => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
        error.ConnectionTimedOut => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionTimedOut", "ConnectionTimedOut".len),
        error.NotOpenForReading => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "NotOpenForReading", "NotOpenForReading".len),

        error.Unexpected => ctx.vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
    }
}

export fn FileReadAll(ctx: *api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        ctx.vm.bz_peek(0).integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };

    const content: []u8 = file.readToEndAllocOptions(
        api.VM.allocator,
        std.math.maxInt(u64),
        null,
        @alignOf(u8),
        null,
    ) catch |err| {
        handleFileReadWriteError(ctx, err);

        return -1;
    };

    ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (content.len > 0) @ptrCast([*]const u8, content) else null, content.len) orelse {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    });

    return 1;
}

fn handleFileReadLineError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.InputOutput => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len),
        error.IsDir => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.SystemResources => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.WouldBlock => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
        error.OperationAborted => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "OperationAborted", "OperationAborted".len),
        error.BrokenPipe => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "BrokenPipe", "BrokenPipe".len),
        error.ConnectionResetByPeer => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
        error.ConnectionTimedOut => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionTimedOut", "ConnectionTimedOut".len),
        error.NotOpenForReading => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "NotOpenForReading", "NotOpenForReading".len),
        error.StreamTooLong => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "StreamTooLong", "StreamTooLong".len),

        error.Unexpected => ctx.vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.OutOfMemory => ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
    }
}

export fn FileReadLine(ctx: *api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        ctx.vm.bz_peek(0).integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    var buffer = reader.readUntilDelimiterOrEofAlloc(api.VM.allocator, '\n', 16 * 8 * 64) catch |err| {
        handleFileReadLineError(ctx, err);

        return -1;
    };

    if (buffer) |ubuffer| {
        ctx.vm.bz_pushString(
            api.ObjString.bz_string(
                ctx.vm,
                if (ubuffer.len > 0) @ptrCast([*]const u8, ubuffer) else null,
                ubuffer.len,
            ) orelse {
                ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

                return -1;
            },
        );
    } else {
        ctx.vm.bz_pushNull();
    }

    return 1;
}

fn handleFileReadAllError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.InputOutput => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len),
        error.IsDir => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.SystemResources => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.WouldBlock => ctx.vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
        error.OperationAborted => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "OperationAborted", "OperationAborted".len),
        error.BrokenPipe => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "BrokenPipe", "BrokenPipe".len),
        error.ConnectionResetByPeer => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionResetByPeer", "ConnectionResetByPeer".len),
        error.ConnectionTimedOut => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "ConnectionTimedOut", "ConnectionTimedOut".len),
        error.NotOpenForReading => ctx.vm.bz_pushErrorEnum("lib.errors.ReadWriteError", "lib.errors.ReadWriteError".len, "NotOpenForReading", "NotOpenForReading".len),

        error.Unexpected => ctx.vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
    }
}

export fn FileRead(ctx: *api.NativeCtx) c_int {
    const n = ctx.vm.bz_peek(0).integer();
    if (n < 0) {
        ctx.vm.bz_pushError("lib.errors.InvalidArgumentError", "lib.errors.InvalidArgumentError".len);

        return -1;
    }

    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        ctx.vm.bz_peek(1).integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    var buffer = api.VM.allocator.alloc(u8, @intCast(usize, n)) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };

    // bz_string will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch |err| {
        handleFileReadAllError(ctx, err);

        return -1;
    };

    if (read == 0) {
        ctx.vm.bz_pushNull();
    } else {
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (buffer[0..read].len > 0) @ptrCast([*]const u8, buffer[0..read]) else null, read) orelse {
            ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return -1;
        });
    }

    return 1;
}

// extern fun File_write(int fd, [int] bytes) > void;
export fn FileWrite(ctx: *api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        ctx.vm.bz_peek(1).integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };

    var len: usize = 0;
    var value = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
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

        return -1;
    };

    return 0;
}

export fn runFile(ctx: *api.NativeCtx) c_int {
    // Read file
    var len: usize = 0;
    const filename_string = ctx.vm.bz_peek(0).bz_valueToString(&len);

    const filename: []const u8 = filename_string.?[0..len];
    const filename_slice: []const u8 = std.mem.sliceTo(filename, 0);

    var file: std.fs.File = (if (std.fs.path.isAbsolute(filename_slice))
        std.fs.openFileAbsolute(filename_slice, .{})
    else
        std.fs.cwd().openFile(filename_slice, .{})) catch |err| {
        handleFileOpenError(ctx, err);

        return -1;
    };
    defer file.close();

    const source = api.VM.allocator.alloc(u8, (file.stat() catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    }).size) catch {
        ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };

    _ = file.readAll(source) catch |err| {
        handleFileReadAllError(ctx, err);

        return -1;
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

        return -1;
    };

    // Run
    if (!vm.bz_interpret(function)) {
        ctx.vm.bz_pushError("lib.errors.InterpretError", "lib.errors.InterpretError".len);

        return -1;
    }

    return 0;
}
