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
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
        error.DeviceBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DeviceBusy"),
        error.FileBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileBusy"),
        error.FileTooBig => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileTooBig"),
        error.InvalidHandle => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidHandle"),
        error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
        error.IsDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir"),
        error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
        error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
        error.NoSpaceLeft => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoSpaceLeft"),
        error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
        error.PathAlreadyExists => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PathAlreadyExists"),
        error.PipeBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PipeBusy"),
        error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ProcessFdQuotaExceeded"),
        error.SharingViolation => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SharingViolation"),
        error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
        error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemFdQuotaExceeded"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
        error.WouldBlock => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "WouldBlock"),
        error.FileLocksNotSupported => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileLocksNotSupported"),

        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        error.FileNotFound => ctx.vm.pushError("lib.errors.FileNotFoundError"),
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
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.FileTooBig => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileTooBig"),
        error.InputOutput => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InputOutput"),
        error.IsDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
        error.WouldBlock => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "WouldBlock"),
        error.OperationAborted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "OperationAborted"),
        error.BrokenPipe => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "BrokenPipe"),
        error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "ConnectionResetByPeer"),
        error.ConnectionTimedOut => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "ConnectionTimedOut"),
        error.NotOpenForReading => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NotOpenForReading"),
        error.NetNameDeleted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NetNameDeleted"),

        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        error.OutOfMemory => ctx.vm.pushError("lib.errors.OutOfMemoryError"),
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
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

        return -1;
    });

    return 1;
}

fn handleFileReadLineError(ctx: *api.NativeCtx, err: anytype) void {
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
                ctx.vm.pushError("lib.errors.OutOfMemoryError");

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
        error.NetNameDeleted => ctx.vm.pushErrorEnum("lib.errors.ReadWriteError", "NetNameDeleted"),

        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
    }
}

export fn FileRead(ctx: *api.NativeCtx) c_int {
    const n = ctx.vm.bz_peek(0).integer();
    if (n < 0) {
        ctx.vm.pushError("lib.errors.InvalidArgumentError");

        return -1;
    }

    const handle: std.fs.File.Handle = @intCast(
        std.fs.File.Handle,
        ctx.vm.bz_peek(1).integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    var buffer = api.VM.allocator.alloc(u8, @intCast(usize, n)) catch {
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

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
            ctx.vm.pushError("lib.errors.OutOfMemoryError");

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
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

        return -1;
    }).size) catch {
        ctx.vm.pushError("lib.errors.OutOfMemoryError");

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
        ctx.vm.pushError("lib.errors.CompileError");

        return -1;
    };

    // Run
    if (!vm.bz_interpret(function)) {
        ctx.vm.pushError("lib.errors.InterpretError");

        return -1;
    }

    return 0;
}
