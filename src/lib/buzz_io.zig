const std = @import("std");
const api = @import("buzz_api.zig");

export fn getStdIn(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushInteger(@intCast(std.io.getStdIn().handle));

    return 1;
}

export fn getStdOut(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushInteger(@intCast(std.io.getStdOut().handle));

    return 1;
}

export fn getStdErr(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushInteger(@intCast(std.io.getStdErr().handle));

    return 1;
}

export fn FileIsTTY(ctx: api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        ctx.vm.bz_peek(0).integer(),
    );

    ctx.vm.bz_pushBool(std.os.isatty(handle));

    return 1;
}

fn handleFileOpenError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.DeviceBusy,
        error.FileBusy,
        error.FileLocksNotSupported,
        error.FileTooBig,
        error.InvalidHandle,
        error.InvalidUtf8,
        error.IsDir,
        error.NameTooLong,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.PathAlreadyExists,
        error.PipeBusy,
        error.ProcessFdQuotaExceeded,
        error.SharingViolation,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.WouldBlock,
        error.FileNotFound,
        error.NetworkNotFound,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

export fn FileOpen(ctx: *api.NativeCtx) c_int {
    const mode: u8 = @intCast(ctx.vm.bz_peek(0).integer());
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

    ctx.vm.bz_pushInteger(@intCast(file.handle));

    return 1;
}

export fn FileClose(ctx: *api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        ctx.vm.bz_peek(0).integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    file.close();

    return 0;
}

fn handleFileReadWriteError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.FileTooBig,
        error.InputOutput,
        error.IsDir,
        error.SystemResources,
        error.WouldBlock,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.OperationAborted,
        error.BrokenPipe,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.NotOpenForReading,
        error.NetNameDeleted,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        error.OutOfMemory => @panic("Out of memory"),
    }
}

export fn FileReadAll(ctx: *api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        ctx.vm.bz_peek(1).integer(),
    );
    const max_size = ctx.vm.bz_peek(0);

    const file: std.fs.File = std.fs.File{ .handle = handle };

    const content: []u8 = file.readToEndAllocOptions(
        api.VM.allocator,
        if (max_size.isNull())
            std.math.maxInt(usize)
        else
            @intCast(max_size.integer()),
        null,
        @alignOf(u8),
        null,
    ) catch |err| {
        handleFileReadWriteError(ctx, err);

        return -1;
    };

    ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (content.len > 0) @as([*]const u8, @ptrCast(content)) else null, content.len) orelse {
        @panic("Out of memory");
    });

    return 1;
}

fn handleFileReadLineError(ctx: *api.NativeCtx, err: anytype) void {
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

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

export fn FileReadLine(ctx: *api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        ctx.vm.bz_peek(1).integer(),
    );
    const max_size = ctx.vm.bz_peek(0);

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    var buffer = reader.readUntilDelimiterOrEofAlloc(
        api.VM.allocator,
        '\n',
        if (max_size.isNull())
            std.math.maxInt(usize)
        else
            @intCast(max_size.integer()),
    ) catch |err| {
        handleFileReadLineError(ctx, err);

        return -1;
    };

    if (buffer) |ubuffer| {
        ctx.vm.bz_pushString(
            api.ObjString.bz_string(
                ctx.vm,
                if (ubuffer.len > 0) @as([*]const u8, @ptrCast(ubuffer)) else null,
                ubuffer.len,
            ) orelse {
                @panic("Out of memory");
            },
        );
    } else {
        ctx.vm.bz_pushNull();
    }

    return 1;
}

fn handleFileReadAllError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.InputOutput,
        error.IsDir,
        error.SystemResources,
        error.WouldBlock,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.OperationAborted,
        error.BrokenPipe,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.NotOpenForReading,
        error.NetNameDeleted,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

export fn FileRead(ctx: *api.NativeCtx) c_int {
    const n = ctx.vm.bz_peek(0).integer();
    if (n < 0) {
        ctx.vm.pushError("errors.InvalidArgumentError", null);

        return -1;
    }

    const handle: std.fs.File.Handle = @intCast(
        ctx.vm.bz_peek(1).integer(),
    );

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    var buffer = api.VM.allocator.alloc(u8, @as(usize, @intCast(n))) catch {
        @panic("Out of memory");
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
        ctx.vm.bz_pushString(api.ObjString.bz_string(ctx.vm, if (buffer[0..read].len > 0) @as([*]const u8, @ptrCast(buffer[0..read])) else null, read) orelse {
            @panic("Out of memory");
        });
    }

    return 1;
}

// extern fun File_write(int fd, [int] bytes) > void;
export fn FileWrite(ctx: *api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
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
            error.AccessDenied => ctx.vm.pushErrorEnum("errors.FileSystemError", "AccessDenied"),
            error.BrokenPipe => ctx.vm.pushErrorEnum("errors.ReadWriteError", "BrokenPipe"),
            error.ConnectionResetByPeer => ctx.vm.pushErrorEnum("errors.ReadWriteError", "ConnectionResetByPeer"),
            error.DeviceBusy => ctx.vm.pushErrorEnum("errors.FileSystemError", "DeviceBusy"),
            error.DiskQuota => ctx.vm.pushErrorEnum("errors.FileSystemError", "DiskQuota"),
            error.FileTooBig => ctx.vm.pushErrorEnum("errors.FileSystemError", "FileTooBig"),
            error.InputOutput => ctx.vm.pushErrorEnum("errors.SocketError", "InputOutput"),
            error.InvalidArgument => ctx.vm.pushError("errors.InvalidArgumentError", null),
            error.LockViolation => ctx.vm.pushErrorEnum("errors.ReadWriteError", "LockViolation"),
            error.NoSpaceLeft => ctx.vm.pushErrorEnum("errors.FileSystemError", "NoSpaceLeft"),
            error.NotOpenForWriting => ctx.vm.pushErrorEnum("errors.ReadWriteError", "NotOpenForWriting"),
            error.OperationAborted => ctx.vm.pushErrorEnum("errors.ReadWriteError", "OperationAborted"),
            error.SystemResources => ctx.vm.pushErrorEnum("errors.FileSystemError", "SystemResources"),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
            error.WouldBlock => ctx.vm.pushErrorEnum("errors.FileSystemError", "WouldBlock"),
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
        @panic("Out of memory");
    }).size) catch {
        @panic("Out of memory");
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
        if (source.len > 0) @as([*]const u8, @ptrCast(source)) else null,
        source.len,
        if (filename.len > 0) @as([*]const u8, @ptrCast(filename)) else null,
        filename.len,
    ) orelse {
        ctx.vm.pushError("errors.CompileError", null);

        return -1;
    };

    // Run
    if (!vm.bz_interpret(function)) {
        ctx.vm.pushError("errors.InterpretError", null);

        return -1;
    }

    return 0;
}
