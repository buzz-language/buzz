const std = @import("std");
const api = @import("buzz_api.zig");
const io = @import("io.zig");

pub export fn getStdIn(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(api.Value.fromInteger(@intCast(std.io.getStdIn().handle)));

    return 1;
}

pub export fn getStdOut(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(api.Value.fromInteger(@intCast(std.io.getStdOut().handle)));

    return 1;
}

pub export fn getStdErr(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(api.Value.fromInteger(@intCast(std.io.getStdErr().handle)));

    return 1;
}

pub export fn FileIsTTY(ctx: api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        ctx.vm.bz_peek(0).integer(),
    );

    ctx.vm.bz_push(api.Value.fromBoolean(std.posix.isatty(handle)));

    return 1;
}

fn handleFileOpenError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.AntivirusInterference,
        error.BadPathName,
        error.DeviceBusy,
        error.FileBusy,
        error.FileLocksNotSupported,
        error.FileTooBig,
        error.InvalidWtf8,
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

pub export fn FileOpen(ctx: *api.NativeCtx) c_int {
    const mode: u8 = @intCast(ctx.vm.bz_peek(0).integer());
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(1).bz_valueToString(&len);
    const filename_slice = filename.?[0..len];

    const file: std.fs.File = if (std.fs.path.isAbsolute(filename_slice))
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

    ctx.vm.bz_push(api.Value.fromInteger(@intCast(file.handle)));

    return 1;
}

pub export fn FileClose(ctx: *api.NativeCtx) c_int {
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
        error.SocketNotConnected,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Canceled,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.OperationAborted,
        error.BrokenPipe,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.NotOpenForReading,
        error.LockViolation,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.ProcessNotFound,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        },
    }
}

pub export fn FileReadAll(ctx: *api.NativeCtx) c_int {
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

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(
            ctx.vm,
            if (content.len > 0) @as([*]const u8, @ptrCast(content)) else null,
            content.len,
        ),
    );

    return 1;
}

fn handleFileReadLineError(ctx: *api.NativeCtx, err: anytype) void {
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

        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        },

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn FileReadLine(ctx: *api.NativeCtx) c_int {
    const handle: std.fs.File.Handle = @intCast(
        ctx.vm.bz_peek(1).integer(),
    );
    const max_size = ctx.vm.bz_peek(0);

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    const buffer = reader.readUntilDelimiterOrEofAlloc(
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
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (ubuffer.len > 0) @as([*]const u8, @ptrCast(ubuffer)) else null,
                ubuffer.len,
            ),
        );
    } else {
        ctx.vm.bz_push(api.Value.Null);
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
        error.SocketNotConnected,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Canceled,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.OperationAborted,
        error.BrokenPipe,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.NotOpenForReading,
        error.LockViolation,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

        error.ProcessNotFound,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn FileRead(ctx: *api.NativeCtx) c_int {
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
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    // bz_stringToValue will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer) catch |err| {
        handleFileReadAllError(ctx, err);

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

// extern fun File_write(int fd, [int] bytes) > void;
pub export fn FileWrite(ctx: *api.NativeCtx) c_int {
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
            error.AccessDenied,
            error.DeviceBusy,
            error.DiskQuota,
            error.FileTooBig,
            error.NoSpaceLeft,
            error.SystemResources,
            error.WouldBlock,
            => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

            error.BrokenPipe,
            error.ConnectionResetByPeer,
            error.LockViolation,
            error.NotOpenForWriting,
            error.OperationAborted,
            => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

            error.InputOutput => ctx.vm.pushErrorEnum("errors.SocketError", "InputOutput"),
            error.InvalidArgument => ctx.vm.pushError("errors.InvalidArgumentError", null),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),

            error.ProcessNotFound,
            => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),
        }

        return -1;
    };

    return 0;
}

pub export fn runFile(ctx: *api.NativeCtx) c_int {
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

    const source = api.VM.allocator.alloc(
        u8,
        (file.stat() catch {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        }).size,
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
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

    // Run
    if (!vm.bz_run(
        if (source.len > 0) @ptrCast(source) else null,
        source.len,
        if (filename.len > 0) @ptrCast(filename) else null,
        filename.len,
    )) {
        ctx.vm.pushError("errors.InterpretError", null);

        return -1;
    }

    return 0;
}
