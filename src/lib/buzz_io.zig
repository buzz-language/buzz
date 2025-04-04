const std = @import("std");
const api = @import("buzz_api.zig");
const io = @import("io.zig");
const builtin = @import("builtin");

pub export fn getStdIn(ctx: *api.NativeCtx) callconv(.c) c_int {
    ctx.vm.bz_push(
        api.Value.fromInteger(
            if (builtin.os.tag == .windows)
                @intCast(@intFromPtr(std.io.getStdIn().handle))
            else
                @intCast(std.io.getStdIn().handle),
        ),
    );

    return 1;
}

pub export fn getStdOut(ctx: *api.NativeCtx) callconv(.c) c_int {
    ctx.vm.bz_push(
        api.Value.fromInteger(
            if (builtin.os.tag == .windows)
                @intCast(@intFromPtr(std.io.getStdOut().handle))
            else
                @intCast(std.io.getStdOut().handle),
        ),
    );

    return 1;
}

pub export fn getStdErr(ctx: *api.NativeCtx) callconv(.c) c_int {
    ctx.vm.bz_push(
        api.Value.fromInteger(
            if (builtin.os.tag == .windows)
                @intCast(@intFromPtr(std.io.getStdErr().handle))
            else
                @intCast(std.io.getStdErr().handle),
        ),
    );

    return 1;
}

pub export fn FileIsTTY(ctx: api.NativeCtx) callconv(.c) c_int {
    const handle: std.fs.File.Handle =
        if (builtin.os.tag == .windows)
            @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(0).integer())))
        else
            @intCast(
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

        error.PermissionDenied,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn FileOpen(ctx: *api.NativeCtx) callconv(.c) c_int {
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

    ctx.vm.bz_push(
        api.Value.fromInteger(
            if (builtin.os.tag == .windows)
                @intCast(@intFromPtr(file.handle))
            else
                @intCast(file.handle),
        ),
    );

    return 1;
}

pub export fn FileClose(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.fs.File.Handle =
        if (builtin.os.tag == .windows)
            @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(0).integer())))
        else
            @intCast(
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

pub export fn FileReadAll(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.fs.File.Handle =
        if (builtin.os.tag == .windows)
            @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
        else
            @intCast(
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

pub export fn FileReadLine(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.fs.File.Handle =
        if (builtin.os.tag == .windows)
            @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
        else
            @intCast(
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

pub export fn FileRead(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n = ctx.vm.bz_peek(0).integer();
    if (n <= 0) {
        ctx.vm.pushError("errors.InvalidArgumentError", null);

        return -1;
    }

    const handle: std.fs.File.Handle =
        if (builtin.os.tag == .windows)
            @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
        else
            @intCast(
                ctx.vm.bz_peek(1).integer(),
            );

    if (n <= 255) {
        return fileSmallRead(ctx, handle, @intCast(n));
    }

    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    // Avoid heap allocation if we read less than 255 bytes
    var buffer = api.VM.allocator.alloc(u8, @as(usize, @intCast(n))) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    // bz_stringToValue will copy it
    defer api.VM.allocator.free(buffer);

    const read = reader.readAll(buffer[0..@intCast(n)]) catch |err| {
        handleFileReadAllError(ctx, err);

        return -1;
    };

    if (read == 0) {
        ctx.vm.bz_push(api.Value.Null);
    } else {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (buffer[0..read].len > 0)
                    @as([*]const u8, @ptrCast(buffer[0..read]))
                else
                    null,
                read,
            ),
        );
    }

    return 1;
}

fn fileSmallRead(ctx: *api.NativeCtx, handle: std.fs.File.Handle, n: usize) c_int {
    const file: std.fs.File = std.fs.File{ .handle = handle };
    const reader = file.reader();

    // Avoid heap allocation if we read less than 255 bytes
    var buffer = [_]u8{0} ** 255;

    const read = reader.readAll(
        buffer[0..@intCast(@min(255, n))],
    ) catch |err| {
        handleFileReadAllError(ctx, err);

        return -1;
    };

    if (read == 0) {
        ctx.vm.bz_push(api.Value.Null);
    } else {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (buffer[0..read].len > 0)
                    @as([*]const u8, @ptrCast(buffer[0..read]))
                else
                    null,
                read,
            ),
        );
    }

    return 1;
}

pub export fn FileWrite(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.fs.File.Handle =
        if (builtin.os.tag == .windows)
            @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(1).integer())))
        else
            @intCast(
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
            error.NoDevice,
            => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

            error.BrokenPipe,
            error.ConnectionResetByPeer,
            error.LockViolation,
            error.NotOpenForWriting,
            error.OperationAborted,
            => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),

            error.MessageTooBig,
            error.InputOutput,
            => ctx.vm.pushErrorEnum("errors.SocketError", "InputOutput"),

            error.InvalidArgument => ctx.vm.pushError("errors.InvalidArgumentError", null),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),

            error.ProcessNotFound,
            error.PermissionDenied,
            => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),
        }

        return -1;
    };

    return 0;
}

const FileEnum = enum {
    file,
};

pub export fn FileGetPoller(ctx: *api.NativeCtx) callconv(.c) c_int {
    const handle: std.fs.File.Handle = if (builtin.os.tag == .windows)
        @ptrFromInt(@as(usize, @intCast(ctx.vm.bz_peek(0).integer())))
    else
        @intCast(
            ctx.vm.bz_peek(0).integer(),
        );
    const file = std.fs.File{ .handle = handle };

    const poller = api.VM.allocator.create(std.io.Poller(FileEnum)) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    poller.* = std.io.poll(
        api.VM.allocator,
        FileEnum,
        .{ .file = file },
    );

    ctx.vm.bz_push(
        ctx.vm.bz_newUserData(@intFromPtr(poller)),
    );

    return 1;
}

fn pollerFromUserData(userdata: u64) *std.io.Poller(FileEnum) {
    return @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(@as(usize, @truncate(userdata))))));
}

pub export fn PollerPoll(ctx: *api.NativeCtx) callconv(.c) c_int {
    const poller = pollerFromUserData(
        ctx.vm.bz_peek(1).bz_getUserDataPtr(),
    );
    const timeout_value = ctx.vm.bz_peek(0);
    const timeout: u64 = @as(
        u64,
        @intCast(
            if (timeout_value.isInteger())
                timeout_value.integer()
            else
                0,
        ),
    ) * 1_000_000;

    const got_something = poller.pollTimeout(timeout) catch |err| {
        if (builtin.os.tag != .windows)
            handlePollError(ctx, err)
        else
            handleWindowsPollError(ctx, err);

        return -1;
    };

    if (got_something) {
        const fifo = poller.fifo(.file);
        const len = fifo.readableLength();

        if (len > 0) {
            const read = fifo.readableSliceOfLen(len);

            ctx.vm.bz_push(
                ctx.vm.bz_stringToValue(read.ptr, len),
            );

            fifo.discard(len);
        } else {
            ctx.vm.bz_push(api.Value.Null);
        }
    } else {
        ctx.vm.bz_push(api.Value.Null);
    }

    return 1;
}

pub export fn PollerDeinit(ctx: *api.NativeCtx) callconv(.c) c_int {
    const poller = pollerFromUserData(
        ctx.vm.bz_peek(0).bz_getUserDataPtr(),
    );

    poller.deinit();
    api.VM.allocator.destroy(poller);

    return 0;
}

fn handlePollError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.InputOutput,
        error.AccessDenied,
        error.SystemResources,
        error.WouldBlock,
        error.IsDir,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),
        error.OperationAborted,
        error.LockViolation,
        error.NotOpenForReading,
        => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.SocketNotConnected,
        error.Canceled,
        error.NetworkSubsystemFailed,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),
        error.ProcessNotFound,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),
        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        },
    }
}

fn handleWindowsPollError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        },
    }
}

pub export fn runFile(ctx: *api.NativeCtx) callconv(.c) c_int {
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
    var vm = api.VM.bz_newVM();
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
