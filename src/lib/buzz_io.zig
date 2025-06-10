const std = @import("std");
const api = @import("buzz_api.zig");
const io = @import("io.zig");
const builtin = @import("builtin");

const File = struct {
    file: std.fs.File,
    reader_buffer: ?[]u8 = null,
    io_reader: ?std.fs.File.Reader = null,
    reader: ?io.AllocatedReader = null,

    pub fn init(allocator: std.mem.Allocator, fs_file: std.fs.File) !*File {
        const file = try allocator.create(File);

        file.* = .{
            .file = fs_file,
        };

        return file;
    }

    pub fn fromUserData(userdata: u64) *File {
        return @ptrCast(
            @alignCast(
                @as(
                    *anyopaque,
                    @ptrFromInt(
                        @as(
                            usize,
                            @truncate(userdata),
                        ),
                    ),
                ),
            ),
        );
    }

    pub fn toUserData(self: *File, ctx: *api.NativeCtx) api.Value {
        return api.VM.bz_newUserData(ctx.vm, @intFromPtr(self));
    }

    pub fn deinit(self: *File, allocator: std.mem.Allocator) void {
        self.file.close();

        if (self.reader_buffer) |b| {
            allocator.free(b);
        }

        allocator.destroy(self);
    }

    pub fn getOrCreateReader(self: *File, allocator: std.mem.Allocator, max_size: ?usize) !*io.AllocatedReader {
        if (self.reader) |*r| {
            return r;
        }

        self.reader_buffer = try allocator.alloc(u8, 255);
        self.io_reader = self.file.reader(self.reader_buffer.?);
        self.reader = .{
            .reader = &self.io_reader.?.interface,
            .max_size = max_size,
        };

        return &self.reader.?;
    }
};

pub export fn getStdIn(ctx: *api.NativeCtx) callconv(.c) c_int {
    const stdin = File.init(
        api.VM.allocator,
        std.fs.File.stdin(),
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    ctx.vm.bz_push(stdin.toUserData(ctx));

    return 1;
}

pub export fn getStdOut(ctx: *api.NativeCtx) callconv(.c) c_int {
    const stdin = File.init(
        api.VM.allocator,
        std.fs.File.stdout(),
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    ctx.vm.bz_push(stdin.toUserData(ctx));

    return 1;
}

pub export fn getStdErr(ctx: *api.NativeCtx) callconv(.c) c_int {
    const stdin = File.init(
        api.VM.allocator,
        std.fs.File.stdout(),
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    ctx.vm.bz_push(stdin.toUserData(ctx));

    return 1;
}

pub export fn FileIsTTY(ctx: api.NativeCtx) callconv(.c) c_int {
    const file = File.fromUserData(ctx.vm.bz_peek(0).bz_getUserDataPtr());

    ctx.vm.bz_push(
        api.Value.fromBoolean(
            std.posix.isatty(file.file.handle),
        ),
    );

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
        error.ProcessNotFound,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn FileOpen(ctx: *api.NativeCtx) callconv(.c) c_int {
    const mode: u8 = @intCast(ctx.vm.bz_peek(0).integer());
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(1).bz_valueToString(&len);
    const filename_slice = filename.?[0..len];

    const fs_file = if (std.fs.path.isAbsolute(filename_slice))
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

    const file = File.init(
        api.VM.allocator,
        fs_file,
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    ctx.vm.bz_push(file.toUserData(ctx));

    return 1;
}

pub export fn FileClose(ctx: *api.NativeCtx) callconv(.c) c_int {
    File.fromUserData(
        ctx.vm.bz_peek(0).bz_getUserDataPtr(),
    ).deinit(api.VM.allocator);

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
    const max_size = ctx.vm.bz_peek(0);

    const file = File.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const reader = file.getOrCreateReader(
        api.VM.allocator,
        if (max_size.isInteger()) @intCast(max_size.integer()) else null,
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    const content = reader.readAll(api.VM.allocator) catch |err| {
        switch (err) {
            error.ReadFailed => {
                ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    };
    defer api.VM.allocator.free(content);

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(
            ctx.vm,
            if (content.len > 0) @as([*]const u8, @ptrCast(content)) else null,
            content.len,
        ),
    );

    return 1;
}

pub export fn FileReadLine(ctx: *api.NativeCtx) callconv(.c) c_int {
    const max_size = ctx.vm.bz_peek(0);

    const file = File.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const reader = file.getOrCreateReader(
        api.VM.allocator,
        if (max_size.isInteger()) @intCast(max_size.integer()) else null,
    ) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    if (reader.readUntilDelimiterOrEof(api.VM.allocator, '\n') catch |err| {
        switch (err) {
            error.ReadFailed => {
                ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    }) |ubuffer| {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (ubuffer.len > 0) @as([*]const u8, @ptrCast(ubuffer)) else null,
                ubuffer.len,
            ),
        );

        api.VM.allocator.free(ubuffer);
    } else {
        ctx.vm.bz_push(api.Value.Null);
    }

    return 1;
}

pub export fn FileRead(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n = ctx.vm.bz_peek(0).integer();
    if (n <= 0) {
        ctx.vm.pushError("errors.InvalidArgumentError", null);

        return -1;
    }

    const file = File.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());
    const reader = file.getOrCreateReader(api.VM.allocator, null) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    const content = reader.readN(api.VM.allocator, @intCast(n)) catch |err| {
        switch (err) {
            error.ReadFailed => {
                ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    };
    defer api.VM.allocator.free(content);

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(
            ctx.vm,
            if (content.len > 0)
                @as([*]const u8, @ptrCast(content))
            else
                null,
            content.len,
        ),
    );

    return 1;
}

pub export fn FileWrite(ctx: *api.NativeCtx) callconv(.c) c_int {
    const file = File.fromUserData(ctx.vm.bz_peek(1).bz_getUserDataPtr());

    var len: usize = 0;
    var value = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    _ = file.file.write(value.?[0..len]) catch |err| {
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
    const file = File.fromUserData(ctx.vm.bz_peek(0).bz_getUserDataPtr());

    const poller = api.VM.allocator.create(std.io.Poller(FileEnum)) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    poller.* = std.io.poll(
        api.VM.allocator,
        FileEnum,
        .{ .file = file.file },
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
        const poll_reader = poller.reader(.file);
        var reader = io.AllocatedReader{
            .reader = poll_reader,
        };
        const read = reader.readAll(api.VM.allocator) catch |err| {
            switch (err) {
                error.ReadFailed => {
                    ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err));
                    return -1;
                },
                error.OutOfMemory => {
                    ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                    unreachable;
                },
            }
        };
        defer api.VM.allocator.free(read);

        if (read.len > 0) {
            ctx.vm.bz_push(
                ctx.vm.bz_stringToValue(read.ptr, read.len),
            );
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

    var file = (if (std.fs.path.isAbsolute(filename_slice))
        std.fs.openFileAbsolute(filename_slice, .{})
    else
        std.fs.cwd().openFile(filename_slice, .{})) catch |err| {
        handleFileOpenError(ctx, err);

        return -1;
    };
    defer file.close();

    var reader_buffer = [_]u8{0};
    var file_reader = file.reader(reader_buffer[0..]);
    var reader = io.AllocatedReader{
        .reader = &file_reader.interface,
    };

    const source = reader.readAll(api.VM.allocator) catch |err| {
        switch (err) {
            error.ReadFailed => {
                ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }
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
