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
        return api.bz_newUserData(ctx.vm, @intFromPtr(self));
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
        self.reader = .init(
            api.VM.allocator,
            &self.io_reader.?.interface,
            max_size,
        );

        return &self.reader.?;
    }
};

pub export fn getStdIn(ctx: *api.NativeCtx) callconv(.c) c_int {
    const stdin = File.init(
        api.VM.allocator,
        std.fs.File.stdin(),
    ) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    api.bz_push(ctx.vm, stdin.toUserData(ctx));

    return 1;
}

pub export fn getStdOut(ctx: *api.NativeCtx) callconv(.c) c_int {
    const stdin = File.init(
        api.VM.allocator,
        std.fs.File.stdout(),
    ) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    api.bz_push(ctx.vm, stdin.toUserData(ctx));

    return 1;
}

pub export fn getStdErr(ctx: *api.NativeCtx) callconv(.c) c_int {
    const stdin = File.init(
        api.VM.allocator,
        std.fs.File.stdout(),
    ) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    api.bz_push(ctx.vm, stdin.toUserData(ctx));

    return 1;
}

pub export fn FileIsTTY(ctx: api.NativeCtx) callconv(.c) c_int {
    const file = File.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
    );

    api.bz_push(
        ctx.vm,
        .fromBoolean(
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
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        error.ProcessNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
    }
}

pub export fn FileOpen(ctx: *api.NativeCtx) callconv(.c) c_int {
    const mode: u8 = @intCast(api.bz_peek(ctx.vm, 0).integer());
    var len: usize = 0;
    const filename = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 1),
        &len,
    );
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
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    api.bz_push(ctx.vm, file.toUserData(ctx));

    return 1;
}

pub export fn FileClose(ctx: *api.NativeCtx) callconv(.c) c_int {
    File.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
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
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.Canceled,
        => api.pushErrorEnum(ctx.vm, "errors.SocketError", @errorName(err)),

        error.OperationAborted,
        error.BrokenPipe,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.NotOpenForReading,
        error.LockViolation,
        => api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err)),

        error.ProcessNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },
    }
}

pub export fn FileReadAll(ctx: *api.NativeCtx) callconv(.c) c_int {
    const max_size = api.bz_peek(ctx.vm, 0);

    const file = File.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const reader = file.getOrCreateReader(
        api.VM.allocator,
        if (max_size.isInteger()) @intCast(max_size.integer()) else null,
    ) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    const content = reader.readAll() catch |err| {
        switch (err) {
            error.ReadFailed => {
                api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.OutOfMemory, error.WriteFailed => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    };

    api.bz_push(
        ctx.vm,
        api.bz_stringToValue(
            ctx.vm,
            if (content.len > 0) @as([*]const u8, @ptrCast(content)) else null,
            content.len,
        ),
    );

    api.VM.allocator.free(content);

    return 1;
}

pub export fn FileReadLine(ctx: *api.NativeCtx) callconv(.c) c_int {
    const max_size = api.bz_peek(ctx.vm, 0);

    const file = File.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const reader = file.getOrCreateReader(
        api.VM.allocator,
        if (max_size.isInteger()) @intCast(max_size.integer()) else null,
    ) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    if (reader.readUntilDelimiterOrEof('\n') catch |err| {
        switch (err) {
            error.ReadFailed => {
                api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.OutOfMemory, error.WriteFailed => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    }) |ubuffer| {
        api.bz_push(
            ctx.vm,
            api.bz_stringToValue(
                ctx.vm,
                if (ubuffer.len > 0) @as([*]const u8, @ptrCast(ubuffer)) else null,
                ubuffer.len,
            ),
        );

        api.VM.allocator.free(ubuffer);
    } else {
        api.bz_push(ctx.vm, api.Value.Null);
    }

    return 1;
}

pub export fn FileRead(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n = api.bz_peek(ctx.vm, 0).integer();
    if (n <= 0) {
        api.pushError(ctx.vm, "errors.InvalidArgumentError", null);

        return -1;
    }

    const file = File.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );
    const reader = file.getOrCreateReader(api.VM.allocator, null) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    const content = reader.readN(@intCast(n)) catch |err| {
        switch (err) {
            error.ReadFailed => {
                api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.OutOfMemory, error.WriteFailed => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    };
    defer api.VM.allocator.free(content);

    api.bz_push(
        ctx.vm,
        api.bz_stringToValue(
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
    const file = File.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 1),
        ),
    );

    var len: usize = 0;
    var value = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );

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
            => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

            error.BrokenPipe,
            error.ConnectionResetByPeer,
            error.LockViolation,
            error.NotOpenForWriting,
            error.OperationAborted,
            => api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err)),

            error.MessageTooBig,
            error.InputOutput,
            => api.pushErrorEnum(ctx.vm, "errors.SocketError", "InputOutput"),

            error.InvalidArgument => api.pushError(ctx.vm, "errors.InvalidArgumentError", null),
            error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),

            error.ProcessNotFound,
            error.PermissionDenied,
            => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),
        }

        return -1;
    };

    return 0;
}

const FileEnum = enum {
    file,
};

pub export fn FileGetPoller(ctx: *api.NativeCtx) callconv(.c) c_int {
    const file = File.fromUserData(
        api.bz_getUserDataPtr(
            ctx.vm,
            api.bz_peek(ctx.vm, 0),
        ),
    );

    const poller = api.VM.allocator.create(std.Io.Poller(FileEnum)) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    poller.* = std.Io.poll(
        api.VM.allocator,
        FileEnum,
        .{ .file = file.file },
    );

    // poller.reader(.file).* = ;

    api.bz_push(
        ctx.vm,
        api.bz_newUserData(ctx.vm, @intFromPtr(poller)),
    );

    return 1;
}

fn pollerFromUserData(userdata: u64) *std.Io.Poller(FileEnum) {
    return @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(@as(usize, @truncate(userdata))))));
}

pub export fn PollerPoll(ctx: *api.NativeCtx) callconv(.c) c_int {
    const poller = pollerFromUserData(
        api.bz_getUserDataPtr(ctx.vm, api.bz_peek(ctx.vm, 1)),
    );
    const timeout_value = api.bz_peek(ctx.vm, 0);
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
        var reader = io.AllocatedReader.init(
            api.VM.allocator,
            poll_reader,
            null,
        );

        const read = reader.readAll() catch |err| {
            switch (err) {
                error.ReadFailed => {
                    api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                    return -1;
                },
                error.OutOfMemory, error.WriteFailed => {
                    api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                    unreachable;
                },
            }
        };
        defer api.VM.allocator.free(read);

        if (read.len > 0) {
            api.bz_push(
                ctx.vm,
                api.bz_stringToValue(ctx.vm, read.ptr, read.len),
            );
        } else {
            api.bz_push(ctx.vm, api.Value.Null);
        }
    } else {
        api.bz_push(ctx.vm, api.Value.Null);
    }

    return 1;
}

pub export fn PollerDeinit(ctx: *api.NativeCtx) callconv(.c) c_int {
    const poller = pollerFromUserData(
        api.bz_getUserDataPtr(ctx.vm, api.bz_peek(ctx.vm, 0)),
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
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),
        error.OperationAborted,
        error.LockViolation,
        error.NotOpenForReading,
        => api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err)),
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.SocketNotConnected,
        error.Canceled,
        error.NetworkSubsystemFailed,
        => api.pushErrorEnum(ctx.vm, "errors.SocketError", @errorName(err)),
        error.ProcessNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),
        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },
    }
}

fn handleWindowsPollError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },
    }
}

pub export fn runFile(ctx: *api.NativeCtx) callconv(.c) c_int {
    // Read file
    var len: usize = 0;
    const filename_string = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );

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
    var reader = io.AllocatedReader.init(
        api.VM.allocator,
        &file_reader.interface,
        null,
    );

    const source = reader.readAll() catch |err| {
        switch (err) {
            error.ReadFailed => {
                api.pushErrorEnum(ctx.vm, "errors.ReadWriteError", @errorName(err));
                return -1;
            },
            error.WriteFailed, error.OutOfMemory => {
                api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
                unreachable;
            },
        }
    };
    defer api.VM.allocator.free(source);

    // Init new VM
    const vm = api.bz_newVM();
    defer api.bz_deinitVM(vm);

    // Compile

    // Run
    if (!api.bz_run(
        vm,
        if (source.len > 0) @ptrCast(source) else null,
        source.len,
        if (filename.len > 0) @ptrCast(filename) else null,
        filename.len,
    )) {
        api.pushError(ctx.vm, "errors.InterpretError", null);

        return -1;
    }

    return 0;
}
