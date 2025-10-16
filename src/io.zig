//! Because of https://ziglang.org/download/0.12.0/release-notes.html#Bring-Your-Own-OS-API-Layer-Regressed
//! we have to add this abstraction layer to avoid using io.getStdIn/Err/Out

const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const wasm = @import("wasm.zig");

var fs_stderr_writer = std.fs.File.stderr().writer(&.{});
var wasm_stderr_writer: std.Io.Writer = .{
    .buffer = &.{},
    .vtable = &.{
        .drain = wasm.WasmStderrWriter.drain,
    },
};

pub var stderrWriter = if (!is_wasm)
    &fs_stderr_writer.interface
else
    &wasm_stderr_writer;

var fs_stdout_writer = std.fs.File.stdout().writer(&.{});
var wasm_stdout_writer: std.Io.Writer = .{
    .buffer = &.{},
    .vtable = &.{
        .drain = wasm.WasmStdoutWriter.drain,
    },
};

pub var stdoutWriter = if (!is_wasm)
    &fs_stdout_writer.interface
else
    &wasm_stdout_writer;

pub fn stdinReader(buffer: []u8) std.Io.Reader {
    return if (!is_wasm)
        std.fs.File.stdin().reader(buffer).interface
    else
        .{
            .end = 0,
            .seek = 0,
            .buffer = buffer,
            .vtable = &.{
                .stream = wasm.WasmStdinReader.stream,
            },
        };
}

var stderr_mutex = std.Thread.Mutex{};

pub fn print(comptime fmt: []const u8, args: anytype) void {
    if (is_wasm) {
        stderr_mutex.lock();
        defer stderr_mutex.unlock();

        stderrWriter.print(fmt, args) catch return;
    } else {
        std.debug.print(fmt, args);
    }
}

pub const AllocatedReader = struct {
    pub const Error = error{
        ReadFailed,
        WriteFailed,
        OutOfMemory,
    };

    buffer: std.Io.Writer.Allocating,
    max_size: ?usize = null,
    reader: *std.Io.Reader,

    pub fn init(allocator: std.mem.Allocator, reader: *std.Io.Reader, max_size: ?usize) @This() {
        return .{
            .buffer = .init(allocator),
            .reader = reader,
            .max_size = max_size,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.buffer.deinit();
    }

    pub fn readUntilDelimiterOrEof(self: *AllocatedReader, delimiter: u8) Error!?[]u8 {
        std.debug.assert(self.reader.buffer.len > 0);

        var count: usize = 0;
        while (self.max_size == null or count < self.max_size.?) : (count += 1) {
            const byte = self.reader.takeByte() catch |err| {
                switch (err) {
                    error.EndOfStream => return if (count > 0)
                        try self.buffer.toOwnedSlice()
                    else
                        null,
                    error.ReadFailed => return error.ReadFailed,
                }
            };

            if (byte == delimiter) {
                break;
            }

            try self.buffer.writer.writeByte(byte);
        }

        return try self.buffer.toOwnedSlice();
    }

    pub fn readAll(self: *AllocatedReader) Error![]u8 {
        std.debug.assert(self.reader.buffer.len > 0);

        while (true) {
            const byte = self.reader.takeByte() catch |err| {
                switch (err) {
                    error.EndOfStream => break,
                    error.ReadFailed => return error.ReadFailed,
                }
            };

            try self.buffer.writer.writeByte(byte);
        }

        return try self.buffer.toOwnedSlice();
    }

    pub fn readN(self: *AllocatedReader, n: usize) Error![]u8 {
        std.debug.assert(self.reader.buffer.len > 0);

        var count: usize = 0;
        while (count < n and (self.max_size == null or count < self.max_size.?)) : (count += 1) {
            const byte = self.reader.takeByte() catch |err| {
                switch (err) {
                    error.EndOfStream => break,
                    error.ReadFailed => return error.ReadFailed,
                }
            };

            try self.buffer.writer.writeByte(byte);
        }

        return try self.buffer.toOwnedSlice();
    }
};
