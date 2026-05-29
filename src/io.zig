//! Because of https://ziglang.org/download/0.12.0/release-notes.html#Bring-Your-Own-OS-API-Layer-Regressed
//! we have to add this abstraction layer to avoid using io.getStdIn/Err/Out

const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const is_windows = builtin.os.tag == .windows;
const wasm = @import("wasm.zig");

pub const Io = if (is_wasm)
    void
else
    std.Io;

pub fn stdoutWriter(io: Io) std.Io.File.Writer {
    return if (!is_wasm)
        std.Io.File.stdout().writer(io, &.{})
    else
        .{
            .io = undefined,
            .file = undefined,
            .interface = .{
                .buffer = &.{},
                .vtable = &.{
                    .drain = wasm.WasmStdoutWriter.drain,
                },
            },
        };
}

pub fn stderrWriter(io: Io) std.Io.File.Writer {
    return if (!is_wasm)
        std.Io.File.stderr().writer(io, &.{})
    else
        .{
            .io = undefined,
            .file = undefined,
            .interface = .{
                .buffer = &.{},
                .vtable = &.{
                    .drain = wasm.WasmStderrWriter.drain,
                },
            },
        };
}

/// Writer type used for Buzz-owned terminal UI output.
pub const UiWriter = if (is_windows and !is_wasm) WindowsConsoleUtf16Writer else std.Io.File.Writer;

/// Returns a stdout writer for Buzz-owned UI text.
pub fn uiStdoutWriter(io: Io, allocator: std.mem.Allocator) UiWriter {
    if (comptime is_windows and !is_wasm) {
        return WindowsConsoleUtf16Writer.init(std.Io.File.stdout(), io, allocator);
    } else {
        return stdoutWriter(io);
    }
}

/// Returns a stderr writer for Buzz-owned UI text.
pub fn uiStderrWriter(io: Io, allocator: std.mem.Allocator) UiWriter {
    if (comptime is_windows and !is_wasm) {
        return WindowsConsoleUtf16Writer.init(std.Io.File.stderr(), io, allocator);
    } else {
        return stderrWriter(io);
    }
}

/// UTF-16 console writer for Windows terminal UI output.
const WindowsConsoleUtf16Writer = if (is_windows and !is_wasm) struct {
    const windows = std.os.windows;

    /// Windows API used to identify real console handles.
    extern "kernel32" fn GetConsoleMode(
        hConsoleHandle: windows.HANDLE,
        lpMode: *windows.DWORD,
    ) callconv(.winapi) windows.BOOL;

    /// Windows API used to write UTF-16 code units to a console.
    extern "kernel32" fn WriteConsoleW(
        hConsoleOutput: windows.HANDLE,
        lpBuffer: [*]const u16,
        nNumberOfCharsToWrite: windows.DWORD,
        lpNumberOfCharsWritten: *windows.DWORD,
        lpReserved: ?*anyopaque,
    ) callconv(.winapi) windows.BOOL;

    /// Allocator used for transient UTF-8 and UTF-16 buffers.
    allocator: std.mem.Allocator,
    /// Console or stream file handle.
    file: std.Io.File,
    /// Existing byte writer used for pipes, files, and invalid UTF-8 fallback.
    fallback: std.Io.File.Writer,
    /// Whether `file` is a Windows console handle.
    is_console: bool,
    /// Writer interface exposed to callers.
    interface: std.Io.Writer,

    /// Creates a UI writer for a Windows stdout/stderr handle.
    pub fn init(file: std.Io.File, io: Io, allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .file = file,
            .fallback = file.writer(io, &.{}),
            .is_console = isConsole(file),
            .interface = .{
                .buffer = &.{},
                .vtable = &.{
                    .drain = drain,
                },
            },
        };
    }

    /// Detects whether the handle accepts Windows console APIs.
    fn isConsole(file: std.Io.File) bool {
        var mode: windows.DWORD = 0;

        return GetConsoleMode(file.handle, &mode).toBool();
    }

    /// Drains UTF-8 UI bytes, transcoding only for real Windows consoles.
    fn drain(io_w: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        const self: *@This() = @alignCast(@fieldParentPtr("interface", io_w));
        const buffered = io_w.buffered();
        const consumed = std.Io.Writer.countSplat(data, splat);

        if (!self.is_console) {
            try self.writeRaw(buffered, data, splat);
            io_w.end = 0;

            return consumed;
        }

        const total = buffered.len + consumed;
        if (total == 0) {
            io_w.end = 0;

            return consumed;
        }

        var allocated_utf8: ?[]u8 = null;
        defer if (allocated_utf8) |bytes| self.allocator.free(bytes);

        const utf8 = if (buffered.len == 0 and data.len == 1 and splat == 1)
            data[0]
        else utf8: {
            var bytes = self.allocator.alloc(u8, total) catch return error.WriteFailed;
            allocated_utf8 = bytes;

            var index: usize = 0;
            @memcpy(bytes[index..][0..buffered.len], buffered);
            index += buffered.len;

            for (data[0 .. data.len - 1]) |data_bytes| {
                @memcpy(bytes[index..][0..data_bytes.len], data_bytes);
                index += data_bytes.len;
            }

            const pattern = data[data.len - 1];
            for (0..splat) |_| {
                @memcpy(bytes[index..][0..pattern.len], pattern);
                index += pattern.len;
            }

            break :utf8 bytes;
        };

        const utf16 = std.unicode.utf8ToUtf16LeAlloc(self.allocator, utf8) catch |err| switch (err) {
            error.InvalidUtf8 => {
                try self.fallback.interface.writeAll(utf8);
                io_w.end = 0;

                return consumed;
            },
            error.OutOfMemory => return error.WriteFailed,
        };
        defer self.allocator.free(utf16);

        try self.writeConsoleUtf16(utf16);
        io_w.end = 0;

        return consumed;
    }

    /// Writes bytes through the existing byte-oriented file writer.
    fn writeRaw(self: *@This(), buffered: []const u8, data: []const []const u8, splat: usize) std.Io.Writer.Error!void {
        try self.fallback.interface.writeAll(buffered);

        for (data[0 .. data.len - 1]) |bytes| {
            try self.fallback.interface.writeAll(bytes);
        }

        const pattern = data[data.len - 1];
        for (0..splat) |_| {
            try self.fallback.interface.writeAll(pattern);
        }
    }

    /// Writes UTF-16 code units directly to a Windows console handle.
    fn writeConsoleUtf16(self: *@This(), utf16: []const u16) std.Io.Writer.Error!void {
        var remaining = utf16;

        while (remaining.len > 0) {
            const chunk_len: windows.DWORD = @intCast(@min(
                remaining.len,
                @as(usize, std.math.maxInt(windows.DWORD)),
            ));
            var written: windows.DWORD = 0;

            if (!WriteConsoleW(
                self.file.handle,
                remaining.ptr,
                chunk_len,
                &written,
                null,
            ).toBool()) {
                return error.WriteFailed;
            }

            if (written == 0) {
                return error.WriteFailed;
            }

            remaining = remaining[written..];
        }
    }
} else void;

pub fn stdinReader(io: Io, buffer: []u8) std.Io.File.Reader {
    return if (!is_wasm)
        std.Io.File.stdin().reader(io, buffer)
    else
        .{
            .io = undefined,
            .file = undefined,
            .interface = .{
                .end = 0,
                .seek = 0,
                .buffer = buffer,
                .vtable = &.{
                    .stream = wasm.WasmStdinReader.stream,
                },
            },
        };
}

pub fn print(io: Io, comptime fmt: []const u8, args: anytype) void {
    if (is_wasm) {
        var writer = stderrWriter(io);
        writer.interface.print(fmt, args) catch return;
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
