// Because of https://ziglang.org/download/0.12.0/release-notes.html#Bring-Your-Own-OS-API-Layer-Regressed
// we have to add this abstraction layer to avoid using io.getStdIn/Err/Out

const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const wasm = @import("wasm.zig");

fn stdErrWrite(_: void, bytes: []const u8) std.posix.WriteError!usize {
    return std.io.getStdErr().write(bytes);
}

fn stdOutWrite(_: void, bytes: []const u8) std.posix.WriteError!usize {
    return std.io.getStdOut().write(bytes);
}

fn stdInRead(_: void, buffer: []u8) std.posix.ReadError!usize {
    return std.io.getStdIn().read(buffer);
}

const StdErrWriter = std.io.Writer(
    void,
    std.posix.WriteError,
    if (is_wasm)
        wasm.stdErrWrite
    else
        stdErrWrite,
);

pub const stdErrWriter = StdErrWriter{ .context = {} };

const StdOutWriter = std.io.Writer(
    void,
    std.posix.WriteError,
    if (is_wasm)
        wasm.stdOutWrite
    else
        stdOutWrite,
);

pub const stdOutWriter = StdOutWriter{ .context = {} };

const StdInReader = std.io.Reader(
    void,
    std.posix.ReadError,
    if (is_wasm)
        wasm.stdInRead
    else
        stdInRead,
);

pub const stdInReader = StdInReader{ .context = {} };

var stderr_mutex = std.Thread.Mutex{};

pub fn print(comptime fmt: []const u8, args: anytype) void {
    if (is_wasm) {
        stderr_mutex.lock();
        defer stderr_mutex.unlock();

        stdErrWriter.print(fmt, args) catch return;
    } else {
        std.debug.print(fmt, args);
    }
}
