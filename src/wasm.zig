const std = @import("std");

extern fn writeToStderr(string_ptr: [*]const u8, string_length: usize) void;
extern fn writeToStdout(string_ptr: [*]const u8, string_length: usize) void;
extern fn readFromStdin(buffer_ptr: [*]const u8, buffer_length: usize) isize;

pub const system = struct {
    var errno: E = undefined;

    pub const E = std.os.wasi.E;

    pub fn getErrno(rc: anytype) E {
        return if (rc == -1) errno else .SUCCESS;
    }

    pub const fd_t = std.os.wasi.fd_t;

    pub const STDERR_FILENO = std.os.wasi.STDERR_FILENO;
    pub const STDOUT_FILENO = std.os.wasi.STDOUT_FILENO;
    pub const STDIN_FILENO = std.os.wasi.STDIN_FILENO;

    pub fn write(fd: i32, buf: [*]const u8, count: usize) isize {
        // We only support writing to stderr or stdout
        if (fd != std.os.STDERR_FILENO and fd != std.os.STDOUT_FILENO) {
            errno = .PERM;
            return -1;
        }

        const clamped_count = @min(count, std.math.maxInt(isize));
        writeToStderr(buf, clamped_count);
        return @intCast(clamped_count);
    }

    pub fn read(fd: i32, buf: [*]u8, count: usize) usize {
        // We only support reading from stdin
        if (fd != std.os.STDIN_FILENO) {
            errno = .PERM;
            return 0;
        }

        const clamped_count = @min(count, std.math.maxInt(isize));
        return @intCast(
            readFromStdin(
                buf,
                clamped_count,
            ),
        );
    }
};

// Find a way to share BuildOptions between exe and buzz_api
pub const BuildOptions = .{
    .version = "0.4.0",
    .sha = "aaaaaaa",
    .mimalloc = false,
    .cycle_limit = @as(?u128, null),
    .recursive_call_limit = @as(?u32, null),
    .stack_size = @as(usize, 100_000),
    .debug = false,
    .debug_stack = false,
    .debug_current_instruction = false,
    .show_perf = false,
    .stop_on_report = false,
    .debug_placeholders = false,
    .gc_debug = false,
    .gc_debug_light = false,
    .gc_debug_access = false,
    .gc = true,
    .initial_gc = @as(usize, 1),
    .next_gc_ratio = @as(usize, 2),
    .next_full_gc_ratio = @as(usize, 4),
    .memory_limit = @as(?usize, null),
    .jit_debug = false,
    .jit_always_on = false,
    .jit = false,
    .jit_prof_threshold = @as(f128, 5.0e-02),
};
