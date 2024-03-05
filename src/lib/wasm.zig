const std = @import("std");

extern fn writeToStderr(string_ptr: [*]const u8, string_length: usize) void;
extern fn writeToStdout(string_ptr: [*]const u8, string_length: usize) void;
extern fn readFromStdin(buffer_ptr: [*]const u8, buffer_length: usize) isize;

pub const system = struct {
    var errno: E = undefined;

    pub const E = std.os.emscripten.E;

    pub fn getErrno(rc: anytype) E {
        return if (rc == -1) errno else .SUCCESS;
    }

    pub const fd_t = std.os.emscripten.fd_t;

    pub const STDERR_FILENO = std.os.emscripten.STDERR_FILENO;
    pub const STDOUT_FILENO = std.os.emscripten.STDOUT_FILENO;
    pub const STDIN_FILENO = std.os.emscripten.STDIN_FILENO;

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
