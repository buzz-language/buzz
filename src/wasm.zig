const std = @import("std");

extern fn writeToStderr(string_ptr: [*]const u8, string_length: usize) callconv(.c) void;
extern fn writeToStdout(string_ptr: [*]const u8, string_length: usize) callconv(.c) void;
extern fn readFromStdin(buffer_ptr: [*]const u8, buffer_length: usize) callconv(.c) isize;

pub const WasmStderrWriter = struct {
    fn drain(w: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        if (w.buffer.len > 0) {
            writeToStderr(w.buffer.ptr, w.buffer.len);
        }

        var written: usize = 0;
        for (data[0 .. data.len - 1]) |element| {
            writeToStderr(element.ptr, element.len);
            written += element.len;
        }

        const last = data[data.len - 1];
        for (0..splat) |_| {
            writeToStderr(last.ptr, last.len);
            written += last.len;
        }

        return written;
    }
};

pub const WasmStdoutWriter = struct {
    fn drain(w: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        if (w.buffer.len > 0) {
            writeToStdout(w.buffer.ptr, w.buffer.len);
        }

        var written: usize = 0;
        for (data[0 .. data.len - 1]) |element| {
            writeToStdout(element.ptr, element.len);
            written += element.len;
        }

        const last = data[data.len - 1];
        for (0..splat) |_| {
            writeToStdout(last.ptr, last.len);
        }

        return written + last.len * splat;
    }
};

pub const WasmStdinReader = struct {
    fn stream(_: *std.Io.Reader, w: *std.Io.Writer, _: std.Io.Reader.Limit) std.Io.Reader.StreamError!usize {
        const dest = try w.writableSliceGreedy(1);

        return @intCast(
            readFromStdin(dest.ptr, dest.len),
        );
    }
};

pub const io = struct {
    pub fn getStdInHandle() std.posix.fd_t {
        return std.os.emscripten.STDIN_FILENO;
    }

    pub fn getStdErrHandle() std.posix.fd_t {
        return std.os.emscripten.STDERR_FILENO;
    }

    pub fn getStdOutHandle() std.posix.fd_t {
        return std.os.emscripten.STDOUT_FILENO;
    }
};

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
