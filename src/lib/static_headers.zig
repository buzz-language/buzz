//! Shared registry for statically bundled Buzz header files.

/// Describes a statically bundled Buzz header.
pub const Header = struct {
    /// Name used by Buzz imports.
    name: []const u8,
    /// Header file installed under `lib/buzz` and embedded from `src/lib`.
    path: []const u8,
};

pub const buffer = Header{ .name = "buffer", .path = "buffer.buzz" };
pub const crypto = Header{ .name = "crypto", .path = "crypto.buzz" };
pub const debug = Header{ .name = "debug", .path = "debug.buzz" };
pub const errors = Header{ .name = "errors", .path = "errors.buzz" };
pub const ffi = Header{ .name = "ffi", .path = "ffi.buzz" };
pub const fs = Header{ .name = "fs", .path = "fs.buzz" };
pub const gc = Header{ .name = "gc", .path = "gc.buzz" };
pub const http = Header{ .name = "http", .path = "http.buzz" };
pub const io = Header{ .name = "io", .path = "io.buzz" };
pub const math = Header{ .name = "math", .path = "math.buzz" };
pub const os = Header{ .name = "os", .path = "os.buzz" };
pub const serialize = Header{ .name = "serialize", .path = "serialize.buzz" };
pub const std = Header{ .name = "std", .path = "std.buzz" };
pub const testing = Header{ .name = "test", .path = "testing.buzz" };
pub const toml = Header{ .name = "toml", .path = "toml.buzz" };

/// Buzz headers bundled with the compiler/runtime and installed for tooling.
pub const all = [_]Header{
    buffer,
    crypto,
    debug,
    errors,
    ffi,
    fs,
    gc,
    http,
    io,
    math,
    os,
    serialize,
    std,
    testing,
    toml,
};
