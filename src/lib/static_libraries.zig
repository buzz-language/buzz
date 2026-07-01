//! Shared registry for statically bundled Buzz headers and native libraries.

const std = @import("std");
const buzz_api = @import("buzz_api.zig");
const static_headers = @import("static_headers.zig");

/// Describes a statically bundled Buzz library.
pub const Library = struct {
    /// Header metadata for import name and installed file path.
    header: static_headers.Header,
    /// Native Zig implementation beside this file, when the library exposes externs.
    zig_path: ?[]const u8,
    /// Whether the native Zig implementation is available in wasm builds.
    wasm_native: bool,
};

/// Libraries bundled with the compiler and runtime.
pub const all = [_]Library{
    .{ .header = static_headers.buffer, .zig_path = "buzz_buffer.zig", .wasm_native = true },
    .{ .header = static_headers.crypto, .zig_path = "buzz_crypto.zig", .wasm_native = true },
    .{ .header = static_headers.debug, .zig_path = "buzz_debug.zig", .wasm_native = true },
    .{ .header = static_headers.errors, .zig_path = null, .wasm_native = false },
    .{ .header = static_headers.ffi, .zig_path = "buzz_ffi.zig", .wasm_native = false },
    .{ .header = static_headers.fs, .zig_path = "buzz_fs.zig", .wasm_native = false },
    .{ .header = static_headers.gc, .zig_path = "buzz_gc.zig", .wasm_native = true },
    .{ .header = static_headers.http, .zig_path = "buzz_http.zig", .wasm_native = false },
    .{ .header = static_headers.io, .zig_path = "buzz_io.zig", .wasm_native = false },
    .{ .header = static_headers.manifest, .zig_path = null, .wasm_native = false },
    .{ .header = static_headers.math, .zig_path = "buzz_math.zig", .wasm_native = true },
    .{ .header = static_headers.os, .zig_path = "buzz_os.zig", .wasm_native = false },
    .{ .header = static_headers.serialize, .zig_path = "buzz_serialize.zig", .wasm_native = true },
    .{ .header = static_headers.std, .zig_path = "buzz_std.zig", .wasm_native = true },
    .{ .header = static_headers.testing, .zig_path = null, .wasm_native = false },
};

/// Returns the library registered for a Buzz import name.
pub fn byName(name: []const u8) ?Library {
    inline for (all) |library| {
        if (std.mem.eql(u8, name, library.header.name)) {
            return library;
        }
    }

    return null;
}

/// Returns the native method map for statically linked libraries on the target.
pub fn nativeLibraries(comptime is_wasm: bool) std.StaticStringMap(std.StaticStringMap(buzz_api.NativeFn)) {
    comptime {
        const count = nativeLibraryCount(is_wasm);
        var entries: [count]struct { []const u8, std.StaticStringMap(buzz_api.NativeFn) } = undefined;
        var index = 0;

        for (all) |library| {
            if (hasNativeLibrary(library, is_wasm)) {
                entries[index] = .{ library.header.name, nativeMethods(library) };
                index += 1;
            }
        }

        return std.StaticStringMap(std.StaticStringMap(buzz_api.NativeFn)).initComptime(entries);
    }
}

/// Counts native libraries available on the selected target.
fn nativeLibraryCount(comptime is_wasm: bool) comptime_int {
    comptime {
        var count = 0;

        for (all) |library| {
            if (hasNativeLibrary(library, is_wasm)) {
                count += 1;
            }
        }

        return count;
    }
}

/// Returns whether a library has a native implementation for the selected target.
fn hasNativeLibrary(comptime library: Library, comptime is_wasm: bool) bool {
    return library.zig_path != null and (!is_wasm or library.wasm_native);
}

/// Returns native methods for a registry entry with a native Zig implementation.
fn nativeMethods(comptime library: Library) std.StaticStringMap(buzz_api.NativeFn) {
    const zig_path = library.zig_path.?;

    if (std.mem.eql(u8, zig_path, "buzz_buffer.zig")) return checkedNativeMethods(library, @import("buzz_buffer.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_crypto.zig")) return checkedNativeMethods(library, @import("buzz_crypto.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_debug.zig")) return checkedNativeMethods(library, @import("buzz_debug.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_ffi.zig")) return checkedNativeMethods(library, @import("buzz_ffi.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_fs.zig")) return checkedNativeMethods(library, @import("buzz_fs.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_gc.zig")) return checkedNativeMethods(library, @import("buzz_gc.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_http.zig")) return checkedNativeMethods(library, @import("buzz_http.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_io.zig")) return checkedNativeMethods(library, @import("buzz_io.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_math.zig")) return checkedNativeMethods(library, @import("buzz_math.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_os.zig")) return checkedNativeMethods(library, @import("buzz_os.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_serialize.zig")) return checkedNativeMethods(library, @import("buzz_serialize.zig").library);
    if (std.mem.eql(u8, zig_path, "buzz_std.zig")) return checkedNativeMethods(library, @import("buzz_std.zig").library);

    @compileError("unknown native library path: " ++ zig_path);
}

/// Verifies a native library against its registry entry and returns its methods.
fn checkedNativeMethods(comptime library: Library, comptime native_library: anytype) std.StaticStringMap(buzz_api.NativeFn) {
    if (!std.mem.eql(u8, native_library.name, library.header.name)) {
        @compileError("native library name mismatch for " ++ library.zig_path.?);
    }

    return native_library.methods;
}
