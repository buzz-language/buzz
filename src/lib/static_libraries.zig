//! Shared registry for statically bundled Buzz headers and native libraries.

const std = @import("std");
const buzz_api = @import("buzz_api.zig");

/// Describes a statically bundled Buzz library.
pub const Library = struct {
    /// Name used by Buzz imports.
    name: []const u8,
    /// Header file installed under `lib/buzz` and embedded from `src/lib`.
    header_path: []const u8,
    /// Native Zig implementation beside this file, when the library exposes externs.
    zig_path: ?[]const u8,
    /// Whether the native Zig implementation is available in wasm builds.
    wasm_native: bool,
};

/// Libraries bundled with the compiler and runtime.
pub const all = [_]Library{
    .{ .name = "buffer", .header_path = "buffer.buzz", .zig_path = "buzz_buffer.zig", .wasm_native = true },
    .{ .name = "crypto", .header_path = "crypto.buzz", .zig_path = "buzz_crypto.zig", .wasm_native = true },
    .{ .name = "debug", .header_path = "debug.buzz", .zig_path = "buzz_debug.zig", .wasm_native = true },
    .{ .name = "errors", .header_path = "errors.buzz", .zig_path = null, .wasm_native = false },
    .{ .name = "ffi", .header_path = "ffi.buzz", .zig_path = "buzz_ffi.zig", .wasm_native = false },
    .{ .name = "fs", .header_path = "fs.buzz", .zig_path = "buzz_fs.zig", .wasm_native = false },
    .{ .name = "gc", .header_path = "gc.buzz", .zig_path = "buzz_gc.zig", .wasm_native = true },
    .{ .name = "http", .header_path = "http.buzz", .zig_path = "buzz_http.zig", .wasm_native = false },
    .{ .name = "io", .header_path = "io.buzz", .zig_path = "buzz_io.zig", .wasm_native = false },
    .{ .name = "math", .header_path = "math.buzz", .zig_path = "buzz_math.zig", .wasm_native = true },
    .{ .name = "os", .header_path = "os.buzz", .zig_path = "buzz_os.zig", .wasm_native = false },
    .{ .name = "serialize", .header_path = "serialize.buzz", .zig_path = "buzz_serialize.zig", .wasm_native = true },
    .{ .name = "std", .header_path = "std.buzz", .zig_path = "buzz_std.zig", .wasm_native = true },
    .{ .name = "test", .header_path = "testing.buzz", .zig_path = null, .wasm_native = false },
    .{ .name = "toml", .header_path = "toml.buzz", .zig_path = null, .wasm_native = false },
};

/// Returns the library registered for a Buzz import name.
pub fn byName(name: []const u8) ?Library {
    inline for (all) |library| {
        if (std.mem.eql(u8, name, library.name)) {
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
                entries[index] = .{ library.name, nativeMethods(library) };
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
    if (!std.mem.eql(u8, native_library.name, library.name)) {
        @compileError("native library name mismatch for " ++ library.zig_path.?);
    }

    return native_library.methods;
}
