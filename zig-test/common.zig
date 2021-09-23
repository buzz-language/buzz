const std = @import("std");

const A = struct {
    name: []const u8
};

export fn printName(a: *A) void {
    std.debug.warn("{s}\n", .{a.name});
}

extern fn dlerror() [*:0]u8;

pub fn main() !void {
    var buf: [100] u8 = undefined;
    std.debug.warn("cwd: {s}\n", .{std.os.getcwd(buf[0..])});

    var libexport: ?std.DynLib = std.DynLib.open("./libexport.dylib") catch null;
    
    if (libexport) |*lib| {
        if (lib.lookup(fn (*A) void, "printPrintName")) |p| {
            var a = A{
                .name = "hello world"
            };

            p(&a);
        }
    } else {
        std.debug.warn("{s}\n", .{dlerror()});
    }
}

// Build with: zig build-exe common.zig -dynamic
//             zig build-lib common.zig -dynamic