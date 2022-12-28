// zig build-lib -dynamic -lmimalloc -I/usr/local/include -I/usr/include -lpcre -lc -lbuzz -L/Users/giann/git/buzz/dist/lib -L/usr/local/lib/ -rpath /Users/giann/git/buzz/dist/lib tests/llvm/basic.zig --main-pkg-path ./src

const std = @import("std");
const api = @import("../../src/lib/buzz_api.zig");

export fn print(obj_string_addr: usize) void {
    const obj_string = @intToPtr(*api.ObjString, obj_string_addr);

    var len: usize = undefined;
    const string = obj_string.bz_objStringToString(&len);
    const slice = if (string) |ustring| ustring[0..len] else "";

    std.io.getStdOut().writer().print("{s}\n", .{slice}) catch unreachable;
}
