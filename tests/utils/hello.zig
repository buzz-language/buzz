const std = @import("std");
const api = @import("buzz_api.zig");

pub export fn sayHello(_: *api.NativeCtx) callconv(.c) c_int {
    std.debug.print("hello world\n", .{});

    return 0;
}

pub export fn hello(symbol: [*:0]const u8) callconv(.c) ?api.NativeFn {
    if (std.mem.eql(u8, std.mem.span(symbol), "sayHello")) {
        return &sayHello;
    }

    return null;
}
