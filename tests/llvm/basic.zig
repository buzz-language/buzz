const std = @import("std");
const api = @import("../../src/lib/buzz_api.zig");

export fn print_raw(_: *api.NativeCtx, value: api.Value) void {
    var len: usize = undefined;
    const slice = if (value.bz_valueToString(&len)) |ustring| ustring[0..len] else "";

    std.io.getStdOut().writer().print("{s}\n", .{slice}) catch unreachable;
}

export fn print(ctx: *api.NativeCtx) c_int {
    const string = ctx.vm.bz_peek(0);

    @call(.always_inline, print_raw, .{ ctx, string });

    return 0;
}
