const std = @import("std");
const api = @import("buzz_api.zig");

pub export fn reflect(ctx: *api.NativeCtx) callconv(.c) c_int {
    const value = ctx.vm.bz_peek(0);

    ctx.vm.bz_push(
        ctx.vm.bz_reflect(value),
    );

    return 0;
}
