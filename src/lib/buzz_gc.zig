const std = @import("std");
const api = @import("buzz_api.zig");

export fn allocated(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushInteger(@intCast(ctx.vm.bz_allocated()));

    return 1;
}

export fn collect(ctx: *api.NativeCtx) c_int {
    if (!ctx.vm.bz_collect()) {
        ctx.vm.pushError("gc.CollectError", null);

        return -1;
    }

    return 0;
}
