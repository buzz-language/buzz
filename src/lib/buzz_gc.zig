const std = @import("std");
const api = @import("buzz_api.zig");

export fn allocated(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushInteger(@intCast(i32, ctx.vm.bz_allocated()));

    return 1;
}

export fn collect(ctx: *api.NativeCtx) c_int {
    if (!ctx.vm.bz_collect()) {
        ctx.vm.bz_pushError("lib.gc.CollectError", "lib.gc.CollectError".len);

        return -1;
    }

    return 0;
}
