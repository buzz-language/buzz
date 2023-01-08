const std = @import("std");
const api = @import("buzz_api.zig");

export fn allocated_raw(ctx: *api.NativeCtx) api.Value {
    return api.Value.fromInteger(@intCast(i32, ctx.vm.bz_allocated()));
}

export fn allocated(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(allocated_raw(ctx));

    return 1;
}

export fn collect_raw(ctx: *api.NativeCtx) api.Value {
    if (!ctx.vm.bz_collect()) {
        ctx.vm.bz_pushError("lib.gc.CollectError", "lib.gc.CollectError".len);

        return api.Value.Error;
    }

    return api.Value.Void;
}

export fn collect(ctx: *api.NativeCtx) c_int {
    const result = collect_raw(ctx);

    if (result.isError()) {
        return -1;
    }

    return 0;
}
