const std = @import("std");
const api = @import("buzz_api.zig");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

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
