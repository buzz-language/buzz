const std = @import("std");
const api = @import("buzz_api.zig");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn allocated(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(api.Value.fromInteger(@intCast(ctx.vm.bz_allocated())));

    return 1;
}

pub export fn collect(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_collect();

    return 0;
}
