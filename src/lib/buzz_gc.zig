const std = @import("std");
const api = @import("buzz_api.zig");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn allocated(ctx: *api.NativeCtx) callconv(.c) c_int {
    api.bz_push(
        ctx.vm,
        .fromInteger(
            @intCast(api.bz_allocated(
                ctx.vm,
            )),
        ),
    );

    return 1;
}

pub export fn collect(ctx: *api.NativeCtx) callconv(.c) c_int {
    api.bz_collect(ctx.vm);

    return 0;
}
