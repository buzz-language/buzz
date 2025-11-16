const std = @import("std");
const api = @import("buzz_api.zig");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn serializeValue(ctx: *api.NativeCtx) callconv(.c) c_int {
    const to_serialize = api.bz_peek(ctx.vm, 0);

    var error_value = api.Value.Void;
    const serialized = api.bz_serialize(ctx.vm, to_serialize, &error_value);

    if (error_value.val != api.Value.Void.val) {
        api.bz_push(ctx.vm, error_value);

        return -1;
    }

    api.bz_push(ctx.vm, serialized);

    return 1;
}
