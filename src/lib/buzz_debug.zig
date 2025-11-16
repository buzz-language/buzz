const std = @import("std");
const api = @import("buzz_api.zig");
const io = @import("io.zig");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn dump(ctx: *api.NativeCtx) callconv(.c) c_int {
    api.bz_valueDump(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
    );

    io.print("\n", .{});

    return 0;
}
