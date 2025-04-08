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
    ctx.vm.bz_peek(0).bz_valueDump(ctx.vm);

    io.print("\n", .{});

    return 0;
}
