const std = @import("std");
const api = @import("buzz_api.zig");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn abs(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(if (n_f < 0) n_f * -1 else n_f));

    return 1;
}

pub export fn acos(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(std.math.acos(n_f)));

    return 1;
}

pub export fn asin(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(std.math.asin(n_f)));

    return 1;
}

pub export fn atan(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(std.math.atan(n_f)));

    return 1;
}

pub export fn bzceil(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromInteger(@intFromFloat(std.math.ceil(n_f))));

    return 1;
}

pub export fn bzcos(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(std.math.cos(n_f)));

    return 1;
}

pub export fn bzexp(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(std.math.exp(n_f)));

    return 1;
}

pub export fn bzfloor(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromInteger(@intFromFloat(std.math.floor(n_f))));

    return 1;
}

pub export fn bzlog(ctx: *api.NativeCtx) c_int {
    const base_i: f64 = ctx.vm.bz_peek(1).float();
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(std.math.log(f64, base_i, n_f)));

    return 1;
}

pub export fn maxFloat(ctx: *api.NativeCtx) c_int {
    const a_f = ctx.vm.bz_peek(0).float();
    const b_f = ctx.vm.bz_peek(1).float();

    ctx.vm.bz_push(api.Value.fromFloat(@max(a_f, b_f)));

    return 1;
}

pub export fn minFloat(ctx: *api.NativeCtx) c_int {
    const a_f = ctx.vm.bz_peek(0).float();
    const b_f = ctx.vm.bz_peek(1).float();

    ctx.vm.bz_push(api.Value.fromFloat(@min(a_f, b_f)));

    return 1;
}

pub export fn maxInt(ctx: *api.NativeCtx) c_int {
    const a_f = ctx.vm.bz_peek(0).integer();
    const b_f = ctx.vm.bz_peek(1).integer();

    ctx.vm.bz_push(api.Value.fromInteger(@max(a_f, b_f)));

    return 1;
}

pub export fn minInt(ctx: *api.NativeCtx) c_int {
    const a_f = ctx.vm.bz_peek(0).integer();
    const b_f = ctx.vm.bz_peek(1).integer();

    ctx.vm.bz_push(api.Value.fromInteger(@min(a_f, b_f)));

    return 1;
}

pub export fn bzsin(ctx: *api.NativeCtx) c_int {
    const n: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(std.math.sin(n)));

    return 1;
}

pub export fn bzsqrt(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(std.math.sqrt(n_f)));

    return 1;
}

pub export fn bztan(ctx: *api.NativeCtx) c_int {
    const n: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_push(api.Value.fromFloat(std.math.tan(n)));

    return 1;
}

pub export fn pow(ctx: *api.NativeCtx) c_int {
    const n = ctx.vm.bz_peek(1);
    const p = ctx.vm.bz_peek(0);

    const n_i: ?i32 = if (n.isInteger()) n.integer() else null;
    const n_f: ?f64 = if (n.isFloat()) n.float() else null;

    const p_i: ?i32 = if (p.isInteger()) p.integer() else null;
    const p_f: ?f64 = if (p.isFloat()) p.float() else null;

    if (p_f) |pf| {
        ctx.vm.bz_push(api.Value.fromFloat(
            std.math.pow(f64, n_f orelse @as(f64, @floatFromInt(n_i.?)), pf),
        ));
    } else if (n_f) |nf| {
        ctx.vm.bz_push(api.Value.fromFloat(
            std.math.pow(f64, nf, p_f orelse @as(f64, @floatFromInt(p_i.?))),
        ));
    } else {
        ctx.vm.bz_push(
            api.Value.fromInteger(
                std.math.powi(i32, n_i.?, p_i.?) catch |err| {
                    switch (err) {
                        error.Overflow => ctx.vm.pushError("errors.OverflowError", null),
                        error.Underflow => ctx.vm.pushError("errors.UnderflowError", null),
                    }

                    return -1;
                },
            ),
        );
    }

    return 1;
}
