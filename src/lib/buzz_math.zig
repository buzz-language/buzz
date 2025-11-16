const std = @import("std");
const api = @import("buzz_api.zig");

const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn abs(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(
        ctx.vm,
        .fromDouble(if (n_f < 0) n_f * -1 else n_f),
    );

    return 1;
}

pub export fn acos(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(
        ctx.vm,
        .fromDouble(std.math.acos(n_f)),
    );

    return 1;
}

pub export fn asin(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromDouble(std.math.asin(n_f)));

    return 1;
}

pub export fn atan(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromDouble(std.math.atan(n_f)));

    return 1;
}

pub export fn bzceil(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromInteger(@intFromFloat(std.math.ceil(n_f))));

    return 1;
}

pub export fn bzcos(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromDouble(std.math.cos(n_f)));

    return 1;
}

pub export fn bzexp(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromDouble(std.math.exp(n_f)));

    return 1;
}

pub export fn bzfloor(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromInteger(@intFromFloat(std.math.floor(n_f))));

    return 1;
}

pub export fn bzlog(ctx: *api.NativeCtx) callconv(.c) c_int {
    const base_i = api.bz_peek(ctx.vm, 1).double();
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromDouble(std.math.log(api.Double, base_i, n_f)));

    return 1;
}

pub export fn maxDouble(ctx: *api.NativeCtx) callconv(.c) c_int {
    const a_f = api.bz_peek(ctx.vm, 0).double();
    const b_f = api.bz_peek(ctx.vm, 1).double();

    api.bz_push(ctx.vm, .fromDouble(@max(a_f, b_f)));

    return 1;
}

pub export fn minDouble(ctx: *api.NativeCtx) callconv(.c) c_int {
    const a_f = api.bz_peek(ctx.vm, 0).double();
    const b_f = api.bz_peek(ctx.vm, 1).double();

    api.bz_push(ctx.vm, .fromDouble(@min(a_f, b_f)));

    return 1;
}

pub export fn maxInt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const a_f = api.bz_peek(ctx.vm, 0).integer();
    const b_f = api.bz_peek(ctx.vm, 1).integer();

    api.bz_push(ctx.vm, .fromInteger(@max(a_f, b_f)));

    return 1;
}

pub export fn minInt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const a_f = api.bz_peek(ctx.vm, 0).integer();
    const b_f = api.bz_peek(ctx.vm, 1).integer();

    api.bz_push(ctx.vm, .fromInteger(@min(a_f, b_f)));

    return 1;
}

pub export fn bzsin(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromDouble(std.math.sin(n)));

    return 1;
}

pub export fn bzsqrt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n_f = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromDouble(std.math.sqrt(n_f)));

    return 1;
}

pub export fn bztan(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n = api.bz_peek(ctx.vm, 0).double();

    api.bz_push(ctx.vm, .fromDouble(std.math.tan(n)));

    return 1;
}

pub export fn pow(ctx: *api.NativeCtx) callconv(.c) c_int {
    const n = api.bz_peek(ctx.vm, 1);
    const p = api.bz_peek(ctx.vm, 0);

    const n_i = if (n.isInteger()) n.integer() else null;
    const n_f = if (n.isFloat()) n.double() else null;

    const p_i = if (p.isInteger()) p.integer() else null;
    const p_f = if (p.isFloat()) p.double() else null;

    if (p_f) |pf| {
        api.bz_push(
            ctx.vm,
            .fromDouble(
                std.math.pow(api.Double, n_f orelse @as(api.Double, @floatFromInt(n_i.?)), pf),
            ),
        );
    } else if (n_f) |nf| {
        api.bz_push(
            ctx.vm,
            .fromDouble(
                std.math.pow(api.Double, nf, p_f orelse @as(api.Double, @floatFromInt(p_i.?))),
            ),
        );
    } else {
        api.bz_push(
            ctx.vm,
            .fromInteger(
                std.math.powi(api.Integer, n_i.?, p_i.?) catch |err| {
                    switch (err) {
                        error.Overflow => api.pushError(ctx.vm, "errors.OverflowError", null),
                        error.Underflow => api.pushError(ctx.vm, "errors.UnderflowError", null),
                    }

                    return -1;
                },
            ),
        );
    }

    return 1;
}
