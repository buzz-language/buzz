const std = @import("std");
const api = @import("./buzz_api.zig");

export fn abs_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromFloat(if (n_f < 0) n_f * -1 else n_f);
}

export fn abs(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(abs_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn acos_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromFloat(std.math.acos(n_f));
}

export fn acos(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(acos_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn asin_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromFloat(std.math.asin(n_f));
}

export fn asin(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(asin_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn atan_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromFloat(std.math.atan(n_f));
}

export fn atan(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(atan_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn bzceil_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromInteger(@floatToInt(i32, std.math.ceil(n_f)));
}

export fn bzceil(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(bzceil_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn bzcos_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromFloat(std.math.cos(n_f));
}

export fn bzcos(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(bzcos_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn bzexp_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromFloat(std.math.exp(n_f));
}

export fn bzexp(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(bzexp_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn bzfloor_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromInteger(@floatToInt(i32, std.math.floor(n_f)));
}

export fn bzfloor(ctx: *api.NativeCtx) c_int {
    const n_f: f64 = ctx.vm.bz_peek(0).float();

    ctx.vm.bz_pushInteger(@floatToInt(i32, std.math.floor(n_f)));

    return 1;
}

export fn bzlog_raw(_: *api.NativeCtx, base_value: api.Value, n_value: api.Value) api.Value {
    const base_f: f64 = base_value.float();
    const n_f: f64 = n_value.float();

    return api.Value.fromFloat(std.math.log(f64, base_f, n_f));
}

export fn bzlog(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(
        bzlog_raw(
            ctx,
            ctx.vm.bz_peek(1),
            ctx.vm.bz_peek(0),
        ),
    );

    return 1;
}

export fn max_raw(_: *api.NativeCtx, a_value: api.Value, b_value: api.Value) api.Value {
    const a_f: f64 = a_value.float();
    const b_f: f64 = b_value.float();

    return api.Value.fromFloat(std.math.max(a_f, b_f));
}

export fn max(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(
        max_raw(
            ctx,
            ctx.vm.bz_peek(1),
            ctx.vm.bz_peek(0),
        ),
    );

    return 1;
}

export fn min_raw(_: *api.NativeCtx, a_value: api.Value, b_value: api.Value) api.Value {
    const a_f: f64 = a_value.float();
    const b_f: f64 = b_value.float();

    return api.Value.fromFloat(std.math.min(a_f, b_f));
}

export fn min(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(
        min_raw(
            ctx,
            ctx.vm.bz_peek(1),
            ctx.vm.bz_peek(0),
        ),
    );

    return 1;
}

export fn random_raw(_: *api.NativeCtx) api.Value {
    return api.Value.fromFloat(std.crypto.random.float(f64));
}

export fn random(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(random_raw(ctx));

    return 1;
}

export fn bzsin_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n: f64 = n_value.float();

    return api.Value.fromFloat(std.math.sin(n));
}

export fn bzsin(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(bzsin_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn bzsqrt_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromFloat(std.math.sqrt(n_f));
}

export fn bzsqrt(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(bzsqrt_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn bztan_raw(_: *api.NativeCtx, n_value: api.Value) api.Value {
    const n_f: f64 = n_value.float();

    return api.Value.fromFloat(std.math.tan(n_f));
}

export fn bztan(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(bztan_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn pow_raw(ctx: *api.NativeCtx, n: api.Value, p: api.Value) api.Value {
    const n_i: ?i32 = if (n.isInteger()) n.integer() else null;
    const n_f: ?f64 = if (n.isFloat()) n.float() else null;

    const p_i: ?i32 = if (p.isInteger()) p.integer() else null;
    const p_f: ?f64 = if (p.isFloat()) p.float() else null;

    if (p_f) |pf| {
        return api.Value.fromFloat(
            std.math.pow(f64, n_f orelse @intToFloat(f64, n_i.?), pf),
        );
    } else if (n_f) |nf| {
        return api.Value.fromFloat(
            std.math.pow(f64, nf, p_f orelse @intToFloat(f64, p_i.?)),
        );
    }

    return api.Value.fromInteger(
        std.math.powi(i32, n_i.?, p_i.?) catch |err| {
            switch (err) {
                error.Overflow => ctx.vm.bz_pushError("lib.std.Overflow", "lib.std.Overflow".len),
                error.Underflow => ctx.vm.bz_pushError("lib.std.Underflow", "lib.std.Underflow".len),
            }

            return api.Value.Error;
        },
    );
}

export fn pow(ctx: *api.NativeCtx) c_int {
    const result = pow_raw(ctx, ctx.vm.bz_peek(1), ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}
