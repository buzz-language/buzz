const std = @import("std");
const api = @import("./buzz_api.zig");

// FIXME: all those function operate on floats discarding integer that don't fit in a f64

export fn abs(vm: *api.VM) c_int {
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(if (n_f < 0) n_f * -1 else n_f);

    return 1;
}

export fn acos(vm: *api.VM) c_int {
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(std.math.acos(n_f));

    return 1;
}

export fn asin(vm: *api.VM) c_int {
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(std.math.asin(n_f));

    return 1;
}

export fn atan(vm: *api.VM) c_int {
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(std.math.atan(n_f));

    return 1;
}

export fn bzceil(vm: *api.VM) c_int {
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushInteger(@floatToInt(i64, std.math.ceil(n_f)));

    return 1;
}

export fn bzcos(vm: *api.VM) c_int {
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(std.math.cos(n_f));

    return 1;
}

export fn bzexp(vm: *api.VM) c_int {
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(std.math.exp(n_f));

    return 1;
}

export fn bzfloor(vm: *api.VM) c_int {
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushInteger(@floatToInt(i64, std.math.floor(n_f)));

    return 1;
}

export fn bzlog(vm: *api.VM) c_int {
    const base_i: f64 = api.Value.bz_valueToFloat(vm.bz_peek(1));
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(std.math.log(f64, base_i, n_f));

    return 1;
}

export fn max(vm: *api.VM) c_int {
    const a_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));
    const b_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(1));

    vm.bz_pushFloat(std.math.max(a_f, b_f));

    return 1;
}

export fn min(vm: *api.VM) c_int {
    const a_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));
    const b_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(1));

    vm.bz_pushFloat(std.math.min(a_f, b_f));

    return 1;
}

export fn random(vm: *api.VM) c_int {
    vm.bz_pushFloat(std.crypto.random.float(f64));

    return 1;
}

export fn bzsin(vm: *api.VM) c_int {
    const n: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(std.math.sin(n));

    return 1;
}

export fn bzsqrt(vm: *api.VM) c_int {
    const n_f: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(std.math.sqrt(n_f));

    return 1;
}

export fn bztan(vm: *api.VM) c_int {
    const n: f64 = api.Value.bz_valueToFloat(vm.bz_peek(0));

    vm.bz_pushFloat(std.math.tan(n));

    return 1;
}

export fn pow(vm: *api.VM) c_int {
    const n = vm.bz_peek(1);
    const p = vm.bz_peek(0);

    const n_i: ?i64 = if (n.bz_valueIsInteger()) n.bz_valueToInteger() else null;
    const n_f: ?f64 = if (n.bz_valueIsFloat()) n.bz_valueToFloat() else null;

    const p_i: ?i64 = if (p.bz_valueIsInteger()) p.bz_valueToInteger() else null;
    const p_f: ?f64 = if (p.bz_valueIsFloat()) p.bz_valueToFloat() else null;

    if (p_f) |pf| {
        vm.bz_pushFloat(
            std.math.pow(f64, n_f orelse @intToFloat(f64, n_i.?), pf),
        );
    } else if (n_f) |nf| {
        vm.bz_pushFloat(
            std.math.pow(f64, nf, p_f orelse @intToFloat(f64, p_i.?)),
        );
    } else {
        vm.bz_pushInteger(
            std.math.powi(i64, n_i.?, p_i.?) catch |err| {
                switch (err) {
                    error.Overflow => vm.bz_pushError("lib.std.Overflow", "lib.std.Overflow".len),
                    error.Underflow => vm.bz_pushError("lib.std.Underflow", "lib.std.Underflow".len),
                }

                return -1;
            },
        );
    }

    return 1;
}
