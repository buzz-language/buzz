const std = @import("std");
const api = @import("./buzz_api.zig");

export fn abs(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(if (n < 0) n * -1 else n);

    return 1;
}

export fn acos(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.acos(n));

    return 1;
}

export fn asin(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.asin(n));

    return 1;
}

export fn atan(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.atan(n));

    return 1;
}

export fn bzceil(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.ceil(n));

    return 1;
}

export fn bzcos(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.cos(n));

    return 1;
}

export fn bzexp(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.exp(n));

    return 1;
}

export fn bzfloor(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.floor(n));

    return 1;
}

export fn bzlog(vm: *api.VM) c_int {
    const base = api.Value.bz_valueToNumber(vm.bz_peek(1));
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.log(f64, base, n));

    return 1;
}

export fn max(vm: *api.VM) c_int {
    const a = api.Value.bz_valueToNumber(vm.bz_peek(1));
    const b = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.max(a, b));

    return 1;
}

export fn min(vm: *api.VM) c_int {
    const a = api.Value.bz_valueToNumber(vm.bz_peek(1));
    const b = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.min(a, b));

    return 1;
}

export fn random(vm: *api.VM) c_int {
    vm.bz_pushNum(std.crypto.random.float(f64));

    return 1;
}

export fn bzsin(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.sin(n));

    return 1;
}

export fn bzsqrt(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.sqrt(n));

    return 1;
}

export fn bztan(vm: *api.VM) c_int {
    const n = api.Value.bz_valueToNumber(vm.bz_peek(0));

    vm.bz_pushNum(std.math.tan(n));

    return 1;
}
