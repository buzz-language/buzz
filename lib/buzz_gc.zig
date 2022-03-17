const std = @import("std");
const api = @import("buzz_api.zig");

export fn allocated(vm: *api.VM) bool {
    vm.bz_pushNum(@intToFloat(f64, vm.bz_allocated()));

    return true;
}

export fn collect(vm: *api.VM) bool {
    vm.bz_collect();

    return false;
}