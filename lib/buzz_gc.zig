const std = @import("std");
const api = @import("buzz_api.zig");

export fn allocated(vm: *api.VM) c_int {
    vm.bz_pushNum(@intToFloat(f64, vm.bz_allocated()));

    return 1;
}

export fn collect(vm: *api.VM) c_int {
    if (!vm.bz_collect()) {
        vm.bz_throwString("Could not collect");

        return -1;
    }

    return 0;
}
