const std = @import("std");
const api = @import("buzz_api.zig");

export fn allocated(vm: *api.VM) c_int {
    vm.bz_pushInteger(@intCast(i64, vm.bz_allocated()));

    return 1;
}

export fn collect(vm: *api.VM) c_int {
    if (!vm.bz_collect()) {
        vm.bz_pushError("lib.gc.CollectError", "lib.gc.CollectError".len);

        return -1;
    }

    return 0;
}
