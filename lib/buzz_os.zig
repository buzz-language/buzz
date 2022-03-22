const std = @import("std");
const api = @import("./buzz_api.zig");
const utils = @import("../src/utils.zig");

export fn time(_: *api.VM) bool {
    unreachable;
}

export fn env(vm: *api.VM) bool {
    const key = api.Value.bz_valueToString(vm.bz_peek(0)) orelse "";

    if (std.os.getenvZ(key)) |value| {
        vm.bz_pushString(api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, value) orelse {
            vm.bz_throwString("Could not get environment variable");

            return false;
        }) orelse {
            vm.bz_throwString("Could not get environment variable");

            return false;
        });

        return true;
    }

    return false;
}

export fn tmpDir(_: *api.VM) bool {
    unreachable;
}

export fn tmpName(_: *api.VM) bool {
    unreachable;
}

export fn exit(_: *api.VM) bool {
    unreachable;
}

export fn execute(_: *api.VM) bool {
    unreachable;
}
