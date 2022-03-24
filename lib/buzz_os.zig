const std = @import("std");
const api = @import("./buzz_api.zig");
const utils = @import("../src/utils.zig");

export fn time(vm: *api.VM) bool {
    vm.bz_pushNum(@intToFloat(f64, std.time.milliTimestamp()));

    return true;
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

// If it was named `exit` it would be considered by zig as a callback when std.os.exit is called
export fn buzzExit(vm: *api.VM) bool {
    const exitCode: u8 = @floatToInt(u8, api.Value.bz_valueToNumber(vm.bz_peek(0)));

    std.os.exit(exitCode);

    return false;
}

export fn execute(_: *api.VM) bool {
    unreachable;
}
