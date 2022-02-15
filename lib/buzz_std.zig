const std = @import("std");
const api = @import("buzz_api.zig");

export fn assert(vm: *api.VM) bool {
    var condition: bool = vm.bz_peek(1).bz_valueToBool();

    if (!condition) {
        vm.bz_throw(vm.bz_peek(0));
    }

    return false;
}

export fn print(vm: *api.VM) bool {
    _ = std.io.getStdOut().write(std.mem.sliceTo(vm.bz_peek(0).bz_valueToString().?, 0)) catch {
        // TODO: throw something?
        return false;
    };
    _ = std.io.getStdOut().write("\n") catch {
        // TODO: throw something?
        return false;
    };

    return false;
}