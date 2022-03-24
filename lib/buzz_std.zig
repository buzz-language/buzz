const std = @import("std");
const api = @import("buzz_api.zig");

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

export fn parseNumber(vm: *api.VM) bool {
    const string = std.mem.sliceTo(vm.bz_peek(0).bz_valueToString().?, 0);

    const number: f64 = std.fmt.parseFloat(f64, string) catch {
        vm.bz_pushNull();

        return true;
    };

    vm.bz_pushNum(number);

    return true;
}
