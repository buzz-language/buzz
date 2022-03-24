const std = @import("std");
const api = @import("buzz_api.zig");

export fn print(vm: *api.VM) c_int {
    _ = std.io.getStdOut().write(std.mem.sliceTo(vm.bz_peek(0).bz_valueToString().?, 0)) catch {
        // TODO: throw something?
        return -1;
    };
    _ = std.io.getStdOut().write("\n") catch {
        // TODO: throw something?
        return -1;
    };

    return 0;
}

export fn parseNumber(vm: *api.VM) c_int {
    const string = std.mem.sliceTo(vm.bz_peek(0).bz_valueToString().?, 0);

    const number: f64 = std.fmt.parseFloat(f64, string) catch {
        vm.bz_pushNull();

        return 1;
    };

    vm.bz_pushNum(number);

    return 1;
}
