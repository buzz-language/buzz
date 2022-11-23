const std = @import("std");
const api = @import("buzz_api.zig");

export fn print(vm: *api.VM) c_int {
    var len: usize = 0;
    const string = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    _ = std.io.getStdOut().write(string.?[0..len]) catch {
        // TODO: throw something?
        return -1;
    };
    _ = std.io.getStdOut().write("\n") catch {
        // TODO: throw something?
        return -1;
    };

    return 0;
}

export fn toInt(vm: *api.VM) c_int {
    vm.bz_pushInteger(vm.bz_peek(0).bz_valueToInteger());

    return 1;
}

export fn toFloat(vm: *api.VM) c_int {
    vm.bz_pushFloat(vm.bz_peek(0).bz_valueToFloat());

    return 1;
}

export fn parseInt(vm: *api.VM) c_int {
    var len: usize = 0;
    const string = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        vm.bz_pushNull();

        return 1;
    }

    const string_slice = string.?[0..len];

    const number: i64 = std.fmt.parseInt(i64, string_slice, 10) catch {
        vm.bz_pushNull();

        return 1;
    };

    vm.bz_pushInteger(number);

    return 1;
}

export fn parseFloat(vm: *api.VM) c_int {
    var len: usize = 0;
    const string = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        vm.bz_pushNull();

        return 1;
    }

    const string_slice = string.?[0..len];

    const number: f64 = std.fmt.parseFloat(f64, string_slice) catch {
        vm.bz_pushNull();

        return 1;
    };

    vm.bz_pushFloat(number);

    return 1;
}

export fn char(vm: *api.VM) c_int {
    var byte = vm.bz_peek(0).bz_valueToInteger();

    if (byte > 255) {
        byte = 255;
    } else if (byte < 0) {
        byte = 0;
    }

    const str = [_]u8{@intCast(u8, byte)};

    if (api.ObjString.bz_string(vm, str[0..], 1)) |obj_string| {
        vm.bz_pushString(obj_string);

        return 1;
    }

    vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);
    return -1;
}
