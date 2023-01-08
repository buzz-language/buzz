const std = @import("std");
const api = @import("buzz_api.zig");

export fn print(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const string = ctx.vm.bz_peek(0).bz_valueToString(&len);

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

export fn toInt(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushInteger(ctx.vm.bz_peek(0).bz_valueToInteger());

    return 1;
}

export fn toFloat(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_pushFloat(ctx.vm.bz_peek(0).bz_valueToFloat());

    return 1;
}

export fn parseInt(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const string = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        ctx.vm.bz_pushNull();

        return 1;
    }

    const string_slice = string.?[0..len];

    const number: i32 = std.fmt.parseInt(i32, string_slice, 10) catch {
        ctx.vm.bz_pushNull();

        return 1;
    };

    ctx.vm.bz_pushInteger(number);

    return 1;
}

export fn parseFloat(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const string = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        ctx.vm.bz_pushNull();

        return 1;
    }

    const string_slice = string.?[0..len];

    const number: f64 = std.fmt.parseFloat(f64, string_slice) catch {
        ctx.vm.bz_pushNull();

        return 1;
    };

    ctx.vm.bz_pushFloat(number);

    return 1;
}

export fn char(ctx: *api.NativeCtx) c_int {
    var byte = ctx.vm.bz_peek(0).bz_valueToInteger();

    if (byte > 255) {
        byte = 255;
    } else if (byte < 0) {
        byte = 0;
    }

    const str = [_]u8{@intCast(u8, byte)};

    if (api.ObjString.bz_string(ctx.vm, str[0..], 1)) |obj_string| {
        ctx.vm.bz_pushString(obj_string);

        return 1;
    }

    ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);
    return -1;
}
