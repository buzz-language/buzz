const std = @import("std");
const api = @import("buzz_api.zig");

export fn print_raw(_: *api.NativeCtx, string_value: api.Value) api.Value {
    var len: usize = 0;
    const string = string_value.bz_valueToString(&len);

    if (len == 0) {
        return api.Value.Void;
    }

    _ = std.io.getStdOut().write(string.?[0..len]) catch {
        // TODO: throw something?
        return api.Value.Error;
    };
    _ = std.io.getStdOut().write("\n") catch {
        // TODO: throw something?
        return api.Value.Error;
    };

    return api.Value.Void;
}

export fn print(ctx: *api.NativeCtx) c_int {
    if (print_raw(ctx, ctx.vm.bz_peek(0)).isError()) {
        return -1;
    }

    return 0;
}

export fn toInt_raw(_: *api.NativeCtx, value: api.Value) api.Value {
    return api.Value.fromInteger(if (value.isFloat()) @floatToInt(i32, value.float()) else value.integer());
}

export fn toInt(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(toInt_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn toFloat_raw(_: *api.NativeCtx, value: api.Value) api.Value {
    return api.Value.fromFloat(if (value.isInteger()) @intToFloat(f64, value.integer()) else value.float());
}

export fn toFloat(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(toFloat_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn parseInt_raw(_: *api.NativeCtx, string_value: api.Value) api.Value {
    var len: usize = 0;
    const string = string_value.bz_valueToString(&len);

    if (len == 0) {
        return api.Value.Null;
    }

    const string_slice = string.?[0..len];

    const number: i32 = std.fmt.parseInt(i32, string_slice, 10) catch {
        return api.Value.Null;
    };

    return api.Value.fromInteger(number);
}

export fn parseInt(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(parseInt_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn parseFloat_raw(_: *api.NativeCtx, string_value: api.Value) api.Value {
    var len: usize = 0;
    const string = string_value.bz_valueToString(&len);

    if (len == 0) {
        return api.Value.Null;
    }

    const string_slice = string.?[0..len];

    const number: f64 = std.fmt.parseFloat(f64, string_slice) catch {
        return api.Value.Null;
    };

    return api.Value.fromFloat(number);
}

export fn parseFloat(ctx: *api.NativeCtx) c_int {
    ctx.vm.bz_push(parseFloat_raw(ctx, ctx.vm.bz_peek(0)));

    return 1;
}

export fn char_raw(ctx: *api.NativeCtx, byte_value: api.Value) api.Value {
    var byte = byte_value.integer();

    if (byte > 255) {
        byte = 255;
    } else if (byte < 0) {
        byte = 0;
    }

    const str = [_]u8{@intCast(u8, byte)};

    if (api.ObjString.bz_string(ctx.vm, str[0..], 1)) |obj_string| {
        return obj_string.bz_objStringToValue();
    }

    ctx.vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);
    return api.Value.Error;
}

export fn char(ctx: *api.NativeCtx) c_int {
    const result = char_raw(ctx, ctx.vm.bz_peek(0));

    if (result.isError()) {
        return -1;
    }

    ctx.vm.bz_push(result);

    return 1;
}
