const std = @import("std");
const api = @import("buzz_api.zig");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const io = @import("io.zig");

pub const os = if (is_wasm)
    @import("wasm.zig")
else
    std.os;

pub export fn args(ctx: *api.NativeCtx) callconv(.c) c_int {
    api.bz_push(ctx.vm, api.bz_at(ctx.vm, 1));

    return 1;
}

pub export fn random(ctx: *api.NativeCtx) callconv(.c) c_int {
    if (is_wasm) {
        unreachable;
    }

    const min = api.bz_peek(ctx.vm, 1);
    const max = api.bz_peek(ctx.vm, 0);

    api.bz_push(
        ctx.vm,
        .fromInteger(
            std.crypto.random.intRangeAtMost(
                api.Integer,
                if (min.isInteger())
                    min.integer()
                else
                    0,
                if (max.isInteger())
                    max.integer()
                else
                    (if (min.isInteger()) min.integer() else 0) + 1,
            ),
        ),
    );

    return 1;
}

pub export fn print(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const string = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );

    if (len == 0) {
        return 0;
    }

    io.stdoutWriter.print(
        "{s}\n",
        .{
            string.?[0..len],
        },
    ) catch return 0;

    return 0;
}

pub export fn toInt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const value = api.bz_peek(ctx.vm, 0);

    api.bz_push(
        ctx.vm,
        .fromInteger(@intFromFloat(value.double())),
    );

    return 1;
}

pub export fn toDouble(ctx: *api.NativeCtx) callconv(.c) c_int {
    const value = api.bz_peek(ctx.vm, 0);

    api.bz_push(
        ctx.vm,
        .fromDouble(@floatFromInt(value.integer())),
    );

    return 1;
}

pub export fn toUd(ctx: *api.NativeCtx) callconv(.c) c_int {
    const value = api.bz_peek(ctx.vm, 0);

    api.bz_push(
        ctx.vm,
        api.bz_newUserData(
            ctx.vm,
            if (value.isInteger())
                @intCast(value.integer())
            else if (value.isFloat())
                @intFromFloat(value.double())
            else
                0,
        ),
    );

    return 1;
}

pub export fn parseInt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const string_value = api.bz_peek(ctx.vm, 0);

    var len: usize = 0;
    const string = api.bz_valueToString(
        ctx.vm,
        string_value,
        &len,
    );

    if (len == 0) {
        api.bz_push(ctx.vm, .Null);

        return 1;
    }

    const string_slice = string.?[0..len];

    const number = std.fmt.parseInt(api.Integer, string_slice, 10) catch {
        api.bz_push(ctx.vm, .Null);

        return 1;
    };

    api.bz_push(ctx.vm, .fromInteger(number));

    return 1;
}

pub export fn parseUd(ctx: *api.NativeCtx) callconv(.c) c_int {
    const string_value = api.bz_peek(ctx.vm, 0);

    var len: usize = 0;
    const string = api.bz_valueToString(ctx.vm, string_value, &len);

    if (len == 0) {
        api.bz_push(ctx.vm, .Null);

        return 1;
    }

    const string_slice = string.?[0..len];

    const number = std.fmt.parseInt(u64, string_slice, 10) catch {
        api.bz_push(ctx.vm, .Null);

        return 1;
    };

    api.bz_push(
        ctx.vm,
        api.bz_newUserData(ctx.vm, number),
    );

    return 1;
}

pub export fn parseDouble(ctx: *api.NativeCtx) callconv(.c) c_int {
    const string_value = api.bz_peek(ctx.vm, 0);

    var len: usize = 0;
    const string = api.bz_valueToString(ctx.vm, string_value, &len);

    if (len == 0) {
        api.bz_push(ctx.vm, .Null);

        return 1;
    }

    const string_slice = string.?[0..len];

    const number = std.fmt.parseFloat(f64, string_slice) catch {
        api.bz_push(ctx.vm, .Null);

        return 1;
    };

    api.bz_push(ctx.vm, .fromDouble(number));

    return 1;
}

pub export fn char(ctx: *api.NativeCtx) callconv(.c) c_int {
    const byte_value = api.bz_peek(ctx.vm, 0);

    var byte = byte_value.integer();

    if (byte > 255) {
        byte = 255;
    } else if (byte < 0) {
        byte = 0;
    }

    const str = [_]u8{@as(u8, @intCast(byte))};

    api.bz_push(
        ctx.vm,
        api.bz_stringToValue(ctx.vm, str[0..], 1),
    );

    return 1;
}

pub export fn assert(ctx: *api.NativeCtx) callconv(.c) c_int {
    const condition_value = api.bz_peek(ctx.vm, 1);
    const message_value = api.bz_peek(ctx.vm, 0);

    if (!condition_value.boolean()) {
        if (message_value.isObj()) {
            var len: usize = 0;
            const message = api.bz_valueToString(ctx.vm, message_value, &len).?;
            io.stdoutWriter.print(
                "Assert failed: {s}\n",
                .{
                    message[0..len],
                },
            ) catch unreachable;
        } else {
            io.stdoutWriter.print("Assert failed\n", .{}) catch unreachable;
        }

        if (!is_wasm) {
            std.process.exit(1);
        }
    }

    return 0;
}

pub export fn currentFiber(ctx: *api.NativeCtx) callconv(.c) c_int {
    api.bz_push(
        ctx.vm,
        api.bz_currentFiber(ctx.vm),
    );

    return 1;
}

pub export fn buzzPanic(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const message = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    ).?;

    api.bz_panic(ctx.vm, message, len);

    unreachable;
}
