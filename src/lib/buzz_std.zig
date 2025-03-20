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
    ctx.vm.bz_push(ctx.vm.bz_at(1));

    return 1;
}

pub export fn random(ctx: *api.NativeCtx) callconv(.c) c_int {
    if (is_wasm) {
        unreachable;
    }

    const min = ctx.vm.bz_peek(1);
    const max = ctx.vm.bz_peek(0);

    ctx.vm.bz_push(
        api.Value.fromInteger(
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
    const string = ctx.vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    _ = io.stdOutWriter.print(
        "{s}\n",
        .{
            string.?[0..len],
        },
    ) catch return 0;

    return 0;
}

pub export fn toInt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const value = ctx.vm.bz_peek(0);

    ctx.vm.bz_push(
        api.Value.fromInteger(@intFromFloat(value.double())),
    );

    return 1;
}

pub export fn toDouble(ctx: *api.NativeCtx) callconv(.c) c_int {
    const value = ctx.vm.bz_peek(0);

    ctx.vm.bz_push(
        api.Value.fromDouble(@floatFromInt(value.integer())),
    );

    return 1;
}

pub export fn toUd(ctx: *api.NativeCtx) callconv(.c) c_int {
    const value = ctx.vm.bz_peek(0);

    ctx.vm.bz_push(
        ctx.vm.bz_newUserData(if (value.isInteger())
            @intCast(value.integer())
        else if (value.isFloat())
            @intFromFloat(value.double())
        else
            0),
    );

    return 1;
}

pub export fn parseInt(ctx: *api.NativeCtx) callconv(.c) c_int {
    const string_value = ctx.vm.bz_peek(0);

    var len: usize = 0;
    const string = string_value.bz_valueToString(&len);

    if (len == 0) {
        ctx.vm.bz_push(api.Value.Null);

        return 1;
    }

    const string_slice = string.?[0..len];

    const number = std.fmt.parseInt(api.Integer, string_slice, 10) catch {
        ctx.vm.bz_push(api.Value.Null);

        return 1;
    };

    ctx.vm.bz_push(api.Value.fromInteger(number));

    return 1;
}

pub export fn parseUd(ctx: *api.NativeCtx) callconv(.c) c_int {
    const string_value = ctx.vm.bz_peek(0);

    var len: usize = 0;
    const string = string_value.bz_valueToString(&len);

    if (len == 0) {
        ctx.vm.bz_push(api.Value.Null);

        return 1;
    }

    const string_slice = string.?[0..len];

    const number = std.fmt.parseInt(u64, string_slice, 10) catch {
        ctx.vm.bz_push(api.Value.Null);

        return 1;
    };

    ctx.vm.bz_push(
        api.VM.bz_newUserData(
            ctx.vm,
            number,
        ),
    );

    return 1;
}

pub export fn parseDouble(ctx: *api.NativeCtx) callconv(.c) c_int {
    const string_value = ctx.vm.bz_peek(0);

    var len: usize = 0;
    const string = string_value.bz_valueToString(&len);

    if (len == 0) {
        ctx.vm.bz_push(api.Value.Null);

        return 1;
    }

    const string_slice = string.?[0..len];

    const number = std.fmt.parseFloat(f64, string_slice) catch {
        ctx.vm.bz_push(api.Value.Null);

        return 1;
    };

    ctx.vm.bz_push(api.Value.fromDouble(number));

    return 1;
}

pub export fn char(ctx: *api.NativeCtx) callconv(.c) c_int {
    const byte_value = ctx.vm.bz_peek(0);

    var byte = byte_value.integer();

    if (byte > 255) {
        byte = 255;
    } else if (byte < 0) {
        byte = 0;
    }

    const str = [_]u8{@as(u8, @intCast(byte))};

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(ctx.vm, str[0..], 1),
    );

    return 1;
}

pub export fn assert(ctx: *api.NativeCtx) callconv(.c) c_int {
    const condition_value = ctx.vm.bz_peek(1);
    const message_value = ctx.vm.bz_peek(0);

    if (!condition_value.boolean()) {
        if (message_value.isObj()) {
            var len: usize = 0;
            const message = api.Value.bz_valueToString(message_value, &len).?;
            io.stdOutWriter.print(
                "Assert failed: {s}\n",
                .{
                    message[0..len],
                },
            ) catch unreachable;
        } else {
            io.stdOutWriter.print("Assert failed\n", .{}) catch unreachable;
        }

        if (!is_wasm) {
            std.process.exit(1);
        }
    }

    return 0;
}

pub export fn currentFiber(ctx: *api.NativeCtx) callconv(.c) c_int {
    ctx.vm.bz_push(ctx.vm.bz_currentFiber());

    return 1;
}

pub export fn buzzPanic(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const message = api.Value.bz_valueToString(ctx.vm.bz_peek(0), &len).?;

    ctx.vm.bz_panic(message, len);

    unreachable;
}
