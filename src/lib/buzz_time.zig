const api = @import("buzz_api.zig");
const std = @import("std");

pub export fn timeNow(ctx: *api.NativeCtx) callconv(.c) c_int {
    const ts = std.Io.Clock.now(.real, ctx.getIo());
    const ms = ts.toMilliseconds();
    ctx.vm.bz_push(.fromDouble(@floatFromInt(ms)));
    return 1;
}

pub export fn timeMonotonic(ctx: *api.NativeCtx) callconv(.c) c_int {
    const ts = std.Io.Clock.now(.awake, ctx.getIo());
    const ms = ts.toMilliseconds();
    ctx.vm.bz_push(.fromDouble(@floatFromInt(ms)));
    return 1;
}

pub export fn timeCpu(ctx: *api.NativeCtx) callconv(.c) c_int {
    const ts = std.Io.Clock.now(.cpu_process, ctx.getIo()) catch {
        ctx.vm.pushError("errors.UnexpectedError", "CPU clock not available");
        return -1;
    };
    const ms = ts.toMilliseconds();
    ctx.vm.bz_push(.fromDouble(@floatFromInt(ms)));
    return 1;
}

pub export fn timeFormat(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const ts_ptr = ctx.vm.bz_peek(0).bz_valueToString(&len);
    const ts_str = ts_ptr.?[0..len];

    const ts = std.fmt.parseFloat(api.Double, ts_str) catch {
        ctx.vm.pushError("errors.InvalidArgumentError", "Invalid timestamp");
        return -1;
    };

    const seconds: i64 = @intFromFloat(@floor(ts / 1000.0));
    const nanos: i64 = @intFromFloat(@mod(ts, 1000.0) * 1_000_000);

    const epoch = std.Io.Clock.Timestamp.fromNanoseconds(@as(i96, seconds) * 1_000_000_000 + @as(i96, nanos));

    var buf: [64]u8 = undefined;
    const formatted = std.Io.Clock.Timestamp.formatNumber(epoch, "{s}", &buf) catch {
        ctx.vm.pushError("errors.UnexpectedError", "Failed to format time");
        return -1;
    };

    ctx.vm.bz_push(
        api.VM.bz_stringToValue(ctx.vm, formatted.ptr, formatted.len),
    );

    return 1;
}

pub export fn timeParse(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const iso_ptr = ctx.vm.bz_peek(0).bz_valueToString(&len);
    const iso_str = iso_ptr.?[0..len];

    const parsed = std.Io.Clock.Timestamp.parse(iso_str) catch {
        ctx.vm.pushError("errors.InvalidArgumentError", "Invalid ISO 8601 string");
        return -1;
    };

    const ms = parsed.toMilliseconds();
    ctx.vm.bz_push(.fromDouble(@floatFromInt(ms)));
    return 1;
}

pub const library = api.BuzzApi(
    "time",
    &.{
        &.{ "now", timeNow },
        &.{ "monotonic", timeMonotonic },
        &.{ "cpu", timeCpu },
        &.{ "format", timeFormat },
        &.{ "parse", timeParse },
    },
){};
