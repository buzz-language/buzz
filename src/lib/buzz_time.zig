const api = @import("buzz_api.zig");
const std = @import("std");

pub export fn timeNow(ctx: *api.NativeCtx) callconv(.c) c_int {
    const ts = std.Io.Timestamp.now(ctx.getIo(), .real);
    const ms = ts.toMilliseconds();
    ctx.vm.bz_push(.fromDouble(@floatFromInt(ms)));
    return 1;
}

pub export fn timeMonotonic(ctx: *api.NativeCtx) callconv(.c) c_int {
    const ts = std.Io.Timestamp.now(ctx.getIo(), .awake);
    const ms = ts.toMilliseconds();
    ctx.vm.bz_push(.fromDouble(@floatFromInt(ms)));
    return 1;
}

pub export fn timeCpu(ctx: *api.NativeCtx) callconv(.c) c_int {
    const ts = std.Io.Timestamp.now(ctx.getIo(), .cpu_process);
    const ms = ts.toMilliseconds();
    ctx.vm.bz_push(.fromDouble(@floatFromInt(ms)));
    return 1;
}

pub const library = api.BuzzApi(
    "time",
    &.{
        &.{ "now", timeNow },
        &.{ "monotonic", timeMonotonic },
        &.{ "cpu", timeCpu },
    },
){};
