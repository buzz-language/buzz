const std = @import("std");
const o = @import("../obj.zig");
const VM = @import("../vm.zig").VM;
const v = @import("../value.zig");

pub fn over(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjFiber.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(v.Value.fromBoolean(self.fiber.status == .Over));

    return 1;
}

pub fn cancel(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjFiber.cast(ctx.vm.peek(0).obj()).?;

    // Main fiber can't be cancelled
    if (self.fiber.parent_fiber == null) {
        return 0;
    }

    self.fiber.status = .Over;

    return 0;
}

pub fn isMain(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjFiber.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(v.Value.fromBoolean(self.fiber.parent_fiber == null));

    return 1;
}
