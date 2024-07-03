const std = @import("std");
const _obj = @import("../obj.zig");
const ObjFiber = _obj.ObjFiber;
const NativeCtx = _obj.NativeCtx;
const VM = @import("../vm.zig").VM;
const _value = @import("../value.zig");
const Value = _value.Value;

pub fn over(ctx: *NativeCtx) c_int {
    const self = ObjFiber.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromBoolean(self.fiber.status == .Over));

    return 1;
}

pub fn cancel(ctx: *NativeCtx) c_int {
    const self = ObjFiber.cast(ctx.vm.peek(0).obj()).?;

    // Main fiber can't be cancelled
    if (self.fiber.parent_fiber == null) {
        return 0;
    }

    self.fiber.status = .Over;

    return 0;
}

pub fn isMain(ctx: *NativeCtx) c_int {
    const self = ObjFiber.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromBoolean(self.fiber.parent_fiber == null));

    return 1;
}
