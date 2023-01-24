const std = @import("std");
const _obj = @import("../obj.zig");
const ObjPattern = _obj.ObjPattern;
const ObjString = _obj.ObjString;
const NativeCtx = _obj.NativeCtx;
const VM = @import("../vm.zig").VM;
const _value = @import("../value.zig");
const Value = _value.Value;

pub fn match(ctx: *NativeCtx) c_int {
    const self = ObjPattern.cast(ctx.vm.peek(1).obj()).?;
    const subject = ObjString.cast(ctx.vm.peek(0).obj()).?.string;

    var offset: usize = 0;
    if (self.rawMatch(
        ctx.vm,
        if (subject.len > 0) @ptrCast([*]const u8, subject) else null,
        subject.len,
        &offset,
    ) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not match") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.False);

        return -1;
    }) |results| {
        ctx.vm.push(results.toValue());
    } else {
        ctx.vm.push(Value.Null);
    }

    return 1;
}

pub fn matchAll(ctx: *NativeCtx) c_int {
    var self = ObjPattern.cast(ctx.vm.peek(1).obj()).?;
    var subject = ObjString.cast(ctx.vm.peek(0).obj()).?.string;

    if (self.rawMatchAll(
        ctx.vm,
        if (subject.len > 0) @ptrCast([*]const u8, subject) else null,
        subject.len,
    ) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not match") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.False);

        return -1;
    }) |results| {
        ctx.vm.push(results.toValue());
    } else {
        ctx.vm.push(Value.Null);
    }

    return 1;
}
