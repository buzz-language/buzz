const obj = @import("../obj.zig");
const Value = @import("../value.zig").Value;

pub fn toList(ctx: *obj.NativeCtx) c_int {
    const range = ctx.vm.peek(0).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;

    var list: *obj.ObjList = ctx.vm.gc.allocateObject(
        obj.ObjList,
        obj.ObjList.init(
            ctx.vm.gc.allocator,
            ctx.vm.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Integer,
                },
            ) catch @panic("Could not instanciate list"),
        ),
    ) catch @panic("Could not instanceiate range");

    ctx.vm.push(Value.fromObj(list.toObj()));

    if (range.low < range.high) {
        var i: i32 = range.low;
        while (i < range.high) : (i += 1) {
            list.rawAppend(ctx.vm.gc, Value.fromInteger(i)) catch @panic("Could not append to list");
        }
    } else {
        var i: i32 = range.low;
        while (i > range.high) : (i -= 1) {
            list.rawAppend(ctx.vm.gc, Value.fromInteger(i)) catch @panic("Could not append to list");
        }
    }

    return 1;
}
