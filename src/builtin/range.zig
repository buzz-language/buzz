const obj = @import("../obj.zig");
const Value = @import("../value.zig").Value;

pub fn toList(ctx: *obj.NativeCtx) c_int {
    const range = ctx.vm.peek(0).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;

    var list: *obj.ObjList = ctx.vm.gc.allocateObject(
        obj.ObjList,
        obj.ObjList.init(
            ctx.vm.gc.allocator,
            ctx.vm.gc.type_registry.int_type,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(Value.fromObj(list.toObj()));

    if (range.low < range.high) {
        var i: i32 = range.low;
        while (i < range.high) : (i += 1) {
            list.rawAppend(ctx.vm.gc, Value.fromInteger(i)) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    } else {
        var i: i32 = range.low;
        while (i > range.high) : (i -= 1) {
            list.rawAppend(ctx.vm.gc, Value.fromInteger(i)) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    return 1;
}

pub fn len(ctx: *obj.NativeCtx) c_int {
    const range = ctx.vm.peek(0).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;

    ctx.vm.push(
        Value.fromInteger(
            if (range.low < range.high)
                range.high - range.low
            else
                range.low - range.high,
        ),
    );

    return 1;
}

pub fn invert(ctx: *obj.NativeCtx) c_int {
    const range = ctx.vm.peek(0).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;

    ctx.vm.push(
        Value.fromObj((ctx.vm.gc.allocateObject(
            obj.ObjRange,
            .{
                .high = range.low,
                .low = range.high,
            },
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        }).toObj()),
    );

    return 1;
}

pub fn subsetOf(ctx: *obj.NativeCtx) c_int {
    const rangeA = ctx.vm.peek(1).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;
    const rangeB = ctx.vm.peek(0).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;

    ctx.vm.push(
        Value.fromBoolean(
            @min(rangeA.low, rangeA.high) >= @min(rangeB.low, rangeB.high) and
                @max(rangeA.low, rangeA.high) <= @max(rangeB.low, rangeB.high),
        ),
    );

    return 1;
}

pub fn intersect(ctx: *obj.NativeCtx) c_int {
    const rangeA = ctx.vm.peek(1).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;
    const rangeB = ctx.vm.peek(0).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;

    ctx.vm.push(
        Value.fromObj((ctx.vm.gc.allocateObject(
            obj.ObjRange,
            .{
                .high = @max(
                    @min(rangeB.low, rangeB.high),
                    @min(rangeA.low, rangeA.high),
                ),
                .low = @min(
                    @max(rangeB.low, rangeB.high),
                    @max(rangeA.low, rangeA.high),
                ),
            },
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        }).toObj()),
    );

    return 1;
}

pub fn @"union"(ctx: *obj.NativeCtx) c_int {
    const rangeA = ctx.vm.peek(1).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;
    const rangeB = ctx.vm.peek(0).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?;

    ctx.vm.push(
        Value.fromObj((ctx.vm.gc.allocateObject(
            obj.ObjRange,
            .{
                .high = @min(
                    @min(rangeB.low, rangeB.high),
                    @min(rangeA.low, rangeA.high),
                ),
                .low = @max(
                    @max(rangeB.low, rangeB.high),
                    @max(rangeA.low, rangeA.high),
                ),
            },
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        }).toObj()),
    );

    return 1;
}

pub fn high(ctx: *obj.NativeCtx) c_int {
    ctx.vm.push(
        Value.fromInteger(
            ctx.vm.peek(0).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?.high,
        ),
    );

    return 1;
}

pub fn low(ctx: *obj.NativeCtx) c_int {
    ctx.vm.push(
        Value.fromInteger(
            ctx.vm.peek(0).obj().access(obj.ObjRange, .Range, ctx.vm.gc).?.low,
        ),
    );

    return 1;
}
