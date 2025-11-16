const o = @import("../obj.zig");
const v = @import("../value.zig");

pub fn toList(ctx: *o.NativeCtx) callconv(.c) c_int {
    const range = ctx.vm.peekAsIdx(o.ObjRange, 0);

    const list = ctx.vm.gc.allocateObject(
        o.ObjList.init(
            ctx.vm.gc.allocator,
            ctx.vm.gc.type_registry.getTypeDef(
                .{
                    .def_type = .List,
                    .resolved_type = .{
                        .List = o.ObjList.ListDef.init(
                            ctx.vm.gc.type_registry.int_type,
                            false,
                        ),
                    },
                },
            ) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            },
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(
        .fromObj(
            .{
                .index = list.index,
                .obj_type = .List,
            },
        ),
    );

    if (range.get(ctx.vm.gc).low < range.get(ctx.vm.gc).high) {
        var i = range.get(ctx.vm.gc).low;
        while (i < range.get(ctx.vm.gc).high) : (i += 1) {
            o.ObjList.rawAppend(list, ctx.vm.gc, .fromInteger(i)) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    } else {
        var i = range.get(ctx.vm.gc).low;
        while (i > range.get(ctx.vm.gc).high) : (i -= 1) {
            o.ObjList.rawAppend(list, ctx.vm.gc, .fromInteger(i)) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    return 1;
}

pub fn len(ctx: *o.NativeCtx) callconv(.c) c_int {
    const range = ctx.vm.peekAsIdx(o.ObjRange, 0).get(ctx.vm.gc);

    ctx.vm.push(
        .fromInteger(
            if (range.low < range.high)
                range.high - range.low
            else
                range.low - range.high,
        ),
    );

    return 1;
}

pub fn invert(ctx: *o.NativeCtx) callconv(.c) c_int {
    const range = ctx.vm.peekAsIdx(o.ObjRange, 0);

    ctx.vm.push(
        .fromObj(
            .{
                .index = (ctx.vm.gc.allocateObject(
                    o.ObjRange{
                        .high = range.get(ctx.vm.gc).low,
                        .low = range.get(ctx.vm.gc).high,
                    },
                ) catch {
                    ctx.vm.panic("Out of memory");
                    unreachable;
                }).index,
                .obj_type = .Range,
            },
        ),
    );

    return 1;
}

pub fn subsetOf(ctx: *o.NativeCtx) callconv(.c) c_int {
    const rangeA = ctx.vm.peekAsIdx(o.ObjRange, 1).get(ctx.vm.gc);
    const rangeB = ctx.vm.peekAsIdx(o.ObjRange, 0).get(ctx.vm.gc);

    ctx.vm.push(
        .fromBoolean(
            @min(rangeA.low, rangeA.high) >= @min(rangeB.low, rangeB.high) and
                @max(rangeA.low, rangeA.high) <= @max(rangeB.low, rangeB.high),
        ),
    );

    return 1;
}

pub fn intersect(ctx: *o.NativeCtx) callconv(.c) c_int {
    const rangeA = ctx.vm.peekAsIdx(o.ObjRange, 1).get(ctx.vm.gc);
    const rangeB = ctx.vm.peekAsIdx(o.ObjRange, 0).get(ctx.vm.gc);

    ctx.vm.push(
        .fromObj(
            .{
                .index = (ctx.vm.gc.allocateObject(
                    o.ObjRange{
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
                }).index,
                .obj_type = .Range,
            },
        ),
    );

    return 1;
}

pub fn @"union"(ctx: *o.NativeCtx) callconv(.c) c_int {
    const rangeA = ctx.vm.peekAsIdx(o.ObjRange, 1).get(ctx.vm.gc);
    const rangeB = ctx.vm.peekAsIdx(o.ObjRange, 0).get(ctx.vm.gc);

    ctx.vm.push(
        .fromObj(
            .{
                .index = (ctx.vm.gc.allocateObject(
                    o.ObjRange{
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
                }).index,
                .obj_type = .Range,
            },
        ),
    );

    return 1;
}

pub fn high(ctx: *o.NativeCtx) callconv(.c) c_int {
    ctx.vm.push(
        .fromInteger(
            ctx.vm.peekAsIdx(o.ObjRange, 0).get(ctx.vm.gc).high,
        ),
    );

    return 1;
}

pub fn low(ctx: *o.NativeCtx) callconv(.c) c_int {
    ctx.vm.push(
        .fromInteger(
            ctx.vm.peekAsIdx(o.ObjRange, 0).get(ctx.vm.gc).low,
        ),
    );

    return 1;
}

pub fn contains(ctx: *o.NativeCtx) callconv(.c) c_int {
    const range = ctx.vm.peekAsIdx(o.ObjRange, 1).get(ctx.vm.gc);
    const value = ctx.vm.peek(0).integer();

    ctx.vm.push(
        .fromBoolean(
            (range.high >= range.low and value >= range.low and value < range.high) or
                (range.low >= range.high and value >= range.high and value < range.low),
        ),
    );

    return 1;
}
