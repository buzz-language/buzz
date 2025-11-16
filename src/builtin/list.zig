const builtin = @import("builtin");
const std = @import("std");
const o = @import("../obj.zig");
const VM = @import("../vm.zig").VM;
const v = @import("../value.zig");
const buzz_api = @import("../buzz_api.zig");
const Pool = @import("../pool.zig").Pool;

pub fn append(ctx: *o.NativeCtx) callconv(.c) c_int {
    o.ObjList.rawAppend(
        ctx.vm.peekAsIdx(o.ObjList, 1),
        ctx.vm.gc,
        ctx.vm.peek(0),
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    return 0;
}

pub fn insert(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list_idx = ctx.vm.peekAsIdx(o.ObjList, 2);

    var index = ctx.vm.peek(1).integer();
    const value = ctx.vm.peek(0);

    if (index < 0 or list_idx.get(ctx.vm.gc).items.items.len == 0) {
        index = 0;
    } else if (index >= list_idx.get(ctx.vm.gc).items.items.len) {
        index = @as(v.Integer, @intCast(list_idx.get(ctx.vm.gc).items.items.len)) - 1;
    }

    o.ObjList.rawInsert(
        list_idx,
        ctx.vm.gc,
        @as(usize, @intCast(index)),
        value,
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(value);

    return 1;
}

pub fn len(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = ctx.vm.peekAsIdx(o.ObjList, 0).get(ctx.vm.gc);

    ctx.vm.push(
        .fromInteger(
            @intCast(list.items.items.len),
        ),
    );

    return 1;
}

pub fn reverse(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = ctx.vm.peekAsIdx(o.ObjList, 0);

    const new_list_idx = ctx.vm.gc.allocateObject(
        o.ObjList.init(ctx.vm.gc.allocator, list.get(ctx.vm.gc).type_def) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    new_list_idx.get(ctx.vm.gc).items.appendSlice(ctx.vm.gc.allocator, list.get(ctx.vm.gc).items.items) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };
    std.mem.reverse(v.Value, new_list_idx.get(ctx.vm.gc).items.items);

    ctx.vm.push(
        .fromObj(
            .{
                .index = new_list_idx.index,
                .obj_type = .List,
            },
        ),
    );

    return 1;
}

pub fn pop(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = ctx.vm.peekAsIdx(o.ObjList, 0).get(ctx.vm.gc);

    if (list.items.items.len > 0) {
        ctx.vm.push(list.items.pop().?);
    } else {
        ctx.vm.push(.Null);
    }

    return 1;
}

pub fn remove(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list_idx = ctx.vm.peekAsIdx(o.ObjList, 1);
    const list_index = ctx.vm.peek(0).integer();

    if (list_index < 0 or list_index >= list_idx.get(ctx.vm.gc).items.items.len) {
        ctx.vm.push(.Null);

        return 1;
    }

    ctx.vm.push(
        list_idx.get(ctx.vm.gc).items.orderedRemove(@as(usize, @intCast(list_index))),
    );
    ctx.vm.gc.markObjDirty(o.ObjList, list_idx) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    return 1;
}

const SortContext = struct {
    sort_closure: v.Value,
    ctx: *o.NativeCtx,
    had_error: bool = false,
};

fn lessThan(context: *SortContext, lhs: v.Value, rhs: v.Value) bool {
    var args = [_]*const v.Value{ &lhs, &rhs };

    if (!buzz_api.bz_call(
        context.ctx.vm,
        context.sort_closure,
        @ptrCast(&args),
        @intCast(args.len),
        null,
    )) {
        context.had_error = true;

        return false;
    }

    return context.ctx.vm.pop().boolean();
}

pub fn sort(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self_idx = ctx.vm.peekAsIdx(o.ObjList, 1);
    // fun compare(T lhs, T rhs) > bool
    const sort_closure = ctx.vm.peek(0);

    var context = SortContext{
        .sort_closure = sort_closure,
        .ctx = ctx,
    };
    std.sort.insertion(
        v.Value,
        self_idx.get(ctx.vm.gc).items.items,
        &context,
        lessThan,
    );

    if (context.had_error) {
        return -2;
    }

    ctx.vm.gc.markObjDirty(
        o.ObjList,
        self_idx,
    ) catch @panic("Out of memory");

    ctx.vm.push(
        .fromObj(
            .{
                .index = self_idx.index,
                .obj_type = .List,
            },
        ),
    );

    return 1;
}

pub fn indexOf(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = ctx.vm.peekAsIdx(o.ObjList, 1).get(ctx.vm.gc);
    const needle = ctx.vm.peek(0);

    var index: ?usize = null;
    var i: usize = 0;
    for (self.items.items) |item| {
        if (needle.eql(item, ctx.vm.gc)) {
            index = i;
            break;
        }

        i += 1;
    }

    ctx.vm.push(
        if (index) |uindex|
            .fromInteger(@intCast(uindex))
        else
            .Null,
    );

    return 1;
}

fn cloneRaw(ctx: *o.NativeCtx, mutable: bool) void {
    const self = ctx.vm.peekAsIdx(o.ObjList, 0);

    const new_list = ctx.vm.gc.allocateObject(
        o.ObjList.init(
            ctx.vm.gc.allocator,
            o.ObjTypeDef.cloneMutable(
                self.get(ctx.vm.gc).type_def,
                &ctx.vm.gc.type_registry,
                mutable,
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

    new_list.get(ctx.vm.gc)
        .items.appendSlice(ctx.vm.gc.allocator, self.get(ctx.vm.gc).items.items) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(
        .fromObj(
            .{
                .index = new_list.index,
                .obj_type = .List,
            },
        ),
    );
}

pub fn cloneImmutable(ctx: *o.NativeCtx) callconv(.c) c_int {
    cloneRaw(ctx, false);

    return 1;
}

pub fn cloneMutable(ctx: *o.NativeCtx) callconv(.c) c_int {
    cloneRaw(ctx, true);

    return 1;
}

pub fn join(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = ctx.vm.peekAsIdx(o.ObjList, 1);
    const separator = ctx.vm.peekAsIdx(o.ObjString, 0);

    var result = std.Io.Writer.Allocating.init(ctx.vm.gc.allocator);
    defer result.deinit();
    for (self.get(ctx.vm.gc).items.items, 0..) |item, i| {
        item.toString(ctx.vm.gc, &result.writer) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };

        if (i + 1 < self.get(ctx.vm.gc).items.items.len) {
            result.writer.writeAll(separator.get(ctx.vm.gc).string) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(
        .fromObj(
            .{
                .index = (ctx.vm.gc.copyString(result.written()) catch {
                    ctx.vm.panic("Out of memory");
                    unreachable;
                }).index,
                .obj_type = .String,
            },
        ),
    );

    return 1;
}

pub fn sub(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = ctx.vm.peekAsIdx(o.ObjList, 2);
    const start = @min(
        @max(
            0,
            ctx.vm.peek(1).integer(),
        ),
        self.get(ctx.vm.gc).items.items.len - 1,
    );
    const upto = if (ctx.vm.peek(0).integerOrNull()) |u|
        @max(0, u)
    else
        null;

    const limit: usize = if (upto != null and @as(usize, @intCast(start + upto.?)) < self.get(ctx.vm.gc).items.items.len)
        @intCast(start + upto.?)
    else
        self.get(ctx.vm.gc).items.items.len;
    const substr = self.get(ctx.vm.gc).items.items[@intCast(start)..limit];

    var methods = std.ArrayList(?Pool(o.ObjNative).Idx)
        .fromOwnedSlice(self.get(ctx.vm.gc).methods)
        .clone(ctx.vm.gc.allocator) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    const list = ctx.vm.gc.allocateObject(
        o.ObjList{
            .type_def = self.get(ctx.vm.gc).type_def,
            .methods = methods.toOwnedSlice(ctx.vm.gc.allocator) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            },
            .items = std.ArrayList(v.Value){},
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

    list.get(ctx.vm.gc)
        .items.appendSlice(ctx.vm.gc.allocator, substr) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    return 1;
}

pub fn next(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = ctx.vm.peekAsIdx(o.ObjList, 1);
    const list_index = ctx.vm.peek(0);

    const next_index = list.get(ctx.vm.gc).rawNext(
        ctx.vm,
        list_index.integerOrNull(),
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(
        if (next_index) |unext_index|
            .fromInteger(unext_index)
        else
            .Null,
    );

    return 1;
}

pub fn forEach(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = ctx.vm.peekAsIdx(o.ObjList, 1);
    const closure = ctx.vm.peek(0);

    for (list.get(ctx.vm.gc).items.items, 0..) |item, index| {
        const index_value = v.Value.fromInteger(@as(v.Integer, @intCast(index)));

        var args = [_]*const v.Value{ &index_value, &item };

        if (!buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        )) {
            return -2;
        }
    }

    return 0;
}

pub fn reduce(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = ctx.vm.peekAsIdx(o.ObjList, 2);
    const closure = ctx.vm.peek(1);
    var accumulator = ctx.vm.peek(0);

    for (list.get(ctx.vm.gc).items.items, 0..) |item, index| {
        const index_value = v.Value.fromInteger(@as(v.Integer, @intCast(index)));

        var args = [_]*const v.Value{
            &index_value,
            &item,
            &accumulator,
        };

        if (!buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        )) {
            return -2;
        }

        accumulator = ctx.vm.pop();
    }

    ctx.vm.push(accumulator);

    return 1;
}

pub fn filter(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = ctx.vm.peekAsIdx(o.ObjList, 1);
    const closure = ctx.vm.peek(0);

    const new_list = ctx.vm.gc.allocateObject(
        o.ObjList.init(
            ctx.vm.gc.allocator,
            list.get(ctx.vm.gc).type_def,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    for (list.get(ctx.vm.gc).items.items, 0..) |item, index| {
        const index_value = v.Value.fromInteger(@as(v.Integer, @intCast(index)));
        var args = [_]*const v.Value{ &index_value, &item };

        if (!buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        )) {
            return -2;
        }

        if (ctx.vm.pop().boolean()) {
            o.ObjList.rawAppend(new_list, ctx.vm.gc, item) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(
        .fromObj(
            .{
                .index = new_list.index,
                .obj_type = .List,
            },
        ),
    );

    return 1;
}

pub fn map(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = ctx.vm.peekAsIdx(o.ObjList, 1);
    const closure = ctx.vm.peek(0);

    const mapped_type = ctx.vm.gc.ptr(o.ObjClosure, .idx(closure.obj().index)).?
        .function.get(ctx.vm.gc)
        .type_def.get(ctx.vm.gc)
        .resolved_type.?.Function
        .return_type;

    const new_list = ctx.vm.gc.allocateObject(
        o.ObjList.init(
            ctx.vm.gc.allocator,
            ctx.vm.gc.type_registry.getTypeDef(
                .{
                    .def_type = .List,
                    .resolved_type = .{
                        .List = o.ObjList.ListDef.init(
                            mapped_type,
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

    for (list.get(ctx.vm.gc).items.items, 0..) |item, index| {
        const index_value = v.Value.fromInteger(@as(v.Integer, @intCast(index)));
        var args = [_]*const v.Value{ &index_value, &item };

        if (!buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        )) {
            return -2;
        }

        o.ObjList.rawAppend(new_list, ctx.vm.gc, ctx.vm.pop()) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
    }

    ctx.vm.push(
        .fromObj(
            .{
                .index = new_list.index,
                .obj_type = .List,
            },
        ),
    );

    return 1;
}

pub fn fill(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self_idx = ctx.vm.peekAsIdx(o.ObjList, 3);
    const value = ctx.vm.peek(2);
    const start: usize = @intCast(
        @min(
            @max(
                0,
                ctx.vm.peek(1).integerOrNull() orelse 0,
            ),
            self_idx.get(ctx.vm.gc).items.items.len - 1,
        ),
    );
    const count: ?usize = if (ctx.vm.peek(0).integerOrNull()) |c|
        @intCast(@max(0, c))
    else
        null;

    const limit: usize = if (count != null and start + count.? < self_idx.get(ctx.vm.gc).items.items.len)
        start + count.?
    else
        self_idx.get(ctx.vm.gc).items.items.len;

    for (start..limit) |i| {
        self_idx.get(ctx.vm.gc).items.items[i] = value;
    }

    ctx.vm.push(
        .fromObj(
            .{
                .index = self_idx.index,
                .obj_type = .List,
            },
        ),
    );

    return 1;
}
