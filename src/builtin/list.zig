const builtin = @import("builtin");
const std = @import("std");
const o = @import("../obj.zig");
const VM = @import("../vm.zig").VM;
const v = @import("../value.zig");
const buzz_api = @import("../buzz_api.zig");

pub fn append(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list_value = ctx.vm.peek(1);
    const list = o.ObjList.cast(list_value.obj()).?;
    const value = ctx.vm.peek(0);

    list.rawAppend(
        ctx.vm.gc,
        value,
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    return 0;
}

pub fn insert(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list_value = ctx.vm.peek(2);
    const list = o.ObjList.cast(list_value.obj()).?;
    var index = ctx.vm.peek(1).integer();
    const value = ctx.vm.peek(0);

    if (index < 0 or list.items.items.len == 0) {
        index = 0;
    } else if (index >= list.items.items.len) {
        index = @as(v.Integer, @intCast(list.items.items.len)) - 1;
    }

    list.rawInsert(
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
    const list = o.ObjList.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(v.Value.fromInteger(@as(v.Integer, @intCast(list.items.items.len))));

    return 1;
}

pub fn reverse(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = o.ObjList.cast(ctx.vm.peek(0).obj()).?;

    var new_list = ctx.vm.gc.allocateObject(
        o.ObjList.init(ctx.vm.gc.allocator, list.type_def) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    new_list.items.appendSlice(ctx.vm.gc.allocator, list.items.items) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };
    std.mem.reverse(v.Value, new_list.items.items);

    ctx.vm.push(new_list.toValue());

    return 1;
}

pub fn pop(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = o.ObjList.cast(ctx.vm.peek(0).obj()).?;

    if (list.items.items.len > 0) {
        ctx.vm.push(list.items.pop().?);
    } else {
        ctx.vm.push(v.Value.Null);
    }

    return 1;
}

pub fn remove(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = o.ObjList.cast(ctx.vm.peek(1).obj()).?;
    const list_index = ctx.vm.peek(0).integer();

    if (list_index < 0 or list_index >= list.items.items.len) {
        ctx.vm.push(v.Value.Null);

        return 1;
    }

    ctx.vm.push(list.items.orderedRemove(@as(usize, @intCast(list_index))));
    ctx.vm.gc.markObjDirty(&list.obj) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    return 1;
}

const SortContext = struct {
    sort_closure: v.Value,
    ctx: *o.NativeCtx,
};

fn lessThan(context: SortContext, lhs: v.Value, rhs: v.Value) bool {
    var args = [_]*const v.Value{ &lhs, &rhs };

    buzz_api.bz_call(
        context.ctx.vm,
        context.sort_closure,
        @ptrCast(&args),
        @intCast(args.len),
        null,
    );

    return context.ctx.vm.pop().boolean();
}

pub fn sort(ctx: *o.NativeCtx) callconv(.c) c_int {
    var self = o.ObjList.cast(ctx.vm.peek(1).obj()).?;
    // fun compare(T lhs, T rhs) > bool
    const sort_closure = ctx.vm.peek(0);

    std.sort.insertion(
        v.Value,
        self.items.items,
        SortContext{
            .sort_closure = sort_closure,
            .ctx = ctx,
        },
        lessThan,
    );
    ctx.vm.gc.markObjDirty(self.toObj()) catch @panic("Out of memory");

    ctx.vm.push(self.toValue());

    return 1;
}

pub fn indexOf(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjList.cast(ctx.vm.peek(1).obj()).?;
    const needle = ctx.vm.peek(0);

    var index: ?usize = null;
    var i: usize = 0;
    for (self.items.items) |item| {
        if (needle.eql(item)) {
            index = i;
            break;
        }

        i += 1;
    }

    ctx.vm.push(if (index) |uindex|
        v.Value.fromInteger(@as(v.Integer, @intCast(uindex)))
    else
        v.Value.Null);

    return 1;
}

fn cloneRaw(ctx: *o.NativeCtx, mutable: bool) void {
    const self = o.ObjList.cast(ctx.vm.peek(0).obj()).?;

    var new_list = ctx.vm.gc.allocateObject(
        o.ObjList.init(
            ctx.vm.gc.allocator,
            self.type_def.cloneMutable(&ctx.vm.gc.type_registry, mutable) catch {
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

    new_list.items.appendSlice(ctx.vm.gc.allocator, self.items.items) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(new_list.toValue());
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
    const self = o.ObjList.cast(ctx.vm.peek(1).obj()).?;
    const separator = o.ObjString.cast(ctx.vm.peek(0).obj()).?;

    var result = std.ArrayList(u8).empty;
    var writer = result.writer(ctx.vm.gc.allocator);
    defer result.deinit(ctx.vm.gc.allocator);
    for (self.items.items, 0..) |item, i| {
        item.toString(&writer) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };

        if (i + 1 < self.items.items.len) {
            writer.writeAll(separator.string) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(
        (ctx.vm.gc.copyString(result.items) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        }).toValue(),
    );

    return 1;
}

pub fn sub(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjList.cast(ctx.vm.peek(2).obj()).?;
    const start = @min(
        @max(
            0,
            ctx.vm.peek(1).integer(),
        ),
        self.items.items.len - 1,
    );
    const upto = if (ctx.vm.peek(0).integerOrNull()) |u|
        @max(0, u)
    else
        null;

    const limit: usize = if (upto != null and @as(usize, @intCast(start + upto.?)) < self.items.items.len)
        @intCast(start + upto.?)
    else
        self.items.items.len;
    const substr = self.items.items[@intCast(start)..limit];

    var methods = std.ArrayList(?*o.ObjNative)
        .fromOwnedSlice(self.methods)
        .clone(ctx.vm.gc.allocator) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var list = ctx.vm.gc.allocateObject(
        o.ObjList{
            .type_def = self.type_def,
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

    ctx.vm.push(list.toValue());

    list.items.appendSlice(ctx.vm.gc.allocator, substr) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    return 1;
}

pub fn next(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list_value = ctx.vm.peek(1);
    const list = o.ObjList.cast(list_value.obj()).?;
    const list_index = ctx.vm.peek(0);

    const next_index: ?v.Integer = list.rawNext(
        ctx.vm,
        list_index.integerOrNull(),
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(
        if (next_index) |unext_index|
            v.Value.fromInteger(unext_index)
        else
            v.Value.Null,
    );

    return 1;
}

pub fn forEach(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = o.ObjList.cast(ctx.vm.peek(1).obj()).?;
    const closure = ctx.vm.peek(0);

    for (list.items.items, 0..) |item, index| {
        const index_value = v.Value.fromInteger(@as(v.Integer, @intCast(index)));

        var args = [_]*const v.Value{ &index_value, &item };

        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        );
    }

    return 0;
}

pub fn reduce(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = o.ObjList.cast(ctx.vm.peek(2).obj()).?;
    const closure = ctx.vm.peek(1);
    var accumulator = ctx.vm.peek(0);

    for (list.items.items, 0..) |item, index| {
        const index_value = v.Value.fromInteger(@as(v.Integer, @intCast(index)));

        var args = [_]*const v.Value{
            &index_value,
            &item,
            &accumulator,
        };

        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        );

        accumulator = ctx.vm.pop();
    }

    ctx.vm.push(accumulator);

    return 1;
}

pub fn filter(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = o.ObjList.cast(ctx.vm.peek(1).obj()).?;
    const closure = ctx.vm.peek(0);

    var new_list = ctx.vm.gc.allocateObject(
        o.ObjList.init(
            ctx.vm.gc.allocator,
            list.type_def,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    for (list.items.items, 0..) |item, index| {
        const index_value = v.Value.fromInteger(@as(v.Integer, @intCast(index)));
        var args = [_]*const v.Value{ &index_value, &item };

        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        );

        if (ctx.vm.pop().boolean()) {
            new_list.rawAppend(ctx.vm.gc, item) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(new_list.toValue());

    return 1;
}

pub fn map(ctx: *o.NativeCtx) callconv(.c) c_int {
    const list = o.ObjList.cast(ctx.vm.peek(1).obj()).?;
    const closure = ctx.vm.peek(0);

    const mapped_type = o.ObjClosure.cast(closure.obj()).?
        .function
        .type_def.resolved_type.?.Function
        .return_type;

    var new_list = ctx.vm.gc.allocateObject(
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

    for (list.items.items, 0..) |item, index| {
        const index_value = v.Value.fromInteger(@as(v.Integer, @intCast(index)));
        var args = [_]*const v.Value{ &index_value, &item };

        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        );

        new_list.rawAppend(ctx.vm.gc, ctx.vm.pop()) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
    }

    ctx.vm.push(new_list.toValue());

    return 1;
}

pub fn fill(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjList.cast(ctx.vm.peek(3).obj()).?;
    const value = ctx.vm.peek(2);
    const start: usize = @intCast(
        @min(
            @max(
                0,
                ctx.vm.peek(1).integerOrNull() orelse 0,
            ),
            self.items.items.len - 1,
        ),
    );
    const count: ?usize = if (ctx.vm.peek(0).integerOrNull()) |c|
        @intCast(@max(0, c))
    else
        null;

    const limit: usize = if (count != null and start + count.? < self.items.items.len)
        start + count.?
    else
        self.items.items.len;

    for (start..limit) |i| {
        self.items.items[i] = value;
    }

    ctx.vm.push(self.toValue());

    return 1;
}
