const std = @import("std");
const o = @import("../obj.zig");
const VM = @import("../vm.zig").VM;
const v = @import("../value.zig");
const buzz_api = @import("../buzz_api.zig");

fn cloneRaw(ctx: *o.NativeCtx, mutable: bool) void {
    const self = o.ObjMap.cast(ctx.vm.peek(0).obj()).?;

    var new_map = ctx.vm.gc.allocate(
        o.ObjMap,
        o.ObjMap.init(
            ctx.vm.gc.allocator,
            self.type_def.cloneMutable(
                ctx.vm.gc,
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
    new_map.map.deinit(ctx.vm.gc.allocator);
    new_map.map = self.map.clone(ctx.vm.gc.allocator) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(new_map.toValue());
}

pub fn cloneMutable(ctx: *o.NativeCtx) callconv(.c) c_int {
    cloneRaw(ctx, true);

    return 1;
}

pub fn cloneImmutable(ctx: *o.NativeCtx) callconv(.c) c_int {
    cloneRaw(ctx, false);

    return 1;
}

pub fn reduce(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjMap.cast(ctx.vm.peek(2).obj()).?;
    const closure = ctx.vm.peek(1);
    var accumulator = ctx.vm.peek(0);

    var it = self.map.iterator();
    while (it.next()) |kv| {
        var args = [_]*const v.Value{ kv.key_ptr, kv.value_ptr, &accumulator };

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
    const self = o.ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const closure = ctx.vm.peek(0);

    var new_map: *o.ObjMap = ctx.vm.gc.allocate(
        o.ObjMap,
        o.ObjMap.init(
            ctx.vm.gc.allocator,
            self.type_def,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var it = self.map.iterator();
    while (it.next()) |kv| {
        var args = [_]*const v.Value{ kv.key_ptr, kv.value_ptr };

        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        );

        if (ctx.vm.pop().boolean()) {
            new_map.set(ctx.vm.gc, kv.key_ptr.*, kv.value_ptr.*) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(new_map.toValue());

    return 1;
}

pub fn forEach(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const closure = ctx.vm.peek(0);

    var it = self.map.iterator();
    while (it.next()) |kv| {
        var args = [_]*const v.Value{ kv.key_ptr, kv.value_ptr };

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

pub fn map(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = o.ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const closure = ctx.vm.peek(0);

    const mapped_type = o.ObjClosure.cast(closure.obj()).?.function.type_def.resolved_type.?.Function
        .return_type.resolved_type.?.ObjectInstance.of
        .resolved_type.?.Object;

    var new_map: *o.ObjMap = ctx.vm.gc.allocate(
        o.ObjMap,
        o.ObjMap.init(
            ctx.vm.gc.allocator,
            ctx.vm.gc.type_registry.getTypeDef(
                ctx.vm.gc,
                .{
                    .optional = false,
                    .def_type = .Map,
                    .resolved_type = .{
                        .Map = o.ObjMap.MapDef.init(
                            mapped_type.fields.get("key").?.type_def,
                            mapped_type.fields.get("value").?.type_def,
                            self.type_def.resolved_type.?.Map.mutable,
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

    var it = self.map.iterator();
    while (it.next()) |kv| {
        var args = [_]*const v.Value{ kv.key_ptr, kv.value_ptr };

        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        );

        const instance = o.ObjObjectInstance.cast(ctx.vm.pop().obj()).?;
        const object_def = instance.type_def.resolved_type.?.ObjectInstance.of
            .resolved_type.?.Object;

        new_map.set(
            ctx.vm.gc,
            instance.fields[object_def.fields.get("key").?.index],
            instance.fields[object_def.fields.get("value").?.index],
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
    }

    ctx.vm.push(new_map.toValue());

    return 1;
}

const SortContext = struct {
    sort_closure: v.Value,
    ctx: *o.NativeCtx,
    map: *o.ObjMap,

    pub fn lessThan(context: SortContext, lhs_index: usize, rhs_index: usize) bool {
        const map_keys = context.map.map.keys();
        const lhs = map_keys[lhs_index];
        const rhs = map_keys[rhs_index];

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
};

pub fn sort(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self: *o.ObjMap = o.ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const sort_closure = ctx.vm.peek(0);

    self.map.sort(
        SortContext{
            .sort_closure = sort_closure,
            .ctx = ctx,
            .map = self,
        },
    );
    // ctx.vm.gc.markObjDirty(self.toObj()) catch @panic("Out of memory");

    ctx.vm.push(self.toValue());

    return 1;
}

pub fn diff(ctx: *o.NativeCtx) callconv(.c) c_int {
    const lhs: *o.ObjMap = o.ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const rhs: *o.ObjMap = o.ObjMap.cast(ctx.vm.peek(0).obj()).?;

    var new_map: *o.ObjMap = ctx.vm.gc.allocate(
        o.ObjMap,
        o.ObjMap.init(
            ctx.vm.gc.allocator,
            lhs.type_def,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var it = lhs.map.iterator();
    while (it.next()) |kv| {
        // If not present in rhs map, add it to the new map
        if (rhs.map.get(kv.key_ptr.*) == null) {
            new_map.set(
                ctx.vm.gc,
                kv.key_ptr.*,
                kv.value_ptr.*,
            ) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(new_map.toValue());

    return 1;
}

pub fn intersect(ctx: *o.NativeCtx) callconv(.c) c_int {
    const lhs: *o.ObjMap = o.ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const rhs: *o.ObjMap = o.ObjMap.cast(ctx.vm.peek(0).obj()).?;

    var new_map: *o.ObjMap = ctx.vm.gc.allocate(
        o.ObjMap,
        o.ObjMap.init(
            ctx.vm.gc.allocator,
            lhs.type_def,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var it = lhs.map.iterator();
    while (it.next()) |kv| {
        // If not present in rhs map, add it to the new map
        if (rhs.map.get(kv.key_ptr.*) != null) {
            new_map.set(
                ctx.vm.gc,
                kv.key_ptr.*,
                kv.value_ptr.*,
            ) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(new_map.toValue());

    return 1;
}

pub fn size(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self: *o.ObjMap = o.ObjMap.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(v.Value.fromInteger(@intCast(self.map.count())));

    return 1;
}

pub fn remove(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self: *o.ObjMap = o.ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const map_key = ctx.vm.peek(0);

    if (self.map.fetchOrderedRemove(map_key)) |removed| {
        ctx.vm.push(removed.value);
    } else {
        ctx.vm.push(v.Value.Null);
    }

    return 1;
}

pub fn keys(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self: *o.ObjMap = o.ObjMap.cast(ctx.vm.peek(0).obj()).?;

    const map_keys = self.map.keys();
    var result = std.ArrayList(v.Value){};
    for (map_keys) |key| {
        result.append(ctx.vm.gc.allocator, key) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
    }

    var list_def_type: *o.ObjTypeDef = ctx.vm.gc.type_registry.getTypeDef(
        ctx.vm.gc,
        .{
            .def_type = .List,
            .optional = false,
            .resolved_type = .{
                .List = o.ObjList.ListDef.init(
                    self.type_def.resolved_type.?.Map.key_type,
                    false,
                ),
            },
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    // Prevent collection
    ctx.vm.push(list_def_type.toValue());

    var list = ctx.vm.gc.allocate(
        o.ObjList,
        o.ObjList.init(ctx.vm.gc.allocator, list_def_type) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    list.items.deinit(ctx.vm.gc.allocator);
    list.items = result;

    _ = ctx.vm.pop();
    ctx.vm.push(list.toValue());

    return 1;
}

pub fn values(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self: *o.ObjMap = o.ObjMap.cast(ctx.vm.peek(0).obj()).?;

    const map_values: []v.Value = self.map.values();
    var result = std.ArrayList(v.Value){};
    result.appendSlice(ctx.vm.gc.allocator, map_values) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    const list_def_type = ctx.vm.gc.type_registry.getTypeDef(
        ctx.vm.gc,
        o.ObjTypeDef{
            .def_type = .List,
            .optional = false,
            .resolved_type = .{
                .List = o.ObjList.ListDef.init(
                    self.type_def.resolved_type.?.Map.value_type,
                    false,
                ),
            },
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var list = ctx.vm.gc.allocate(
        o.ObjList,
        o.ObjList.init(ctx.vm.gc.allocator, list_def_type) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    list.items.deinit(ctx.vm.gc.allocator);
    list.items = result;

    ctx.vm.push(list.toValue());

    return 1;
}
