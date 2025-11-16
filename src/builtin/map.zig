const std = @import("std");
const o = @import("../obj.zig");
const VM = @import("../vm.zig").VM;
const v = @import("../value.zig");
const buzz_api = @import("../buzz_api.zig");

fn cloneRaw(ctx: *o.NativeCtx, mutable: bool) void {
    const self = ctx.vm.peekAsIdx(o.ObjMap, 0);

    const new_map_idx = ctx.vm.gc.allocateObject(
        o.ObjMap.init(
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

    new_map_idx.get(ctx.vm.gc).map.deinit(ctx.vm.gc.allocator);
    new_map_idx.get(ctx.vm.gc).map = self.get(ctx.vm.gc).map.clone(ctx.vm.gc.allocator) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    ctx.vm.push(
        .fromObj(
            .{
                .index = new_map_idx.index,
                .obj_type = .Map,
            },
        ),
    );
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
    const self = ctx.vm.peekAsIdx(o.ObjMap, 2);
    const closure = ctx.vm.peek(1);
    var accumulator = ctx.vm.peek(0);

    var it = self.get(ctx.vm.gc).map.iterator();
    while (it.next()) |kv| {
        var args = [_]*const v.Value{ kv.key_ptr, kv.value_ptr, &accumulator };

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
    const self = ctx.vm.peekAsIdx(o.ObjMap, 1);
    const closure = ctx.vm.peek(0);

    const new_map = ctx.vm.gc.allocateObject(
        o.ObjMap.init(
            ctx.vm.gc.allocator,
            self.get(ctx.vm.gc).type_def,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var it = self.get(ctx.vm.gc).map.iterator();
    while (it.next()) |kv| {
        var args = [_]*const v.Value{ kv.key_ptr, kv.value_ptr };

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
            o.ObjMap.set(
                new_map,
                ctx.vm.gc,
                kv.key_ptr.*,
                kv.value_ptr.*,
            ) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(
        .fromObj(
            .{
                .index = new_map.index,
                .obj_type = .Map,
            },
        ),
    );

    return 1;
}

pub fn forEach(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = ctx.vm.peekAsIdx(o.ObjMap, 1);
    const closure = ctx.vm.peek(0);

    var it = self.get(ctx.vm.gc).map.iterator();
    while (it.next()) |kv| {
        var args = [_]*const v.Value{ kv.key_ptr, kv.value_ptr };

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

pub fn map(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = ctx.vm.peekAsIdx(o.ObjMap, 1);
    const closure_idx = ctx.vm.peekAsIdx(o.ObjClosure, 0);

    const mapped_type = closure_idx.get(ctx.vm.gc)
        .function.get(ctx.vm.gc)
        .type_def.get(ctx.vm.gc)
        .resolved_type.?.Function.return_type.get(ctx.vm.gc)
        .resolved_type.?.ObjectInstance.of.get(ctx.vm.gc)
        .resolved_type.?.Object;

    const new_map = ctx.vm.gc.allocateObject(
        o.ObjMap.init(
            ctx.vm.gc.allocator,
            ctx.vm.gc.type_registry.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Map,
                    .resolved_type = .{
                        .Map = o.ObjMap.MapDef.init(
                            mapped_type.fields.get("key").?.type_def,
                            mapped_type.fields.get("value").?.type_def,
                            self.get(ctx.vm.gc)
                                .type_def.get(ctx.vm.gc)
                                .resolved_type.?.Map.mutable,
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

    var it = self.get(ctx.vm.gc).map.iterator();
    while (it.next()) |kv| {
        var args = [_]*const v.Value{ kv.key_ptr, kv.value_ptr };

        if (!buzz_api.bz_call(
            ctx.vm,
            .fromObj(.{ .index = closure_idx.index, .obj_type = .Closure }),
            @ptrCast(&args),
            @intCast(args.len),
            null,
        )) {
            return -2;
        }

        const instance = ctx.vm.popAsIdx(o.ObjObjectInstance);
        const object_def = instance.get(ctx.vm.gc)
            .type_def.get(ctx.vm.gc)
            .resolved_type.?.ObjectInstance.of.get(ctx.vm.gc)
            .resolved_type.?.Object;

        o.ObjMap.set(
            new_map,
            ctx.vm.gc,
            instance.get(ctx.vm.gc).fields[object_def.fields.get("key").?.index],
            instance.get(ctx.vm.gc).fields[object_def.fields.get("value").?.index],
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
    }

    ctx.vm.push(
        .fromObj(
            .{
                .index = new_map.index,
                .obj_type = .Map,
            },
        ),
    );

    return 1;
}

const SortContext = struct {
    sort_closure: v.Value,
    ctx: *o.NativeCtx,
    map: *o.ObjMap,
    had_error: bool = false,

    pub fn lessThan(context: *SortContext, lhs_index: usize, rhs_index: usize) bool {
        const map_keys = context.map.map.keys();
        const lhs = map_keys[lhs_index];
        const rhs = map_keys[rhs_index];

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
};

pub fn sort(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self_idx = ctx.vm.peekAsIdx(o.ObjMap, 1);
    const sort_closure = ctx.vm.peek(0);

    var context = SortContext{
        .sort_closure = sort_closure,
        .ctx = ctx,
        .map = self_idx.get(ctx.vm.gc),
    };
    self_idx.get(ctx.vm.gc).map.sort(&context);

    if (context.had_error) {
        return -2;
    }

    ctx.vm.gc.markObjDirty(o.ObjMap, self_idx) catch @panic("Out of memory");

    ctx.vm.push(
        .fromObj(
            .{
                .index = self_idx.index,
                .obj_type = .Map,
            },
        ),
    );

    return 1;
}

pub fn diff(ctx: *o.NativeCtx) callconv(.c) c_int {
    const lhs = ctx.vm.peekAsIdx(o.ObjMap, 1);
    const rhs = ctx.vm.peekAsIdx(o.ObjMap, 0);

    const new_map = ctx.vm.gc.allocateObject(
        o.ObjMap.init(
            ctx.vm.gc.allocator,
            lhs.get(ctx.vm.gc).type_def,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var it = lhs.get(ctx.vm.gc).map.iterator();
    while (it.next()) |kv| {
        // If not present in rhs map, add it to the new map
        if (rhs.get(ctx.vm.gc).map.get(kv.key_ptr.*) == null) {
            o.ObjMap.set(
                new_map,
                ctx.vm.gc,
                kv.key_ptr.*,
                kv.value_ptr.*,
            ) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(
        .fromObj(
            .{
                .index = new_map.index,
                .obj_type = .Map,
            },
        ),
    );

    return 1;
}

pub fn intersect(ctx: *o.NativeCtx) callconv(.c) c_int {
    const lhs = ctx.vm.peekAsIdx(o.ObjMap, 1);
    const rhs = ctx.vm.peekAsIdx(o.ObjMap, 0);

    const new_map = ctx.vm.gc.allocateObject(
        o.ObjMap.init(
            ctx.vm.gc.allocator,
            lhs.get(ctx.vm.gc).type_def,
        ) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    var it = lhs.get(ctx.vm.gc).map.iterator();
    while (it.next()) |kv| {
        // If not present in rhs map, add it to the new map
        if (rhs.get(ctx.vm.gc).map.get(kv.key_ptr.*) != null) {
            o.ObjMap.set(
                new_map,
                ctx.vm.gc,
                kv.key_ptr.*,
                kv.value_ptr.*,
            ) catch {
                ctx.vm.panic("Out of memory");
                unreachable;
            };
        }
    }

    ctx.vm.push(
        .fromObj(
            .{
                .index = new_map.index,
                .obj_type = .Map,
            },
        ),
    );

    return 1;
}

pub fn size(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = ctx.vm.peekAsIdx(o.ObjMap, 0);

    ctx.vm.push(
        .fromInteger(
            @intCast(self.get(ctx.vm.gc).map.count()),
        ),
    );

    return 1;
}

pub fn remove(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = ctx.vm.peekAsIdx(o.ObjMap, 1);
    const map_key = ctx.vm.peek(0);

    if (self.get(ctx.vm.gc).map.fetchOrderedRemove(map_key)) |removed| {
        ctx.vm.push(removed.value);
    } else {
        ctx.vm.push(.Null);
    }

    return 1;
}

pub fn keys(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = ctx.vm.peekAsIdx(o.ObjMap, 0);

    const map_keys = self.get(ctx.vm.gc).map.keys();
    var result = std.ArrayList(v.Value){};
    for (map_keys) |key| {
        result.append(ctx.vm.gc.allocator, key) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        };
    }

    const list_def_type = ctx.vm.gc.type_registry.getTypeDef(
        .{
            .def_type = .List,
            .optional = false,
            .resolved_type = .{
                .List = o.ObjList.ListDef.init(
                    self.get(ctx.vm.gc)
                        .type_def.get(ctx.vm.gc)
                        .resolved_type.?.Map.key_type,
                    false,
                ),
            },
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    // Prevent collection
    ctx.vm.push(
        .fromObj(
            .{
                .index = list_def_type.index,
                .obj_type = .Type,
            },
        ),
    );

    const list_idx = ctx.vm.gc.allocateObject(
        o.ObjList.init(ctx.vm.gc.allocator, list_def_type) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    list_idx.get(ctx.vm.gc).items.deinit(ctx.vm.gc.allocator);
    list_idx.get(ctx.vm.gc).items = result;

    _ = ctx.vm.pop();
    ctx.vm.push(
        .fromObj(
            .{
                .index = list_idx.index,
                .obj_type = .List,
            },
        ),
    );

    return 1;
}

pub fn values(ctx: *o.NativeCtx) callconv(.c) c_int {
    const self = ctx.vm.peekAsIdx(o.ObjMap, 0);

    const map_values = self.get(ctx.vm.gc).map.values();
    var result = std.ArrayList(v.Value){};
    result.appendSlice(ctx.vm.gc.allocator, map_values) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    const list_def_type = ctx.vm.gc.type_registry.getTypeDef(
        o.ObjTypeDef{
            .def_type = .List,
            .optional = false,
            .resolved_type = .{
                .List = o.ObjList.ListDef.init(
                    self.get(ctx.vm.gc)
                        .type_def.get(ctx.vm.gc)
                        .resolved_type.?.Map.value_type,
                    false,
                ),
            },
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    const list_idx = ctx.vm.gc.allocateObject(
        o.ObjList.init(ctx.vm.gc.allocator, list_def_type) catch {
            ctx.vm.panic("Out of memory");
            unreachable;
        },
    ) catch {
        ctx.vm.panic("Out of memory");
        unreachable;
    };

    list_idx.get(ctx.vm.gc).items.deinit(ctx.vm.gc.allocator);
    list_idx.get(ctx.vm.gc).items = result;

    ctx.vm.push(
        .fromObj(
            .{
                .index = list_idx.index,
                .obj_type = .List,
            },
        ),
    );

    return 1;
}
