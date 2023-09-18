const std = @import("std");
const _obj = @import("../obj.zig");
const ObjString = _obj.ObjString;
const ObjMap = _obj.ObjMap;
const ObjList = _obj.ObjList;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjClosure = _obj.ObjClosure;
const ObjObjectInstance = _obj.ObjObjectInstance;
const NativeCtx = _obj.NativeCtx;
const VM = @import("../vm.zig").VM;
const _value = @import("../value.zig");
const buzz_api = @import("../buzz_api.zig");
const Value = _value.Value;
const floatToInteger = _value.floatToInteger;
const valueEql = _value.valueEql;
const valueToString = _value.valueToString;

pub fn clone(ctx: *NativeCtx) c_int {
    const self = ObjMap.cast(ctx.vm.peek(0).obj()).?;

    var new_map = ctx.vm.gc.allocateObject(
        ObjMap,
        ObjMap.init(ctx.vm.gc.allocator, self.type_def),
    ) catch @panic("Out of memory");
    new_map.map.deinit();
    new_map.map = self.map.clone() catch @panic("Out of memory");

    ctx.vm.push(new_map.toValue());

    return 1;
}

pub fn reduce(ctx: *NativeCtx) c_int {
    const self = ObjMap.cast(ctx.vm.peek(2).obj()).?;
    const closure = ObjClosure.cast(ctx.vm.peek(1).obj()).?;
    var accumulator = ctx.vm.peek(0);

    var it = self.map.iterator();
    while (it.next()) |kv| {
        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&.{
                kv.key_ptr.*,
                kv.value_ptr.*,
                accumulator,
            }),
            3,
            null,
        );

        accumulator = ctx.vm.pop();
    }

    ctx.vm.push(accumulator);

    return 1;
}

pub fn filter(ctx: *NativeCtx) c_int {
    const self = ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const closure = ObjClosure.cast(ctx.vm.peek(0).obj()).?;

    var new_map: *ObjMap = ctx.vm.gc.allocateObject(
        ObjMap,
        ObjMap.init(
            ctx.vm.gc.allocator,
            self.type_def,
        ),
    ) catch unreachable;

    var it = self.map.iterator();
    while (it.next()) |kv| {
        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&.{
                kv.key_ptr.*,
                kv.value_ptr.*,
            }),
            2,
            null,
        );

        if (ctx.vm.pop().boolean()) {
            new_map.set(ctx.vm.gc, kv.key_ptr.*, kv.value_ptr.*) catch unreachable;
        }
    }

    ctx.vm.push(new_map.toValue());

    return 1;
}

pub fn forEach(ctx: *NativeCtx) c_int {
    const self = ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const closure = ObjClosure.cast(ctx.vm.peek(0).obj()).?;

    var it = self.map.iterator();
    while (it.next()) |kv| {
        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&.{
                kv.key_ptr.*,
                kv.value_ptr.*,
            }),
            2,
            null,
        );
    }

    return 0;
}

pub fn map(ctx: *NativeCtx) c_int {
    const self = ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const closure = ObjClosure.cast(ctx.vm.peek(0).obj()).?;

    const mapped_type = closure.function.type_def.resolved_type.?.Function.return_type.resolved_type.?.ObjectInstance.resolved_type.?.Object;

    const map_def = ObjMap.MapDef.init(
        ctx.vm.gc.allocator,
        mapped_type.fields.get("key").?,
        mapped_type.fields.get("value").?,
    );

    const resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
        .Map = map_def,
    };

    var new_map: *ObjMap = ctx.vm.gc.allocateObject(
        ObjMap,
        ObjMap.init(
            ctx.vm.gc.allocator,
            ctx.vm.gc.type_registry.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Map,
                    .resolved_type = resolved_type,
                },
            ) catch unreachable,
        ),
    ) catch unreachable; // TODO: handle error

    const key_str = ctx.vm.gc.copyString("key") catch unreachable;
    const value_str = ctx.vm.gc.copyString("value") catch unreachable;
    var it = self.map.iterator();
    while (it.next()) |kv| {
        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&.{
                kv.key_ptr.*,
                kv.value_ptr.*,
            }),
            2,
            null,
        );

        const entry = ObjObjectInstance.cast(ctx.vm.pop().obj()).?;
        const key = entry.fields.get(key_str).?;
        const value = entry.fields.get(value_str).?;

        new_map.set(ctx.vm.gc, key, value) catch unreachable;
    }

    ctx.vm.push(new_map.toValue());

    return 1;
}

const SortContext = struct {
    sort_closure: *ObjClosure,
    ctx: *NativeCtx,
    map: *ObjMap,

    pub fn lessThan(context: SortContext, lhs_index: usize, rhs_index: usize) bool {
        const map_keys = context.map.map.keys();
        const lhs = map_keys[lhs_index];
        const rhs = map_keys[rhs_index];

        buzz_api.bz_call(
            context.ctx.vm,
            context.sort_closure,
            @ptrCast(&.{ lhs, rhs }),
            2,
            null,
        );

        return context.ctx.vm.pop().boolean();
    }
};

pub fn sort(ctx: *NativeCtx) c_int {
    const self: *ObjMap = ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const sort_closure = ObjClosure.cast(ctx.vm.peek(0).obj()).?;

    self.map.sort(
        SortContext{
            .sort_closure = sort_closure,
            .ctx = ctx,
            .map = self,
        },
    );

    ctx.vm.push(self.toValue());

    return 1;
}

pub fn diff(ctx: *NativeCtx) c_int {
    const lhs: *ObjMap = ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const rhs: *ObjMap = ObjMap.cast(ctx.vm.peek(0).obj()).?;

    var new_map: *ObjMap = ctx.vm.gc.allocateObject(
        ObjMap,
        ObjMap.init(
            ctx.vm.gc.allocator,
            lhs.type_def,
        ),
    ) catch unreachable;

    var it = lhs.map.iterator();
    while (it.next()) |kv| {
        // If not present in rhs map, add it to the new map
        if (rhs.map.get(kv.key_ptr.*) == null) {
            new_map.set(
                ctx.vm.gc,
                kv.key_ptr.*,
                kv.value_ptr.*,
            ) catch unreachable;
        }
    }

    ctx.vm.push(new_map.toValue());

    return 1;
}

pub fn intersect(ctx: *NativeCtx) c_int {
    const lhs: *ObjMap = ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const rhs: *ObjMap = ObjMap.cast(ctx.vm.peek(0).obj()).?;

    var new_map: *ObjMap = ctx.vm.gc.allocateObject(
        ObjMap,
        ObjMap.init(
            ctx.vm.gc.allocator,
            lhs.type_def,
        ),
    ) catch unreachable;

    var it = lhs.map.iterator();
    while (it.next()) |kv| {
        // If not present in rhs map, add it to the new map
        if (rhs.map.get(kv.key_ptr.*) != null) {
            new_map.set(
                ctx.vm.gc,
                kv.key_ptr.*,
                kv.value_ptr.*,
            ) catch unreachable;
        }
    }

    ctx.vm.push(new_map.toValue());

    return 1;
}

pub fn size(ctx: *NativeCtx) c_int {
    const self: *ObjMap = ObjMap.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromInteger(@as(i32, @intCast(self.map.count()))));

    return 1;
}

pub fn remove(ctx: *NativeCtx) c_int {
    const self: *ObjMap = ObjMap.cast(ctx.vm.peek(1).obj()).?;
    const map_key = ctx.vm.peek(0);

    if (self.map.fetchOrderedRemove(map_key)) |removed| {
        ctx.vm.push(removed.value);
    } else {
        ctx.vm.push(Value.Null);
    }

    return 1;
}

pub fn keys(ctx: *NativeCtx) c_int {
    const self: *ObjMap = ObjMap.cast(ctx.vm.peek(0).obj()).?;

    const map_keys = self.map.keys();
    var result = std.ArrayList(Value).init(ctx.vm.gc.allocator);
    for (map_keys) |key| {
        result.append(key) catch {
            var err: ?*ObjString = ctx.vm.gc.copyString("could not get map keys") catch null;
            ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.False);

            return -1;
        };
    }

    var list_def: ObjList.ListDef = ObjList.ListDef.init(
        ctx.vm.gc.allocator,
        self.type_def.resolved_type.?.Map.key_type,
    );

    var list_def_union: ObjTypeDef.TypeUnion = .{
        .List = list_def,
    };

    var list_def_type: *ObjTypeDef = ctx.vm.gc.type_registry.getTypeDef(ObjTypeDef{
        .def_type = .List,
        .optional = false,
        .resolved_type = list_def_union,
    }) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("could not get map keys") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.False);

        return -1;
    };

    // Prevent collection
    ctx.vm.push(list_def_type.toValue());

    var list = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(ctx.vm.gc.allocator, list_def_type),
    ) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("could not get map keys") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.False);

        return -1;
    };

    list.items.deinit();
    list.items = result;

    _ = ctx.vm.pop();
    ctx.vm.push(list.toValue());

    return 1;
}

pub fn values(ctx: *NativeCtx) c_int {
    const self: *ObjMap = ObjMap.cast(ctx.vm.peek(0).obj()).?;

    const map_values: []Value = self.map.values();
    var result = std.ArrayList(Value).init(ctx.vm.gc.allocator);
    result.appendSlice(map_values) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("could not get map values") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.False);

        return -1;
    };

    var list_def: ObjList.ListDef = ObjList.ListDef.init(
        ctx.vm.gc.allocator,
        self.type_def.resolved_type.?.Map.value_type,
    );

    var list_def_union: ObjTypeDef.TypeUnion = .{
        .List = list_def,
    };

    var list_def_type: *ObjTypeDef = ctx.vm.gc.type_registry.getTypeDef(ObjTypeDef{
        .def_type = .List,
        .optional = false,
        .resolved_type = list_def_union,
    }) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("could not get map values") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.False);

        return -1;
    };

    var list = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(ctx.vm.gc.allocator, list_def_type),
    ) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("could not get map values") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.False);

        return -1;
    };

    list.items.deinit();
    list.items = result;

    ctx.vm.push(list.toValue());

    return 1;
}
