const std = @import("std");
const _obj = @import("../obj.zig");
const ObjString = _obj.ObjString;
const ObjMap = _obj.ObjMap;
const ObjList = _obj.ObjList;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjClosure = _obj.ObjClosure;
const NativeCtx = _obj.NativeCtx;
const VM = @import("../vm.zig").VM;
const _value = @import("../value.zig");
const buzz_api = @import("../buzz_api.zig");
const Value = _value.Value;
const floatToInteger = _value.floatToInteger;
const valueEql = _value.valueEql;
const valueToString = _value.valueToString;

pub fn size(ctx: *NativeCtx) c_int {
    var map: *ObjMap = ObjMap.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromInteger(@as(i32, @intCast(map.map.count()))));

    return 1;
}

pub fn remove(ctx: *NativeCtx) c_int {
    var map: *ObjMap = ObjMap.cast(ctx.vm.peek(1).obj()).?;
    var map_key = ctx.vm.peek(0);

    if (map.map.fetchOrderedRemove(map_key)) |removed| {
        ctx.vm.push(removed.value);
    } else {
        ctx.vm.push(Value.Null);
    }

    return 1;
}

pub fn keys(ctx: *NativeCtx) c_int {
    var self: *ObjMap = ObjMap.cast(ctx.vm.peek(0).obj()).?;

    var map_keys = self.map.keys();
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
    var self: *ObjMap = ObjMap.cast(ctx.vm.peek(0).obj()).?;

    var map_values: []Value = self.map.values();
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
