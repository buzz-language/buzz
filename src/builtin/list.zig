const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const std = @import("std");
const _obj = @import("../obj.zig");
const ObjString = _obj.ObjString;
const ObjList = _obj.ObjList;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjClosure = _obj.ObjClosure;
const NativeCtx = _obj.NativeCtx;
const VM = @import("../vm.zig").VM;
const Value = @import("../value.zig").Value;
const buzz_api = @import("../buzz_api.zig");

pub fn append(ctx: *NativeCtx) c_int {
    const list_value: Value = ctx.vm.peek(1);
    const list: *ObjList = ObjList.cast(list_value.obj()).?;
    const value: Value = ctx.vm.peek(0);

    list.rawAppend(ctx.vm.gc, value) catch {
        const messageValue: Value = (ctx.vm.gc.copyString("Could not append to list") catch {
            std.debug.print("Could not append to list", .{});
            if (!is_wasm) {
                std.os.exit(1);
            } else {
                unreachable;
            }
        }).toValue();

        ctx.vm.push(messageValue);
        return -1;
    };

    return 0;
}

pub fn insert(ctx: *NativeCtx) c_int {
    const list_value: Value = ctx.vm.peek(2);
    const list: *ObjList = ObjList.cast(list_value.obj()).?;
    var index = ctx.vm.peek(1).integer();
    const value: Value = ctx.vm.peek(0);

    if (index < 0 or list.items.items.len == 0) {
        index = 0;
    } else if (index >= list.items.items.len) {
        index = @as(i32, @intCast(list.items.items.len)) - 1;
    }

    list.rawInsert(ctx.vm.gc, @as(usize, @intCast(index)), value) catch {
        const messageValue: Value = (ctx.vm.gc.copyString("Could not insert into list") catch {
            std.debug.print("Could not insert into list", .{});
            if (!is_wasm) {
                std.os.exit(1);
            } else {
                unreachable;
            }
        }).toValue();

        ctx.vm.push(messageValue);
        return -1;
    };

    ctx.vm.push(value);

    return 1;
}

pub fn len(ctx: *NativeCtx) c_int {
    const list: *ObjList = ObjList.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromInteger(@as(i32, @intCast(list.items.items.len))));

    return 1;
}

pub fn pop(ctx: *NativeCtx) c_int {
    const list: *ObjList = ObjList.cast(ctx.vm.peek(0).obj()).?;

    if (list.items.items.len > 0) {
        ctx.vm.push(list.items.pop());
    } else {
        ctx.vm.push(Value.Null);
    }

    return 1;
}

pub fn remove(ctx: *NativeCtx) c_int {
    const list: *ObjList = ObjList.cast(ctx.vm.peek(1).obj()).?;
    const list_index = ctx.vm.peek(0).integer();

    if (list_index < 0 or list_index >= list.items.items.len) {
        ctx.vm.push(Value.Null);

        return 1;
    }

    ctx.vm.push(list.items.orderedRemove(@as(usize, @intCast(list_index))));
    ctx.vm.gc.markObjDirty(&list.obj) catch {
        std.debug.print("Could not remove from list", .{});
        if (!is_wasm) {
            std.os.exit(1);
        } else {
            unreachable;
        }
    };

    return 1;
}

const SortContext = struct {
    sort_closure: *ObjClosure,
    ctx: *NativeCtx,
};

fn lessThan(context: SortContext, lhs: Value, rhs: Value) bool {
    var args = [_]*const Value{ &lhs, &rhs };

    buzz_api.bz_call(
        context.ctx.vm,
        context.sort_closure,
        @ptrCast(&args),
        @intCast(args.len),
        null,
    );

    return context.ctx.vm.pop().boolean();
}

pub fn sort(ctx: *NativeCtx) c_int {
    var self = ObjList.cast(ctx.vm.peek(1).obj()).?;
    // fun compare(T lhs, T rhs) > bool
    const sort_closure = ObjClosure.cast(ctx.vm.peek(0).obj()).?;

    std.sort.insertion(
        Value,
        self.items.items,
        SortContext{
            .sort_closure = sort_closure,
            .ctx = ctx,
        },
        lessThan,
    );

    ctx.vm.push(self.toValue());

    return 1;
}

pub fn indexOf(ctx: *NativeCtx) c_int {
    const self: *ObjList = ObjList.cast(ctx.vm.peek(1).obj()).?;
    const needle: Value = ctx.vm.peek(0);

    var index: ?usize = null;
    var i: usize = 0;
    for (self.items.items) |item| {
        if (needle.eql(item)) {
            index = i;
            break;
        }

        i += 1;
    }

    ctx.vm.push(if (index) |uindex| Value.fromInteger(@as(i32, @intCast(uindex))) else Value.Null);

    return 1;
}

pub fn clone(ctx: *NativeCtx) c_int {
    const self: *ObjList = ObjList.cast(ctx.vm.peek(0).obj()).?;

    var new_list = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(ctx.vm.gc.allocator, self.type_def),
    ) catch @panic("Out of memory");
    new_list.items.appendSlice(self.items.items) catch @panic("Out of memory");

    ctx.vm.push(new_list.toValue());

    return 1;
}

pub fn join(ctx: *NativeCtx) c_int {
    const self = ObjList.cast(ctx.vm.peek(1).obj()).?;
    const separator = ObjString.cast(ctx.vm.peek(0).obj()).?;

    var result = std.ArrayList(u8).init(ctx.vm.gc.allocator);
    var writer = result.writer();
    defer result.deinit();
    for (self.items.items, 0..) |item, i| {
        item.toString(&writer) catch {
            const err = ctx.vm.gc.copyString("could not stringify item") catch null;
            ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

            return -1;
        };

        if (i + 1 < self.items.items.len) {
            writer.writeAll(separator.string) catch {
                const err = ctx.vm.gc.copyString("could not join list") catch null;
                ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

                return -1;
            };
        }
    }

    ctx.vm.push((ctx.vm.gc.copyString(result.items) catch {
        const err = ctx.vm.gc.copyString("could not join list") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }).toValue());

    return 1;
}

pub fn sub(ctx: *NativeCtx) c_int {
    const self: *ObjList = ObjList.cast(ctx.vm.peek(2).obj()).?;
    const start = ctx.vm.peek(1).integer();
    const upto = ctx.vm.peek(0).integerOrNull();

    if (start < 0 or start >= self.items.items.len) {
        const err = ctx.vm.gc.copyString("`start` is out of bound") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }

    if (upto != null and upto.? < 0) {
        const err = ctx.vm.gc.copyString("`len` must greater or equal to 0") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }

    const limit: usize = if (upto != null and @as(usize, @intCast(start + upto.?)) < self.items.items.len)
        @as(usize, @intCast(start + upto.?))
    else
        self.items.items.len;
    const substr = self.items.items[@as(usize, @intCast(start))..limit];

    var list = ctx.vm.gc.allocateObject(ObjList, ObjList{
        .type_def = self.type_def,
        .methods = self.methods.clone() catch {
            const err = ctx.vm.gc.copyString("Could not get sub list") catch null;
            ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

            return -1;
        },
        .items = std.ArrayList(Value).init(ctx.vm.gc.allocator),
    }) catch {
        const err = ctx.vm.gc.copyString("Could not get sub list") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    ctx.vm.push(list.toValue());

    list.items.appendSlice(substr) catch {
        const err = ctx.vm.gc.copyString("Could not get sub list") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    return 1;
}

pub fn next(ctx: *NativeCtx) c_int {
    const list_value: Value = ctx.vm.peek(1);
    const list: *ObjList = ObjList.cast(list_value.obj()).?;
    const list_index: Value = ctx.vm.peek(0);

    const next_index: ?i32 = list.rawNext(ctx.vm, list_index.integerOrNull()) catch @panic("Out of memory");

    ctx.vm.push(if (next_index) |unext_index| Value.fromInteger(unext_index) else Value.Null);

    return 1;
}

pub fn forEach(ctx: *NativeCtx) c_int {
    const list = ObjList.cast(ctx.vm.peek(1).obj()).?;
    const closure = ObjClosure.cast(ctx.vm.peek(0).obj()).?;

    for (list.items.items, 0..) |item, index| {
        const index_value = Value.fromInteger(@as(i32, @intCast(index)));

        var args = [_]*const Value{ &index_value, &item };

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

pub fn reduce(ctx: *NativeCtx) c_int {
    const list = ObjList.cast(ctx.vm.peek(2).obj()).?;
    const closure = ObjClosure.cast(ctx.vm.peek(1).obj()).?;
    var accumulator = ctx.vm.peek(0);

    for (list.items.items, 0..) |item, index| {
        const index_value = Value.fromInteger(@as(i32, @intCast(index)));

        var args = [_]*const Value{ &index_value, &item, &accumulator };

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

pub fn filter(ctx: *NativeCtx) c_int {
    const list = ObjList.cast(ctx.vm.peek(1).obj()).?;
    const closure = ObjClosure.cast(ctx.vm.peek(0).obj()).?;

    var new_list: *ObjList = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(
            ctx.vm.gc.allocator,
            list.type_def,
        ),
    ) catch unreachable;

    for (list.items.items, 0..) |item, index| {
        const index_value = Value.fromInteger(@as(i32, @intCast(index)));
        var args = [_]*const Value{ &index_value, &item };

        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        );

        if (ctx.vm.pop().boolean()) {
            new_list.rawAppend(ctx.vm.gc, item) catch unreachable;
        }
    }

    ctx.vm.push(new_list.toValue());

    return 1;
}

pub fn map(ctx: *NativeCtx) c_int {
    const list = ObjList.cast(ctx.vm.peek(1).obj()).?;
    const closure = ObjClosure.cast(ctx.vm.peek(0).obj()).?;

    const mapped_type = closure.function.type_def.resolved_type.?.Function.return_type;
    var new_list: *ObjList = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(
            ctx.vm.gc.allocator,
            mapped_type,
        ),
    ) catch unreachable; // TODO: handle error

    for (list.items.items, 0..) |item, index| {
        const index_value = Value.fromInteger(@as(i32, @intCast(index)));
        var args = [_]*const Value{ &index_value, &item };

        buzz_api.bz_call(
            ctx.vm,
            closure,
            @ptrCast(&args),
            @intCast(args.len),
            null,
        );

        new_list.rawAppend(ctx.vm.gc, ctx.vm.pop()) catch unreachable;
    }

    ctx.vm.push(new_list.toValue());

    return 1;
}
