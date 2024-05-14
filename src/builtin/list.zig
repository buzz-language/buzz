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

    list.rawAppend(
        ctx.vm.gc,
        value,
    ) catch @panic("Out of memory");

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

    list.rawInsert(
        ctx.vm.gc,
        @as(usize, @intCast(index)),
        value,
    ) catch @panic("Out of memory");

    ctx.vm.push(value);

    return 1;
}

pub fn len(ctx: *NativeCtx) c_int {
    const list: *ObjList = ObjList.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromInteger(@as(i32, @intCast(list.items.items.len))));

    return 1;
}

pub fn reverse(ctx: *NativeCtx) c_int {
    const list: *ObjList = ObjList.cast(ctx.vm.peek(0).obj()).?;

    var new_list = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(ctx.vm.gc.allocator, list.type_def),
    ) catch @panic("Out of memory");

    new_list.items.appendSlice(list.items.items) catch @panic("Out of memory");
    std.mem.reverse(Value, new_list.items.items);

    ctx.vm.push(new_list.toValue());

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
    ctx.vm.gc.markObjDirty(&list.obj) catch @panic("Out of memory");

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

    ctx.vm.push(if (index) |uindex|
        Value.fromInteger(@as(i32, @intCast(uindex)))
    else
        Value.Null);

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
        item.toString(&writer) catch @panic("Out of memory");

        if (i + 1 < self.items.items.len) {
            writer.writeAll(separator.string) catch @panic("Out of memory");
        }
    }

    ctx.vm.push(
        (ctx.vm.gc.copyString(result.items) catch @panic("Out of memory")).toValue(),
    );

    return 1;
}

pub fn sub(ctx: *NativeCtx) c_int {
    const self: *ObjList = ObjList.cast(ctx.vm.peek(2).obj()).?;
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

    var list = ctx.vm.gc.allocateObject(
        ObjList,
        .{
            .type_def = self.type_def,
            .methods = self.methods.clone() catch @panic("Out of memory"),
            .items = std.ArrayList(Value).init(ctx.vm.gc.allocator),
        },
    ) catch @panic("Out of memory");

    ctx.vm.push(list.toValue());

    list.items.appendSlice(substr) catch @panic("Out of memory");

    return 1;
}

pub fn next(ctx: *NativeCtx) c_int {
    const list_value: Value = ctx.vm.peek(1);
    const list: *ObjList = ObjList.cast(list_value.obj()).?;
    const list_index: Value = ctx.vm.peek(0);

    const next_index: ?i32 = list.rawNext(
        ctx.vm,
        list_index.integerOrNull(),
    ) catch @panic("Out of memory");

    ctx.vm.push(
        if (next_index) |unext_index|
            Value.fromInteger(unext_index)
        else
            Value.Null,
    );

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

        var args = [_]*const Value{
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

pub fn filter(ctx: *NativeCtx) c_int {
    const list = ObjList.cast(ctx.vm.peek(1).obj()).?;
    const closure = ObjClosure.cast(ctx.vm.peek(0).obj()).?;

    var new_list: *ObjList = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(
            ctx.vm.gc.allocator,
            list.type_def,
        ),
    ) catch @panic("Out of memory");

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
            new_list.rawAppend(ctx.vm.gc, item) catch @panic("Out of memory");
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
    ) catch @panic("Out of memory");

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

        new_list.rawAppend(ctx.vm.gc, ctx.vm.pop()) catch @panic("Out of memory");
    }

    ctx.vm.push(new_list.toValue());

    return 1;
}

pub fn fill(ctx: *NativeCtx) c_int {
    const self: *ObjList = ObjList.cast(ctx.vm.peek(3).obj()).?;
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
