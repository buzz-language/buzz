const std = @import("std");
const _obj = @import("../obj.zig");
const ObjString = _obj.ObjString;
const ObjList = _obj.ObjList;
const ObjTypeDef = _obj.ObjTypeDef;
const NativeCtx = _obj.NativeCtx;
const VM = @import("../vm.zig").VM;
const _value = @import("../value.zig");
const Value = _value.Value;

pub fn trim(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    var trimmed = std.mem.trim(u8, str.string, " ");
    trimmed = std.mem.trim(u8, trimmed, "\t");
    trimmed = std.mem.trim(u8, trimmed, "\r");
    trimmed = std.mem.trim(u8, trimmed, "\n");

    ctx.vm.push((ctx.vm.gc.copyString(trimmed) catch @panic("Could not create string")).toValue());

    return 1;
}

pub fn len(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromInteger(@intCast(str.string.len)));

    return 1;
}

pub fn utf8Len(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(
        Value.fromInteger(
            @intCast(std.unicode.utf8CountCodepoints(str.string) catch 0),
        ),
    );

    return 1;
}

pub fn utf8Valid(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(
        Value.fromBoolean(
            std.unicode.utf8ValidateSlice(str.string),
        ),
    );

    return 1;
}

pub fn utf8Codepoints(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    var list_def: ObjList.ListDef = ObjList.ListDef.init(
        ctx.vm.gc.allocator,
        ctx.vm.gc.type_registry.getTypeDef(.{ .def_type = .String }) catch @panic("Could not create list"),
    );

    var list_def_union: ObjTypeDef.TypeUnion = .{
        .List = list_def,
    };

    var list_def_type: *ObjTypeDef = ctx.vm.gc.type_registry.getTypeDef(ObjTypeDef{
        .def_type = .List,
        .optional = false,
        .resolved_type = list_def_union,
    }) catch @panic("Could not create list");

    var list = (ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(ctx.vm.gc.allocator, list_def_type),
    ) catch @panic("Could not create list"));

    if (std.unicode.utf8ValidateSlice(str.string)) {
        const view = std.unicode.Utf8View.init(str.string) catch unreachable;
        var it = view.iterator();
        while (it.nextCodepointSlice()) |codepoint| {
            const codepoint_str = ctx.vm.gc.copyString(codepoint) catch @panic("Could not get codepoints");

            list.rawAppend(ctx.vm.gc, codepoint_str.toValue()) catch @panic("Could not get codepoints");
        }
    }

    ctx.vm.push(list.toValue());

    return 1;
}

pub fn repeat(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const n = ctx.vm.peek(0);
    const n_i = if (n.isInteger()) n.integer() else null;

    if (n_i) |ni| {
        var new_string: std.ArrayList(u8) = std.ArrayList(u8).init(ctx.vm.gc.allocator);
        var i: usize = 0;
        while (i < ni) : (i += 1) {
            new_string.appendSlice(str.string) catch @panic("Could not create string");
        }

        const new_objstring = ctx.vm.gc.copyString(new_string.items) catch @panic("Could not create string");

        ctx.vm.push(new_objstring.toValue());

        return 1;
    }

    var err: ?*ObjString = ctx.vm.gc.copyString("`n` should be an integer") catch null;
    ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

    return -1;
}

pub fn byte(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const index = ctx.vm.peek(0);
    const index_i = index.integer();

    if (index_i < 0 or index_i >= self.string.len) {
        var err: ?*ObjString = ctx.vm.gc.copyString("Out of bound access to str") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }

    ctx.vm.push(Value.fromInteger(@intCast(self.string[@as(usize, @intCast(index_i))])));

    return 1;
}

pub fn indexOf(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const needle: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const index = std.mem.indexOf(u8, self.string, needle.string);

    ctx.vm.push(if (index) |uindex| Value.fromInteger(@intCast(uindex)) else Value.Null);

    return 1;
}

pub fn startsWith(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const needle: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromBoolean(std.mem.startsWith(u8, self.string, needle.string)));

    return 1;
}

pub fn endsWith(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const needle: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromBoolean(std.mem.endsWith(u8, self.string, needle.string)));

    return 1;
}

pub fn replace(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(2).obj()).?;
    const needle: *ObjString = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const replacement: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const new_string = std.mem.replaceOwned(
        u8,
        ctx.vm.gc.allocator,
        self.string,
        needle.string,
        replacement.string,
    ) catch @panic("Could not create string");

    ctx.vm.push(
        (ctx.vm.gc.copyString(new_string) catch @panic("Could not create string")).toValue(),
    );

    return 1;
}

pub fn sub(ctx: *NativeCtx) c_int {
    const self = ObjString.cast(ctx.vm.peek(2).obj()).?;
    const start = ctx.vm.peek(1).integer();
    const upto = ctx.vm.peek(0).integerOrNull();

    if (start < 0 or start >= self.string.len) {
        var err: ?*ObjString = ctx.vm.gc.copyString("`start` is out of bound") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }

    if (upto != null and upto.? < 0) {
        var err: ?*ObjString = ctx.vm.gc.copyString("`len` must greater or equal to 0") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }

    const limit: usize = if (upto != null and @as(usize, @intCast(start + upto.?)) < self.string.len) @intCast(start + upto.?) else self.string.len;
    var substr: []const u8 = self.string[@as(usize, @intCast(start))..limit];

    ctx.vm.push(
        (ctx.vm.gc.copyString(substr) catch @panic("Could not create string")).toValue(),
    );

    return 1;
}

pub fn split(ctx: *NativeCtx) c_int {
    const self: *ObjString = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const separator: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    // std.mem.split(u8, self.string, separator.string);
    var list_def: ObjList.ListDef = ObjList.ListDef.init(
        ctx.vm.gc.allocator,
        ctx.vm.gc.type_registry.getTypeDef(ObjTypeDef{
            .def_type = .String,
        }) catch @panic("Could not create string"),
    );

    var list_def_union: ObjTypeDef.TypeUnion = .{
        .List = list_def,
    };

    // TODO: reuse already allocated similar typedef
    var list_def_type: *ObjTypeDef = ctx.vm.gc.type_registry.getTypeDef(ObjTypeDef{
        .def_type = .List,
        .optional = false,
        .resolved_type = list_def_union,
    }) catch @panic("Could not create string");

    var list: *ObjList = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(ctx.vm.gc.allocator, list_def_type),
    ) catch @panic("Could not create string");

    // Prevent gc & is result
    ctx.vm.push(list.toValue());

    if (separator.string.len > 0) {
        var it = std.mem.splitSequence(u8, self.string, separator.string);
        while (it.next()) |fragment| {
            const fragment_str = ctx.vm.gc.copyString(fragment) catch @panic("Could not create string");

            list.rawAppend(ctx.vm.gc, fragment_str.toValue()) catch @panic("Could not create string");
        }
    } else {
        for (self.string) |char| {
            const fragment_str = ctx.vm.gc.copyString(&([_]u8{char})) catch @panic("Could not create string");

            list.rawAppend(ctx.vm.gc, fragment_str.toValue()) catch @panic("Could not create string");
        }
    }

    return 1;
}

pub fn encodeBase64(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    var encoded = ctx.vm.gc.allocator.alloc(u8, std.base64.standard.Encoder.calcSize(str.string.len)) catch @panic("Could not create string");
    defer ctx.vm.gc.allocator.free(encoded);

    var new_string = ctx.vm.gc.copyString(
        std.base64.standard.Encoder.encode(encoded, str.string),
    ) catch @panic("Could not create string");

    ctx.vm.push(new_string.toValue());

    return 1;
}

pub fn decodeBase64(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const size = std.base64.standard.Decoder.calcSizeForSlice(str.string) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not decode string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };
    var decoded = ctx.vm.gc.allocator.alloc(u8, size) catch @panic("Could not create string");
    defer ctx.vm.gc.allocator.free(decoded);

    std.base64.standard.Decoder.decode(decoded, str.string) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not decode string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    var new_string = ctx.vm.gc.copyString(decoded) catch @panic("Could not create string");

    ctx.vm.push(new_string.toValue());

    return 1;
}

pub fn upper(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var new_str = ctx.vm.gc.allocator.alloc(u8, str.string.len) catch @panic("Could not create string");
    defer ctx.vm.gc.allocator.free(new_str);

    for (str.string, 0..) |char, index| {
        switch (char) {
            'a'...'z' => new_str[index] = std.ascii.toUpper(char),
            else => new_str[index] = char,
        }
    }

    var obj_string = ctx.vm.gc.copyString(new_str) catch @panic("Could not create string");

    ctx.vm.push(obj_string.toValue());

    return 1;
}

pub fn lower(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var new_str = ctx.vm.gc.allocator.alloc(u8, str.string.len) catch @panic("Could not create string");
    defer ctx.vm.gc.allocator.free(new_str);

    for (str.string, 0..) |char, index| {
        switch (char) {
            'A'...'Z' => new_str[index] = std.ascii.toLower(char),
            else => new_str[index] = char,
        }
    }

    var obj_string = ctx.vm.gc.copyString(new_str) catch @panic("Could not create string");

    ctx.vm.push(obj_string.toValue());

    return 1;
}

pub fn hex(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var result = std.ArrayList(u8).init(ctx.vm.gc.allocator);
    defer result.deinit();
    var writer = result.writer();

    for (str.string) |char| {
        writer.print("{x:0>2}", .{char}) catch @panic("Could not create string");
    }

    var obj_string = ctx.vm.gc.copyString(result.items) catch @panic("Could not create string");

    ctx.vm.push(obj_string.toValue());

    return 1;
}

pub fn bin(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var result = ctx.vm.gc.allocator.alloc(u8, str.string.len / 2) catch @panic("Could not create string");
    defer ctx.vm.gc.allocator.free(result);

    for (0..result.len) |i| {
        result[i] = std.fmt.parseInt(u8, str.string[(i * 2)..(i * 2 + 2)], 16) catch {
            var err: ?*ObjString = ctx.vm.gc.copyString("String does not contain valid hex values") catch null;
            ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

            return -1;
        };
    }

    var obj_string = ctx.vm.gc.copyString(result) catch @panic("Could not create string");

    ctx.vm.push(obj_string.toValue());

    return 1;
}
