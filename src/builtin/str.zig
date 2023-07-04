const std = @import("std");
const _obj = @import("../obj.zig");
const ObjString = _obj.ObjString;
const ObjList = _obj.ObjList;
const ObjTypeDef = _obj.ObjTypeDef;
const NativeCtx = _obj.NativeCtx;
const VM = @import("../vm.zig").VM;
const _value = @import("../value.zig");
const Value = _value.Value;
const floatToInteger = _value.floatToInteger;

pub fn trim(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    var trimmed = std.mem.trim(u8, str.string, " ");
    trimmed = std.mem.trim(u8, trimmed, "\t");
    trimmed = std.mem.trim(u8, trimmed, "\r");
    trimmed = std.mem.trim(u8, trimmed, "\n");

    ctx.vm.push((ctx.vm.gc.copyString(trimmed) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not trim string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }).toValue());

    return 1;
}

pub fn len(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromInteger(@as(i32, @intCast(str.string.len))));

    return 1;
}

pub fn repeat(ctx: *NativeCtx) c_int {
    const str = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const n = floatToInteger(ctx.vm.peek(0));
    const n_i = if (n.isInteger()) n.integer() else null;

    if (n_i) |ni| {
        var new_string: std.ArrayList(u8) = std.ArrayList(u8).init(ctx.vm.gc.allocator);
        var i: usize = 0;
        while (i < ni) : (i += 1) {
            new_string.appendSlice(str.string) catch {
                var err: ?*ObjString = ctx.vm.gc.copyString("Could not repeat string") catch null;
                ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

                return -1;
            };
        }

        const new_objstring = ctx.vm.gc.copyString(new_string.items) catch {
            var err: ?*ObjString = ctx.vm.gc.copyString("Could not repeat string") catch null;
            ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

            return -1;
        };

        ctx.vm.push(new_objstring.toValue());

        return 1;
    }

    var err: ?*ObjString = ctx.vm.gc.copyString("`n` should be an integer") catch null;
    ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

    return -1;
}

pub fn byte(ctx: *NativeCtx) c_int {
    const self: *ObjString = ObjString.cast(ctx.vm.peek(1).obj()).?;
    const index = floatToInteger(ctx.vm.peek(0));
    const index_i = if (index.isInteger()) index.integer() else null;

    if (index_i == null or index_i.? < 0 or index_i.? >= self.string.len) {
        var err: ?*ObjString = ctx.vm.gc.copyString("Out of bound access to str") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }

    ctx.vm.push(Value.fromInteger(@intCast(self.string[@as(usize, @intCast(index_i.?))])));

    return 1;
}

pub fn indexOf(ctx: *NativeCtx) c_int {
    var self: *ObjString = ObjString.cast(ctx.vm.peek(1).obj()).?;
    var needle: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    var index = std.mem.indexOf(u8, self.string, needle.string);

    ctx.vm.push(if (index) |uindex| Value.fromInteger(@intCast(uindex)) else Value.Null);

    return 1;
}

pub fn startsWith(ctx: *NativeCtx) c_int {
    var self: *ObjString = ObjString.cast(ctx.vm.peek(1).obj()).?;
    var needle: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromBoolean(std.mem.startsWith(u8, self.string, needle.string)));

    return 1;
}

pub fn endsWith(ctx: *NativeCtx) c_int {
    var self: *ObjString = ObjString.cast(ctx.vm.peek(1).obj()).?;
    var needle: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    ctx.vm.push(Value.fromBoolean(std.mem.endsWith(u8, self.string, needle.string)));

    return 1;
}

pub fn replace(ctx: *NativeCtx) c_int {
    var self: *ObjString = ObjString.cast(ctx.vm.peek(2).obj()).?;
    var needle: *ObjString = ObjString.cast(ctx.vm.peek(1).obj()).?;
    var replacement: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const new_string = std.mem.replaceOwned(u8, ctx.vm.gc.allocator, self.string, needle.string, replacement.string) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not replace string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    ctx.vm.push(
        (ctx.vm.gc.copyString(new_string) catch {
            var err: ?*ObjString = ctx.vm.gc.copyString("Could not replace string") catch null;
            ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

            return -1;
        }).toValue(),
    );

    return 1;
}

pub fn sub(ctx: *NativeCtx) c_int {
    var self: *ObjString = ObjString.cast(ctx.vm.peek(2).obj()).?;
    var start_value = floatToInteger(ctx.vm.peek(1));
    var start: ?i32 = if (start_value.isInteger()) start_value.integer() else null;
    var upto_value: Value = floatToInteger(ctx.vm.peek(0));
    var upto: ?i32 = if (upto_value.isInteger())
        upto_value.integer()
    else if (upto_value.isFloat())
        @intFromFloat(upto_value.float())
    else
        null;

    if (start == null or start.? < 0 or start.? >= self.string.len) {
        var err: ?*ObjString = ctx.vm.gc.copyString("`start` is out of bound") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }

    if (upto != null and upto.? < 0) {
        var err: ?*ObjString = ctx.vm.gc.copyString("`len` must greater or equal to 0") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    }

    const limit: usize = if (upto != null and @as(usize, @intCast(start.? + upto.?)) < self.string.len) @intCast(start.? + upto.?) else self.string.len;
    var substr: []const u8 = self.string[@as(usize, @intCast(start.?))..limit];

    ctx.vm.push(
        (ctx.vm.gc.copyString(substr) catch {
            var err: ?*ObjString = ctx.vm.gc.copyString("Could not get sub string") catch null;
            ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

            return -1;
        }).toValue(),
    );

    return 1;
}

pub fn split(ctx: *NativeCtx) c_int {
    var self: *ObjString = ObjString.cast(ctx.vm.peek(1).obj()).?;
    var separator: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    // std.mem.split(u8, self.string, separator.string);
    var list_def: ObjList.ListDef = ObjList.ListDef.init(
        ctx.vm.gc.allocator,
        ctx.vm.gc.type_registry.getTypeDef(ObjTypeDef{
            .def_type = .String,
        }) catch {
            var err: ?*ObjString = ctx.vm.gc.copyString("Could not split string") catch null;
            ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

            return -1;
        },
    );

    var list_def_union: ObjTypeDef.TypeUnion = .{
        .List = list_def,
    };

    // TODO: reuse already allocated similar typedef
    var list_def_type: *ObjTypeDef = ctx.vm.gc.type_registry.getTypeDef(ObjTypeDef{
        .def_type = .List,
        .optional = false,
        .resolved_type = list_def_union,
    }) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not split string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    var list: *ObjList = ctx.vm.gc.allocateObject(
        ObjList,
        ObjList.init(ctx.vm.gc.allocator, list_def_type),
    ) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not split string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    // Prevent gc & is result
    ctx.vm.push(list.toValue());

    if (separator.string.len > 0) {
        var it = std.mem.split(u8, self.string, separator.string);
        while (it.next()) |fragment| {
            var fragment_str: ?*ObjString = ctx.vm.gc.copyString(fragment) catch {
                var err: ?*ObjString = ctx.vm.gc.copyString("Could not split string") catch null;
                ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

                return -1;
            };

            list.rawAppend(ctx.vm.gc, fragment_str.?.toValue()) catch {
                var err: ?*ObjString = ctx.vm.gc.copyString("Could not split string") catch null;
                ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

                return -1;
            };
        }
    } else {
        for (self.string) |char| {
            var fragment_str: ?*ObjString = ctx.vm.gc.copyString(&([_]u8{char})) catch {
                var err: ?*ObjString = ctx.vm.gc.copyString("Could not split string") catch null;
                ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

                return -1;
            };

            list.rawAppend(ctx.vm.gc, fragment_str.?.toValue()) catch {
                var err: ?*ObjString = ctx.vm.gc.copyString("Could not split string") catch null;
                ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

                return -1;
            };
        }
    }

    return 1;
}

pub fn encodeBase64(ctx: *NativeCtx) c_int {
    var str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    var encoded = ctx.vm.gc.allocator.alloc(u8, std.base64.standard.Encoder.calcSize(str.string.len)) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not encode string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };
    defer ctx.vm.gc.allocator.free(encoded);

    var new_string = ctx.vm.gc.copyString(
        std.base64.standard.Encoder.encode(encoded, str.string),
    ) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not encode string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    ctx.vm.push(new_string.toValue());

    return 1;
}

pub fn decodeBase64(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    const size = std.base64.standard.Decoder.calcSizeForSlice(str.string) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not decode string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };
    var decoded = ctx.vm.gc.allocator.alloc(u8, size) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not decode string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };
    defer ctx.vm.gc.allocator.free(decoded);

    std.base64.standard.Decoder.decode(decoded, str.string) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not decode string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    var new_string = ctx.vm.gc.copyString(decoded) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not decode string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    ctx.vm.push(new_string.toValue());

    return 1;
}

pub fn upper(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var new_str = ctx.vm.gc.allocator.alloc(u8, str.string.len) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not get uppercased string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };
    defer ctx.vm.gc.allocator.free(new_str);

    for (str.string, 0..) |char, index| {
        switch (char) {
            'a'...'z' => new_str[index] = std.ascii.toUpper(char),
            else => new_str[index] = char,
        }
    }

    var obj_string = ctx.vm.gc.copyString(new_str) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not get uppercased string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    ctx.vm.push(obj_string.toValue());

    return 1;
}

pub fn lower(ctx: *NativeCtx) c_int {
    const str: *ObjString = ObjString.cast(ctx.vm.peek(0).obj()).?;

    if (str.string.len == 0) {
        ctx.vm.push(str.toValue());

        return 1;
    }

    var new_str = ctx.vm.gc.allocator.alloc(u8, str.string.len) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not get uppercased string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };
    defer ctx.vm.gc.allocator.free(new_str);

    for (str.string, 0..) |char, index| {
        switch (char) {
            'A'...'Z' => new_str[index] = std.ascii.toLower(char),
            else => new_str[index] = char,
        }
    }

    var obj_string = ctx.vm.gc.copyString(new_str) catch {
        var err: ?*ObjString = ctx.vm.gc.copyString("Could not get uppercased string") catch null;
        ctx.vm.push(if (err) |uerr| uerr.toValue() else Value.fromBoolean(false));

        return -1;
    };

    ctx.vm.push(obj_string.toValue());

    return 1;
}
