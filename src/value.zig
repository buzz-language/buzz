const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const _obj = @import("./obj.zig");
const Obj = _obj.Obj;
const objToString = _obj.objToString;
const copyObj = _obj.copyObj;
const ObjTypeDef = _obj.ObjTypeDef;

pub const ValueType = enum { Boolean, Float, Integer, Null, Void, Obj };

pub const Value = union(ValueType) {
    Boolean: bool,
    Float: f64,
    Integer: i64,
    Null: ?bool,
    Void: ?bool,
    Obj: *Obj,
};

// We can't hash f64
pub const HashableValue = union(ValueType) {
    Boolean: bool,
    Float: i64,
    Integer: i64,
    Null: ?bool,
    Void: ?bool,
    Obj: *Obj,
};

// If nothing in its decimal part, will return a Value.Integer
pub inline fn floatToInteger(value: Value) Value {
    if (value == .Float and value.Float - @intToFloat(f64, @floatToInt(i64, value.Float)) == 0) {
        return Value{ .Integer = @floatToInt(i64, value.Float) };
    }

    return value;
}

pub fn valueToHashable(value: Value) HashableValue {
    switch (value) {
        .Boolean => return HashableValue{ .Boolean = value.Boolean },
        .Integer => return HashableValue{ .Integer = value.Integer },
        .Float => {
            const number: f64 = value.Float;
            if (number - @intToFloat(f64, @floatToInt(i64, number)) == 0) {
                return HashableValue{ .Float = @floatToInt(i64, value.Float) };
            } else {
                // TODO: something like: https://github.com/lua/lua/blob/master/ltable.c#L117-L143
                // See: https://github.com/ziglang/zig/pull/6145
                unreachable;
            }
        },
        .Null => return HashableValue{ .Null = value.Null },
        .Void => return HashableValue{ .Void = value.Void },
        .Obj => return HashableValue{ .Obj = value.Obj },
    }
}

pub fn hashableToValue(hashable: HashableValue) Value {
    switch (hashable) {
        .Boolean => return Value{ .Boolean = hashable.Boolean },
        .Integer => return Value{ .Integer = hashable.Integer },
        .Float => {
            return Value{ .Float = @intToFloat(f64, hashable.Float) };
        },
        .Null => return Value{ .Null = hashable.Null },
        .Void => return Value{ .Void = hashable.Void },
        .Obj => return Value{ .Obj = hashable.Obj },
    }
}

pub fn valueToStringAlloc(allocator: Allocator, value: Value) (Allocator.Error || std.fmt.BufPrintError)![]const u8 {
    var str = std.ArrayList(u8).init(allocator);

    try valueToString(str.writer(), value);

    return str.items;
}

pub fn valueToString(writer: std.ArrayList(u8).Writer, value: Value) (Allocator.Error || std.fmt.BufPrintError)!void {
    switch (value) {
        .Boolean => try writer.print("{}", .{value.Boolean}),
        .Integer => try writer.print("{d}", .{value.Integer}),
        .Float => try writer.print("{d}", .{value.Float}),
        .Null => try writer.print("null", .{}),
        .Void => try writer.print("void", .{}),

        .Obj => try objToString(writer, value.Obj),
    }
}

pub fn valueEql(a: Value, b: Value) bool {
    // zig fmt: off
    if (@as(ValueType, a) != @as(ValueType, b)
        and (
            ((a == .Integer or a == .Float) and b != .Float and b != .Integer)
            or ((b == .Integer or b == .Float) and a != .Float and a != .Integer)
            or (a != .Integer and a != .Float and b != .Integer and b != .Float)
        )
    ) {
        return false;
    }
    // zig fmt: on

    return switch (a) {
        .Boolean => a.Boolean == b.Boolean,
        .Integer, .Float => number: {
            const aa = floatToInteger(a);
            const bb = floatToInteger(b);

            const a_f: ?f64 = if (aa == .Float) aa.Float else null;
            const b_f: ?f64 = if (bb == .Float) bb.Float else null;
            const a_i: ?i64 = if (aa == .Integer) aa.Integer else null;
            const b_i: ?i64 = if (bb == .Integer) bb.Integer else null;

            if (a_f) |af| {
                if (b_f) |bf| {
                    break :number af == bf;
                } else {
                    break :number af == @intToFloat(f64, b_i.?);
                }
            } else {
                if (b_f) |bf| {
                    break :number @intToFloat(f64, a_i.?) == bf;
                } else {
                    break :number a_i.? == b_i.?;
                }
            }
        },
        .Null => true,
        .Void => true,
        .Obj => a.Obj.eql(b.Obj),
    };
}

pub fn valueIs(type_def_val: Value, value: Value) bool {
    const type_def: *ObjTypeDef = ObjTypeDef.cast(type_def_val.Obj).?;

    return switch (value) {
        .Boolean => type_def.def_type == .Bool,
        .Integer => type_def.def_type == .Number,
        .Float => type_def.def_type == .Number,
        // TODO: this one is ambiguous at runtime, is it the `null` constant? or an optional local with a null value?
        .Null => type_def.def_type == .Void or type_def.optional,
        .Void => type_def.def_type == .Void,
        .Obj => value.Obj.is(type_def),
    };
}

pub fn valueTypeEql(self: Value, type_def: *ObjTypeDef) bool {
    return switch (self) {
        .Boolean => type_def.def_type == .Bool,
        .Integer => type_def.def_type == .Number,
        .Float => type_def.def_type == .Number,
        // TODO: this one is ambiguous at runtime, is it the `null` constant? or an optional local with a null value?
        .Null => type_def.def_type == .Void or type_def.optional,
        .Void => type_def.def_type == .Void,
        .Obj => self.Obj.typeEql(type_def),
    };
}
