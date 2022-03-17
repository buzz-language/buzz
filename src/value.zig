const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const _obj = @import("./obj.zig");
const Obj = _obj.Obj;
const objToString = _obj.objToString;
const ObjTypeDef = _obj.ObjTypeDef;

pub const ValueType = enum {
    Boolean,
    Number,
    Null,
    Void,
    Obj
};

pub const Value = union(ValueType) {
    Boolean: bool,
    Number: f64,
    Null: ?bool,
    Void: ?bool,
    Obj: *Obj,
};

// We can't hash f64
pub const HashableValue = union(ValueType) {
    Boolean: bool,
    Number: i64,
    Null: ?bool,
    Void: ?bool,
    Obj: *Obj
};

pub fn valueToHashable(value: Value) HashableValue {
    switch (value) {
        .Boolean => return HashableValue { .Boolean = value.Boolean },
        .Number => {
            const number: f64 = value.Number;
            if (number - @intToFloat(f64, @floatToInt(i64, number)) == 0) {
                return HashableValue { .Number = @floatToInt(i64, value.Number) };
            } else {
                // TODO: something like: https://github.com/lua/lua/blob/master/ltable.c#L117-L143
                // See: https://github.com/ziglang/zig/pull/6145
                unreachable;
            }            
        },
        .Null => return HashableValue { .Null = value.Null },
        .Void => return HashableValue { .Void = value.Void },
        .Obj => return HashableValue { .Obj = value.Obj },
    }
}

pub fn hashableToValue(hashable: HashableValue) Value {
    switch (hashable) {
        .Boolean => return Value { .Boolean = hashable.Boolean },
        .Number => {
            return Value { .Number = @intToFloat(f64, hashable.Number) };
        },
        .Null => return Value { .Null = hashable.Null },
        .Void => return Value { .Void = hashable.Void },
        .Obj => return Value { .Obj = hashable.Obj },
    }
}

pub fn valueToString(allocator: Allocator, value: Value) (Allocator.Error || std.fmt.BufPrintError)![]const u8 {
    var buf: []u8 = try allocator.alloc(u8, 1000);

    return switch (value) {
        .Boolean => try std.fmt.bufPrint(buf, "{}", .{ value.Boolean }),
        .Number => try std.fmt.bufPrint(buf, "{d}", .{ value.Number }),
        .Null => try std.fmt.bufPrint(buf, "null", .{}),
        .Void => try std.fmt.bufPrint(buf, "void", .{}),

        .Obj => try objToString(allocator, buf, value.Obj),
    };
}

pub fn valueEql(a: Value, b: Value) bool {
    if (@as(ValueType, a) != @as(ValueType, b)) {
        return false;
    }

    return switch (a) {
        .Boolean => a.Boolean == b.Boolean,
        .Number => a.Number == b.Number,
        .Null => true,
        .Void => true,
        .Obj => a.Obj.eql(b.Obj),
    };
}

pub fn valueIs(type_def_val: Value, value: Value) bool {
    const type_def: *ObjTypeDef = ObjTypeDef.cast(type_def_val.Obj).?;

    return switch (value) {
        .Boolean => type_def.def_type == .Bool,
        .Number => type_def.def_type == .Number,
        // TODO: this one is ambiguous at runtime, is it the `null` constant? or an optional local with a null value?
        .Null => type_def.def_type == .Void or type_def.optional,
        .Void => type_def.def_type == .Void,
        .Obj => value.Obj.is(type_def),
    };
}

pub fn valueTypeEql(self: Value, type_def: *ObjTypeDef) bool {
    return switch (self) {
        .Boolean => type_def.def_type == .Bool,
        .Number => type_def.def_type == .Number,
        // TODO: this one is ambiguous at runtime, is it the `null` constant? or an optional local with a null value?
        .Null => type_def.def_type == .Void or type_def.optional,
        .Void => type_def.def_type == .Void,
        .Obj => self.Obj.typeEql(type_def)
    };
}