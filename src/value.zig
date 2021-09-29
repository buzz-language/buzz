const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const _obj = @import("./obj.zig");
const Obj = _obj.Obj;
const objToString = _obj.objToString;

pub const ValueType = enum {
    Boolean,
    Number,
    Null,
    Obj
};

pub const Value = union(ValueType) {
    Boolean: bool,
    Number: f64,
    Null: ?bool,
    Obj: *Obj,
};

// We can't hash f64
pub const HashableValue = union(ValueType) {
    Boolean: bool,
    Number: i64,
    Null: ?bool,
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
        .Obj => return Value { .Obj = hashable.Obj },
    }
}

pub fn valueToString(allocator: *Allocator, value: Value) (Allocator.Error || std.fmt.BufPrintError)![]const u8 {
    var buf: []u8 = try allocator.alloc(u8, 1000);

    return switch (value) {
        .Boolean => try std.fmt.bufPrint(buf, "{}", .{ value.Boolean }),
        .Number => try std.fmt.bufPrint(buf, "{d}", .{ value.Number }),
        .Null => try std.fmt.bufPrint(buf, "null", .{}),

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
        .Obj => a.Obj.eql(b.Obj),
    };
}