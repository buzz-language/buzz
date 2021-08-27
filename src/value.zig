const std = @import("std");
const StringHashMap = std.StringHashMap;
const Obj = @import("./obj.zig").Obj;

pub const ValueType = enum {
    Boolean,
    Number,
    Byte,
    Null,
    Obj
};

pub const Value = union(ValueType) {
    Boolean: bool,
    Number: f64,
    Byte: u8,
    Null: ?u8,
    Obj: *Obj
};

pub const HashableValue = union(ValueType) {
    Boolean: bool,
    Number: i64,
    Byte: u8,
    Null: ?u8,
    Obj: *Obj
};

pub fn valueToHashable(value: Value) HashableValue {
    switch (value) {
        .Boolean => return HashableValue { .Boolean = value.Boolean },
        .Number => {
            return HashableValue { .Number = @floatToInt(i64, value.Number) };
        },
        .Byte => return HashableValue { .Byte = value.Byte },
        .Null => return HashableValue { .Null = value.Null },
        .Obj => return HashableValue { .Obj = value.Obj },
    }
}