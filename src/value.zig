const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const _obj = @import("./obj.zig");
const Obj = _obj.Obj;
const objToString = _obj.objToString;

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
    Null: ?bool,
    Obj: *Obj,
};

pub const HashableValue = union(ValueType) {
    Boolean: bool,
    Number: i64,
    Byte: u8,
    Null: ?bool,
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

pub fn valueToString(allocator: *Allocator, value: Value) anyerror![]const u8 {
    var buf: []u8 = try allocator.alloc(u8, 1000);

    return switch (value) {
        .Boolean => try std.fmt.bufPrint(buf, "{}", .{ value.Boolean }),
        .Number => try std.fmt.bufPrint(buf, "{d}", .{ value.Number }),
        .Byte => try std.fmt.bufPrint(buf, "0x{x}", .{ value.Byte }),
        .Null => try std.fmt.bufPrint(buf, "null", .{}),

        .Obj => try objToString(allocator, buf, value.Obj),
    };
}