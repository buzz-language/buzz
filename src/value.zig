const std = @import("std");
const StringHashMap = std.StringHashMap;
const Obj = @import("./obj.zig").Obj;

const ValueType = enum {
    Boolean,
    Number,
    Byte,
    Obj
};

const Value = union(ValueType) {
    Boolean: bool,
    Number: f64,
    Byte: u8,
    Obj: *Obj
};