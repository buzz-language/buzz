const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const _obj = @import("./obj.zig");
const Obj = _obj.Obj;
const objToString = _obj.objToString;
const copyObj = _obj.copyObj;
const ObjTypeDef = _obj.ObjTypeDef;

// TODO: do it for little endian?

const Tag = u3;
pub const TagBoolean: Tag = 0;
pub const TagInteger: Tag = 1;
pub const TagNull: Tag = 2;
pub const TagVoid: Tag = 3;
pub const TagObj: Tag = 4;

const BooleanT: u32 = TagBoolean;
const IntegerT: u32 = TagInteger;
const NullT: u32 = TagNull;
const VoidT: u32 = TagVoid;
const ObjT: u32 = TagObj;

/// Most significant bit.
const SignMask: u64 = 1 << 63;

/// QNAN and one extra bit to the right.
const TaggedValueMask: u64 = 0x7ffc000000000000;

/// TaggedMask + Sign bit indicates a pointer value.
const PointerMask: u64 = TaggedValueMask | SignMask;

const BooleanMask: u64 = TaggedValueMask | (@as(u64, TagBoolean) << 32);
const FalseMask: u64 = BooleanMask;
const TrueBitMask: u64 = 1;
const TrueMask: u64 = BooleanMask | TrueBitMask;

const IntegerMask: u64 = TaggedValueMask | (@as(u64, TagInteger) << 32);
const NullMask: u64 = TaggedValueMask | (@as(u64, TagNull) << 32);
const VoidMask: u64 = TaggedValueMask | (@as(u64, TagVoid) << 32);
const ObjMask: u64 = TaggedValueMask | (@as(u64, TagObj) << 32);

const TagMask: u32 = (1 << 3) - 1;
const TaggedPrimitiveMask = TaggedValueMask | (@as(u64, TagMask) << 32);

pub const Value = extern struct {
    val: u64,

    pub const Null = Value{ .val = NullMask };
    pub const Void = Value{ .val = VoidMask };
    pub const True = Value{ .val = TrueMask };
    pub const False = Value{ .val = FalseMask };

    pub inline fn fromBoolean(val: bool) Value {
        return if (val) True else False;
    }

    pub inline fn fromInteger(val: i32) Value {
        return .{ .val = IntegerMask | @bitCast(u32, val) };
    }

    pub inline fn fromFloat(val: f64) Value {
        return .{ .val = @bitCast(u64, val) };
    }

    pub inline fn fromObj(val: *Obj) Value {
        return .{ .val = PointerMask | @ptrToInt(val) };
    }

    pub inline fn getTag(self: Value) u3 {
        return @intCast(Tag, @intCast(u32, self.val >> 32) & TagMask);
    }

    pub inline fn isBool(self: Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == BooleanMask;
    }

    pub inline fn isInteger(self: Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == IntegerMask;
    }

    pub inline fn isFloat(self: Value) bool {
        return self.val & TaggedValueMask != TaggedValueMask;
    }

    pub inline fn isNumber(self: Value) bool {
        return self.isFloat() or self.isInteger();
    }

    pub inline fn isObj(self: Value) bool {
        return self.val & PointerMask == PointerMask;
    }

    pub inline fn isNull(self: Value) bool {
        return self.val == NullMask;
    }

    pub inline fn isVoid(self: Value) bool {
        return self.val == VoidMask;
    }

    pub inline fn boolean(self: Value) bool {
        return self.val == TrueMask;
    }

    pub inline fn integer(self: Value) i32 {
        return @bitCast(i32, @intCast(u32, self.val & 0xffffffff));
    }

    pub inline fn float(self: Value) f64 {
        return @bitCast(f64, self.val);
    }

    pub inline fn obj(self: Value) *Obj {
        return @intToPtr(*Obj, self.val & ~PointerMask);
    }
};

test "NaN boxing" {
    const boolean = Value.fromBoolean(true);
    const integer = Value.fromInteger(42);
    const float = Value.fromFloat(42.24);

    std.debug.assert(boolean.isBool() and boolean.boolean());
    std.debug.assert(integer.isInteger() and integer.integer() == 42);
    std.debug.assert(float.isFloat() and float.float() == 42.24);
    std.debug.assert(Value.Null.isNull());
    std.debug.assert(Value.Void.isVoid());
    std.debug.assert(Value.False.isBool());
    std.debug.assert(Value.True.isBool());
}

// If nothing in its decimal part, will return a Value.Integer
pub inline fn floatToInteger(value: Value) Value {
    const float = value.float();

    // FIXME also check that the f64 can fit in the i32
    if (value.isFloat() and std.math.floor(float) == float) {
        return Value.fromInteger(@floatToInt(i32, float));
    }

    return value;
}

pub fn valueToStringAlloc(allocator: Allocator, value: Value) (Allocator.Error || std.fmt.BufPrintError)![]const u8 {
    var str = std.ArrayList(u8).init(allocator);

    try valueToString(&str.writer(), value);

    return str.items;
}

pub fn valueToString(writer: *const std.ArrayList(u8).Writer, value: Value) (Allocator.Error || std.fmt.BufPrintError)!void {
    if (value.isObj()) {
        try objToString(writer, value.obj());

        return;
    }

    switch (value.getTag()) {
        TagBoolean => try writer.print("{}", .{value.boolean()}),
        TagInteger => try writer.print("{d}", .{value.integer()}),
        TagNull => try writer.print("null", .{}),
        TagVoid => try writer.print("void", .{}),
        else => try writer.print("{d}", .{value.float()}),
    }
}

pub fn valueEql(a: Value, b: Value) bool {
    // zig fmt: off
    if (a.isObj() != b.isObj()
        or a.isNumber() != b.isNumber()
        or (!a.isNumber() and !b.isNumber() and a.getTag() != b.getTag())) {
        return false;
    }
    // zig fmt: on

    if (a.isObj()) {
        return a.obj().eql(b.obj());
    }

    if (a.isInteger() or a.isFloat()) {
        const aa = floatToInteger(a);
        const bb = floatToInteger(b);

        const a_f: ?f64 = if (aa.isFloat()) aa.float() else null;
        const b_f: ?f64 = if (bb.isFloat()) bb.float() else null;
        const a_i: ?i32 = if (aa.isInteger()) aa.integer() else null;
        const b_i: ?i32 = if (bb.isInteger()) bb.integer() else null;

        if (a_f) |af| {
            if (b_f) |bf| {
                return af == bf;
            } else {
                return af == @intToFloat(f64, b_i.?);
            }
        } else {
            if (b_f) |bf| {
                return @intToFloat(f64, a_i.?) == bf;
            } else {
                return a_i.? == b_i.?;
            }
        }
    }

    return switch (a.getTag()) {
        TagBoolean => a.boolean() == b.boolean(),
        TagNull => true,
        TagVoid => true,
        else => unreachable,
    };
}

pub fn valueIs(type_def_val: Value, value: Value) bool {
    const type_def: *ObjTypeDef = ObjTypeDef.cast(type_def_val.obj()).?;

    if (value.isObj()) {
        return value.obj().is(type_def);
    }

    return switch (value.getTag()) {
        TagBoolean => type_def.def_type == .Bool,
        TagInteger => type_def.def_type == .Integer,
        // TODO: this one is ambiguous at runtime, is it the `null` constant? or an optional local with a null value?
        TagNull => type_def.def_type == .Void or type_def.optional,
        TagVoid => type_def.def_type == .Void,
        else => type_def.def_type == .Float,
    };
}

pub fn valueTypeEql(value: Value, type_def: *ObjTypeDef) bool {
    if (value.isObj()) {
        return value.obj().typeEql(type_def);
    }

    return switch (value.getTag()) {
        TagBoolean => type_def.def_type == .Bool,
        TagInteger => type_def.def_type == .Integer,
        // TODO: this one is ambiguous at runtime, is it the `null` constant? or an optional local with a null value?
        TagNull => type_def.def_type == .Void or type_def.optional,
        TagVoid => type_def.def_type == .Void,
        else => type_def.def_type == .Float,
    };
}
