const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const o = @import("obj.zig");
const VM = @import("vm.zig").VM;
const GarbageCollector = @import("memory.zig").GarbageCollector;

pub const Double = f64;
pub const Integer = i32;
const Tag = u3;

pub const Value = packed struct {
    pub const TagBoolean: Tag = 0;
    pub const TagInteger: Tag = 1;
    pub const TagNull: Tag = 2;
    pub const TagVoid: Tag = 3;
    pub const TagObj: Tag = 4;
    pub const TagError: Tag = 5;

    /// Most significant bit.
    pub const SignMask: u64 = 1 << 63;

    /// QNAN and one extra bit to the right.
    pub const TaggedValueMask: u64 = 0x7ffc000000000000;

    /// TaggedMask + Sign bit indicates a pointer value.
    pub const PointerMask: u64 = TaggedValueMask | SignMask;

    pub const BooleanMask: u64 = TaggedValueMask | (@as(u64, TagBoolean) << 32);
    pub const FalseMask: u64 = BooleanMask;
    pub const TrueBitMask: u64 = 1;
    pub const TrueMask: u64 = BooleanMask | TrueBitMask;

    // FIXME: Their's room to have a bigger integer. How would MIR handle it?
    pub const IntegerMask: u64 = TaggedValueMask | (@as(u64, TagInteger) << 32);
    pub const NullMask: u64 = TaggedValueMask | (@as(u64, TagNull) << 32);
    pub const VoidMask: u64 = TaggedValueMask | (@as(u64, TagVoid) << 32);
    pub const ErrorMask: u64 = TaggedValueMask | (@as(u64, TagError) << 32);

    pub const TagMask: u32 = (1 << 3) - 1;
    pub const TaggedPrimitiveMask = TaggedValueMask | (@as(u64, TagMask) << 32);

    val: u64,

    pub const Null = Value{ .val = NullMask };
    pub const Void = Value{ .val = VoidMask };
    pub const True = Value{ .val = TrueMask };
    pub const False = Value{ .val = FalseMask };
    // We only need this so that an NativeFn can see the error returned by its raw function
    pub const Error = Value{ .val = ErrorMask };

    pub inline fn fromBoolean(val: bool) Value {
        return if (val) True else False;
    }

    pub inline fn fromInteger(val: i32) Value {
        return .{ .val = IntegerMask | @as(u32, @bitCast(val)) };
    }

    pub inline fn fromFloat(val: f64) Value {
        return .{ .val = @as(u64, @bitCast(val)) };
    }

    pub inline fn fromObj(val: *o.Obj) Value {
        return .{ .val = PointerMask | @intFromPtr(val) };
    }

    pub inline fn getTag(self: Value) Tag {
        return @as(Tag, @intCast(@as(u32, @intCast(self.val >> 32)) & TagMask));
    }

    pub inline fn isBool(self: Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == BooleanMask;
    }

    pub inline fn isInteger(self: Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == IntegerMask;
    }

    pub inline fn isFloat(self: Value) bool {
        return !self.isBool() and !self.isError() and !self.isInteger() and !self.isNull() and !self.isObj() and !self.isVoid();
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

    pub inline fn isError(self: Value) bool {
        return self.val == ErrorMask;
    }

    pub inline fn boolean(self: Value) bool {
        return self.val == TrueMask;
    }

    pub inline fn integer(self: Value) i32 {
        return @as(i32, @bitCast(@as(u32, @intCast(self.val & 0xffffffff))));
    }

    pub inline fn double(self: Value) f64 {
        return @as(f64, @bitCast(self.val));
    }

    pub inline fn obj(self: Value) *o.Obj {
        return @as(*o.Obj, @ptrFromInt(@as(usize, @truncate(self.val & ~PointerMask))));
    }

    pub inline fn booleanOrNull(self: Value) ?bool {
        return if (self.isBool()) self.boolean() else null;
    }

    pub inline fn integerOrNull(self: Value) ?i32 {
        return if (self.isInteger()) self.integer() else null;
    }

    pub inline fn floatOrNull(self: Value) ?f64 {
        return if (self.isFloat()) self.double() else null;
    }

    pub inline fn objOrNull(self: Value) ?*o.Obj {
        return if (self.isObj()) self.obj() else null;
    }

    pub fn typeOf(self: Value, gc: *GarbageCollector) !*o.ObjTypeDef {
        if (self.isObj()) {
            return try self.obj().typeOf(gc);
        }

        if (self.isFloat()) {
            return gc.type_registry.float_type;
        }

        return switch (self.getTag()) {
            TagBoolean => gc.type_registry.bool_type,
            TagInteger => gc.type_registry.int_type,
            TagNull, TagVoid => gc.type_registry.void_type,
            else => gc.type_registry.float_type,
        };
    }

    pub fn serialize(self: Value, vm: *VM, seen: *std.AutoHashMap(*o.Obj, void)) !Value {
        if (self.isObj()) {
            return try self.obj().serialize(vm, seen);
        }

        return self;
    }

    pub fn toStringAlloc(value: Value, allocator: Allocator) (Allocator.Error || std.fmt.BufPrintError)![]const u8 {
        var str = std.ArrayList(u8).init(allocator);

        try value.toString(&str.writer());

        return try str.toOwnedSlice();
    }

    pub fn toString(self: Value, writer: *const std.ArrayList(u8).Writer) (Allocator.Error || std.fmt.BufPrintError)!void {
        if (self.isObj()) {
            try self.obj().toString(writer);

            return;
        }

        if (self.isFloat()) {
            try writer.print("{d}", .{self.double()});
            return;
        }

        switch (self.getTag()) {
            TagBoolean => try writer.print("{}", .{self.boolean()}),
            TagInteger => try writer.print("{d}", .{self.integer()}),
            TagNull => try writer.print("null", .{}),
            TagVoid => try writer.print("void", .{}),
            else => try writer.print("{d}", .{self.double()}),
        }
    }

    pub fn eql(a: Value, b: Value) bool {
        if (a.isObj() != b.isObj() or
            a.isNumber() != b.isNumber() or
            (!a.isNumber() and !b.isNumber() and a.getTag() != b.getTag()))
        {
            return false;
        }

        if (a.isObj()) {
            return a.obj().eql(b.obj());
        }

        if (a.isInteger() or a.isFloat()) {
            const a_f: ?f64 = if (a.isFloat()) a.double() else null;
            const b_f: ?f64 = if (b.isFloat()) b.double() else null;
            const a_i: ?i32 = if (a.isInteger()) a.integer() else null;
            const b_i: ?i32 = if (b.isInteger()) b.integer() else null;

            if (a_f) |af| {
                if (b_f) |bf| {
                    return af == bf;
                } else {
                    return af == @as(f64, @floatFromInt(b_i.?));
                }
            } else {
                if (b_f) |bf| {
                    return @as(f64, @floatFromInt(a_i.?)) == bf;
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

    pub fn is(type_def_val: Value, value: Value) bool {
        const type_def: *o.ObjTypeDef = o.ObjTypeDef.cast(type_def_val.obj()).?;

        if (type_def.def_type == .Any) {
            return true;
        }

        if (value.isObj()) {
            return value.obj().is(type_def);
        }

        if (value.isFloat()) {
            return type_def.def_type == .Double;
        }

        return switch (value.getTag()) {
            TagBoolean => type_def.def_type == .Bool,
            TagInteger => type_def.def_type == .Integer,
            // TODO: this one is ambiguous at runtime, is it the `null` constant? or an optional local with a null value?
            TagNull => type_def.def_type == .Void or type_def.optional,
            TagVoid => type_def.def_type == .Void,
            else => type_def.def_type == .Double,
        };
    }

    pub fn typeEql(value: Value, type_def: *o.ObjTypeDef) bool {
        if (value.isObj()) {
            return value.obj().typeEql(type_def);
        }

        if (value.isFloat()) {
            return type_def.def_type == .Double;
        }

        return switch (value.getTag()) {
            TagBoolean => type_def.def_type == .Bool,
            TagInteger => type_def.def_type == .Integer,
            // TODO: this one is ambiguous at runtime, is it the `null` constant? or an optional local with a null value?
            TagNull => type_def.def_type == .Void or type_def.optional,
            TagVoid => type_def.def_type == .Void,
            else => type_def.def_type == .Double,
        };
    }
};
