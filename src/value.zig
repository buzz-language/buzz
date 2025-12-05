const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const o = @import("obj.zig");
const VM = @import("vm.zig").VM;
const GC = @import("GC.zig");

pub const Double = f64;
pub const Integer = i48;
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
    pub const TaggedUpperValueMask: u64 = 0xffff000000000000;

    /// TaggedMask + Sign bit indicates a pointer value.
    pub const PointerMask: u64 = TaggedValueMask | SignMask;
    pub const BooleanMask: u64 = TaggedValueMask | (@as(u64, TagBoolean) << 32);
    pub const FalseMask: u64 = BooleanMask;
    pub const TrueBitMask: u64 = 1;
    pub const TrueMask: u64 = BooleanMask | TrueBitMask;
    pub const IntegerMask: u64 = TaggedValueMask | (@as(u64, TagInteger) << 49);
    pub const NullMask: u64 = TaggedValueMask | (@as(u64, TagNull) << 32);
    pub const VoidMask: u64 = TaggedValueMask | (@as(u64, TagVoid) << 32);
    pub const ErrorMask: u64 = TaggedValueMask | (@as(u64, TagError) << 32);

    pub const TagMask: u32 = (1 << 3) - 1;
    pub const TaggedPrimitiveMask = TaggedValueMask | (@as(u64, TagMask) << 32) | IntegerMask;

    val: u64,

    pub const Null = Value{ .val = NullMask };
    pub const Void = Value{ .val = VoidMask };
    pub const True = Value{ .val = TrueMask };
    pub const False = Value{ .val = FalseMask };
    // We only need this so that an NativeFn can see the error returned by its raw function
    pub const Error = Value{ .val = ErrorMask };

    pub fn fromBoolean(val: bool) Value {
        return if (val) True else False;
    }

    pub fn fromInteger(val: Integer) Value {
        return .{ .val = IntegerMask | @as(u48, @bitCast(val)) };
    }

    pub fn fromDouble(val: Double) Value {
        return .{ .val = @as(u64, @bitCast(val)) };
    }

    pub fn fromObj(val: *o.Obj) Value {
        return .{ .val = PointerMask | @intFromPtr(val) };
    }

    pub fn getTag(self: Value) Tag {
        return @as(Tag, @intCast(@as(u32, @intCast(self.val >> 32)) & TagMask));
    }

    pub fn isBool(self: Value) bool {
        return self.val & (TaggedPrimitiveMask | SignMask) == BooleanMask;
    }

    pub fn isInteger(self: Value) bool {
        return self.val & (TaggedUpperValueMask | SignMask) == IntegerMask;
    }

    pub fn isDouble(self: Value) bool {
        return !self.isBool() and !self.isError() and !self.isInteger() and !self.isNull() and !self.isObj() and !self.isVoid();
    }

    pub fn isNumber(self: Value) bool {
        return self.isDouble() or self.isInteger();
    }

    pub fn isObj(self: Value) bool {
        return self.val & PointerMask == PointerMask;
    }

    pub fn isNull(self: Value) bool {
        return self.val == NullMask;
    }

    pub fn isVoid(self: Value) bool {
        return self.val == VoidMask;
    }

    pub fn isError(self: Value) bool {
        return self.val == ErrorMask;
    }

    pub fn boolean(self: Value) bool {
        return self.val == TrueMask;
    }

    pub fn integer(self: Value) Integer {
        return @bitCast(@as(u48, @intCast(self.val & 0xffffffffffff)));
    }

    pub fn double(self: Value) Double {
        return @bitCast(self.val);
    }

    pub fn obj(self: Value) *o.Obj {
        return @ptrFromInt(@as(usize, @truncate(self.val & ~PointerMask)));
    }

    pub fn booleanOrNull(self: Value) ?bool {
        return if (self.isBool()) self.boolean() else null;
    }

    pub fn integerOrNull(self: Value) ?Integer {
        return if (self.isInteger()) self.integer() else null;
    }

    pub fn doubleOrNull(self: Value) ?Double {
        return if (self.isDouble()) self.double() else null;
    }

    pub fn objOrNull(self: Value) ?*o.Obj {
        return if (self.isObj()) self.obj() else null;
    }

    pub fn typeOf(self: Value, gc: *GC) !*o.ObjTypeDef {
        if (self.isObj()) {
            return try self.obj().typeOf(gc);
        }

        if (self.isDouble()) {
            return gc.type_registry.double_type;
        }

        if (self.isInteger()) {
            return gc.type_registry.int_type;
        }

        return switch (self.getTag()) {
            TagBoolean => gc.type_registry.bool_type,
            TagNull, TagVoid => gc.type_registry.void_type,
            else => gc.type_registry.double_type,
        };
    }

    pub fn serialize(self: Value, vm: *VM, seen: *std.AutoHashMapUnmanaged(*o.Obj, void)) !Value {
        if (self.isObj()) {
            return try self.obj().serialize(vm, seen);
        }

        return self;
    }

    pub fn format(value: Value, w: *std.Io.Writer) std.Io.Writer.Error!void {
        value.toString(w) catch return error.WriteFailed;
    }

    pub fn toStringAlloc(value: Value, allocator: Allocator) (Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})![]const u8 {
        var str = std.Io.Writer.Allocating.init(allocator);

        try value.toString(&str.writer);

        return try str.toOwnedSlice();
    }

    // FIXME: should be a std.io.Writer once it exists for ArrayLists
    pub fn toString(self: Value, writer: *std.Io.Writer) (Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!void {
        if (self.isObj()) {
            try self.obj().toString(writer);

            return;
        }

        if (self.isDouble()) {
            try writer.print("{d}", .{self.double()});
            return;
        }

        if (self.isInteger()) {
            try writer.print("{d}", .{self.integer()});
            return;
        }

        switch (self.getTag()) {
            TagBoolean => try writer.print("{}", .{self.boolean()}),
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

        if (a.isInteger() or a.isDouble()) {
            const a_f = if (a.isDouble()) a.double() else null;
            const b_f = if (b.isDouble()) b.double() else null;
            const a_i = if (a.isInteger()) a.integer() else null;
            const b_i = if (b.isInteger()) b.integer() else null;

            if (a_f) |af| {
                if (b_f) |bf| {
                    return af == bf;
                } else {
                    return af == @as(Double, @floatFromInt(b_i.?));
                }
            } else {
                if (b_f) |bf| {
                    return @as(Double, @floatFromInt(a_i.?)) == bf;
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

        if (value.isDouble()) {
            return type_def.def_type == .Double;
        }

        if (value.isInteger()) {
            return type_def.def_type == .Integer;
        }

        return switch (value.getTag()) {
            TagBoolean => type_def.def_type == .Bool,
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

        if (value.isDouble()) {
            return type_def.def_type == .Double;
        }

        if (value.isInteger()) {
            return type_def.def_type == .Integer;
        }

        return switch (value.getTag()) {
            TagBoolean => type_def.def_type == .Bool,
            // TODO: this one is ambiguous at runtime, is it the `null` constant? or an optional local with a null value?
            TagNull => type_def.def_type == .Void or type_def.optional,
            TagVoid => type_def.def_type == .Void,
            else => type_def.def_type == .Double,
        };
    }
};
