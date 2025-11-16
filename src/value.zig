const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const o = @import("obj.zig");
const VM = @import("vm.zig").VM;
const GC = @import("GC.zig");
const Pool = @import("pool.zig").Pool;

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

    const ObjTypeBits = 5;
    const ObjIndexBits = 43;
    const ObjTypeTag = u5;
    const ObjIndexType = u43;
    const ObjIndexMask: u64 = (1 << ObjIndexBits) - 1;
    const ObjTypeMask: u64 = ((1 << ObjTypeBits) - 1) << ObjIndexBits;

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

    pub fn fromObj(val: o.ObjIdx) Value {
        const type_bits: u64 = @intCast(@intFromEnum(val.obj_type));
        const index_bits: u64 = @intCast(val.index);
        const payload = (type_bits << ObjIndexBits) | (index_bits & ObjIndexMask);

        return .{ .val = PointerMask | payload };
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

    pub fn obj(self: Value) o.ObjIdx {
        const payload = self.val & ~PointerMask;
        const type_bits = (payload >> ObjIndexBits) & ((@as(u64, 1) << ObjTypeBits) - 1);
        const index_bits = payload & ObjIndexMask;

        return .{
            .obj_type = @enumFromInt(@as(ObjTypeTag, @intCast(type_bits))),
            .index = @as(ObjIndexType, @intCast(index_bits)),
        };
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

    pub fn objOrNull(self: Value) ?o.ObjIdx {
        return if (self.isObj()) self.obj() else null;
    }

    pub fn typeOf(self: Value, gc: *GC) !Pool(o.ObjTypeDef).Idx {
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

    pub fn serialize(self: Value, vm: *GC, seen: *std.AutoHashMapUnmanaged(*o.Obj, void)) !Value {
        if (self.isObj()) {
            return try self.obj().ptr(vm.gc).serialize(vm, seen);
        }

        return self;
    }

    pub fn format(value: Value, w: *std.Io.Writer) std.Io.Writer.Error!void {
        value.toString(w) catch return error.WriteFailed;
    }

    pub fn toStringAlloc(value: Value, gc: *GC) (Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})![]const u8 {
        var str = std.Io.Writer.Allocating.init(gc.allocator);

        try value.toString(&str.writer, gc);

        return try str.toOwnedSlice();
    }

    // FIXME: should be a std.io.Writer once it exists for ArrayLists
    pub fn toString(self: Value, gc: *GC, writer: *std.Io.Writer) (Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!void {
        if (self.isObj()) {
            try self.obj().ptr(gc).toString(writer);

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

    pub fn eql(a: Value, b: Value, gc: *GC) bool {
        if (a.isObj() != b.isObj() or
            a.isNumber() != b.isNumber() or
            (!a.isNumber() and !b.isNumber() and a.getTag() != b.getTag()))
        {
            return false;
        }

        if (a.isObj()) {
            return a.obj().ptr(gc).eql(
                b.obj().ptr(gc),
                gc,
            );
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

    pub fn is(type_def_val: Value, value: Value, gc: *GC) bool {
        const type_def = gc.get(o.ObjTypeDef, .idx(type_def_val.obj().index)).?;

        if (type_def.def_type == .Any) {
            return true;
        }

        if (value.isObj()) {
            return value.obj().ptr(gc).is(
                .idx(type_def_val.obj().index),
                gc,
            );
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

    pub fn typeEql(value: Value, type_def: Pool(o.ObjTypeDef).Idx, gc: *GC) bool {
        if (value.isObj()) {
            return value.obj().ptr(gc).typeEql(gc.get(o.ObjTypeDef, type_def));
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
