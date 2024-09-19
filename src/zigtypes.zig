const std = @import("std");

// std.builtin.Type can't be used at runtime, so we make our own non-comptime types
// We use std.builtin things when we can

pub const Type = union(enum) {
    Type: void,
    Void: void,
    Bool: void,
    NoReturn: void,
    Int: IntType,
    Float: FloatType,
    Pointer: PointerType,
    Array: ArrayType,
    Struct: StructType,
    ComptimeFloat: void,
    ComptimeInt: void,
    Undefined: void,
    Null: void,
    Optional: OptionalType,
    ErrorUnion: ErrorUnionType,
    ErrorSet: ErrorSetType,
    Enum: EnumType,
    Union: UnionType,
    Fn: FnType,
    Opaque: OpaqueType,
    Frame: FrameType,
    AnyFrame: AnyFrameType,
    Vector: VectorType,
    EnumLiteral: void,

    // FIXME: should be expressed in bits, because alignment and shit
    pub fn size(self: *const Type) usize {
        return switch (self.*) {
            .Bool => 1,
            .Int => self.Int.bits / 8,
            .Float => self.Float.bits / 8,
            .Pointer => 8,
            .Struct => str: {
                const struct_type = self.Struct;
                var struct_size: usize = 0;
                var next_field: ?Type.StructField = null;
                var struct_align: u16 = 0;
                for (struct_type.fields, 0..) |field, idx| {
                    next_field = if (idx < struct_type.fields.len - 1)
                        struct_type.fields[idx + 1]
                    else
                        null;

                    struct_align = if (field.alignment > struct_align)
                        field.alignment
                    else
                        struct_align;

                    struct_size += field.type.size();

                    if (next_field) |next| {
                        const next_field_align = next.alignment;
                        const current_field_size = field.type.size();

                        const div = @as(f64, @floatFromInt(current_field_size)) / @as(f64, @floatFromInt(next_field_align));
                        const fpart = std.math.modf(div).fpart;
                        const padding = @as(usize, @intFromFloat(fpart * @as(f64, @floatFromInt(next_field_align))));

                        struct_size += padding;
                    } else {
                        const div = @as(f64, @floatFromInt(struct_size)) / @as(f64, @floatFromInt(struct_align));
                        const fpart = std.math.modf(div).fpart;
                        const padding = @as(usize, @intFromFloat(fpart * @as(f64, @floatFromInt(struct_align))));

                        struct_size += padding;
                    }
                }

                break :str struct_size;
            },
            .Union => uni: {
                const union_type = self.Union;
                var union_size: usize = 0;
                for (union_type.fields) |field| {
                    union_size = @max(union_size, field.type.size());
                }

                break :uni union_size;
            },
            else => unreachable,
        };
    }

    pub fn alignment(self: *const Type) u16 {
        return switch (self.*) {
            .Bool => 1,
            .Int => @as(u16, @intCast(self.Int.bits)) / 8,
            .Float => @as(u16, @intCast(self.Float.bits)) / 8,
            .Struct => str: {
                const struct_type = self.Struct;
                var max_align: u16 = 0;
                for (struct_type.fields) |field| {
                    max_align = if (field.alignment > max_align)
                        field.alignment
                    else
                        max_align;
                }
                break :str max_align;
            },
            .Union => uni: {
                const union_type = self.Union;
                var max_align: u16 = 0;
                for (union_type.fields) |field| {
                    max_align = if (field.alignment > max_align)
                        field.alignment
                    else
                        max_align;
                }
                break :uni max_align;
            },
            .Pointer => 8,
            else => unreachable,
        };
    }

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const IntType = struct {
        signedness: std.builtin.Signedness,
        bits: u16,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const FloatType = struct {
        bits: u16,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const PointerType = struct {
        size: std.builtin.Type.Pointer.Size,
        is_const: bool,
        is_volatile: bool,
        alignment: u16,
        address_space: std.builtin.AddressSpace,
        child: *const Type,
        is_allowzero: bool,

        /// The type of the sentinel is the element type of the pointer, which is
        /// the value of the `child` field in this struct. However there is no way
        /// to refer to that type here, so we use pointer to `anyopaque`.
        sentinel: ?*const Type,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ArrayType = struct {
        len: usize,
        child: *const Type,

        /// The type of the sentinel is the element type of the array, which is
        /// the value of the `child` field in this struct. However there is no way
        /// to refer to that type here, so we use pointer to `anyopaque`.
        sentinel: ?*const anyopaque,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const StructField = struct {
        name: []const u8,
        type: *const Type,
        default_value: ?*const anyopaque,
        is_comptime: bool,
        alignment: u16,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const StructType = struct {
        layout: std.builtin.Type.ContainerLayout,
        /// Only valid if layout is .Packed
        backing_integer: ?*const Type = null,
        fields: []const StructField,
        decls: []const Declaration,
        is_tuple: bool,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const OptionalType = struct {
        child: *const Type,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ErrorUnionType = struct {
        error_set: *const Type,
        payload: *const Type,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Error = struct {
        name: []const u8,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const ErrorSetType = ?[]const Error;

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const EnumField = struct {
        name: []const u8,
        value: usize,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const EnumType = struct {
        tag_type: *const Type,
        fields: []const EnumField,
        decls: []const Declaration,
        is_exhaustive: bool,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const UnionField = struct {
        name: []const u8,
        type: *const Type,
        alignment: u16,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const UnionType = struct {
        layout: std.builtin.Type.ContainerLayout,
        tag_type: ?*const Type,
        fields: []const UnionField,
        decls: []const Declaration,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const FnType = struct {
        calling_convention: std.builtin.CallingConvention,
        alignment: u16,
        is_generic: bool,
        is_var_args: bool,
        /// TODO change the language spec to make this not optional.
        return_type: ?*const Type,
        params: []const Param,

        /// This data structure is used by the Zig language code generation and
        /// therefore must be kept in sync with the compiler implementation.
        pub const Param = struct {
            is_generic: bool,
            is_noalias: bool,
            type: ?*const Type,
        };
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const OpaqueType = struct {
        decls: []const Declaration,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const FrameType = struct {
        function: *const anyopaque,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const AnyFrameType = struct {
        child: ?*const Type,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const VectorType = struct {
        len: usize,
        child: *const Type,
    };

    /// This data structure is used by the Zig language code generation and
    /// therefore must be kept in sync with the compiler implementation.
    pub const Declaration = struct {
        name: []const u8,
    };
};
