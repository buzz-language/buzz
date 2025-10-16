const std = @import("std");
const Ast = @import("Ast.zig");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
const Token = @import("Token.zig");

allocator: std.mem.Allocator,
/// AST
ast: Ast.Slice,
/// List of opcodes to execute
code: std.ArrayList(u32) = .empty,
/// List of locations
locations: std.ArrayList(Ast.TokenIndex) = .empty,
/// List of constants defined in this chunk
constants: std.ArrayList(Value) = .empty,

pub fn init(allocator: std.mem.Allocator, ast: Ast.Slice) Self {
    return Self{
        .allocator = allocator,
        .ast = ast,
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit(self.allocator);
    self.constants.deinit(self.allocator);
    self.locations.deinit(self.allocator);
}

pub fn write(self: *Self, code: u32, where: Ast.TokenIndex) !void {
    try self.code.append(self.allocator, code);
    try self.locations.append(self.allocator, where);
}

pub fn addConstant(self: *Self, vm: ?*VM, value: Value) !u24 {
    if (vm) |uvm| uvm.push(value);
    try self.constants.append(self.allocator, value);
    if (vm) |uvm| _ = uvm.pop();

    return @intCast(self.constants.items.len - 1);
}

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_NULL,
    OP_VOID,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_COPY,
    OP_CLONE,

    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_GET_LIST_SUBSCRIPT,
    OP_GET_MAP_SUBSCRIPT,
    OP_GET_STRING_SUBSCRIPT,
    OP_SET_LIST_SUBSCRIPT,
    OP_SET_MAP_SUBSCRIPT,

    OP_EQUAL,
    OP_IS,
    OP_GREATER,
    OP_LESS,
    OP_ADD_F,
    OP_ADD_I,
    OP_ADD_STRING,
    OP_ADD_LIST,
    OP_ADD_MAP,
    OP_SUBTRACT_I,
    OP_SUBTRACT_F,
    OP_MULTIPLY_I,
    OP_MULTIPLY_F,
    OP_DIVIDE_I,
    OP_DIVIDE_F,
    OP_MOD_I,
    OP_MOD_F,
    OP_BNOT,
    OP_BAND,
    OP_BOR,
    OP_XOR,
    OP_SHL,
    OP_SHR,

    OP_UNWRAP,

    OP_NOT,
    OP_NEGATE_I,
    OP_NEGATE_F,

    OP_SWAP,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_JUMP_IF_NOT_NULL,
    OP_LOOP,
    OP_STRING_FOREACH,
    OP_LIST_FOREACH,
    OP_RANGE_FOREACH,
    OP_ENUM_FOREACH,
    OP_MAP_FOREACH,
    OP_FIBER_FOREACH,

    OP_CALL,
    OP_TAIL_CALL,
    OP_CALL_INSTANCE_PROPERTY,
    OP_TAIL_CALL_INSTANCE_PROPERTY,
    OP_INSTANCE_INVOKE,
    OP_INSTANCE_TAIL_INVOKE,
    OP_PROTOCOL_INVOKE,
    OP_PROTOCOL_TAIL_INVOKE,
    OP_STRING_INVOKE,
    OP_PATTERN_INVOKE,
    OP_FIBER_INVOKE,
    OP_LIST_INVOKE,
    OP_MAP_INVOKE,
    OP_RANGE_INVOKE,

    OP_CLOSURE,
    OP_CLOSE_UPVALUE,

    OP_FIBER,
    OP_RESUME,
    OP_RESOLVE,
    OP_YIELD,

    OP_TRY,
    OP_TRY_END,
    OP_THROW,

    OP_RETURN,

    OP_OBJECT,
    OP_INSTANCE,
    OP_FCONTAINER_INSTANCE,
    OP_PROPERTY,
    OP_OBJECT_DEFAULT,
    OP_GET_OBJECT_PROPERTY,
    OP_GET_INSTANCE_PROPERTY,
    OP_GET_INSTANCE_METHOD,
    OP_GET_PROTOCOL_METHOD,
    OP_GET_FCONTAINER_INSTANCE_PROPERTY,
    OP_GET_LIST_PROPERTY,
    OP_GET_MAP_PROPERTY,
    OP_GET_STRING_PROPERTY,
    OP_GET_PATTERN_PROPERTY,
    OP_GET_FIBER_PROPERTY,
    OP_GET_RANGE_PROPERTY,
    OP_SET_OBJECT_PROPERTY,
    OP_SET_INSTANCE_PROPERTY,
    OP_SET_FCONTAINER_INSTANCE_PROPERTY,

    OP_GET_ENUM_CASE,
    OP_GET_ENUM_CASE_VALUE,
    OP_GET_ENUM_CASE_FROM_VALUE,

    OP_LIST,
    OP_RANGE,
    OP_LIST_APPEND,

    OP_MAP,
    // FIXME: delete and only use OP_SET_MAP_SUBSCRIPT
    OP_SET_MAP,

    OP_EXPORT,
    OP_IMPORT,

    OP_TO_STRING,
    OP_TYPEOF,

    OP_HOTSPOT,
    OP_HOTSPOT_CALL,

    OP_DBG_LOCAL_ENTER,
    OP_DBG_LOCAL_EXIT,
    OP_DBG_GLOBAL_DEFINE,
};

/// A chunk of code to execute
const Self = @This();

pub const max_constants = std.math.maxInt(u24);

const RegistryContext = struct {
    pub fn hash(_: RegistryContext, key: Self) u64 {
        return std.hash.Wyhash.hash(
            0,
            std.mem.sliceAsBytes(key.code.items),
        );
    }

    pub fn eql(_: RegistryContext, a: Self, b: Self) bool {
        return std.mem.eql(u32, a.code.items, b.code.items) and
            std.mem.eql(Value, a.constants.items, b.constants.items);
    }
};

pub fn HashMap(V: type) type {
    return std.HashMapUnmanaged(
        Self,
        V,
        RegistryContext,
        std.hash_map.default_max_load_percentage,
    );
}
