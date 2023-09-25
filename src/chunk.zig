const std = @import("std");
const Allocator = std.mem.Allocator;
const _value = @import("value.zig");
const _vm = @import("vm.zig");
const VM = _vm.VM;
const Value = _value.Value;
const Token = @import("token.zig").Token;

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
    OP_ADD,
    OP_ADD_STRING,
    OP_ADD_LIST,
    OP_ADD_MAP,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_MOD,
    OP_BNOT,
    OP_BAND,
    OP_BOR,
    OP_XOR,
    OP_SHL,
    OP_SHR,

    OP_UNWRAP,

    OP_NOT,
    OP_NEGATE,

    OP_SWAP,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_JUMP_IF_NOT_NULL,
    OP_LOOP,
    OP_STRING_FOREACH,
    OP_LIST_FOREACH,
    OP_ENUM_FOREACH,
    OP_MAP_FOREACH,
    OP_FIBER_FOREACH,

    OP_CALL,
    OP_INSTANCE_INVOKE,
    OP_STRING_INVOKE,
    OP_PATTERN_INVOKE,
    OP_FIBER_INVOKE,
    OP_LIST_INVOKE,
    OP_MAP_INVOKE,

    OP_CLOSURE,
    OP_CLOSE_UPVALUE,

    OP_FIBER,
    OP_INVOKE_FIBER,
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
    OP_METHOD,
    OP_PROPERTY,
    OP_GET_OBJECT_PROPERTY,
    OP_GET_INSTANCE_PROPERTY,
    OP_GET_FCONTAINER_INSTANCE_PROPERTY,
    OP_GET_LIST_PROPERTY,
    OP_GET_MAP_PROPERTY,
    OP_GET_STRING_PROPERTY,
    OP_GET_PATTERN_PROPERTY,
    OP_GET_FIBER_PROPERTY,
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
    // FIXMEL delete and only use OP_SET_MAP_SUBSCRIPT
    OP_SET_MAP,

    OP_EXPORT,
    OP_IMPORT,

    OP_TO_STRING,
    OP_TYPEOF,
};

/// A chunk of code to execute
pub const Chunk = struct {
    const Self = @This();

    pub const max_constants: u24 = 16777215;

    /// List of opcodes to execute
    code: std.ArrayList(u32),
    /// List of lines
    lines: std.ArrayList(Token),
    /// List of constants defined in this chunk
    constants: std.ArrayList(Value),

    pub fn init(allocator: Allocator) Self {
        return Self{
            .code = std.ArrayList(u32).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
            .lines = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Self, code: u32, where: Token) !void {
        try self.code.append(code);
        try self.lines.append(where);
    }

    pub fn addConstant(self: *Self, vm: ?*VM, value: Value) !u24 {
        if (vm) |uvm| uvm.push(value);
        try self.constants.append(value);
        if (vm) |uvm| _ = uvm.pop();

        return @as(u24, @intCast(self.constants.items.len - 1));
    }
};
