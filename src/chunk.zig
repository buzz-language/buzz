const std = @import("std");
const Allocator = std.mem.Allocator;
const _value = @import("./value.zig");
const _vm = @import("./vm.zig");
const VM = _vm.VM;
const Value = _value.Value;

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
    OP_FOREACH,

    OP_CALL,
    OP_INVOKE,

    OP_CLOSURE,
    OP_CLOSE_UPVALUE,

    OP_ROUTINE,
    OP_INVOKE_ROUTINE,
    OP_RESUME,
    OP_RESOLVE,
    OP_YIELD,

    OP_TRY,
    OP_TRY_END,
    OP_THROW,

    OP_RETURN,

    OP_OBJECT,
    OP_INSTANCE,
    OP_METHOD,
    OP_PROPERTY,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,

    OP_ENUM,
    OP_ENUM_CASE,
    OP_GET_ENUM_CASE,
    OP_GET_ENUM_CASE_VALUE,
    OP_GET_ENUM_CASE_FROM_VALUE,

    OP_LIST,
    OP_LIST_APPEND,

    OP_MAP,
    OP_SET_MAP,

    OP_EXPORT,
    OP_IMPORT,

    OP_TO_STRING,
};

/// A chunk of code to execute
pub const Chunk = struct {
    const Self = @This();

    pub const max_constants: u24 = 16777215;

    /// List of opcodes to execute
    code: std.ArrayList(u32),
    /// List of lines
    lines: std.ArrayList(usize),
    /// List of constants defined in this chunk
    constants: std.ArrayList(Value),

    pub fn init(allocator: Allocator) Self {
        return Self{
            .code = std.ArrayList(u32).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
            .lines = std.ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Self, code: u32, line: usize) !void {
        _ = try self.code.append(code);
        _ = try self.lines.append(line);
    }

    pub fn addConstant(self: *Self, vm: ?*VM, value: Value) !u24 {
        if (vm) |uvm| uvm.push(value);
        try self.constants.append(value);
        if (vm) |uvm| _ = uvm.pop();

        return @intCast(u24, self.constants.items.len - 1);
    }
};
