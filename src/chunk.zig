const std = @import("std");
const Allocator = std.mem.Allocator;
const compiler = @import("./compiler.zig");
const _value = @import("./value.zig");
const _vm = @import("./vm.zig");
const VM = _vm.VM;
const Value = _value.Value;

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_NULL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    // TODO: could be OP_GET_PROPERTY/OP_SET_PROPERTY when object use index instead of names
    OP_GET_SUBSCRIPT,
    OP_SET_SUBSCRIPT,
    OP_GET_SUPER,

    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_MOD,
    // OP_BAND,
    // OP_BOR,
    // OP_XOR,
    OP_SHL,
    OP_SHR,

    OP_UNWRAP,

    OP_NOT,
    OP_NEGATE,
    
    OP_SWAP,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,

    OP_CALL,
    OP_INVOKE,
    OP_SUPER_INVOKE,

    OP_CLOSURE,
    OP_CLOSE_UPVALUE,

    OP_THROW,
    OP_CATCH,
    OP_RETURN,

    OP_CLASS,
    OP_OBJECT,
    OP_INHERIT,
    OP_METHOD,
    OP_PROPERTY,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,

    OP_ENUM,
    OP_ENUM_CASE,
    OP_GET_ENUM_CASE,
    OP_GET_ENUM_CASE_VALUE,

    OP_LIST,
    OP_LIST_APPEND,

    OP_MAP,
    OP_SET_MAP,

    // TODO: remove
    OP_PRINT,
};

/// A chunk of code to execute
pub const Chunk = struct {
    const Self = @This();

    pub const max_constants: usize = 255;

    /// List of opcodes to execute
    code: std.ArrayList(u8),
    /// List of lines
    lines: std.ArrayList(usize),
    /// List of constants defined in this chunk
    constants: std.ArrayList(Value),

    // TODO: correlate opcodes and line number in source code

    pub fn init(allocator: *Allocator) !Self {
        return Self {
            .code = std.ArrayList(u8).init(allocator),
            .constants = try std.ArrayList(Value).initCapacity(allocator, max_constants),
            .lines = std.ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Self, byte: u8, line: usize) !void {
        _ = try self.code.append(byte);
        _ = try self.lines.append(line);
    }

    pub fn addConstant(self: *Self, vm: *VM, value: Value) !u8 {
        vm.push(value);
        try self.constants.append(value);
        _ = vm.pop();

        return @intCast(u8, self.constants.items.len - 1);
    }
};