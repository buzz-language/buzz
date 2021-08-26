const std = @import("std");
const compiler = @import("./compiler.zig");
const value = @import("./value.zig");
const ValueType = value.ValueType;

pub const OpCode = enum {
    OP_CONSTANT,
    OP_NULL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
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
    OP_SHL,
    OP_SHR,

    OP_UNWRAP,

    OP_NOT,
    OP_NEGATE,
    
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,

    OP_CALL,
    OP_INVOKE,
    OP_SUPER_INVOKE,

    OP_CLOSURE,
    OP_CLOSE_UPVALUE,

    OP_RETURN,

    OP_CLASS,
    OP_OBJECT,
    OP_INHERIT,
    OP_METHOD,
    OP_PROPERTY,
};

/// A chunk of code to execute
pub const Chunk = struct {
    /// List of opcodes to execute
    code: []OpCode,
    /// List of constants defined in this chunck
    constants: std.ArrayList(ValueType),

    // TODO: correlate opcodes and line number in source code
};