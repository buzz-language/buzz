const std = @import("std");
const compiler = @import("./compiler.zig");
const OpCode = compiler.OpCode;
const value = @import("./value.zig");
const ValueType = value.ValueType;

/// A chunk of code to execute
pub const Chunk = struct {
    /// List of opcodes to execute
    code: []OpCode,
    /// List of constants defined in this chunck
    constants: std.ArrayList(ValueType),
};