const std = @import("std");
const compiler = @import("./compiler.zig");
const value = @import("./value.zig");
const ValueType = value.ValueType;

pub const OpCode = enum {
    
};

/// A chunk of code to execute
pub const Chunk = struct {
    /// List of opcodes to execute
    code: []OpCode,
    /// List of constants defined in this chunck
    constants: std.ArrayList(ValueType),

    // TODO: correlate opcodes and line number in source code
};