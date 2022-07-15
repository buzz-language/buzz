const builtin = @import("builtin");

pub const Config = .{
    .debug = builtin.mode == .Debug and false,
    .debug_stack = builtin.mode == .Debug and false,
    .debug_gc = builtin.mode == .Debug and false,
    .debug_current_instruction = builtin.mode == .Debug and false,
};
