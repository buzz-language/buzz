const builtin = @import("builtin");

pub const Config = .{
    .debug = builtin.mode == .Debug and false,
    .debug_gc = builtin.mode == .Debug and false,
};