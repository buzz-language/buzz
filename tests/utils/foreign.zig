const std = @import("std");

export fn acos(value: f64) f64 {
    std.debug.print("Called with {}, should return {}\n", .{ value, std.math.acos(value) });
    return std.math.acos(value);
}
