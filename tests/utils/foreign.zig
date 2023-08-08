const std = @import("std");

export fn acos(value: f64) f64 {
    return std.math.acos(value);
}

export fn fprint(msg: [*:0]const u8) void {
    std.debug.print("{s}\n", .{msg});
}
