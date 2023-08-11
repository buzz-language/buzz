const std = @import("std");

export fn acos(value: f64) f64 {
    return std.math.acos(value);
}

export fn fprint(msg: [*:0]const u8) void {
    std.debug.print("{s}\n", .{msg});
}

export fn sum(values: [*]i32, len: i32) i32 {
    var total: i32 = 0;
    for (0..@intCast(len)) |i| {
        total += values[i];
    }

    return total;
}

pub const Data = extern struct {
    msg: [*:0]u8,
    id: i32,
};

export fn getDataMsg(data: *Data) [*:0]u8 {
    return data.msg;
}

export fn setDataId(data: *Data) void {
    data.id *= 2;
}
