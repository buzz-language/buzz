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
    id: i32,
    msg: [*:0]u8,
};

export fn get_data_msg(data: *Data) [*:0]u8 {
    return data.msg;
}

export fn set_data_id(data: *Data) void {
    data.id *= 2;
}

pub const Flag = extern struct {
    id: i32,
    value: bool,
};

pub const Misc = extern union {
    id: i32,
    data: Data,
    flag: Flag,
};

export fn get_misc_msg(misc: *Misc) [*:0]u8 {
    return misc.data.msg;
}

export fn get_misc_flag(misc: *Misc) bool {
    return misc.flag.value;
}

export fn set_misc_id(misc: *Misc, new_id: i32) void {
    misc.id = new_id;
}
