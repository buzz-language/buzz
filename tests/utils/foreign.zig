const std = @import("std");

export fn acos(value: f64) callconv(.c) f64 {
    return std.math.acos(value);
}

export fn fprint(msg: [*:0]const u8) callconv(.c) void {
    std.debug.print("{s}\n", .{msg});
}

export fn sum(values: [*]i32, len: i32) callconv(.c) i32 {
    var total: i32 = 0;
    for (0..@intCast(len)) |i| {
        total += values[i];
    }

    return total;
}

pub const Data = extern struct {
    id: i32,
    msg: [*:0]u8,
    value: f64,
};

export fn get_data_msg(data: *Data) callconv(.c) [*:0]u8 {
    return data.msg;
}

export fn set_data_id(data: *Data) callconv(.c) void {
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

export fn get_misc_msg(misc: *Misc) callconv(.c) [*:0]u8 {
    return misc.data.msg;
}

export fn get_misc_flag(misc: *Misc) callconv(.c) bool {
    std.debug.print(
        "> {x:0>2}\n",
        .{
            std.mem.asBytes(misc),
        },
    );
    return misc.flag.value;
}

export fn set_misc_id(misc: *Misc, new_id: i32) callconv(.c) void {
    misc.id = new_id;
}
