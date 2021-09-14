const std = @import("std");

test "split buffer by newline" {
    const source = "hello\nworld\nmother\nfucker\n";

    var lines = std.ArrayList([]const u8).init(std.testing.allocator);
    defer lines.deinit();

    var it = std.mem.split(u8, source, "\n");
    while (it.next()) |line| {
        try lines.append(line);
    }

    std.debug.assert(lines.items.len == 5);
}

test "Get next enum case by index" {
    const Enum = enum {
        one,
        two,
        three
    };

    std.debug.warn("After one comes {}\n", .{ @intToEnum(Enum, @enumToInt(Enum.one) + 1) });
}

test "bitwise" {
    const value: u16 = 2000;
    const byte1: u8 = @intCast(u8, (value >> 8) & 0xff);
    const byte2: u8 = @intCast(u8, value & 0xff);

    std.debug.warn("{x} {x} = {}\n", .{
        byte1,
        byte2,
        (@intCast(u16, byte1) << 8) | @intCast(u16, byte2)
    });
}