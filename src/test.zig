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