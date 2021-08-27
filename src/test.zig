const std = @import("std");
const assert = std.debug.assert;

test "slice.len type?" {
    const hello: []const u8 = "hello";

    std.debug.warn("\nslice.len is a {s}\n", .{@TypeOf(hello.len)});
}

test "pointers behave like C" {
    var array = [_]u8{ 1, 2, 3, 4 };
    var ptr: [*]u8 = array[0..];

    std.debug.warn("\n{}, {}, {}, {}\n", .{
        (ptr)[0],
        (ptr + 1)[0],
        (ptr + 2)[0],
        (ptr + 3)[0],
    });
}

test "How are enum printed?" {
    const OpCode = enum(u8) {
        OP_CONSTANT,
        OP_NULL,
        OP_TRUE,
        OP_FALSE,
        OP_POP,
    };


    std.debug.warn("\nSome -> {}\n", .{ OpCode.OP_CONSTANT });
}

test "Do i need to unwrap optional when printing" {
    const hello: ?[]const u8 = "hello";

    std.debug.warn("\n{s}\n", .{ hello });
}

test "Iterate from end of slice" {
    var array = [_]u8{ 1, 2, 3, 4 };

    var i: usize = array.len - 1;
    while (i >= 0) {
        std.debug.warn("\n{}\n", .{ array[i] });

        if (i > 0) i -= 1 else break;
    }
}

test "What does a switch block returns?" {
    const Yo = enum {
        one, two, three
    };

    std.debug.warn("\n{}\n", .{
        switch (Yo.one) {
            .one => one: {
                std.debug.warn("\n in my case, in my case\n", .{});

                break :one 12;
            },
            else => 13
        }
    });
}

test "How can store sizeOf things" {
    const Yo = struct {
        lo: bool
    };
    
    const size: usize = @sizeOf(Yo);

    std.debug.warn("\nsize of Yo is {}\n", .{ size });
}

test "String hash map behave like expected" {
    var map = std.StringArrayHashMap(bool).init(std.heap.c_allocator);
    defer map.deinit();

    try map.put("hello", true);

    std.debug.warn("\nhello exists: {}\n", .{ map.get("hello") });

    var dyn_string: []u8 = try std.heap.c_allocator.alloc(u8, 3);
    defer std.heap.c_allocator.free(dyn_string);

    _ = try std.fmt.bufPrint(dyn_string, "bye", .{});

    try map.put(dyn_string, true);

    std.debug.warn("\nbye exists: {}\n", .{ map.get("bye") });
}