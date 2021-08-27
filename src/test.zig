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