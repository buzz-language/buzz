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
    var map = std.StringHashMap(bool).init(std.heap.c_allocator);
    defer map.deinit();

    try map.put("hello", true);

    std.debug.warn("\nhello exists: {}\n", .{ map.get("hello") });

    var dyn_string: []u8 = try std.heap.c_allocator.alloc(u8, 3);
    defer std.heap.c_allocator.free(dyn_string);

    _ = try std.fmt.bufPrint(dyn_string, "bye", .{});

    try map.put(dyn_string, true);

    std.debug.warn("\nbye exists: {}\n", .{ map.get("bye") });
}

test "Is hash the same for 2 instance of a struct with same data?" {
    const A = struct {
        a: bool,
        b: bool,
        c: u8
    };

    var map = std.AutoArrayHashMap(A, *A).init(std.heap.c_allocator);
    defer map.deinit();

    var a: A = .{
        .a = true,
        .b = false,
        .c = 1
    };

    var b: A = .{
        .a = true,
        .b = false,
        .c = 1
    };

    try map.put(a, &a);

    std.debug.warn("\nmap(b) == a ? {}\n", .{ map.get(b) == &a });
}

test "Can i use StringHashMap with a []const u8 as key?" {
    var map = std.StringHashMap(bool).init(std.heap.c_allocator);
    defer map.deinit();

    try map.put("yolo", true);
}

fn say(hello: []const u8) void {
    std.debug.warn("{s}\n", .{ hello });
}

test "Can i put a []u8 in a []const u8?" {
    var str: []const u8 = try std.heap.c_allocator.alloc(u8, 5);
    defer std.heap.c_allocator.free(str);

    say(str);
}

test "How do first-class function work?" {
    const A = struct {
        say: fn ([]const u8) void,
    };

    const a: A = .{
        .say = say
    };

    a.say("hello");
}

test "Where can i omit struct name ?" {
    const A = struct {
        hello: bool
    };

    const array = [_]A { .{ .hello = true } };

    std.debug.warn("array[0] {}\n", .{array[0]});
}

test "StringHashMap wat?" {
    var map = std.StringArrayHashMap(bool).init(std.heap.c_allocator);
    defer map.deinit();

    try map.put("hello", true);

    var hello = try std.heap.c_allocator.alloc(u8, 5);
    _ = try std.fmt.bufPrint(hello, "hello", .{});

    std.debug.warn("hello? {}\n", .{ map.get(hello) });
}