const std = @import("std");
const assert = std.debug.assert;
const Ast = @import("../Ast.zig");
const Parser = @import("../Parser.zig");
const mem = @import("../memory.zig");
const Renderer = @import("../renderer.zig").Renderer;

fn testFmt(
    comptime name: []const u8,
    comptime src: []const u8,
    comptime expected: []const u8,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc = try mem.GarbageCollector.init(allocator);
    gc.type_registry = try mem.TypeRegistry.init(&gc);
    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport){};
    var parser = Parser.init(
        &gc,
        &imports,
        false,
        .Fmt,
    );

    var result = std.ArrayList(u8).init(allocator);

    if (parser.parse(src, name, name) catch null) |ast| {
        try Renderer(std.ArrayList(u8).Writer).render(
            allocator,
            result.writer(),
            ast,
        );

        try std.testing.expectEqualStrings(
            expected,
            result.items,
        );
    } else {
        try std.testing.expect(false);
    }
}

test "fmt" {
    try testFmt(
        "simple function",
        \\fun hello() > void *> void// bitch comment
        \\!> int, str {
        \\return "hello";
        \\}
    ,
        \\fun hello() > void *> void // bitch comment
        \\    !> int, str {
        \\    return "hello";
        \\}
        \\
        ,
    );

    try testFmt(
        "arrow function",
        \\fun arrow()// bitch comment
        \\=> "hello";
    ,
        \\fun arrow() // bitch comment
        \\    => "hello";
        \\
        ,
    );

    try testFmt(
        "arrow function",
        \\fun returnObj(_: obj{ yo: int, lo: bool}) > obj{ yo: int, lo: bool}{
        \\    return .{yo = 12,
        \\    // bitch comment
        \\    lo = false};
        \\}
    ,
        \\fun returnObj(_: obj{ yo: int, lo: bool }) > obj{ yo: int, lo: bool } {
        \\    return .{
        \\        yo = 12,
        \\            // bitch comment
        \\        lo = false,
        \\    };
        \\}
        \\
        ,
    );
}
