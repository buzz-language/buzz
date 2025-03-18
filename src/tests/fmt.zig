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
) !?Ast {
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

    if (parser.parse(src, name, name)) |ast| {
        try Renderer(std.ArrayList(u8).Writer).render(
            allocator,
            result.writer(),
            ast,
        );

        std.testing.expectEqualSlices(
            u8,
            expected,
            src,
        );
    } else {
        std.testing.expect(false);
    }
}

test "fmt" {
    testFmt(
        "simple function",
        \\fun hello() > void *> void//bitch comment
        \\!> int, str {
        \\return "hello";
        \\}
    ,
        \\fun hello() > void *> void // bitch comment
        \\    !> int, str {
        \\    return "hello";
        \\}
        ,
    );
}
