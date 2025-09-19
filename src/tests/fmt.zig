const std = @import("std");
const assert = std.debug.assert;
const Ast = @import("../Ast.zig");
const Parser = @import("../Parser.zig");
const GC = @import("../GC.zig");
const TypeRegistry = @import("../TypeRegistry.zig");
const Renderer = @import("../renderer.zig").Renderer;
const WriteableArrayList = @import("../writeable_array_list.zig").WriteableArrayList;

fn testFmt(
    name: []const u8,
    src: []const u8,
    expected: []const u8,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc = try GC.init(allocator);
    gc.type_registry = try TypeRegistry.init(&gc);
    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport){};
    var parser = Parser.init(
        &gc,
        &imports,
        false,
        .Fmt,
    );

    var result = WriteableArrayList(u8).init(allocator);

    if (parser.parse(src, name, name) catch null) |ast| {
        try Renderer.render(
            allocator,
            &result.writer,
            ast,
        );

        try std.testing.expectEqualStrings(
            expected,
            result.list.items,
        );
    } else {
        try std.testing.expect(false);
    }
}

test "fmt" {
    const ignore = std.StaticStringMap(void).initComptime(
        .{
            // stringEscape escapes utf-8 graphemes (emojis)
            .{ "tests/061-utf8.buzz", {} },
            // statements that enforce newline before them don't take comment into account
            .{ "tests/042-anonymous-objects.buzz", {} },
            // bad identation of inline if-else else branch
            .{ "tests/052-inline-if.buzz", {} },
        },
    );

    var test_dir = try std.fs.cwd().openDir(
        "tests",
        .{
            .iterate = true,
        },
    );
    var it = test_dir.iterate();

    while (try it.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".buzz")) {
            var file_name = try std.testing.allocator.alloc(u8, 6 + entry.name.len);
            defer std.testing.allocator.free(file_name);
            file_name = try std.fmt.bufPrint(
                file_name,
                "tests/{s}",
                .{entry.name},
            );

            if (ignore.get(file_name) != null) {
                continue;
            }

            std.debug.print("\n{s}\n", .{file_name});

            var file = (if (std.fs.path.isAbsolute(
                file_name,
            ))
                std.fs.openFileAbsolute(file_name, .{})
            else
                std.fs.cwd().openFile(file_name, .{})) catch {
                std.debug.print("File not found", .{});
                return;
            };
            defer file.close();

            const source = try std.testing.allocator.alloc(u8, (try file.stat()).size);
            defer std.testing.allocator.free(source);

            _ = try file.readAll(source);

            try testFmt(
                file_name,
                source,
                source,
            );
        }
    }
}
