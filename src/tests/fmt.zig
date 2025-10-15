const std = @import("std");
const assert = std.debug.assert;
const Ast = @import("../Ast.zig");
const Parser = @import("../Parser.zig");
const GC = @import("../GC.zig");
const TypeRegistry = @import("../TypeRegistry.zig");
const Renderer = @import("../renderer.zig").Renderer;

const ignore = std.StaticStringMap(void).initComptime(
    .{
        // stringEscape escapes utf-8 graphemes (emojis, etc.)
        .{ "tests/061-utf8.buzz", {} },
        .{ "examples/2048.buzz", {} },
        // statements that enforce newline before them don't take comment into account
        .{ "tests/042-anonymous-objects.buzz", {} },
        // bad indentation of inline if-else else branch
        .{ "tests/052-inline-if.buzz", {} },
        // something wrong with renderCopy signature
        .{ "examples/sdl-wrapped.buzz", {} },
    },
);

fn testFmt(prefix: []const u8, entry: std.fs.Dir.Entry) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    if (entry.kind != .file or !std.mem.endsWith(u8, entry.name, ".buzz")) {
        return;
    }

    var file_name = try allocator.alloc(u8, prefix.len + entry.name.len + 1);
    file_name = try std.fmt.bufPrint(
        file_name,
        "{s}/{s}",
        .{
            prefix,
            entry.name,
        },
    );

    if (ignore.get(file_name) != null) {
        return;
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

    const source = try allocator.alloc(u8, (try file.stat()).size);

    _ = try file.readAll(source);

    var gc = try GC.init(allocator);
    gc.type_registry = try TypeRegistry.init(&gc);
    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport){};
    var parser = Parser.init(
        &gc,
        &imports,
        false,
        .Fmt,
    );

    var result = std.Io.Writer.Allocating.init(allocator);

    if (parser.parse(source, file_name, file_name) catch null) |ast| {
        try Renderer.render(
            allocator,
            &result.writer,
            ast,
        );

        try std.testing.expectEqualStrings(
            source,
            result.written(),
        );
    } else {
        try std.testing.expect(false);
    }
}

test "fmt" {
    inline for (.{ "tests", "examples" }) |dir| {
        var test_dir = try std.fs.cwd().openDir(
            dir,
            .{
                .iterate = true,
            },
        );
        var it = test_dir.iterate();

        while (try it.next()) |entry| {
            try testFmt(dir, entry);
        }
    }
}
