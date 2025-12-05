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

fn testFmt(process: std.process.Init, prefix: []const u8, entry: std.Io.Dir.Entry) !void {
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

    var file = (if (std.fs.path.isAbsolute(
        file_name,
    ))
        std.Io.Dir.openFileAbsolute(process.io, file_name, .{})
    else
        std.Io.Dir.cwd().openFile(process.io, file_name, .{})) catch {
        std.debug.print("File not found", .{});
        return;
    };
    defer file.close(process.io);

    const source = try allocator.alloc(u8, (try file.stat(process.io)).size);

    _ = try file.readPositionalAll(process.io, source, 0);

    var gc = try GC.init(allocator);
    gc.type_registry = try TypeRegistry.init(&gc);
    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport).empty;
    var parser = Parser.init(
        process,
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var environ_map = std.process.Environ.Map.init(std.testing.allocator);
    defer environ_map.deinit();
    try environ_map.put("BUZZ_PATH", "zig-out");

    const process = std.process.Init{
        .minimal = .{
            .environ = .empty,
            .args = .{ .vector = &.{} },
        },
        .arena = &arena,
        .gpa = std.testing.allocator,
        .io = std.testing.io,
        .environ_map = &environ_map,
        .preopens = .empty,
    };

    inline for (.{ "tests", "examples" }) |dir| {
        var test_dir = try std.Io.Dir.cwd().openDir(
            process.io,
            dir,
            .{
                .iterate = true,
            },
        );
        defer test_dir.close(process.io);
        var it = test_dir.iterate();

        while (try it.next(process.io)) |entry| {
            try testFmt(process, dir, entry);
        }
    }
}
