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
        .{ "tests/behavior/utf8.buzz", {} },
        .{ "examples/2048.buzz", {} },
        // statements that enforce newline before them don't take comment into account
        .{ "tests/behavior/anonymous-objects.buzz", {} },
        // bad indentation of inline if-else else branch
        .{ "tests/behavior/inline-if.buzz", {} },
        // something wrong with renderCopy signature
        .{ "examples/sdl-wrapped.buzz", {} },
    },
);

const ignore_dirs = std.StaticStringMap(void).initComptime(
    .{
        // These fixtures are intentionally invalid Buzz programs.
        .{ "tests/compile_errors", {} },
        .{ "tests/fuzzed", {} },
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
    var dlib = std.StringHashMapUnmanaged(Parser.Dlib).empty;
    var parser = Parser.init(
        process,
        &gc,
        &imports,
        &dlib,
        false,
        .Fmt,
    );

    var result = std.Io.Writer.Allocating.init(allocator);

    if (parser.parse(source, ".", null, file_name) catch null) |ast| {
        try Renderer.render(
            allocator,
            &result.writer,
            ast,
            .{},
        );

        try std.testing.expectEqualStrings(
            source,
            result.written(),
        );
    } else {
        try std.testing.expect(false);
    }
}

fn testFmtDir(process: std.process.Init, dir_name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var test_dir = try std.Io.Dir.cwd().openDir(
        process.io,
        dir_name,
        .{
            .iterate = true,
        },
    );
    defer test_dir.close(process.io);

    var it = test_dir.iterate();
    while (try it.next(process.io)) |entry| {
        const child_name = try std.fmt.allocPrint(
            allocator,
            "{s}/{s}",
            .{
                dir_name,
                entry.name,
            },
        );

        switch (entry.kind) {
            .directory => if (ignore_dirs.get(child_name) == null) {
                try testFmtDir(process, child_name);
            },
            .file => try testFmt(process, dir_name, entry),
            else => {},
        }
    }
}

/// Format a source snippet and compare it with the expected output.
fn expectFmtSource(source: []const u8, expected: []const u8, options: Renderer.Options) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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

    var gc = try GC.init(allocator);
    gc.type_registry = try TypeRegistry.init(&gc);
    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport).empty;
    var dlib = std.StringHashMapUnmanaged(Parser.Dlib).empty;
    var parser = Parser.init(
        process,
        &gc,
        &imports,
        &dlib,
        false,
        .Fmt,
    );

    var result = std.Io.Writer.Allocating.init(allocator);

    if (parser.parse(source, ".", null, "fmt-test.buzz") catch null) |ast| {
        try Renderer.render(
            allocator,
            &result.writer,
            ast,
            options,
        );

        try std.testing.expectEqualStrings(expected, result.written());
    } else {
        try std.testing.expect(false);
    }
}

const width_source =
    \\import "buzz:std";
    \\
    \\fun combine(first_argument_name: int, second_argument_name: int, third_argument_name: int) > int {
    \\    return first_argument_name + second_argument_name + third_argument_name;
    \\}
    \\
    \\test "fmt" {
    \\    final condition = "aaaaaaaaaaaaaaaa" == "aaaaaaaaaaaaaaaa" and "bbbbbbbbbbbbbbbb" == "bbbbbbbbbbbbbbbb" or "cccccccccccccccc" == "cccccccccccccccc";
    \\    _ = [ 111111111, 222222222, 333333333, 444444444 ];
    \\    _ = { "aaaaaaaaaaaa": 111111111, "bbbbbbbbbbbb": 222222222, "cccccccccccc": 333333333 };
    \\    std\assert(combine(111111111, second_argument_name: 222222222, third_argument_name: 333333333) == 666666666 and condition);
    \\}
    \\
;

test "fmt wraps long expressions at default line width" {
    try expectFmtSource(
        width_source,
        \\import "buzz:std";
        \\
        \\fun combine(
        \\    first_argument_name: int,
        \\    second_argument_name: int,
        \\    third_argument_name: int
        \\) > int {
        \\    return first_argument_name + second_argument_name + third_argument_name;
        \\}
        \\
        \\test "fmt" {
        \\    final condition =
        \\        "aaaaaaaaaaaaaaaa" == "aaaaaaaaaaaaaaaa"
        \\        and "bbbbbbbbbbbbbbbb" == "bbbbbbbbbbbbbbbb"
        \\        or "cccccccccccccccc" == "cccccccccccccccc";
        \\    _ = [ 111111111, 222222222, 333333333, 444444444 ];
        \\    _ = {
        \\        "aaaaaaaaaaaa": 111111111,
        \\        "bbbbbbbbbbbb": 222222222,
        \\        "cccccccccccc": 333333333
        \\    };
        \\    std\assert(
        \\        combine(
        \\            111111111,
        \\            second_argument_name: 222222222,
        \\            third_argument_name: 333333333
        \\        ) == 666666666 and condition
        \\    );
        \\}
        \\
    ,
        .{},
    );
}

test "fmt line width option controls comma wrapping" {
    try expectFmtSource(
        width_source,
        \\import "buzz:std";
        \\
        \\fun combine(
        \\    first_argument_name: int,
        \\    second_argument_name: int,
        \\    third_argument_name: int
        \\) > int {
        \\    return first_argument_name
        \\        + second_argument_name
        \\        + third_argument_name;
        \\}
        \\
        \\test "fmt" {
        \\    final condition =
        \\        "aaaaaaaaaaaaaaaa" == "aaaaaaaaaaaaaaaa"
        \\        and "bbbbbbbbbbbbbbbb" == "bbbbbbbbbbbbbbbb"
        \\        or "cccccccccccccccc"
        \\            == "cccccccccccccccc";
        \\    _ = [
        \\        111111111,
        \\        222222222,
        \\        333333333,
        \\        444444444
        \\    ];
        \\    _ = {
        \\        "aaaaaaaaaaaa": 111111111,
        \\        "bbbbbbbbbbbb": 222222222,
        \\        "cccccccccccc": 333333333
        \\    };
        \\    std\assert(
        \\        combine(
        \\            111111111,
        \\            second_argument_name: 222222222,
        \\            third_argument_name: 333333333
        \\        ) == 666666666 and condition
        \\    );
        \\}
        \\
    ,
        .{ .line_width = 50 },
    );
}

test "fmt custom wide line width keeps short-enough expressions inline" {
    try expectFmtSource(
        width_source,
        \\import "buzz:std";
        \\
        \\fun combine(first_argument_name: int, second_argument_name: int, third_argument_name: int) > int {
        \\    return first_argument_name + second_argument_name + third_argument_name;
        \\}
        \\
        \\test "fmt" {
        \\    final condition = "aaaaaaaaaaaaaaaa" == "aaaaaaaaaaaaaaaa" and "bbbbbbbbbbbbbbbb" == "bbbbbbbbbbbbbbbb" or "cccccccccccccccc" == "cccccccccccccccc";
        \\    _ = [ 111111111, 222222222, 333333333, 444444444 ];
        \\    _ = { "aaaaaaaaaaaa": 111111111, "bbbbbbbbbbbb": 222222222, "cccccccccccc": 333333333 };
        \\    std\assert(combine(111111111, second_argument_name: 222222222, third_argument_name: 333333333) == 666666666 and condition);
        \\}
        \\
    ,
        .{ .line_width = 200 },
    );
}

test "fmt wraps after assignment tokens and before binary operators" {
    try expectFmtSource(
        \\test "fmt" {
        \\    final lhs = 111111111 + 222222222 + 333333333 + 444444444 + 555555555 + 666666666 + 777777777;
        \\    var total = 0;
        \\    total += 111111111 + 222222222 + 333333333 + 444444444 + 555555555 + 666666666 + 777777777;
        \\    _ = lhs + total;
        \\}
        \\
    ,
        \\test "fmt" {
        \\    final lhs =
        \\        111111111 + 222222222 + 333333333 + 444444444 + 555555555 + 666666666
        \\        + 777777777;
        \\    var total = 0;
        \\    total +=
        \\        111111111 + 222222222 + 333333333 + 444444444 + 555555555 + 666666666
        \\        + 777777777;
        \\    _ = lhs + total;
        \\}
        \\
    ,
        .{},
    );
}

test "fmt prefers comma breaks over assignment breaks" {
    try expectFmtSource(
        \\test "fmt" {
        \\    final value = [ 111111111, 222222222, 333333333, 444444444 ];
        \\    _ = value;
        \\}
        \\
    ,
        \\test "fmt" {
        \\    final value = [
        \\        111111111,
        \\        222222222,
        \\        333333333,
        \\        444444444
        \\    ];
        \\    _ = value;
        \\}
        \\
    ,
        .{ .line_width = 40 },
    );
}

test "fmt prefers assignment breaks over boolean breaks" {
    try expectFmtSource(
        \\test "fmt" {
        \\    final condition = true and false or true and false;
        \\    _ = condition;
        \\}
        \\
    ,
        \\test "fmt" {
        \\    final condition =
        \\        true and false or true and false;
        \\    _ = condition;
        \\}
        \\
    ,
        .{ .line_width = 40 },
    );
}

test "fmt prefers boolean breaks over other binary breaks" {
    try expectFmtSource(
        \\fun check() > bool {
        \\    return 111111111 == 222222222 and 333333333 == 444444444;
        \\}
        \\
    ,
        \\fun check() > bool {
        \\    return 111111111 == 222222222
        \\        and 333333333 == 444444444;
        \\}
        \\
    ,
        .{ .line_width = 50 },
    );
}

test "fmt prefers binary breaks over member chain breaks" {
    try expectFmtSource(
        \\fun check() > bool {
        \\    return [ 1, 2, 3 ].copyImmutable().copyImmutable().len() == 333333333;
        \\}
        \\
    ,
        \\fun check() > bool {
        \\    return [ 1, 2, 3 ].copyImmutable().copyImmutable().len()
        \\        == 333333333;
        \\}
        \\
    ,
        .{ .line_width = 70 },
    );
}

test "fmt keeps multiline call opener after assignment when it fits" {
    try expectFmtSource(
        \\fun make(first: int, second: int, third: int) > int {
        \\    return first + second + third;
        \\}
        \\
        \\test "fmt" {
        \\    final value = make(
        \\        1,
        \\        second: 2,
        \\        third: 3,
        \\    );
        \\    _ = value;
        \\}
        \\
    ,
        \\fun make(first: int, second: int, third: int) > int {
        \\    return first + second + third;
        \\}
        \\
        \\test "fmt" {
        \\    final value = make(
        \\        1,
        \\        second: 2,
        \\        third: 3,
        \\    );
        \\    _ = value;
        \\}
        \\
    ,
        .{},
    );
}

test "fmt keeps multiline callback call opener after assignment when it fits" {
    try expectFmtSource(
        \\object List {
        \\    static fun init(items: [str], onSelectionChanged: fun (index: int, item: str) > void) > List {
        \\        _ = items;
        \\        _ = onSelectionChanged;
        \\        return List{};
        \\    }
        \\}
        \\
        \\test "fmt" {
        \\    var selected = "";
        \\    final list = List.init(
        \\        [ "one", "two", "three" ],
        \\        onSelectionChanged: fun (index: int, item: str) > void {
        \\            selected = "{index}:{item}";
        \\        },
        \\    );
        \\    _ = list;
        \\}
        \\
    ,
        \\object List {
        \\    static fun init(
        \\        items: [str],
        \\        onSelectionChanged: fun (index: int, item: str) > void
        \\    ) > List {
        \\        _ = items;
        \\        _ = onSelectionChanged;
        \\        return List{};
        \\    }
        \\}
        \\
        \\test "fmt" {
        \\    var selected = "";
        \\    final list = List.init(
        \\        [ "one", "two", "three" ],
        \\        onSelectionChanged: fun (index: int, item: str) > void {
        \\            selected = "{index}:{item}";
        \\        },
        \\    );
        \\    _ = list;
        \\}
        \\
    ,
        .{},
    );
}

test "fmt preserves anonymous object call parentheses choice" {
    try expectFmtSource(
        \\object Payload{data:str, fun join(other:Payload)=>"{this.data}:{other.data}";}
        \\fun callMe(payload:Payload)=>payload.data.len();
        \\test "fmt"{final payload=Payload{data="hello"}; _ = callMe .{data="hello"}; _ = callMe(.{data="hello"}); _ = payload.join .{data="world"}; _ = payload.join(.{data="world"});}
        \\
    ,
        \\object Payload {
        \\    data: str,
        \\
        \\    fun join(other: Payload) => "{this.data}:{other.data}";
        \\}
        \\
        \\fun callMe(payload: Payload) => payload.data.len();
        \\
        \\test "fmt" {
        \\    final payload = Payload{ data = "hello" };
        \\    _ = callMe .{ data = "hello" };
        \\    _ = callMe(.{ data = "hello" });
        \\    _ = payload.join .{ data = "world" };
        \\    _ = payload.join(.{ data = "world" });
        \\}
        \\
    ,
        .{},
    );
}

test "fmt preserves nested string interpolation literals" {
    try expectFmtSource(
        \\test "fmt"{final tag="div"; _ = "<{"{tag}"}>";}
        \\
    ,
        \\test "fmt" {
        \\    final tag = "div";
        \\    _ = "<{"{tag}"}>";
        \\}
        \\
    ,
        .{},
    );
}

test "fmt preserves deeply nested string interpolation literals" {
    try expectFmtSource(
        \\test "fmt"{final tag="div"; _ = "<{"<{"<{"{tag}"}>"}>"}>";}
        \\
    ,
        \\test "fmt" {
        \\    final tag = "div";
        \\    _ = "<{"<{"<{"{tag}"}>"}>"}>";
        \\}
        \\
    ,
        .{},
    );
}

test "fmt keeps quoted string interpolations on one line" {
    try expectFmtSource(
        \\test "fmt"{final tag="abc"; final n=3; _="<{"x-{"{tag.len() + n}:{tag.sub(1)}"}"}>";}
        \\
    ,
        \\test "fmt" {
        \\    final tag = "abc";
        \\    final n = 3;
        \\    _ = "<{"x-{"{tag.len() + n}:{tag.sub(1)}"}"}>";
        \\}
        \\
    ,
        .{},
    );
}

test "fmt handles optional typed parameters with defaults" {
    try expectFmtSource(
        \\object Style {}
        \\object Box::<T> {
        \\    value: T,
        \\}
        \\object Handler {
        \\    onSelectionChanged: fun (index: int, item: str) > void?,
        \\}
        \\
        \\fun draw(style: Style? = null) > void {
        \\    _ = style;
        \\}
        \\
        \\fun keepTrailing(
        \\    style: Style?,
        \\) > void {
        \\    _ = style;
        \\}
        \\
        \\fun drawGeneric(box: Box::<int>? = null) > void {
        \\    _ = box;
        \\}
        \\
        \\fun setHandler(onSelectionChanged: fun (index: int, item: str) > void? = null) > void {
        \\    _ = onSelectionChanged;
        \\}
        \\
    ,
        \\object Style {}
        \\
        \\object Box::<T> {
        \\    value: T,
        \\}
        \\
        \\object Handler {
        \\    onSelectionChanged: fun (index: int, item: str) > void?,
        \\}
        \\
        \\fun draw(style: Style? = null) > void {
        \\    _ = style;
        \\}
        \\
        \\fun keepTrailing(
        \\    style: Style?,
        \\) > void {
        \\    _ = style;
        \\}
        \\
        \\fun drawGeneric(box: Box::<int>? = null) > void {
        \\    _ = box;
        \\}
        \\
        \\fun setHandler(
        \\    onSelectionChanged: fun (index: int, item: str) > void? = null
        \\) > void {
        \\    _ = onSelectionChanged;
        \\}
        \\
    ,
        .{},
    );
}

test "fmt wraps function signature suffixes after their prefixes" {
    try expectFmtSource(
        \\object VeryLongReturnValueName {}
        \\object ExtraordinarilyLongFirstErrorName {}
        \\object ExtraordinarilyLongSecondErrorName {}
        \\object ExtraordinarilyLongThirdErrorName {}
        \\
        \\extern fun stream(input: str, output: str, retries: int) > VeryLongReturnValueName !> ExtraordinarilyLongFirstErrorName, ExtraordinarilyLongSecondErrorName, ExtraordinarilyLongThirdErrorName;
        \\
    ,
        \\object VeryLongReturnValueName {}
        \\
        \\object ExtraordinarilyLongFirstErrorName {}
        \\
        \\object ExtraordinarilyLongSecondErrorName {}
        \\
        \\object ExtraordinarilyLongThirdErrorName {}
        \\
        \\extern fun stream(
        \\    input: str,
        \\    output: str,
        \\    retries: int
        \\) > VeryLongReturnValueName
        \\    !>
        \\    ExtraordinarilyLongFirstErrorName,
        \\    ExtraordinarilyLongSecondErrorName,
        \\    ExtraordinarilyLongThirdErrorName;
        \\
    ,
        .{},
    );
}

test "fmt wraps member chains before the member separator" {
    try expectFmtSource(
        \\import "buzz:std";
        \\
        \\test "fmt" {
        \\    std\assert([ 1, 2, 3 ].copyImmutable().copyImmutable().copyImmutable().copyImmutable().len() == 3);
        \\}
        \\
    ,
        \\import "buzz:std";
        \\
        \\test "fmt" {
        \\    std\assert(
        \\        [ 1, 2, 3 ].copyImmutable().copyImmutable()
        \\            .copyImmutable().copyImmutable().len() == 3
        \\    );
        \\}
        \\
    ,
        .{ .line_width = 60 },
    );
}

test "fmt keeps interpolated string expressions inline" {
    try expectFmtSource(
        \\test "fmt" {
        \\    final some_really_long_identifier = "abc";
        \\    _ = "Syntax error: {some_really_long_identifier.sub(0).sub(0).sub(0).sub(0).len()}";
        \\}
        \\
        \\/// A docblock after the string
        \\fun after() {}
        \\
    ,
        \\test "fmt" {
        \\    final some_really_long_identifier =
        \\        "abc";
        \\    _ =
        \\        "Syntax error: {some_really_long_identifier.sub(0).sub(0).sub(0).sub(0).len()}";
        \\}
        \\
        \\/// A docblock after the string
        \\fun after() {}
        \\
    ,
        .{ .line_width = 40 },
    );
}

test "fmt does not render comments while inside interpolated strings" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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

    const source =
        \\test "fmt" {
        \\    final char = "x";
        \\    _ = "Unexpected character `{char}`";
        \\}
        \\
        \\/// Errors raised while decoding TOML.
        \\fun after() {}
        \\
    ;

    var gc = try GC.init(allocator);
    gc.type_registry = try TypeRegistry.init(&gc);
    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport).empty;
    var dlib = std.StringHashMapUnmanaged(Parser.Dlib).empty;
    var parser = Parser.init(
        process,
        &gc,
        &imports,
        &dlib,
        false,
        .Fmt,
    );

    var result = std.Io.Writer.Allocating.init(allocator);

    if (parser.parse(source, ".", null, "fmt-test.buzz") catch null) |parsed| {
        var ast = parsed;
        const token_tags = ast.tokens.items(.tag);
        const lexemes = ast.tokens.items(.lexeme);
        const utility = ast.tokens.items(.utility_token);

        var docblock_token: ?usize = null;
        for (token_tags, 0..) |tag, i| {
            if (tag == .Docblock) {
                docblock_token = i;
                break;
            }
        }

        var char_token: ?usize = null;
        for (token_tags[0..docblock_token.?], 0..) |tag, i| {
            if (tag == .Identifier and std.mem.eql(u8, lexemes[i], "char")) {
                char_token = i;
            }
        }

        // Simulate stale LSP token walking where the interpolation expression
        // skips forward to a later declaration docblock.
        for (char_token.? + 1..docblock_token.? + 1) |i| {
            utility[i] = true;
        }

        try Renderer.render(
            allocator,
            &result.writer,
            ast,
            .{},
        );

        try std.testing.expect(std.mem.indexOf(u8, result.written(), "Unexpected character `{char}`") != null);
        try std.testing.expect(std.mem.indexOf(u8, result.written(), "Unexpected character `{char\n") == null);
    } else {
        try std.testing.expect(false);
    }
}

test "fmt keeps match condition commas before wrapped newlines" {
    try expectFmtSource(
        \\enum TokenTag {
        \\    LeftBracket,
        \\    BareKey,
        \\    LiteralString,
        \\}
        \\
        \\test "fmt" {
        \\    final tag: TokenTag = .LeftBracket;
        \\    _ = match (tag) {
        \\        .LeftBracket,
        \\        .BareKey,
        \\        .LiteralString,
        \\        -> true,
        \\        else -> false,
        \\    };
        \\}
        \\
    ,
        \\enum TokenTag {
        \\    LeftBracket,
        \\    BareKey,
        \\    LiteralString,
        \\}
        \\
        \\test "fmt" {
        \\    final tag: TokenTag = .LeftBracket;
        \\    _ = match (tag) {
        \\        .LeftBracket,
        \\        .BareKey,
        \\        .LiteralString,
        \\        -> true,
        \\        else -> false,
        \\    };
        \\}
        \\
    ,
        .{},
    );
}

test "fmt tolerates stale function type metadata" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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

    const source =
        \\namespace fmt_test;
        \\
        \\fun parse(source: str) > str => source;
        \\
        \\export fun decode(source: str) > str => parse(source);
        \\
    ;

    var gc = try GC.init(allocator);
    gc.type_registry = try TypeRegistry.init(&gc);
    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport).empty;
    var dlib = std.StringHashMapUnmanaged(Parser.Dlib).empty;
    var parser = Parser.init(
        process,
        &gc,
        &imports,
        &dlib,
        false,
        .Fmt,
    );

    var result = std.Io.Writer.Allocating.init(allocator);

    if (parser.parse(source, ".", null, "fmt-test.buzz") catch null) |parsed| {
        var ast = parsed;
        const tags = ast.nodes.items(.tag);
        const type_defs = ast.nodes.items(.type_def);
        for (tags, 0..) |tag, i| {
            if (tag == .Function) {
                type_defs[i] = gc.type_registry.int_type;
            }
        }

        try Renderer.render(
            allocator,
            &result.writer,
            ast,
            .{},
        );

        try std.testing.expect(result.written().len > 0);
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
        try testFmtDir(process, dir);
    }
}
