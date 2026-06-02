const std = @import("std");
const Ast = @import("Ast.zig");
const GC = @import("GC.zig");
const Parser = @import("Parser.zig");
const TypeRegistry = @import("TypeRegistry.zig");
const obj = @import("obj.zig");
const RunFlavor = @import("vm.zig").RunFlavor;

const Allocator = std.mem.Allocator;

pub fn main(init: std.process.Init) !u8 {
    var args = try std.process.Args.Iterator.initAllocator(init.minimal.args, init.gpa);
    defer args.deinit();

    _ = args.next();
    const output_dir = args.next() orelse {
        std.debug.print("usage: update_std_docs <output-dir>\n", .{});
        return 1;
    };
    if (args.next() != null) {
        std.debug.print("usage: update_std_docs <output-dir>\n", .{});
        return 1;
    }

    try updateStdDocs(init, init.gpa, output_dir);

    return 0;
}

fn updateStdDocs(process: std.process.Init, allocator: Allocator, output_dir: []const u8) !void {
    try std.Io.Dir.cwd().createDirPath(process.io, output_dir);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var lib_dir = try std.Io.Dir.cwd().openDir(process.io, "src/lib", .{ .iterate = true });
    defer lib_dir.close(process.io);

    var files: std.ArrayList([]const u8) = .empty;
    defer files.deinit(allocator);

    var it = lib_dir.iterate();
    while (try it.next(process.io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".buzz")) {
            try files.append(allocator, try arena_allocator.dupe(u8, entry.name));
        }
    }

    std.mem.sort([]const u8, files.items, {}, stringLessThan);

    for (files.items) |file_name| {
        var parse_arena = std.heap.ArenaAllocator.init(allocator);
        defer parse_arena.deinit();
        const parse_allocator = parse_arena.allocator();

        const module_name = try replaceExtension(parse_allocator, file_name, "");
        const source_path = try joinPath(parse_allocator, &.{ "src/lib", file_name });
        const source = try std.Io.Dir.cwd().readFileAlloc(process.io, source_path, parse_allocator, .unlimited);

        var parsed = try parseBuzz(process, parse_allocator, source, source_path, module_name);
        defer parsed.deinit();

        var markdown = std.Io.Writer.Allocating.init(allocator);
        defer markdown.deinit();

        try renderAst(parse_allocator, &markdown.writer, parsed.ast.slice());

        const output_name = try replaceExtension(parse_allocator, file_name, ".md");
        const output_path = try joinPath(parse_allocator, &.{ output_dir, output_name });
        try std.Io.Dir.cwd().writeFile(process.io, .{
            .sub_path = output_path,
            .data = markdown.written(),
        });
    }
}

const Parsed = struct {
    allocator: Allocator,
    gc: *GC,
    imports: std.StringHashMapUnmanaged(Parser.ScriptImport),
    dlib_symbols: std.StringHashMapUnmanaged(Parser.Dlib),
    ast: Ast,

    fn deinit(self: *Parsed) void {
        self.imports.deinit(self.allocator);
        self.dlib_symbols.deinit(self.allocator);
        self.gc.type_registry.deinit();
        self.gc.deinit();
        self.allocator.destroy(self.gc);
    }
};

fn parseBuzz(process: std.process.Init, allocator: Allocator, source: []const u8, source_path: []const u8, module_name: []const u8) !Parsed {
    const gc = try allocator.create(GC);
    errdefer allocator.destroy(gc);
    gc.* = try GC.init(allocator);
    errdefer gc.deinit();
    gc.type_registry = try TypeRegistry.init(gc);
    errdefer gc.type_registry.deinit();

    var imports = std.StringHashMapUnmanaged(Parser.ScriptImport).empty;
    errdefer imports.deinit(allocator);
    var dlib_symbols = std.StringHashMapUnmanaged(Parser.Dlib).empty;
    errdefer dlib_symbols.deinit(allocator);
    var parser = Parser.init(
        process,
        gc,
        &imports,
        &dlib_symbols,
        false,
        RunFlavor.Ast,
    );
    defer parser.deinit();

    const root_dir = try std.Io.Dir.cwd().realPathFileAlloc(process.io, ".", allocator);
    defer allocator.free(root_dir);

    const ast = try parser.parse(source, root_dir, source_path, module_name) orelse {
        return error.ParseFailed;
    };

    return .{
        .allocator = allocator,
        .gc = gc,
        .imports = imports,
        .dlib_symbols = dlib_symbols,
        .ast = ast,
    };
}

const ExportedDecl = struct {
    node: Ast.Node.Index,
    public_name: Ast.TokenIndex,
};

fn renderAst(allocator: Allocator, writer: *std.Io.Writer, ast: Ast.Slice) !void {
    const root = ast.root orelse return error.ParseFailed;
    const namespace = (try ast.namespace(allocator, root)) orelse &.{};

    try writer.print("# {s}\n\n", .{try joinTokenLexemes(allocator, ast, namespace, "\\")});

    var exported = std.ArrayList(ExportedDecl).empty;
    defer exported.deinit(allocator);

    try collectExports(allocator, ast, root, &exported);

    var seen_public_names = std.ArrayList(Ast.TokenIndex).empty;
    defer seen_public_names.deinit(allocator);

    for (exported.items) |decl| {
        if (isPrivateDocblock(ast, nodeDocblock(ast, decl.node))) {
            continue;
        }
        if (containsTokenLexeme(ast, seen_public_names.items, decl.public_name)) {
            continue;
        }
        try seen_public_names.append(allocator, decl.public_name);

        try renderDecl(allocator, writer, ast, decl.node, decl.public_name, 2);
        try writer.writeByte('\n');
    }
}

fn containsTokenLexeme(ast: Ast.Slice, tokens: []const Ast.TokenIndex, needle: Ast.TokenIndex) bool {
    const lexemes = ast.tokens.items(.lexeme);
    for (tokens) |token| {
        if (std.mem.eql(u8, lexemes[token], lexemes[needle])) {
            return true;
        }
    }
    return false;
}

fn collectExports(allocator: Allocator, ast: Ast.Slice, root: Ast.Node.Index, exported: *std.ArrayList(ExportedDecl)) !void {
    const components = ast.nodes.items(.components);
    const tags = ast.nodes.items(.tag);
    const lexemes = ast.tokens.items(.lexeme);
    const script = components[root].Function;
    const body = script.body orelse return;

    var declarations = std.StringHashMapUnmanaged(Ast.Node.Index).empty;
    defer declarations.deinit(allocator);

    for (components[body].Block) |statement| {
        switch (tags[statement]) {
            .Export => {
                const exp = components[statement].Export;
                if (exp.declaration) |decl| {
                    try declarations.put(allocator, lexemes[declName(ast, decl).?], decl);
                }
            },
            .FunDeclaration,
            .ObjectDeclaration,
            .Enum,
            .VarDeclaration,
            => if (declName(ast, statement)) |name| {
                try declarations.put(allocator, lexemes[name], statement);
            },
            else => {},
        }
    }

    for (components[body].Block) |statement| {
        if (tags[statement] != .Export) {
            continue;
        }

        const exp = components[statement].Export;
        if (exp.declaration) |decl| {
            try exported.append(allocator, .{
                .node = decl,
                .public_name = declName(ast, decl).?,
            });
        } else if (exp.qualified_name) |qualified_name| {
            if (declarations.get(lexemes[qualified_name.name])) |decl| {
                try exported.append(allocator, .{
                    .node = decl,
                    .public_name = exp.alias orelse qualified_name.name,
                });
            }
        }
    }
}

fn renderDecl(allocator: Allocator, writer: *std.Io.Writer, ast: Ast.Slice, node: Ast.Node.Index, public_name: Ast.TokenIndex, heading_level: u8) !void {
    const tags = ast.nodes.items(.tag);
    const lexemes = ast.tokens.items(.lexeme);

    for (0..heading_level) |_| {
        try writer.writeByte('#');
    }
    try writer.print(" {s}\n", .{lexemes[public_name]});
    try writer.writeAll("```buzz\n");

    switch (tags[node]) {
        .FunDeclaration => try renderFunctionSignature(allocator, writer, ast, ast.nodes.items(.components)[node].FunDeclaration.function, public_name, false),
        .ObjectDeclaration => try writer.print("object {s}\n", .{lexemes[public_name]}),
        .Enum => try renderEnum(allocator, writer, ast, node, public_name),
        .VarDeclaration => try renderFinal(allocator, writer, ast, node, public_name),
        else => return,
    }

    try writer.writeAll("```\n");
    try renderDocblock(writer, ast, nodeDocblock(ast, node));

    if (tags[node] == .ObjectDeclaration) {
        try renderObjectMembers(allocator, writer, ast, node);
    }
}

fn renderObjectMembers(allocator: Allocator, writer: *std.Io.Writer, ast: Ast.Slice, object_node: Ast.Node.Index) !void {
    const components = ast.nodes.items(.components);
    const type_def = ast.nodes.items(.type_def)[object_node].?;
    const object_def = type_def.resolved_type.?.Object;
    const members = components[object_node].ObjectDeclaration.members;

    for (members) |member| {
        if (isPrivateDocblock(ast, member.docblock)) {
            continue;
        }

        const field = object_def.fields.get(ast.tokens.items(.lexeme)[member.name]) orelse continue;
        if (!member.method and member.docblock == null) {
            continue;
        }

        try writer.writeByte('\n');
        try writer.print("### {s}\n", .{ast.tokens.items(.lexeme)[member.name]});
        try writer.writeAll("```buzz\n");

        if (member.method) {
            try renderFunctionSignature(
                allocator,
                writer,
                ast,
                member.method_or_default_value.?,
                member.name,
                field.static,
            );
        } else {
            try writer.print("{s}: {s}\n", .{
                ast.tokens.items(.lexeme)[member.name],
                try typeNodeToString(allocator, ast, member.property_type.?),
            });
        }

        try writer.writeAll("```\n");
        try renderDocblock(writer, ast, member.docblock);
    }
}

fn renderFunctionSignature(allocator: Allocator, writer: *std.Io.Writer, ast: Ast.Slice, function_node: Ast.Node.Index, public_name: Ast.TokenIndex, static: bool) !void {
    const function_type_node = ast.nodes.items(.components)[function_node].Function.function_signature orelse return;
    const signature = ast.nodes.items(.components)[function_type_node].FunctionType;
    const function_def = ast.nodes.items(.type_def)[function_node].?.resolved_type.?.Function;
    const lexemes = ast.tokens.items(.lexeme);

    if (static) {
        try writer.writeAll("static ");
    }
    try writer.print("fun {s}", .{lexemes[public_name]});

    if (signature.generic_types.len > 0) {
        try writer.writeAll("::<");
        for (signature.generic_types, 0..) |generic, index| {
            if (index > 0) {
                try writer.writeAll(", ");
            }
            try writer.writeAll(lexemes[generic]);
        }
        try writer.writeByte('>');
    }

    try writer.writeByte('(');
    for (signature.arguments, 0..) |argument, index| {
        if (index > 0) {
            try writer.writeAll(", ");
        }

        try writer.print("{s}: {s}", .{
            lexemes[argument.name],
            try typeNodeToString(allocator, ast, argument.type),
        });

        if (argument.default) |default| {
            try writer.print(" = {s}", .{nodeSource(ast, default)});
        }
    }
    try writer.writeByte(')');

    if (signature.return_type) |return_type| {
        try writer.print(" > {s}", .{
            try typeNodeToString(allocator, ast, return_type),
        });
    } else if (function_def.return_type.def_type != .Void) {
        try writer.print(" > {s}", .{try typeToString(allocator, function_def.return_type)});
    }

    if (signature.yield_type) |yield_type| {
        try writer.print(" *> {s}", .{
            try typeNodeToString(allocator, ast, yield_type),
        });
    }

    if (signature.error_types.len > 0) {
        try writer.writeAll(" !> ");
        for (signature.error_types, 0..) |error_type, index| {
            if (index > 0) {
                try writer.writeAll(", ");
            }
            try writer.writeAll(try typeNodeToString(allocator, ast, error_type));
        }
    }

    try writer.writeByte('\n');
}

fn renderEnum(allocator: Allocator, writer: *std.Io.Writer, ast: Ast.Slice, node: Ast.Node.Index, public_name: Ast.TokenIndex) !void {
    const components = ast.nodes.items(.components)[node].Enum;
    const lexemes = ast.tokens.items(.lexeme);

    try writer.writeAll("enum");
    if (components.case_type) |case_type| {
        try writer.print("<{s}>", .{try typeToString(allocator, ast.nodes.items(.type_def)[case_type].?)});
    }
    try writer.print(" {s} {{\n", .{lexemes[public_name]});

    for (components.cases) |case| {
        try writer.print("    {s}", .{lexemes[case.name]});
        if (!components.values_omitted) if (case.value) |value| {
            try writer.print(" = {s}", .{nodeSource(ast, value)});
        };
        try writer.writeAll(",\n");
    }

    try writer.writeAll("}\n");
}

fn renderFinal(allocator: Allocator, writer: *std.Io.Writer, ast: Ast.Slice, node: Ast.Node.Index, public_name: Ast.TokenIndex) !void {
    const type_def = ast.nodes.items(.type_def)[node].?;
    try writer.print("final {s}: {s}\n", .{
        ast.tokens.items(.lexeme)[public_name],
        try typeToString(allocator, type_def),
    });
}

fn renderDocblock(writer: *std.Io.Writer, ast: Ast.Slice, docblock: ?Ast.TokenIndex) !void {
    const docblock_token = docblock orelse return;
    var it = std.mem.splitScalar(u8, ast.tokens.items(.lexeme)[docblock_token], '\n');

    while (it.next()) |raw_line| {
        const line = cleanDocLine(raw_line);
        if (line.len == 0 or std.mem.eql(u8, line, "@private")) {
            continue;
        }

        if (std.mem.startsWith(u8, line, "@param ")) {
            const rest = std.mem.trim(u8, line["@param ".len..], " \t");
            const split_at = firstWhitespace(rest) orelse rest.len;
            try writer.print("- **`{s}`:** {s}\n", .{
                rest[0..split_at],
                std.mem.trim(u8, rest[split_at..], " \t"),
            });
        } else if (std.mem.startsWith(u8, line, "@return ")) {
            try writer.print("**Returns:** {s}\n", .{std.mem.trim(u8, line["@return ".len..], " \t")});
        } else if (std.mem.startsWith(u8, line, "@returns ")) {
            try writer.print("**Returns:** {s}\n", .{std.mem.trim(u8, line["@returns ".len..], " \t")});
        } else if (std.mem.startsWith(u8, line, "@returned ")) {
            try writer.print("**Returns:** {s}\n", .{std.mem.trim(u8, line["@returned ".len..], " \t")});
        } else {
            try writer.print("{s}\n", .{line});
        }
    }
}

fn cleanDocLine(raw_line: []const u8) []const u8 {
    var line = std.mem.trim(u8, raw_line, " \t\r");
    if (std.mem.startsWith(u8, line, "///")) {
        line = line["///".len..];
    }
    return std.mem.trim(u8, line, " \t");
}

fn isPrivateDocblock(ast: Ast.Slice, docblock: ?Ast.TokenIndex) bool {
    const docblock_token = docblock orelse return false;
    var it = std.mem.splitScalar(u8, ast.tokens.items(.lexeme)[docblock_token], '\n');
    while (it.next()) |line| {
        if (std.mem.eql(u8, cleanDocLine(line), "@private")) {
            return true;
        }
    }
    return false;
}

fn nodeDocblock(ast: Ast.Slice, node: Ast.Node.Index) ?Ast.TokenIndex {
    const tags = ast.nodes.items(.tag);
    const components = ast.nodes.items(.components);
    if (tags[node] == .FunDeclaration) {
        return components[components[node].FunDeclaration.function].Function.docblock orelse ast.nodes.items(.docblock)[node];
    }
    return ast.nodes.items(.docblock)[node];
}

fn declName(ast: Ast.Slice, node: Ast.Node.Index) ?Ast.TokenIndex {
    const components = ast.nodes.items(.components);
    return switch (ast.nodes.items(.tag)[node]) {
        .FunDeclaration => blk: {
            const function_node = components[node].FunDeclaration.function;
            const signature_node = components[function_node].Function.function_signature orelse return null;
            break :blk components[signature_node].FunctionType.name orelse return null;
        },
        .ObjectDeclaration => components[node].ObjectDeclaration.name,
        .Enum => components[node].Enum.name,
        .VarDeclaration => components[node].VarDeclaration.name,
        else => null,
    };
}

fn typeToString(allocator: Allocator, type_def: *obj.ObjTypeDef) ![]const u8 {
    return type_def.toStringAlloc(allocator, false);
}

fn typeNodeToString(allocator: Allocator, ast: Ast.Slice, node: Ast.Node.Index) ![]const u8 {
    const rendered = try typeToString(allocator, ast.nodes.items(.type_def)[node].?);
    if (std.mem.containsAtLeast(u8, rendered, 1, "generic type #")) {
        return nodeSource(ast, node);
    }
    return rendered;
}

fn nodeSource(ast: Ast.Slice, node: Ast.Node.Index) []const u8 {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const tokens = ast.tokens;
    const start = tokens.items(.offset)[locations[node]];
    const end_token = end_locations[node];
    const end = tokens.items(.offset)[end_token] + tokens.items(.lexeme)[end_token].len;
    return std.mem.trim(u8, tokens.items(.source)[locations[node]][start..end], " \t\r\n");
}

fn joinTokenLexemes(allocator: Allocator, ast: Ast.Slice, tokens: []const Ast.TokenIndex, separator: []const u8) ![]const u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();

    for (tokens, 0..) |token, index| {
        if (index > 0) {
            try out.writer.writeAll(separator);
        }
        try out.writer.writeAll(ast.tokens.items(.lexeme)[token]);
    }

    return try out.toOwnedSlice();
}

fn firstWhitespace(input: []const u8) ?usize {
    for (input, 0..) |c, index| {
        if (std.ascii.isWhitespace(c)) {
            return index;
        }
    }
    return null;
}

fn replaceExtension(allocator: Allocator, file_name: []const u8, extension: []const u8) ![]const u8 {
    const dot = std.mem.lastIndexOfScalar(u8, file_name, '.') orelse file_name.len;
    return std.fmt.allocPrint(allocator, "{s}{s}", .{ file_name[0..dot], extension });
}

fn joinPath(allocator: Allocator, parts: []const []const u8) ![]const u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();

    for (parts, 0..) |part, index| {
        if (index > 0 and out.written().len > 0 and !std.mem.endsWith(u8, out.written(), "/")) {
            try out.writer.writeByte('/');
        }
        try out.writer.writeAll(part);
    }

    return try out.toOwnedSlice();
}

fn stringLessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.mem.lessThan(u8, lhs, rhs);
}
