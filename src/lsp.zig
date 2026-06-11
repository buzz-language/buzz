const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");
const lsp = @import("lsp");
const Ast = @import("Ast.zig");
const GC = @import("GC.zig");
const TypeRegistry = @import("TypeRegistry.zig");
const Parser = @import("Parser.zig");
const Reporter = @import("Reporter.zig");
const Scanner = @import("Scanner.zig");
const CodeGen = @import("Codegen.zig");
const Package = @import("Package.zig");
const Token = @import("Token.zig");
const Renderer = @import("renderer.zig").Renderer;
const o = @import("obj.zig");
const static_libraries = @import("lib/static_libraries.zig");

const log = std.log.scoped(.buzz_lsp);

const MemberDocblockKey = struct {
    owner: Ast.TokenIndex,
    member: Ast.TokenIndex,

    pub const Context = struct {
        ast: Ast.Slice,

        pub fn hash(ctx: Context, key: MemberDocblockKey) u64 {
            var h = std.hash.Wyhash.init(0);
            const lexemes = ctx.ast.tokens.items(.lexeme);

            h.update(lexemes[key.owner]);
            h.update(".");
            h.update(lexemes[key.member]);

            return h.final();
        }

        pub fn eql(ctx: Context, a: MemberDocblockKey, b: MemberDocblockKey) bool {
            const lexemes = ctx.ast.tokens.items(.lexeme);

            return std.mem.eql(u8, lexemes[a.owner], lexemes[b.owner]) and
                std.mem.eql(u8, lexemes[a.member], lexemes[b.member]);
        }
    };

    pub fn HashMap(V: type) type {
        return std.HashMapUnmanaged(
            MemberDocblockKey,
            V,
            Context,
            std.hash_map.default_max_load_percentage,
        );
    }
};

/// Package document kind that needs an LSP-only parser wrapper.
const PackageDocumentKind = enum {
    manifest,
    manifest_lock,
};

const Document = struct {
    pub const Definition = struct {
        location: lsp.types.Location,
        def_node: Ast.Node.Index,
        docblock: ?Ast.TokenIndex = null,
    };

    arena: std.heap.ArenaAllocator,
    process: std.process.Init,
    /// Client document text, unchanged by LSP-only parsing wrappers.
    src: [:0]const u8,
    /// Source passed to the parser. Manifest documents may use wrapped text.
    parse_src: [:0]const u8,
    /// True when the parser source includes an LSP-only manifest wrapper.
    is_wrapped_manifest: bool = false,
    /// Not owned by this struct
    uri: []const u8,
    ast: Ast,
    errors: []const Reporter.Report,

    /// Document symbols collected from parser globals.
    symbols: std.ArrayList(lsp.types.DocumentSymbol) = .empty,

    /// Global and imported labels available for completion.
    completion_labels: std.StringArrayHashMapUnmanaged(void) = .empty,

    /// Cache for previous gotoDefinition
    definitions: std.AutoHashMapUnmanaged(Ast.Node.Index, ?Definition) = .empty,

    /// Cache for node under position
    node_under_position: std.AutoHashMapUnmanaged(lsp.types.Position, ?Ast.Node.Index) = .empty,

    /// Cache for hover markup keyed by the hovered source token.
    token_hover: std.AutoHashMapUnmanaged(Ast.TokenIndex, []const u8) = .empty,

    /// Object member docblocks indexed by object and member token lexemes.
    object_member_docblocks: MemberDocblockKey.HashMap(Ast.TokenIndex) = .empty,

    /// Enum case docblocks indexed by enum and case token lexemes.
    enum_case_docblocks: MemberDocblockKey.HashMap(Ast.TokenIndex) = .empty,

    /// Cache for inlay hints
    inlay_hints: std.ArrayList(lsp.types.InlayHint) = .empty, // I tried to make this a simple slice but the data was lost I don't know why

    /// Resolved information needed to render a hover response.
    const HoverInfo = struct {
        /// Buzz type rendered in the hover code block, when available.
        type_def: ?*o.ObjTypeDef,
        /// Docblock token rendered before the type, when available.
        docblock: ?Ast.TokenIndex,
    };

    /// Kind of hover target selected from the node under the cursor.
    const HoverTargetKind = enum {
        /// Generic node hover using the node type and definition docblock.
        node,
        /// Dot member or enum case hover using the member token.
        dot_member,
        /// Object initializer field hover using the assigned field token.
        object_init_field,
    };

    /// Hover target selected after resolving the node under the cursor.
    const HoverTarget = struct {
        /// Node that owns or resolves the hover target.
        node: Ast.Node.Index,
        /// Source token under the cursor.
        token: Ast.TokenIndex,
        /// How hover information should be built for this target.
        kind: HoverTargetKind,
    };

    pub fn init(process: std.process.Init, parent_allocator: std.mem.Allocator, src: []const u8, uri: []const u8) !Document {
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        errdefer arena.deinit();

        const allocator = arena.allocator();
        const owned_src = try allocator.dupeZ(u8, src);
        const owned_uri = try allocator.dupe(u8, uri);
        const package_document_kind = try packageDocumentKind(allocator, owned_uri, owned_src);
        const parse_src = if (package_document_kind) |kind|
            switch (kind) {
                .manifest => try allocator.dupeZ(u8, try Package.wrapManifest(allocator, owned_src)),
                .manifest_lock => try allocator.dupeZ(u8, try Package.wrapManifestLock(allocator, owned_src)),
            }
        else
            owned_src;

        var gc = try GC.init(allocator);
        gc.type_registry = TypeRegistry.init(&gc) catch return error.OutOfMemory;
        var imports = std.StringHashMapUnmanaged(Parser.ScriptImport).empty;
        var dlib = std.StringHashMapUnmanaged(Parser.Dlib).empty;

        var parser = Parser.init(
            process,
            &gc,
            &imports,
            &dlib,
            false,
            .Ast,
        );
        var codegen = CodeGen.init(
            process,
            &gc,
            &parser,
            .Ast,
            null,
            false,
        );

        const std_lib_script_name = staticScriptNameFromUri(owned_uri);
        const document_script_name = std_lib_script_name orelse
            (try localPathFromFileUri(allocator, owned_uri)) orelse
            owned_uri;
        const document_root_dir = root_dir: {
            if (std_lib_script_name != null) {
                break :root_dir try std.Io.Dir.cwd().realPathFileAlloc(process.io, ".", allocator);
            }

            if (std.fs.path.dirname(document_script_name)) |dir| {
                var it = std.fs.path.componentIterator(dir);
                var maybe_component = it.last();
                while (maybe_component) |prev_dir| : (maybe_component = it.previous()) {
                    if (std.mem.eql(u8, prev_dir.name, "src")) {
                        break :root_dir try allocator.dupe(u8, std.fs.path.dirname(prev_dir.path) orelse ".");
                    }
                }

                break :root_dir try allocator.dupe(u8, dir);
            }

            break :root_dir try std.Io.Dir.cwd().realPathFileAlloc(process.io, ".", allocator);
        };

        // If there's parsing error `parse` does not return the AST, but we can still use it however incomplete
        const ast = (parser.parse(
            parse_src,
            document_root_dir,
            owned_uri,
            document_script_name,
        ) catch parser.ast) orelse
            parser.ast;

        const codegen_errors = if (parser.reporter.last_error == null and
            ((codegen.generate(ast.slice()) catch return error.OutOfMemory) == null) or codegen.reporter.reports.items.len > 0)
            try codegen.reporter.reports.toOwnedSlice(allocator)
        else
            &.{};
        const parse_errors = try parser.reporter.reports.toOwnedSlice(allocator);

        var errors = std.ArrayList(Reporter.Report).empty;
        try errors.appendSlice(allocator, parse_errors);
        try errors.appendSlice(allocator, codegen_errors);

        var doc = Document{
            .arena = arena,
            .process = process,
            .src = owned_src,
            .parse_src = parse_src,
            .is_wrapped_manifest = package_document_kind != null,
            .uri = owned_uri,
            .ast = ast,
            .errors = errors.items,
        };

        const doc_allocator = doc.arena.allocator();
        try doc.collectGlobalSymbols(doc_allocator, parser.globals.items);

        // Keywords are document-independent, so store them with the precomputed
        // global labels once instead of rebuilding them on every completion.
        for (Token.keywords.keys()) |keyword| {
            try doc.completion_labels.put(doc_allocator, keyword, {});
        }

        if (ast.root != null) {
            doc.computeInlayHints() catch return error.OutOfMemory;
            try doc.collectMemberDocblocks(&imports);
        }

        return doc;
    }

    /// Returns true for tokens that belong to the served client document.
    fn isClientToken(self: *const Document, token: Token) bool {
        if (!std.mem.eql(u8, token.script_name, self.uri)) {
            return false;
        }

        if (self.is_wrapped_manifest) {
            return token.line != 0;
        }

        return true;
    }

    /// Collects current-file document symbols and all global completion labels from parser globals.
    fn collectGlobalSymbols(self: *Document, allocator: std.mem.Allocator, globals: []const Parser.Global) !void {
        const ast_slice = self.ast.slice();
        const tags = ast_slice.nodes.items(.tag);
        const components = ast_slice.nodes.items(.components);
        const locations = ast_slice.nodes.items(.location);
        const end_locations = ast_slice.nodes.items(.end_location);
        const lexemes = ast_slice.tokens.items(.lexeme);

        // Parser globals are the resolved top-level visibility table; collecting
        // from it keeps document symbols and imported completions in one place.
        for (globals) |global| {
            if (global.hidden) {
                continue;
            }

            if (global.type_def.def_type == .Placeholder) {
                continue;
            }

            const name = lexemes[global.qualified_name.name];
            if (name.len == 1 and name[0] == '_') {
                continue;
            }

            // Imported globals are completion-only in this document. Their
            // defining node can belong to the imported script, so collect the
            // label before applying current-document symbol checks.
            if (global.imported_from != null) {
                var label = std.Io.Writer.Allocating.init(allocator);

                if (global.qualified_name.namespace.len > 0) {
                    for (global.qualified_name.namespace) |part| {
                        try label.writer.print("{s}\\", .{lexemes[part]});
                    }
                }

                try label.writer.writeAll(name);

                try self.completion_labels.put(allocator, label.written(), {});
                continue;
            }

            const node = global.node;
            if (node == 0 or node >= ast_slice.nodes.len) {
                continue;
            }

            const location = locations[node];
            const end_location = end_locations[node];
            if (location >= ast_slice.tokens.len or end_location >= ast_slice.tokens.len) {
                continue;
            }

            if (!self.isClientToken(ast_slice.tokens.get(location))) {
                continue;
            }

            if (tags[node] == .VarDeclaration and
                !self.isClientToken(ast_slice.tokens.get(components[node].VarDeclaration.name)))
            {
                continue;
            }

            // Document symbols must only describe the document being served.
            // Imported nodes can share the AST backing lists but belong to
            // another script.
            const script_names = ast_slice.tokens.items(.script_name);
            if (!std.mem.eql(u8, script_names[location], self.uri)) {
                continue;
            }

            try self.completion_labels.put(allocator, name, {});

            switch (tags[node]) {
                .VarDeclaration => {
                    const comp = components[node].VarDeclaration;
                    if (comp.slot_type == .Global) {
                        try self.symbols.append(
                            allocator,
                            .{
                                .name = name,
                                .detail = try global.type_def.toStringAlloc(allocator, false),
                                .kind = if (!global.type_def.isMutable() and comp.final)
                                    .Constant
                                else
                                    .Variable,
                                .range = tokenToRange(ast_slice, locations[node], end_locations[node]),
                                .selectionRange = tokenToRange(ast_slice, locations[node], end_locations[node]),
                            },
                        );
                    }
                },
                .Enum => {
                    const comp = components[node].Enum;
                    var children = std.ArrayList(lsp.types.DocumentSymbol).empty;

                    for (comp.cases) |case| {
                        const range = tokenToRange(
                            ast_slice,
                            case.name,
                            if (case.value) |value|
                                end_locations[value]
                            else
                                case.name,
                        );

                        try children.append(
                            allocator,
                            .{
                                .name = lexemes[case.name],
                                .kind = .EnumMember,
                                .range = range,
                                .selectionRange = range,
                            },
                        );
                    }

                    try self.symbols.append(
                        allocator,
                        .{
                            .name = name,
                            .detail = try global.type_def.toStringAlloc(allocator, false),
                            .kind = .Enum,
                            .range = tokenToRange(ast_slice, locations[node], end_locations[node]),
                            .selectionRange = tokenToRange(ast_slice, locations[node], end_locations[node]),
                            .children = try children.toOwnedSlice(allocator),
                        },
                    );
                },
                .ObjectDeclaration => {
                    var children = std.ArrayList(lsp.types.DocumentSymbol).empty;

                    if (global.type_def.def_type == .Object and global.type_def.resolved_type != null) {
                        var it = global.type_def.resolved_type.?.Object.fields.iterator();
                        while (it.next()) |kv| {
                            const field = kv.value_ptr.*;

                            try children.append(
                                allocator,
                                .{
                                    .name = field.name,
                                    .detail = try field.type_def.toStringAlloc(allocator, false),
                                    .kind = if (field.method)
                                        .Method
                                    else
                                        .Property,
                                    .range = tokenToRange(ast_slice, field.location, field.location),
                                    .selectionRange = tokenToRange(ast_slice, field.location, field.location),
                                },
                            );
                        }
                    }

                    try self.symbols.append(
                        allocator,
                        .{
                            .name = name,
                            .detail = try global.type_def.toStringAlloc(allocator, false),
                            .kind = .Struct,
                            .range = tokenToRange(ast_slice, locations[node], end_locations[node]),
                            .selectionRange = tokenToRange(ast_slice, locations[node], end_locations[node]),
                            .children = try children.toOwnedSlice(allocator),
                        },
                    );
                },
                .ProtocolDeclaration => {
                    var children = std.ArrayList(lsp.types.DocumentSymbol).empty;

                    if (global.type_def.def_type == .Protocol and global.type_def.resolved_type != null) {
                        var it = global.type_def.resolved_type.?.Protocol.methods.iterator();
                        while (it.next()) |kv| {
                            const method = kv.value_ptr.*;
                            const method_location = global.type_def.resolved_type.?.Protocol.methods_locations.get(kv.key_ptr.*).?;
                            const range = tokenToRange(ast_slice, method_location, method_location);

                            try children.append(
                                allocator,
                                .{
                                    .name = kv.key_ptr.*,
                                    .detail = try method.type_def.toStringAlloc(allocator, false),
                                    .kind = .Method,
                                    .range = range,
                                    .selectionRange = range,
                                },
                            );
                        }
                    }

                    try self.symbols.append(
                        allocator,
                        .{
                            .name = name,
                            .detail = try global.type_def.toStringAlloc(allocator, false),
                            .kind = .Interface,
                            .range = tokenToRange(ast_slice, locations[node], end_locations[node]),
                            .selectionRange = tokenToRange(ast_slice, locations[node], end_locations[node]),
                            .children = try children.toOwnedSlice(allocator),
                        },
                    );
                },
                .FunDeclaration => fun: {
                    if (global.type_def.def_type != .Function or global.type_def.resolved_type == null) {
                        break :fun;
                    }

                    const fun_def = global.type_def.resolved_type.?.Function;
                    switch (fun_def.function_type) {
                        .Method,
                        .Script,
                        .ScriptEntryPoint,
                        .Anonymous,
                        .Repl,
                        => break :fun,
                        .Function,
                        .EntryPoint,
                        .Test,
                        .Extern,
                        .Abstract,
                        => {},
                    }

                    const function_node = components[node].FunDeclaration.function;
                    const function_comp = components[function_node].Function;
                    try self.symbols.append(
                        allocator,
                        .{
                            .name = if (fun_def.function_type == .Test)
                                lexemes[function_comp.test_message.?]
                            else
                                fun_def.name.string,
                            .detail = try global.type_def.toStringAlloc(allocator, false),
                            .kind = .Function,
                            .range = tokenToRange(ast_slice, locations[function_node], end_locations[function_node]),
                            .selectionRange = tokenToRange(ast_slice, locations[function_node], end_locations[function_node]),
                        },
                    );
                },
                else => {},
            }
        }
    }

    pub fn deinit(self: *Document) void {
        self.arena.deinit();
    }

    pub fn wholeDocumentRange(self: *const Document) lsp.types.Range {
        return .{
            .start = .{
                .line = 0,
                .character = 0,
            },
            .end = .{
                .line = @intCast(
                    std.mem.count(
                        u8,
                        self.src,
                        "\n",
                    ),
                ),
                .character = @intCast(
                    self.src[std.mem.lastIndexOfScalar(
                        u8,
                        self.src,
                        '\n',
                    ) orelse 0 ..].len,
                ),
            },
        };
    }

    const NodeUnderPositionContext = struct {
        result: ?Ast.Node.Index = null,
        /// URI of the client document being searched.
        uri: []const u8,
        position: lsp.types.Position,

        pub fn processNode(
            self: *NodeUnderPositionContext,
            _: std.mem.Allocator,
            ast: Ast.Slice,
            node: Ast.Node.Index,
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            const locations = ast.nodes.items(.location);
            const end_locations = ast.nodes.items(.end_location);
            const location_idx = locations[node];
            const end_location_idx = end_locations[node];
            const script_names = ast.tokens.items(.script_name);

            // Ignore root node
            if (location_idx == 0) {
                return false;
            }

            if (location_idx >= ast.tokens.len or end_location_idx >= ast.tokens.len) {
                return true;
            }

            const location = ast.tokens.get(location_idx);
            const end_location = ast.tokens.get(end_location_idx);

            // Walking from the document root must only visit document-local
            // nodes. Imported scripts share the backing token/node lists, so
            // assert the parser did not attach an imported token to this range.
            std.debug.assert(std.mem.eql(u8, script_names[location_idx], self.uri));
            std.debug.assert(std.mem.eql(u8, script_names[end_location_idx], self.uri));

            // If outside of the node range, don't go deeper
            if (self.position.line < location.line or
                self.position.line > end_location.line or
                (self.position.line == end_location.line and self.position.character + 1 > end_location.column + end_location.lexeme.len) or
                (self.position.line == location.line and self.position.character + 1 < location.column))
            {
                return true;
            }

            // Otherwise this node is a candidate: continue deeper for a more narrow match
            self.result = node;

            return false;
        }
    };

    const NodeContainingRangeContext = struct {
        result: ?Ast.Node.Index = null,
        /// URI of the client document being searched.
        uri: []const u8,
        range: lsp.types.Range,

        pub fn processNode(
            self: *NodeContainingRangeContext,
            _: std.mem.Allocator,
            ast: Ast.Slice,
            node: Ast.Node.Index,
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            const locations = ast.nodes.items(.location);
            const end_locations = ast.nodes.items(.end_location);
            const location_idx = locations[node];
            const end_location_idx = end_locations[node];
            const script_names = ast.tokens.items(.script_name);

            // Ignore root node
            if (location_idx == 0) {
                return false;
            }

            if (location_idx >= ast.tokens.len or end_location_idx >= ast.tokens.len) {
                return true;
            }

            const location = ast.tokens.get(location_idx);
            const end_location = ast.tokens.get(end_location_idx);

            // Walking from the document root must only visit document-local
            // nodes. Imported scripts share the backing token/node lists, so
            // assert the parser did not attach an imported token to this range.
            std.debug.assert(std.mem.eql(u8, script_names[location_idx], self.uri));
            std.debug.assert(std.mem.eql(u8, script_names[end_location_idx], self.uri));

            // If outside of the node range, don't go deeper
            if (self.range.start.line < location.line or
                self.range.end.line > end_location.line or
                (self.range.end.line == end_location.line and self.range.end.character + 1 > end_location.column + end_location.lexeme.len) or
                (self.range.start.line == location.line and self.range.start.character + 1 < location.column))
            {
                return true;
            }

            // Otherwise this node is a candidate: continue deeper for a more narrow match
            self.result = node;

            return false;
        }
    };

    pub fn nodeUnderPosition(self: *Document, position: lsp.types.Position) !?Ast.Node.Index {
        const allocator = self.arena.allocator();

        const nodeEntry = try self.node_under_position.getOrPut(allocator, position);

        if (!nodeEntry.found_existing) {
            const result = if (self.hasErrors())
                self.findNodeContainingRange(.{
                    .start = position,
                    .end = position,
                })
            else clean_ast: {
                const root = self.ast.root orelse break :clean_ast null;
                var node_ctx = NodeUnderPositionContext{
                    .uri = self.uri,
                    .position = position,
                };

                self.ast.slice().walk(
                    allocator,
                    &node_ctx,
                    root,
                    .breadthFirst,
                ) catch |err| {
                    log.err("nodeUnderPosition: {any}", .{err});
                };

                break :clean_ast node_ctx.result;
            };

            if (result) |res| {
                log.debug(
                    "Found node {} {s} under position {},{}",
                    .{
                        res,
                        @tagName(self.ast.nodes.items(.tag)[res]),
                        position.line,
                        position.character,
                    },
                );
            }

            nodeEntry.value_ptr.* = result;
        }

        return nodeEntry.value_ptr.*;
    }

    pub fn nodeContainingRange(self: *Document, range: lsp.types.Range) !?Ast.Node.Index {
        if (self.ast.root == null) {
            return null;
        }

        const allocator = self.arena.allocator();

        var node_ctx = NodeContainingRangeContext{
            .uri = self.uri,
            .range = range,
        };

        self.ast.slice().walk(
            allocator,
            &node_ctx,
            self.ast.root.?,
            .breadthFirst,
        ) catch |err| {
            log.err("nodeWithinRange: {any}", .{err});
        };

        if (node_ctx.result) |res| {
            log.debug(
                "Found node {} {s} withing range [{}:{} .. {}:{}]",
                .{
                    res,
                    @tagName(self.ast.nodes.items(.tag)[res]),
                    range.start.line,
                    range.start.character,
                    range.end.line,
                    range.end.character,
                },
            );

            return res;
        }

        return null;
    }

    /// Returns true when the LSP position is inside the token lexeme.
    fn positionInToken(self: *Document, position: lsp.types.Position, token_index: Ast.TokenIndex) bool {
        const token = self.ast.tokens.get(token_index);
        if (position.line != token.line) {
            return false;
        }

        const start: u32 = @intCast(@max(1, token.column) - 1);
        const end = start + @as(u32, @intCast(token.lexeme.len));

        return position.character >= start and position.character < end;
    }

    /// Returns true when parser or codegen diagnostics contain an error.
    fn hasErrors(self: *const Document) bool {
        for (self.errors) |report| {
            if (report.items.len == 0) {
                return true;
            }

            for (report.items) |item| {
                if (item.kind == .@"error") {
                    return true;
                }
            }
        }

        return false;
    }

    /// Builds object member and enum case hover data for a dot node.
    fn dotMemberHoverInfo(self: *Document, node_index: Ast.Node.Index) !HoverInfo {
        var docblock: ?Ast.TokenIndex = null;
        if (try self.definition(node_index)) |def| {
            docblock = def.docblock orelse self.ast.nodes.items(.docblock)[def.def_node];
        }

        return .{
            .type_def = self.ast.nodes.items(.type_def)[node_index],
            .docblock = docblock,
        };
    }

    /// Finds an object initializer field token when the cursor is on its assigned name.
    fn objectInitFieldToken(
        self: *Document,
        position: lsp.types.Position,
        node_index: Ast.Node.Index,
    ) ?Ast.TokenIndex {
        const object_init = self.ast.nodes.items(.components)[node_index].ObjectInit;
        for (object_init.properties) |property| {
            if (self.isClientToken(self.ast.tokens.get(property.name)) and
                self.positionInToken(position, property.name))
            {
                return property.name;
            }
        }

        return null;
    }

    /// Resolves the cache token and hover kind from the node under the cursor.
    fn hoverTargetForNode(
        self: *Document,
        position: lsp.types.Position,
        node_index: Ast.Node.Index,
    ) ?HoverTarget {
        const tags = self.ast.nodes.items(.tag);
        const components = self.ast.nodes.items(.components);
        const locations = self.ast.nodes.items(.location);

        switch (tags[node_index]) {
            .Dot => {
                const token = components[node_index].Dot.identifier;
                if (self.isClientToken(self.ast.tokens.get(token)) and
                    self.positionInToken(position, token))
                {
                    return .{
                        .node = node_index,
                        .token = token,
                        .kind = .dot_member,
                    };
                }
            },
            .ObjectInit => {
                if (self.objectInitFieldToken(position, node_index)) |token| {
                    return .{
                        .node = node_index,
                        .token = token,
                        .kind = .object_init_field,
                    };
                }
            },
            else => {},
        }

        if (self.ast.nodes.items(.type_def)[node_index] == null) {
            return null;
        }

        const token = locations[node_index];
        if (!self.isClientToken(self.ast.tokens.get(token))) {
            return null;
        }

        return .{
            .node = node_index,
            .token = token,
            .kind = .node,
        };
    }

    /// Builds hover data from a typed AST node and its definition docblock.
    fn nodeHoverInfo(self: *Document, node_index: Ast.Node.Index) !?HoverInfo {
        const type_def = self.ast.nodes.items(.type_def)[node_index] orelse return null;
        var docblock = self.ast.nodes.items(.docblock)[node_index];
        if (docblock == null) {
            const def = try self.definition(node_index);
            if (def) |udef| {
                docblock = udef.docblock orelse self.ast.nodes.items(.docblock)[udef.def_node];
            }
        }

        return .{
            .type_def = type_def,
            .docblock = docblock,
        };
    }

    /// Builds hover data for a field name assigned by an object initializer.
    fn objectInitFieldHoverInfo(self: *Document, node_index: Ast.Node.Index, token: Ast.TokenIndex) ?HoverInfo {
        const ast_slice = self.ast.slice();
        const type_def = ast_slice.nodes.items(.type_def)[node_index] orelse return null;
        if (type_def.def_type != .ObjectInstance) {
            return null;
        }

        const object_def = type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object;
        const field = object_def.fields.get(ast_slice.tokens.items(.lexeme)[token]) orelse return null;

        return .{
            .type_def = field.type_def,
            .docblock = self.object_member_docblocks.getContext(
                .{
                    .owner = object_def.location,
                    .member = field.location,
                },
                .{ .ast = ast_slice },
            ),
        };
    }

    /// Builds hover data for a selected hover target.
    fn hoverInfoForTarget(self: *Document, target: HoverTarget) !?HoverInfo {
        return switch (target.kind) {
            .node => try self.nodeHoverInfo(target.node),
            .dot_member => try self.dotMemberHoverInfo(target.node),
            .object_init_field => self.objectInitFieldHoverInfo(target.node, target.token),
        };
    }

    /// Builds markdown hover contents from a docblock and optional Buzz type.
    fn buildHoverMarkup(self: *Document, allocator: std.mem.Allocator, hover_info: HoverInfo) !?[]const u8 {
        if (hover_info.type_def == null and hover_info.docblock == null) {
            return null;
        }

        var markup = std.Io.Writer.Allocating.init(allocator);

        if (hover_info.docblock) |docblock_token| {
            const doc = self.ast.tokens.items(.lexeme)[docblock_token];

            var it = std.mem.tokenizeSequence(u8, doc, "/// ");
            while (it.next()) |text| {
                try markup.writer.print("{s}", .{text});
            }
        }

        if (hover_info.type_def) |type_def| {
            try markup.writer.writeAll("\n```buzz\n");
            type_def.toString(&markup.writer, false) catch |err| {
                log.err("textDocument/hover: {any}", .{err});
            };
            try markup.writer.writeAll("\n```");
        }

        return try markup.toOwnedSlice();
    }

    /// Produces markdown hover contents for an LSP position.
    fn hoverMarkupAtPosition(self: *Document, position: lsp.types.Position) !?[]const u8 {
        const allocator = self.arena.allocator();

        const node = (try self.nodeUnderPosition(position)) orelse return null;
        const target = self.hoverTargetForNode(position, node) orelse return null;
        if (self.token_hover.get(target.token)) |markup| {
            return markup;
        }

        const hover_info = (try self.hoverInfoForTarget(target)) orelse return null;
        const markup = (try self.buildHoverMarkup(allocator, hover_info)) orelse return null;
        try self.token_hover.put(allocator, target.token, markup);

        return markup;
    }

    /// Returns true when a node has an in-bounds token range in this document.
    fn nodeHasClientRange(self: *Document, node: Ast.Node.Index) bool {
        if (node == 0 or node >= self.ast.nodes.len) {
            return false;
        }

        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const location_idx = locations[node];
        const end_location_idx = end_locations[node];

        if (location_idx == 0 or
            location_idx >= self.ast.tokens.len or
            end_location_idx >= self.ast.tokens.len)
        {
            return false;
        }

        return self.isClientToken(self.ast.tokens.get(location_idx)) and
            self.isClientToken(self.ast.tokens.get(end_location_idx));
    }

    fn isRangeWithinNode(self: *Document, node: Ast.Node.Index, range: lsp.types.Range) bool {
        if (!self.nodeHasClientRange(node)) {
            return false;
        }

        const locations = self.ast.nodes.items(.location);
        const location_idx = locations[node];
        const end_locations = self.ast.nodes.items(.end_location);
        const end_location_idx = end_locations[node];

        const location = self.ast.tokens.get(location_idx);
        const end_location = self.ast.tokens.get(end_location_idx);

        return (range.start.line >= location.line and
            range.end.line <= end_location.line and
            (range.end.line != end_location.line or range.end.character + 1 <= end_location.column + end_location.lexeme.len) and
            (range.start.line != location.line or range.start.character + 1 >= location.column));
    }

    fn isNodeWithinOtherNode(self: *Document, node: Ast.Node.Index, other: Ast.Node.Index) bool {
        if (!self.nodeHasClientRange(node) or !self.nodeHasClientRange(other)) {
            return false;
        }

        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);

        const node_start = self.ast.tokens.get(locations[node]);
        const node_end = self.ast.tokens.get(end_locations[node]);

        const other_start = self.ast.tokens.get(locations[other]);
        const other_end = self.ast.tokens.get(end_locations[other]);

        return node_start.line >= other_start.line and
            node_end.line <= other_end.line and
            (node_end.line != other_end.line or node_end.column <= other_end.column + other_end.lexeme.len) and
            (node_start.line != other_start.line or node_start.column >= other_start.column);
    }

    /// Search for a node within the given range not by walking the AST but by iterating over the nodes list
    /// A normal walk would miss nodes that were not added to the tree because of a parsing error
    pub fn findNodeContainingRange(self: *Document, range: lsp.types.Range) ?Ast.Node.Index {
        const locations = self.ast.nodes.items(.location);

        var result: ?Ast.Node.Index = null;
        for (locations, 0..) |_, node| {
            const node_index: Ast.Node.Index = @intCast(node);
            if (!self.nodeHasClientRange(node_index) or !self.isRangeWithinNode(node_index, range)) {
                continue;
            }

            result = if (result) |previous| result: {
                if (self.isNodeWithinOtherNode(node_index, previous)) {
                    break :result node_index;
                }

                break :result result;
            } else node_index;
        }

        return result;
    }

    pub fn definition(self: *Document, node: Ast.Node.Index) !?Definition {
        if (self.definitions.get(node)) |def| {
            if (def) |udef| {
                return udef;
            }
        }

        const allocator = self.arena.allocator();

        const components = self.ast.nodes.items(.components);
        const ast_slice = self.ast.slice();

        switch (self.ast.nodes.items(.tag)[node]) {
            .Import => {
                if (components[node].Import.import) |script_import| {
                    const location = ast_slice.nodes.items(.location)[script_import.function];

                    try self.definitions.put(
                        allocator,
                        node,
                        .{
                            .location = .{
                                .uri = try scriptNameToUri(
                                    self.process.io,
                                    allocator,
                                    Parser.buzzLibPath(self.process.io, self.process.environ_map),
                                    script_import.absolute_path.string,
                                ),
                                .range = tokenToRange(
                                    ast_slice,
                                    location,
                                    location,
                                ),
                            },
                            .def_node = script_import.function,
                        },
                    );

                    return self.definitions.get(node).?.?;
                }

                return null;
            },
            .NamedVariable => {
                const def = components[node].NamedVariable.definition;
                const location = self.ast.nodes.items(.location)[def];
                const end_location = self.ast.nodes.items(.end_location)[def];
                const script_names = self.ast.tokens.items(.script_name);

                try self.definitions.put(
                    allocator,
                    node,
                    .{
                        .location = .{
                            .uri = try scriptNameToUri(
                                self.process.io,
                                allocator,
                                Parser.buzzLibPath(self.process.io, self.process.environ_map),
                                script_names[location],
                            ),
                            .range = tokenToRange(ast_slice, location, end_location),
                        },
                        .def_node = def,
                    },
                );

                return self.definitions.get(node).?.?;
            },
            .Dot => {
                const comp = components[node].Dot;
                if (ast_slice.nodes.items(.type_def)[comp.callee]) |callee_type_def| {
                    switch (callee_type_def.def_type) {
                        .Enum => {
                            if (comp.member_kind == .EnumCase) {
                                const enum_def = callee_type_def.resolved_type.?.Enum;
                                const case_index = comp.value_or_call_or_enum.EnumCase;

                                if (case_index < enum_def.cases_locations.len) {
                                    const case_location = enum_def.cases_locations[case_index];

                                    try self.definitions.put(
                                        allocator,
                                        node,
                                        .{
                                            .location = .{
                                                .uri = try scriptNameToUri(
                                                    self.process.io,
                                                    allocator,
                                                    Parser.buzzLibPath(self.process.io, self.process.environ_map),
                                                    ast_slice.tokens.items(.script_name)[case_location],
                                                ),
                                                .range = tokenToRange(
                                                    ast_slice,
                                                    case_location,
                                                    case_location,
                                                ),
                                            },

                                            .def_node = node,
                                            .docblock = self.enum_case_docblocks.getContext(
                                                .{
                                                    .owner = enum_def.location,
                                                    .member = case_location,
                                                },
                                                .{ .ast = ast_slice },
                                            ),
                                        },
                                    );

                                    return self.definitions.get(node).?.?;
                                }
                            }
                        },
                        .ObjectInstance, .Object => {
                            const object_def = if (callee_type_def.def_type == .ObjectInstance)
                                callee_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object
                            else
                                callee_type_def.resolved_type.?.Object;

                            if (object_def.fields.get(ast_slice.tokens.items(.lexeme)[comp.identifier])) |field| {
                                try self.definitions.put(
                                    allocator,
                                    node,
                                    .{
                                        .location = .{
                                            .uri = try scriptNameToUri(
                                                self.process.io,
                                                allocator,
                                                Parser.buzzLibPath(self.process.io, self.process.environ_map),
                                                ast_slice.tokens.items(.script_name)[field.location],
                                            ),
                                            .range = tokenToRange(ast_slice, field.location, field.location),
                                        },

                                        .def_node = node,
                                        .docblock = self.object_member_docblocks.getContext(
                                            .{
                                                .owner = object_def.location,
                                                .member = field.location,
                                            },
                                            .{ .ast = ast_slice },
                                        ),
                                    },
                                );

                                return self.definitions.get(node).?.?;
                            }
                        },
                        else => {},
                    }
                }
            },
            .UserType => {
                if (ast_slice.nodes.items(.type_def)[node]) |type_def| {
                    if (switch (type_def.def_type) {
                        .Object => type_def.resolved_type.?.Object.location,
                        .ObjectInstance => type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.location,
                        .Enum => type_def.resolved_type.?.Enum.location,
                        .EnumInstance => type_def.resolved_type.?.EnumInstance.of.resolved_type.?.Enum.location,
                        .Protocol => type_def.resolved_type.?.Protocol.location,
                        .ProtocolInstance => type_def.resolved_type.?.ProtocolInstance.of.resolved_type.?.Protocol.location,
                        else => null,
                    }) |location| {
                        try self.definitions.put(
                            allocator,
                            node,
                            .{
                                .location = .{
                                    .uri = try scriptNameToUri(
                                        self.process.io,
                                        allocator,
                                        Parser.buzzLibPath(self.process.io, self.process.environ_map),
                                        ast_slice.tokens.items(.script_name)[location],
                                    ),
                                    .range = tokenToRange(ast_slice, location, location),
                                },

                                .def_node = node,
                            },
                        );

                        return self.definitions.get(node).?.?;
                    }
                }
            },

            // Not a revelant node
            else => {},
        }

        try self.definitions.put(allocator, node, null);

        return null;
    }

    const InlayHintsContext = struct {
        document: *Document,

        fn addTypeInlay(
            self: *InlayHintsContext,
            allocator: std.mem.Allocator,
            type_def: *o.ObjTypeDef,
            location: Token,
            comptime prefix: []const u8,
            comptime suffix: ?[]const u8,
            offBy: u8,
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!void {
            var inlay = std.Io.Writer.Allocating.init(allocator);

            try inlay.writer.writeAll(prefix);
            try type_def.toStringWithoutUnresolved(&inlay.writer, false);
            if (suffix) |sx| try inlay.writer.writeAll(sx);

            if (!self.document.isClientToken(location)) {
                return;
            }

            try self.document.inlay_hints.append(
                allocator,
                .{
                    .position = .{
                        .line = @intCast(location.line),
                        .character = @intCast(@max(1, location.column + location.lexeme.len) - 1 - offBy),
                    },
                    .label = .{
                        .string = try inlay.toOwnedSlice(),
                    },
                    .kind = .Type,
                },
            );
        }

        pub fn processNode(
            self: *InlayHintsContext,
            allocator: std.mem.Allocator,
            ast: Ast.Slice,
            node: Ast.Node.Index,
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!bool {
            switch (ast.nodes.items(.tag)[node]) {
                .VarDeclaration => {
                    const comp = ast.nodes.items(.components)[node].VarDeclaration;
                    const type_def = ast.nodes.items(.type_def)[node];
                    const name = ast.tokens.get(comp.name);

                    // If type was omitted, provide it
                    if (!comp.implicit and comp.type == null and type_def != null) {
                        try self.addTypeInlay(
                            allocator,
                            type_def.?,
                            name,
                            ": ",
                            null,
                            0,
                        );
                    }
                },
                .Function => {
                    const comp = ast.nodes.items(.components)[node].Function;
                    const fun_type_node = comp.function_signature orelse return false;
                    const fun_type = ast.nodes.items(.components)[fun_type_node].FunctionType;
                    const location = ast.tokens.get(ast.nodes.items(.end_location)[fun_type_node]);
                    const fun_type_def = ast.nodes.items(.type_def)[node];

                    if (comp.test_message == null and fun_type.lambda and fun_type.return_type == null and fun_type_def != null) {
                        try self.addTypeInlay(
                            allocator,
                            fun_type_def.?.resolved_type.?.Function.return_type,
                            location,
                            " > ",
                            null,
                            0,
                        );
                    }
                },
                .ObjectInit => {
                    const comp = ast.nodes.items(.components)[node].ObjectInit;
                    const lexemes = ast.tokens.items(.lexeme);
                    const type_defs = ast.nodes.items(.type_def);
                    const locations = ast.nodes.items(.location);

                    for (comp.properties) |property| {
                        const prop_type = if (comp.object) |object|
                            if (type_defs[object]) |type_def|
                                if (type_def.def_type == .Object)
                                    if (type_def.resolved_type.?.Object.fields.get(lexemes[property.name])) |field|
                                        field.type_def
                                    else
                                        null
                                else
                                    null
                            else
                                null
                        else
                            type_defs[property.value];

                        if (prop_type) |type_def| {
                            const name_token = ast.tokens.get(property.name);

                            if (name_token.utility_token) {
                                try self.addTypeInlay(
                                    allocator,
                                    type_def,
                                    ast.tokens.get(locations[property.value]),
                                    ":",
                                    " ",
                                    1,
                                );
                            } else {
                                try self.addTypeInlay(
                                    allocator,
                                    type_def,
                                    name_token,
                                    ": ",
                                    null,
                                    0,
                                );
                            }
                        }
                    }
                },
                else => {},
            }

            return false;
        }
    };

    const MemberDocblockContext = struct {
        document: *Document,

        pub fn processNode(
            self: *MemberDocblockContext,
            allocator: std.mem.Allocator,
            ast: Ast.Slice,
            node: Ast.Node.Index,
        ) std.mem.Allocator.Error!bool {
            switch (ast.nodes.items(.tag)[node]) {
                .ObjectDeclaration => {
                    const type_def = ast.nodes.items(.type_def)[node] orelse return false;
                    if (type_def.def_type != .Object) {
                        return false;
                    }

                    const object_def = type_def.resolved_type.?.Object;

                    for (ast.nodes.items(.components)[node].ObjectDeclaration.members) |member| {
                        if (member.docblock) |docblock| {
                            try self.document.object_member_docblocks.putContext(
                                allocator,
                                .{
                                    .owner = object_def.location,
                                    .member = member.name,
                                },
                                docblock,
                                .{ .ast = ast },
                            );
                        }
                    }
                },
                .Enum => {
                    const type_def = ast.nodes.items(.type_def)[node] orelse return false;
                    if (type_def.def_type != .Enum) {
                        return false;
                    }

                    const enum_def = type_def.resolved_type.?.Enum;

                    for (ast.nodes.items(.components)[node].Enum.cases) |case| {
                        if (case.docblock) |docblock| {
                            try self.document.enum_case_docblocks.putContext(
                                allocator,
                                .{
                                    .owner = enum_def.location,
                                    .member = case.name,
                                },
                                docblock,
                                .{ .ast = ast },
                            );
                        }
                    }
                },
                else => {},
            }

            return false;
        }
    };

    fn computeInlayHints(self: *Document) !void {
        var ctx = InlayHintsContext{
            .document = self,
        };
        const allocator = self.arena.allocator();

        try self.ast.slice().walk(
            allocator,
            &ctx,
            self.ast.root.?,
            .breadthFirst,
        );
    }

    fn collectMemberDocblocks(self: *Document, imports: *const std.StringHashMapUnmanaged(Parser.ScriptImport)) !void {
        var ctx = MemberDocblockContext{
            .document = self,
        };
        const allocator = self.arena.allocator();

        try self.ast.slice().walk(
            allocator,
            &ctx,
            self.ast.root.?,
            .breadthFirst,
        );

        var imports_it = imports.valueIterator();
        while (imports_it.next()) |import| {
            try self.ast.slice().walk(
                allocator,
                &ctx,
                import.function,
                .breadthFirst,
            );
        }
    }
};

extern fn getpid() std.os.linux.pid_t;

pub fn main(init: std.process.Init) !void {
    const allocator: std.mem.Allocator = if (builtin.mode == .Debug or is_wasm)
        init.gpa
    else if (BuildOptions.mimalloc)
        @import("mimalloc.zig").mim_allocator
    else
        std.heap.c_allocator;

    log.debug("Buzz LSP started with PID {}", .{getpid()});

    var read_buffer = [_]u8{0} ** 256;
    var stdio_transport = lsp.Transport.Stdio.init(
        read_buffer[0..],
        std.Io.File.stdin(),
        std.Io.File.stdout(),
    );

    var handler = Handler{
        .process = init,
        .allocator = allocator,
        .transport = &stdio_transport.transport,
    };

    try lsp.basic_server.run(
        init.io,
        allocator,
        &stdio_transport.transport,
        &handler,
        log.err,
    );
}

const Handler = struct {
    process: std.process.Init,
    allocator: std.mem.Allocator,
    transport: *lsp.Transport,
    documents: std.StringHashMapUnmanaged(Document) = .empty,

    pub fn initialize(
        _: *Handler,
        allocator: std.mem.Allocator,
        params: lsp.types.requests.get("initialize").?.Params.?,
    ) !lsp.types.requests.get("initialize").?.Result {
        log.info(
            "client (PID {}) {s}-{s} sent initialize request",
            .{
                params.processId orelse -1,
                if (params.clientInfo) |ci| ci.name else "???",
                if (params.clientInfo) |ci| ci.version orelse "???" else "???",
            },
        );

        var version = std.Io.Writer.Allocating.init(allocator);

        try version.writer.print(
            "{f}",
            .{BuildOptions.version},
        );

        return .{
            .serverInfo = .{
                .name = "Buzz LSP",
                .version = version.written(),
            },
            .capabilities = .{
                .positionEncoding = .@"utf-16",
                .textDocumentSync = .{
                    .text_document_sync_options = .{
                        .openClose = true,
                        .change = .Full,
                        .save = .{ .bool = true },
                    },
                },

                .hoverProvider = .{
                    .hover_options = .{
                        .workDoneProgress = false,
                    },
                },
                .definitionProvider = .{
                    .definition_options = .{
                        .workDoneProgress = false,
                    },
                },
                .declarationProvider = .{
                    .declaration_options = .{
                        .workDoneProgress = false,
                    },
                },
                .typeDefinitionProvider = .{
                    .type_definition_options = .{
                        .workDoneProgress = false,
                    },
                },
                .implementationProvider = .{
                    .implementation_options = .{
                        .workDoneProgress = false,
                    },
                },
                .documentSymbolProvider = .{
                    .document_symbol_options = .{},
                },
                .inlayHintProvider = .{
                    .bool = true,
                },
                .documentFormattingProvider = .{
                    .bool = true,
                },
                .completionProvider = .{
                    .triggerCharacters = &.{"\\"},
                },

                // Keeping those here so I don't forget about them

                // NYI
                .referencesProvider = null,
                .documentHighlightProvider = null,
                .workspaceSymbolProvider = null,
                .documentRangeFormattingProvider = null,
                .documentOnTypeFormattingProvider = null,
                .callHierarchyProvider = null,
                .signatureHelpProvider = null,
                .workspace = null,

                // Probably never
                .codeActionProvider = null,
                .codeLensProvider = null,
                .documentLinkProvider = null,
                .renameProvider = null,
                .foldingRangeProvider = null,
                .selectionRangeProvider = null,
                .executeCommandProvider = null,
                .diagnosticProvider = null,
                .inlineCompletionProvider = null,
                .notebookDocumentSync = null,
            },
        };
    }

    // Load file and replaces previous entry in cache
    fn loadFile(self: *Handler, src: []const u8, uri: []const u8) !void {
        var doc = try Document.init(
            self.process,
            self.allocator,
            src,
            uri,
        );
        var doc_owned = true;
        errdefer if (doc_owned) doc.deinit();

        var res: lsp.types.publish_diagnostics.Params = .{
            .uri = uri,
            .diagnostics = &.{},
        };

        var diagnostics: ?[]lsp.types.Diagnostic = null;
        defer if (diagnostics) |items| self.allocator.free(items);

        if (doc.errors.len > 0) {
            var diags = std.ArrayList(lsp.types.Diagnostic).empty;
            errdefer diags.deinit(self.allocator);

            for (doc.errors) |report| {
                for (report.items) |item| {
                    if (std.mem.eql(u8, item.location.script_name, doc.uri)) {
                        try diags.append(
                            self.allocator,
                            .{
                                .range = .{
                                    .start = .{
                                        .line = @intCast(item.location.line),
                                        .character = @intCast(@max(1, item.location.column) - 1),
                                    },
                                    .end = .{
                                        .line = @intCast(item.end_location.line),
                                        .character = @intCast(@max(1, item.end_location.column) - 1),
                                    },
                                },
                                .severity = switch (item.kind) {
                                    .@"error" => .Error,
                                    .warning => .Warning,
                                    .hint => .Hint,
                                },
                                .message = item.message,
                            },
                        );
                    }
                }
            }

            diagnostics = try diags.toOwnedSlice(self.allocator);
            res.diagnostics = diagnostics.?;
        }

        log.debug("Loaded document `{s}`", .{uri});

        const gop = try self.documents.getOrPut(self.allocator, uri);
        var remove_inserted_entry = !gop.found_existing;
        errdefer {
            if (remove_inserted_entry) {
                _ = self.documents.remove(uri);
            }
        }

        if (gop.found_existing) {
            gop.value_ptr.deinit();
        } else {
            gop.key_ptr.* = try self.allocator.dupe(u8, uri);
        }

        gop.value_ptr.* = doc;
        doc_owned = false;
        remove_inserted_entry = false;

        try self.transport.writeNotification(
            self.process.io,
            self.allocator,
            "textDocument/publishDiagnostics",
            lsp.types.publish_diagnostics.Params,
            res,
            .{},
        );
    }

    pub fn shutdown(
        _: Handler,
        _: std.mem.Allocator,
        _: void,
    ) !?void {}

    pub fn initialized(
        _: *Handler,
        _: std.mem.Allocator,
        _: lsp.types.InitializedParams,
    ) !void {}

    pub fn exit(
        _: Handler,
        _: std.mem.Allocator,
        _: void,
    ) !void {}

    pub fn @"textDocument/didOpen"(
        self: *Handler,
        _: std.mem.Allocator,
        notification: lsp.types.notifications.get("textDocument/didOpen").?.Params.?,
    ) !void {
        try self.loadFile(
            notification.textDocument.text,
            notification.textDocument.uri,
        );
    }

    pub fn @"textDocument/didChange"(
        self: *Handler,
        allocator: std.mem.Allocator,
        notification: lsp.types.notifications.get("textDocument/didChange").?.Params.?,
    ) !void {
        errdefer |e| log.err("changeDocument failed: {any}", .{e});

        if (notification.contentChanges.len == 0) {
            return;
        }

        const file = self.documents.get(notification.textDocument.uri) orelse {
            log.err(
                "changeDocument failed: unknown file: {any}",
                .{
                    notification.textDocument.uri,
                },
            );

            return error.InvalidParams;
        };

        var current_text = try allocator.dupe(u8, file.src);
        defer allocator.free(current_text);

        for (notification.contentChanges) |change_| {
            const new_text = switch (change_) {
                .text_document_content_change_whole_document => |change| try allocator.dupe(u8, change.text),
                .text_document_content_change_partial => |change| blk: {
                    const old_text = current_text;
                    const range = change.range;

                    const start_idx = lsp.offsets.positionToIndex(
                        old_text,
                        range.start,
                        .@"utf-8",
                    );

                    const end_idx = lsp.offsets.positionToIndex(
                        old_text,
                        range.end,
                        .@"utf-8",
                    );

                    var new_text = std.ArrayList(u8).empty;
                    errdefer new_text.deinit(allocator);

                    try new_text.appendSlice(allocator, old_text[0..start_idx]);
                    try new_text.appendSlice(allocator, change.text);
                    try new_text.appendSlice(allocator, old_text[end_idx..]);

                    break :blk try new_text.toOwnedSlice(allocator);
                },
            };

            allocator.free(current_text);
            current_text = new_text;
        }

        // Would be great to not have to reparse the whole thing
        try self.loadFile(
            current_text,
            notification.textDocument.uri,
        );
    }

    pub fn @"textDocument/didSave"(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.notifications.get("textDocument/didSave").?.Params.?,
    ) !void {}

    pub fn @"textDocument/didClose"(
        self: *Handler,
        _: std.mem.Allocator,
        notification: lsp.types.notifications.get("textDocument/didClose").?.Params.?,
    ) !void {
        var kv = self.documents.fetchRemove(notification.textDocument.uri) orelse return;
        self.allocator.free(kv.key);
        kv.value.deinit();
    }

    pub fn @"textDocument/documentSymbol"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.requests.get("textDocument/documentSymbol").?.Params.?,
    ) !lsp.types.requests.get("textDocument/documentSymbol").?.Result {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            return .{
                .document_symbols = kv.value_ptr.symbols.items,
            };
        }

        return null;
    }

    fn dotCompletion(
        document: *Document,
        allocator: std.mem.Allocator,
        result: *std.ArrayList(lsp.types.completion.Item),
        completion_prefix: CompletionPrefix,
    ) !bool {
        const before = result.items.len;

        // Is there partial or complete dot expression to complete?
        const tags = document.ast.nodes.items(.tag);
        const components = document.ast.nodes.items(.components);

        var dot_callee: ?Ast.Node.Index = null;
        var at_dot = false;

        // If there was no error, we have a clean AST to work with and we can walk it to find our node
        if (document.errors.len == 0) {
            if (try document.nodeContainingRange(completion_prefix.range)) |node| {
                if (tags[node] == .Dot) {
                    dot_callee = components[node].Dot.callee;
                }
            }
        } else if (std.mem.endsWith(u8, completion_prefix.text, ".")) {
            // Otherwise we don't search nodes by walking the AST which might miss nodes appended before errors
            // Are we just after a `.`
            at_dot = true;
            if (document.findNodeContainingRange(
                .{
                    .start = completion_prefix.range.start,
                    .end = .{
                        .line = completion_prefix.range.end.line,
                        .character = completion_prefix.range.end.character - 1,
                    },
                },
            )) |node| {
                dot_callee = node;
            }
            // Else search for a dot node (so the completion must be at something like `callee.completeM|`)
        } else if (document.findNodeContainingRange(completion_prefix.range)) |node| {
            if (tags[node] == .Dot) {
                dot_callee = components[node].Dot.callee;
            }
        }

        if (dot_callee) |callee| {
            if (document.ast.nodes.items(.type_def)[callee]) |callee_type_def| {
                // Adjust prefix: we only want to filter from the `.`
                const prefix = if (at_dot)
                    CompletionPrefix{
                        .range = .{
                            .start = .{
                                .line = completion_prefix.range.end.line,
                                .character = completion_prefix.range.end.character,
                            },
                            .end = completion_prefix.range.end,
                        },
                        .text = "",
                    }
                else if (std.mem.indexOf(u8, completion_prefix.text, ".")) |dot_idx|
                    CompletionPrefix{
                        .range = .{
                            .start = .{
                                .line = completion_prefix.range.start.line,
                                .character = completion_prefix.range.start.character + @as(u32, @intCast(dot_idx)) + 1,
                            },
                            .end = completion_prefix.range.end,
                        },
                        .text = completion_prefix.text[dot_idx + 1 ..],
                    }
                else
                    completion_prefix; // Should not really be used because it means it's not a dot completion

                log.debug(
                    "Possible callee of type {s} with prefix {s}",
                    .{
                        @tagName(callee_type_def.def_type),
                        prefix.text,
                    },
                );

                switch (callee_type_def.def_type) {
                    .List => {
                        for (o.ObjList.members_name.keys()) |key| {
                            try appendCompletionItem(
                                result,
                                allocator,
                                key,
                                prefix,
                            );
                        }
                    },
                    .Map => {
                        for (o.ObjMap.members_name.keys()) |key| {
                            try appendCompletionItem(
                                result,
                                allocator,
                                key,
                                prefix,
                            );
                        }
                    },
                    .String => {
                        for (o.ObjString.members_name.keys()) |key| {
                            try appendCompletionItem(
                                result,
                                allocator,
                                key,
                                prefix,
                            );
                        }
                    },
                    .Range => {
                        for (o.ObjRange.members_name.keys()) |key| {
                            try appendCompletionItem(
                                result,
                                allocator,
                                key,
                                prefix,
                            );
                        }
                    },
                    .Pattern => {
                        for (o.ObjPattern.members_name.keys()) |key| {
                            try appendCompletionItem(
                                result,
                                allocator,
                                key,
                                prefix,
                            );
                        }
                    },
                    .Fiber => {
                        for (o.ObjFiber.members_name.keys()) |key| {
                            try appendCompletionItem(
                                result,
                                allocator,
                                key,
                                prefix,
                            );
                        }
                    },
                    .Object => {
                        var it = callee_type_def.resolved_type.?.Object.fields.iterator();
                        while (it.next()) |kv| {
                            if (kv.value_ptr.*.static) {
                                try appendCompletionItem(
                                    result,
                                    allocator,
                                    kv.key_ptr.*,
                                    prefix,
                                );
                            }
                        }
                    },
                    .ObjectInstance => {
                        var it = callee_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.fields.iterator();
                        while (it.next()) |kv| {
                            if (!kv.value_ptr.*.static) {
                                try appendCompletionItem(
                                    result,
                                    allocator,
                                    kv.key_ptr.*,
                                    prefix,
                                );
                            }
                        }
                    },
                    .ProtocolInstance => {
                        for (callee_type_def.resolved_type.?.ProtocolInstance.of.resolved_type.?.Protocol.methods.keys()) |key| {
                            try appendCompletionItem(
                                result,
                                allocator,
                                key,
                                prefix,
                            );
                        }
                    },
                    .ForeignContainer => {
                        for (callee_type_def.resolved_type.?.ForeignContainer.fields.keys()) |key| {
                            try appendCompletionItem(
                                result,
                                allocator,
                                key,
                                prefix,
                            );
                        }
                    },
                    .EnumInstance => try appendCompletionItem(
                        result,
                        allocator,
                        "value",
                        prefix,
                    ),
                    .Enum => {
                        for (callee_type_def.resolved_type.?.Enum.cases) |key| {
                            try appendCompletionItem(
                                result,
                                allocator,
                                key,
                                prefix,
                            );
                        }
                    },
                    else => {},
                }
            }
        }

        return result.items.len > before;
    }

    pub fn @"textDocument/completion"(
        self: Handler,
        allocator: std.mem.Allocator,
        notification: lsp.types.requests.get("textDocument/completion").?.Params.?,
    ) !lsp.types.requests.get("textDocument/completion").?.Result {
        var result = std.ArrayList(lsp.types.completion.Item).empty;

        if (self.documents.getPtr(notification.textDocument.uri)) |document| {
            const cursor_offset = @min(
                lsp.offsets.positionToIndex(
                    document.src,
                    notification.position,
                    .@"utf-8",
                ),
                document.src.len,
            );
            const completion_prefix = completionPrefixAtOffset(
                document.src,
                document.uri,
                document.ast.slice(),
                cursor_offset,
            );

            // TODO: field completion in object init

            // Try dot completioni
            if (completion_prefix == null or
                !(try dotCompletion(
                    document,
                    allocator,
                    &result,
                    completion_prefix.?,
                )))
            { // Else add global and locals
                // Globals and keyword completion items
                for (document.completion_labels.keys()) |label| {
                    try appendCompletionItem(
                        &result,
                        allocator,
                        label,
                        completion_prefix,
                    );
                }

                // Locals reachable at that offset in the source
                var local_labels = std.StringArrayHashMapUnmanaged(void).empty;
                defer local_labels.deinit(allocator);

                if (document.ast.root) |root| {
                    const ast = document.ast.slice();
                    const tags = ast.nodes.items(.tag);
                    const components = ast.nodes.items(.components);
                    const lexemes = ast.tokens.items(.lexeme);

                    for (try ast.visibleSymbolsAtOffset(
                        allocator,
                        root,
                        cursor_offset,
                    )) |symbol_node| {
                        if (tags[symbol_node] == .VarDeclaration) {
                            const decl = components[symbol_node].VarDeclaration;
                            const label = lexemes[decl.name];

                            if (document.completion_labels.getIndex(label) != null or
                                (try local_labels.getOrPut(allocator, label)).found_existing)
                            {
                                continue;
                            }

                            try appendCompletionItem(
                                &result,
                                allocator,
                                label,
                                completion_prefix,
                            );
                        }
                    }
                }
            }
        }

        return .{
            .completion_list = .{
                .isIncomplete = false,
                .items = try result.toOwnedSlice(allocator),
            },
        };
    }

    pub fn @"textDocument/hover"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.requests.get("textDocument/hover").?.Params.?,
    ) !lsp.types.requests.get("textDocument/hover").?.Result {
        if (self.documents.getPtr(notification.textDocument.uri)) |document| {
            if (try document.hoverMarkupAtPosition(notification.position)) |markup| {
                return .{
                    .contents = .{
                        .markup_content = .{
                            .kind = .markdown,
                            .value = markup,
                        },
                    },
                };
            }
        }
        return null;
    }

    pub fn @"textDocument/definition"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.requests.get("textDocument/definition").?.Params.?,
    ) !lsp.types.requests.get("textDocument/definition").?.Result {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            const document = kv.value_ptr;

            if (try document.nodeUnderPosition(notification.position)) |origin| {
                if (document.definitions.get(origin)) |result| {
                    if (result) |res| {
                        return .{
                            .definition = .{
                                .location = res.location,
                            },
                        };
                    }
                }

                return if (try document.definition(origin)) |def|
                    .{
                        .definition = .{
                            .location = def.location,
                        },
                    }
                else
                    null;
            }
        }

        return null;
    }

    pub fn @"textDocument/references"(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.requests.get("textDocument/references").?.Params.?,
    ) !lsp.types.requests.get("textDocument/references").?.Result {
        return null;
    }

    pub fn @"textDocument/formatting"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.requests.get("textDocument/formatting").?.Params.?,
    ) !lsp.types.requests.get("textDocument/formatting").?.Result {
        if (self.documents.get(notification.textDocument.uri)) |document| {
            var result = std.Io.Writer.Allocating.init(self.allocator);

            Renderer.render(
                self.allocator,
                &result.writer,
                document.ast,
                .{},
            ) catch |err| {
                log.err(
                    "Could not format {s}: {any}",
                    .{
                        notification.textDocument.uri,
                        err,
                    },
                );

                return null;
            };

            var text_edit = std.ArrayList(lsp.types.TextEdit).empty;
            try text_edit.append(
                self.allocator,
                .{
                    .range = document.wholeDocumentRange(),
                    .newText = result.written(),
                },
            );

            return try text_edit.toOwnedSlice(self.allocator);
        }
        return null;
    }

    pub fn @"textDocument/semanticTokens/full"(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.requests.get("textDocument/semanticTokens/full").?.Params.?,
    ) !lsp.types.requests.get("textDocument/semanticTokens/full").?.Result {
        return null;
    }

    pub fn @"textDocument/inlayHint"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.requests.get("textDocument/inlayHint").?.Params.?,
    ) !lsp.types.requests.get("textDocument/inlayHint").?.Result {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            const document = kv.value_ptr;
            const allocator = document.arena.allocator();

            var result = std.ArrayList(lsp.types.InlayHint).empty;

            for (document.inlay_hints.items) |hint| {
                // If inlay position under the requested range
                if ((hint.position.line > notification.range.start.line and
                    hint.position.line < notification.range.end.line) or
                    (hint.position.line == notification.range.start.line and hint.position.character >= notification.range.start.character) or
                    (hint.position.line == notification.range.end.line and hint.position.character <= notification.range.end.character))
                {
                    try result.append(allocator, hint);
                }
            }

            return if (result.items.len > 0)
                try result.toOwnedSlice(allocator)
            else
                null;
        }

        return null;
    }

    /// Handle a response that we have received from the client.
    /// Doesn't usually happen unless we explicitly send a request to the client.
    pub fn onResponse(_: Handler, _: std.mem.Allocator, resp: lsp.JsonRPCMessage.Response) !void {
        const id: []const u8 = if (resp.id) |id|
            switch (id) {
                .string => id.string,
                .number => |nid| {
                    log.warn("received response from client with id '{d}' that has no handler!", .{nid});
                    return;
                },
            }
        else
            "N/A";

        if (resp.result_or_error == .@"error") {
            const err = resp.result_or_error.@"error";
            log.err("Error response for '{s}': {}, {s}", .{ id, err.code, err.message });
            return;
        }

        log.warn("received response from client with id '{s}' that has no handler!", .{id});
    }
};

/// Prefix text and replacement range for a completion request at the cursor.
const CompletionPrefix = struct {
    /// Text already present in the document that completion labels must match.
    text: []const u8,

    /// LSP range replaced by the selected completion item.
    range: lsp.types.Range,
};

/// Appends a completion item, applying prefix filtering and replacement edits when available.
fn appendCompletionItem(
    result: *std.ArrayList(lsp.types.completion.Item),
    allocator: std.mem.Allocator,
    label: []const u8,
    prefix: ?CompletionPrefix,
) !void {
    if (prefix) |completion_prefix| {
        if (!std.mem.startsWith(u8, label, completion_prefix.text)) {
            return;
        }

        try result.append(
            allocator,
            .{
                .label = label,
                .textEdit = .{
                    .text_edit = .{
                        .range = completion_prefix.range,
                        .newText = label,
                    },
                },
            },
        );
    } else {
        try result.append(
            allocator,
            .{ .label = label },
        );
    }
}

/// Finds the contiguous completion prefix immediately before or under the cursor.
fn completionPrefixAtOffset(
    source: []const u8,
    uri: []const u8,
    ast: Ast.Slice,
    cursor_offset: usize,
) ?CompletionPrefix {
    const bounded_cursor = @min(cursor_offset, source.len);
    const tags = ast.tokens.items(.tag);
    const lexemes = ast.tokens.items(.lexeme);
    const offsets = ast.tokens.items(.offset);
    const script_names = ast.tokens.items(.script_name);
    const utility_tokens = ast.tokens.items(.utility_token);

    // `current_prefix_*` describes the contiguous run of prefix tokens we are
    // currently scanning. For `std\pr`, it starts at `std` and ends after `pr`.
    var current_prefix_start_token: ?usize = null;
    var current_prefix_end: usize = 0;

    // Once the cursor is inside the current run, this becomes the first token
    // of the prefix to replace.
    var prefix_start_token: ?usize = null;

    for (tags, 0..) |tag, index| {
        if (utility_tokens[index] or
            !std.mem.eql(u8, script_names[index], uri) or
            !isCompletionPrefixToken(tag, lexemes[index]))
        {
            continue;
        }

        const token_start = offsets[index];
        const token_end = token_start + lexemes[index].len;
        if (token_start >= bounded_cursor) {
            continue;
        }

        // A gap means whitespace, punctuation, or another non-prefix token broke
        // the typed prefix. Start a new run from the current token.
        if (current_prefix_start_token == null or token_start != current_prefix_end) {
            current_prefix_start_token = index;
        }
        current_prefix_end = token_end;

        // The cursor is inside or just after this token, so the current run is
        // the text the selected completion item should replace.
        if (bounded_cursor <= token_end) {
            prefix_start_token = current_prefix_start_token;
            break;
        }
    }

    if (prefix_start_token == null or offsets[prefix_start_token.?] >= bounded_cursor) {
        return null;
    }

    const prefix_start = offsets[prefix_start_token.?];
    const text = source[prefix_start..bounded_cursor];

    // This should be redundant with the contiguous-token tracking above, but it
    // keeps the edit conservative if scanner recovery ever exposes odd gaps.
    if (std.mem.indexOfAny(u8, text, " \t\r\n") != null) {
        return null;
    }

    // Token columns are already LSP UTF-8 columns; use the first prefix token
    // for the range start and the typed byte length for the range end.
    const range_start = tokenToRange(
        ast,
        @intCast(prefix_start_token.?),
        @intCast(prefix_start_token.?),
    ).start;

    return .{
        .text = text,
        .range = .{
            .start = range_start,
            .end = .{
                .line = range_start.line,
                .character = range_start.character + @as(u32, @intCast(text.len)),
            },
        },
    };
}

/// Returns whether a token can be part of a typed completion prefix.
fn isCompletionPrefixToken(tag: Token.Tag, lexeme: []const u8) bool {
    return tag == .AntiSlash or
        tag == .Dot or
        tag == .Identifier or
        Token.keywords.get(lexeme) != null;
}

/// Builds an LSP range from AST token locations.
fn tokenToRange(ast: Ast.Slice, location: Ast.TokenIndex, end_location: Ast.TokenIndex) lsp.types.Range {
    const lines = ast.tokens.items(.line);
    const columns = ast.tokens.items(.column);

    return .{
        .start = .{
            .line = @intCast(lines[location]),
            .character = @intCast(@max(1, columns[location]) - 1),
        },
        .end = .{
            .line = @intCast(lines[end_location]),
            .character = @intCast(@max(1, columns[end_location]) - 1),
        },
    };
}

/// Returns the package wrapper kind for shallow bare manifest objects.
fn packageDocumentKind(allocator: std.mem.Allocator, uri: []const u8, source: []const u8) !?PackageDocumentKind {
    const file_name = if (try localPathFromFileUri(allocator, uri)) |path|
        std.fs.path.basename(path)
    else
        std.fs.path.basename(uri);

    if (!std.mem.startsWith(u8, std.mem.trim(u8, source, " \n\r\t"), ".{")) {
        return null;
    }

    if (std.mem.eql(u8, file_name, Package.MANIFEST)) {
        return .manifest;
    }

    if (std.mem.eql(u8, file_name, Package.MANIFEST_LOCK)) {
        return .manifest_lock;
    }

    return null;
}

/// Converts a local `file://` document URI into the script path used by parser semantics.
/// For example, `file:///tmp/project/main.buzz` becomes `/tmp/project/main.buzz`.
fn localPathFromFileUri(allocator: std.mem.Allocator, uri: []const u8) !?[]const u8 {
    const parsed = std.Uri.parse(uri) catch return null;
    if (!std.ascii.eqlIgnoreCase(parsed.scheme, "file")) {
        return null;
    }

    if (parsed.host) |host| {
        const raw_host = try host.toRawMaybeAlloc(allocator);
        if (raw_host.len > 0 and !std.ascii.eqlIgnoreCase(raw_host, "localhost")) {
            return null;
        }
    }

    return try parsed.path.toRawMaybeAlloc(allocator);
}

fn scriptNameToUri(io: std.Io, allocator: std.mem.Allocator, buzz_lib_path: []const u8, script_name: []const u8) ![]const u8 {
    if (isClientUri(script_name)) {
        return script_name;
    }

    var allocated_path: ?[]u8 = null;
    defer if (allocated_path) |path| allocator.free(path);

    const path = if (staticScriptFileName(script_name)) |file_name| path: {
        allocated_path = try std.fs.path.join(
            allocator,
            &.{ buzz_lib_path, file_name },
        );
        break :path allocated_path.?;
    } else if (std.fs.path.isAbsolute(script_name))
        script_name
    else path: {
        var cwd_buffer: [std.Io.Dir.max_path_bytes]u8 = undefined;
        const cwd_len = try std.process.currentPath(io, &cwd_buffer);
        allocated_path = try std.fs.path.join(
            allocator,
            &.{
                cwd_buffer[0..cwd_len],
                script_name,
            },
        );
        break :path allocated_path.?;
    };

    return std.fmt.allocPrint(
        allocator,
        "{f}",
        .{std.Uri{
            .scheme = "file",
            .host = .{ .percent_encoded = "" },
            .path = .{ .raw = path },
        }},
    );
}

fn isClientUri(text: []const u8) bool {
    return std.mem.startsWith(u8, text, "file:") or
        std.mem.startsWith(u8, text, "untitled:") or
        std.mem.startsWith(u8, text, "vscode-remote:");
}

fn staticScriptFileName(script_name: []const u8) ?[]const u8 {
    return if (static_libraries.byName(script_name)) |library|
        library.header.path
    else
        null;
}

fn staticScriptNameFromUri(uri: []const u8) ?[]const u8 {
    inline for (static_libraries.all) |library| {
        if (isStaticScriptUri(uri, library.header.path)) {
            return library.header.name;
        }
    }

    return null;
}

fn isStaticScriptUri(uri: []const u8, file_name: []const u8) bool {
    var src_lib_buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const src_lib = std.fmt.bufPrint(
        &src_lib_buf,
        "/src/lib/{s}",
        .{file_name},
    ) catch return false;

    var installed_lib_buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const installed_lib = std.fmt.bufPrint(
        &installed_lib_buf,
        "/lib/buzz/{s}",
        .{file_name},
    ) catch return false;

    return std.mem.endsWith(u8, uri, src_lib) or
        std.mem.endsWith(u8, uri, installed_lib);
}
