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
const CodeGen = @import("Codegen.zig");
const Token = @import("Token.zig");
const Renderer = @import("renderer.zig").Renderer;
const o = @import("obj.zig");

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

const Document = struct {
    pub const Definition = struct {
        location: lsp.types.Location,
        def_node: Ast.Node.Index,
        docblock: ?Ast.TokenIndex = null,
    };

    arena: std.heap.ArenaAllocator,
    process: std.process.Init,
    src: [*:0]const u8,
    /// Not owned by this struct
    uri: []const u8,
    ast: Ast,
    errors: []const Reporter.Report,

    /// Symbols collected in the document
    symbols: std.ArrayList(lsp.types.DocumentSymbol) = .empty,

    /// Cache for previous gotoDefinition
    definitions: std.AutoHashMapUnmanaged(Ast.Node.Index, ?Definition) = .empty,

    /// Cache for node under position
    node_under_position: std.AutoHashMapUnmanaged(lsp.types.Position, ?Ast.Node.Index) = .empty,

    /// Cache for hover
    node_hover: std.AutoHashMapUnmanaged(Ast.Node.Index, []const u8) = .empty,

    /// Object member docblocks indexed by object and member token lexemes.
    object_member_docblocks: MemberDocblockKey.HashMap(Ast.TokenIndex) = .empty,

    /// Enum case docblocks indexed by enum and case token lexemes.
    enum_case_docblocks: MemberDocblockKey.HashMap(Ast.TokenIndex) = .empty,

    /// Cache for inlay hints
    inlay_hints: std.ArrayList(lsp.types.InlayHint) = .empty, // I tried to make this a simple slice but the data was lost I don't know why

    pub fn init(process: std.process.Init, parent_allocator: std.mem.Allocator, src: [*:0]const u8, uri: []const u8) !Document {
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        const allocator = arena.allocator();

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

        const owned_uri = try allocator.dupe(u8, uri);
        const std_lib_script_name = staticScriptNameFromUri(owned_uri);

        // If there's parsing error `parse` does not return the AST, but we can still use it however incomplete
        const ast = (parser.parse(
            std.mem.span(src),
            if (std_lib_script_name != null) owned_uri else null,
            std_lib_script_name orelse owned_uri,
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
            .src = src,
            .uri = owned_uri,
            .ast = ast,
            .errors = errors.items,
        };

        if (ast.root != null) {
            doc.computeInlayHints() catch return error.OutOfMemory;
            try doc.collectMemberDocblocks(&imports);
        }

        return doc;
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
                        std.mem.span(self.src),
                        "\n",
                    ),
                ),
                .character = @intCast(
                    std.mem.span(self.src)[std.mem.lastIndexOfScalar(
                        u8,
                        std.mem.span(self.src),
                        '\n',
                    ) orelse 0 ..].len,
                ),
            },
        };
    }

    const NodeUnderPositionContext = struct {
        result: ?Ast.Node.Index = null,
        position: lsp.types.Position,

        pub fn processNode(
            self: *NodeUnderPositionContext,
            _: std.mem.Allocator,
            ast: Ast.Slice,
            node: Ast.Node.Index,
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            const locations = ast.nodes.items(.location);
            const location = ast.tokens.get(locations[node]);
            const end_locations = ast.nodes.items(.end_location);
            const end_location = ast.tokens.get(end_locations[node]);

            // Ignore root node and imports
            if (locations[node] == 0) {
                return false;
            }

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

    pub fn nodeUnderPosition(self: *Document, position: lsp.types.Position) !?Ast.Node.Index {
        if (self.ast.root == null) {
            return null;
        }

        const allocator = self.arena.allocator();

        const nodeEntry = try self.node_under_position.getOrPut(allocator, position);

        if (!nodeEntry.found_existing) {
            var node_ctx = NodeUnderPositionContext{
                .position = position,
            };

            self.ast.slice().walk(allocator, &node_ctx, self.ast.root.?) catch |err| {
                log.err("nodeUnderPosition: {any}", .{err});
            };

            if (node_ctx.result) |res| {
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

            nodeEntry.value_ptr.* = node_ctx.result;
        }

        return nodeEntry.value_ptr.*;
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
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!void {
            var inlay = std.Io.Writer.Allocating.init(allocator);

            try inlay.writer.writeAll(prefix);
            try type_def.toString(&inlay.writer, false);

            try self.document.inlay_hints.append(
                allocator,
                .{
                    .position = .{
                        .line = @intCast(location.line),
                        .character = @intCast(@max(1, location.column + location.lexeme.len) - 1),
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
                        );
                    }
                },
                .ObjectInit => {
                    const comp = ast.nodes.items(.components)[node].ObjectInit;
                    const lexemes = ast.tokens.items(.lexeme);
                    const type_defs = ast.nodes.items(.type_def);

                    for (comp.properties) |property| {
                        const prop_type = if (comp.object) |object|
                            if (type_defs[object]) |type_def|
                                if (type_def.resolved_type.?.Object.fields.get(lexemes[property.name])) |field|
                                    field.type_def
                                else
                                    null
                            else
                                null
                        else
                            type_defs[property.value];

                        if (prop_type) |type_def| {
                            try self.addTypeInlay(
                                allocator,
                                type_def,
                                ast.tokens.get(property.name),
                                ": ",
                            );
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
        );

        var imports_it = imports.valueIterator();
        while (imports_it.next()) |import| {
            try self.ast.slice().walk(
                allocator,
                &ctx,
                import.function,
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
    offset_encoding: lsp.offsets.Encoding = .@"utf-16",

    pub fn initialize(
        self: *Handler,
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
                .positionEncoding = switch (self.offset_encoding) {
                    .@"utf-8" => .@"utf-8",
                    .@"utf-16" => .@"utf-16",
                    .@"utf-32" => .@"utf-32",
                },
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

                // Keeping those here so I don't forget about them

                // NYI
                .completionProvider = null,
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
    fn loadFile(self: *Handler, _: std.mem.Allocator, src: [*:0]const u8, uri: []const u8) !void {
        var res: lsp.types.publish_diagnostics.Params = .{
            .uri = uri,
            .diagnostics = &.{},
        };

        const doc = try Document.init(
            self.process,
            self.allocator,
            src,
            uri,
        );

        log.debug("Loaded document `{s}`", .{uri});

        const gop = try self.documents.getOrPut(self.allocator, uri);
        errdefer _ = self.documents.remove(uri);

        if (gop.found_existing) {
            gop.value_ptr.deinit();
        } else {
            gop.key_ptr.* = try self.allocator.dupe(u8, uri);
        }

        gop.value_ptr.* = doc;

        if (doc.errors.len > 0) {
            var diags = std.ArrayList(lsp.types.Diagnostic).empty;
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

            res.diagnostics = try diags.toOwnedSlice(self.allocator);
        }

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
        const new_text = try self.allocator.dupeZ(
            u8,
            notification.textDocument.text,
        ); // We informed the client that we only do full document syncs
        errdefer self.allocator.free(new_text);

        try self.loadFile(
            self.allocator,
            new_text,
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

        for (notification.contentChanges) |change_| {
            const new_text = switch (change_) {
                .text_document_content_change_whole_document => |change| try self.allocator.dupeZ(u8, change.text),
                .text_document_content_change_partial => |change| blk: {
                    const old_text = std.mem.span(file.src);
                    const range = change.range;

                    const start_idx = lsp.offsets.positionToIndex(
                        old_text,
                        range.start,
                        self.offset_encoding,
                    );

                    const end_idx = lsp.offsets.positionToIndex(
                        old_text,
                        range.end,
                        self.offset_encoding,
                    );

                    var new_text = std.ArrayList(u8).empty;
                    errdefer new_text.deinit(self.allocator);

                    try new_text.appendSlice(self.allocator, old_text[0..start_idx]);
                    try new_text.appendSlice(self.allocator, change.text);
                    try new_text.appendSlice(self.allocator, old_text[end_idx..]);

                    break :blk try new_text.toOwnedSliceSentinel(self.allocator, 0);
                },
            };
            errdefer self.allocator.free(new_text);

            // Would be great to not have to reparse the whole thing
            try self.loadFile(
                allocator,
                new_text,
                notification.textDocument.uri,
            );
        }
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

    const DocumentSymbolContext = struct {
        document: *Document,

        pub fn processNode(
            self: DocumentSymbolContext,
            _: std.mem.Allocator,
            ast: Ast.Slice,
            node: Ast.Node.Index,
        ) (std.mem.Allocator.Error || std.fmt.BufPrintError || error{WriteFailed})!bool {
            const lexemes = ast.tokens.items(.lexeme);
            const locations = ast.nodes.items(.location);
            const end_locations = ast.nodes.items(.end_location);
            const components = ast.nodes.items(.components)[node];
            const type_def = ast.nodes.items(.type_def)[node];
            const allocator = self.document.arena.allocator();

            switch (ast.nodes.items(.tag)[node]) {
                .VarDeclaration => {
                    const name = lexemes[components.VarDeclaration.name];

                    if (components.VarDeclaration.slot_type == .Global and (name.len > 1 or name[0] != '_')) {
                        try self.document.symbols.append(
                            allocator,
                            .{
                                .name = lexemes[components.VarDeclaration.name],
                                .detail = if (type_def) |td|
                                    try td.toStringAlloc(allocator, false)
                                else
                                    null,
                                .kind = if (type_def != null and !type_def.?.isMutable() and components.VarDeclaration.final)
                                    .Constant
                                else
                                    .Variable,
                                .range = tokenToRange(ast, locations[node], end_locations[node]),
                                .selectionRange = tokenToRange(ast, locations[node], end_locations[node]),
                            },
                        );
                    }
                },
                .Enum => {
                    var children = std.ArrayList(lsp.types.DocumentSymbol).empty;

                    for (components.Enum.cases) |case| {
                        const range = tokenToRange(
                            ast,
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

                    try self.document.symbols.append(
                        allocator,
                        .{
                            .name = lexemes[components.Enum.name],
                            .detail = if (type_def) |td|
                                try td.toStringAlloc(allocator, false)
                            else
                                null,
                            .kind = .Enum,
                            .range = tokenToRange(ast, locations[node], end_locations[node]),
                            .selectionRange = tokenToRange(ast, locations[node], end_locations[node]),
                            .children = try children.toOwnedSlice(allocator),
                        },
                    );
                },
                .ObjectDeclaration => {
                    var children = std.ArrayList(lsp.types.DocumentSymbol).empty;

                    if (type_def) |td| {
                        var it = td.resolved_type.?.Object.fields.iterator();
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
                                    .range = tokenToRange(ast, field.location, field.location),
                                    .selectionRange = tokenToRange(ast, field.location, field.location),
                                },
                            );
                        }
                    }

                    try self.document.symbols.append(
                        allocator,
                        .{
                            .name = lexemes[components.ObjectDeclaration.name],
                            .detail = if (type_def) |td|
                                try td.toStringAlloc(allocator, false)
                            else
                                null,
                            .kind = .Struct,
                            .range = tokenToRange(ast, locations[node], end_locations[node]),
                            .selectionRange = tokenToRange(ast, locations[node], end_locations[node]),
                            .children = try children.toOwnedSlice(allocator),
                        },
                    );
                },
                .ProtocolDeclaration => {
                    var children = std.ArrayList(lsp.types.DocumentSymbol).empty;

                    if (type_def) |td| {
                        var it = td.resolved_type.?.Protocol.methods.iterator();
                        while (it.next()) |kv| {
                            const method = kv.value_ptr.*;

                            const method_location = td.resolved_type.?.Protocol.methods_locations.get(kv.key_ptr.*).?;
                            const range = tokenToRange(ast, method_location, method_location);

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

                    try self.document.symbols.append(
                        allocator,
                        .{
                            .name = lexemes[components.ProtocolDeclaration.name],
                            .detail = if (type_def) |td|
                                try td.toStringAlloc(allocator, false)
                            else
                                null,
                            .kind = .Interface,
                            .range = tokenToRange(ast, locations[node], end_locations[node]),
                            .selectionRange = tokenToRange(ast, locations[node], end_locations[node]),
                            .children = try children.toOwnedSlice(allocator),
                        },
                    );
                },
                .Function => fun: {
                    if (type_def) |td| {
                        const fun_def = td.resolved_type.?.Function;

                        switch (fun_def.function_type) {
                            .Method, // Already covered when looking at ObjectDeclaration
                            .Script, // Imported script
                            .ScriptEntryPoint, // Script entry point
                            .Anonymous, // No name to list
                            .Repl, // Should not happen
                            => break :fun,
                            .Function,
                            .EntryPoint,
                            .Test,
                            .Extern,
                            .Abstract,
                            => {},
                        }

                        try self.document.symbols.append(
                            allocator,
                            .{
                                .name = if (fun_def.function_type == .Test)
                                    lexemes[components.Function.test_message.?]
                                else
                                    fun_def.name.string,
                                .detail = try td.toStringAlloc(allocator, false),
                                .kind = .Function,
                                .range = tokenToRange(ast, locations[node], end_locations[node]),
                                .selectionRange = tokenToRange(ast, locations[node], end_locations[node]),
                            },
                        );
                    }
                },
                else => {},
            }

            return false;
        }
    };

    pub fn @"textDocument/documentSymbol"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.requests.get("textDocument/documentSymbol").?.Params.?,
    ) !lsp.types.requests.get("textDocument/documentSymbol").?.Result {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            if (kv.value_ptr.ast.root) |root| {
                if (kv.value_ptr.symbols.items.len == 0) {
                    var document = kv.value_ptr.*;

                    const ctx = DocumentSymbolContext{
                        .document = &document,
                    };

                    document.ast.slice().walk(self.allocator, ctx, root) catch |err| {
                        log.err("textDocument/documentSymbol: {any}", .{err});

                        document.symbols = .empty;
                    };

                    kv.value_ptr.* = document;
                }

                return .{
                    .document_symbols = kv.value_ptr.symbols.items,
                };
            }
        }
        return null;
    }

    pub fn @"textDocument/completion"(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.requests.get("textDocument/completion").?.Params.?,
    ) !lsp.types.requests.get("textDocument/completion").?.Result {
        return .{
            .completion_list = .{
                .isIncomplete = false,
                .items = &.{},
            },
        };
    }

    pub fn @"textDocument/hover"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.requests.get("textDocument/hover").?.Params.?,
    ) !lsp.types.requests.get("textDocument/hover").?.Result {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            var document = kv.value_ptr.*;
            const allocator = document.arena.allocator();

            if (try document.nodeUnderPosition(notification.position)) |origin| {
                const type_def = document.ast.nodes.items(.type_def)[origin];

                if (type_def) |td| {
                    const markupEntry = try document.node_hover.getOrPut(allocator, origin);

                    if (!markupEntry.found_existing) {
                        var markup = std.Io.Writer.Allocating.init(self.allocator);

                        var docblock = document.ast.nodes.items(.docblock)[origin];
                        if (docblock == null) {
                            const def = try document.definition(origin);
                            if (def) |udef| {
                                docblock = udef.docblock orelse document.ast.nodes.items(.docblock)[udef.def_node];
                            }
                        }

                        if (docblock) |docblock_token| {
                            const doc = document.ast.tokens.items(.lexeme)[docblock_token];

                            var it = std.mem.tokenizeSequence(u8, doc, "/// ");
                            while (it.next()) |text| {
                                try markup.writer.print("{s}", .{text});
                            }
                        }

                        try markup.writer.writeAll("\n```buzz\n");
                        td.toString(&markup.writer, false) catch |err| {
                            log.err("textDocument/hover: {any}", .{err});
                        };
                        try markup.writer.writeAll("\n```");

                        markupEntry.value_ptr.* = try markup.toOwnedSlice();
                    }

                    return .{
                        .contents = .{
                            .markup_content = .{
                                .kind = .markdown,
                                .value = markupEntry.value_ptr.*,
                            },
                        },
                    };
                }
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
            var document = kv.value_ptr.*;

            if (try document.nodeUnderPosition(notification.position)) |origin| {
                if (kv.value_ptr.definitions.get(origin)) |result| {
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
            var document = kv.value_ptr.*;
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
    if (std.mem.eql(u8, script_name, "std")) return "std.buzz";
    if (std.mem.eql(u8, script_name, "gc")) return "gc.buzz";
    if (std.mem.eql(u8, script_name, "math")) return "math.buzz";
    if (std.mem.eql(u8, script_name, "debug")) return "debug.buzz";
    if (std.mem.eql(u8, script_name, "buffer")) return "buffer.buzz";
    if (std.mem.eql(u8, script_name, "serialize")) return "serialize.buzz";
    if (std.mem.eql(u8, script_name, "errors")) return "errors.buzz";
    if (std.mem.eql(u8, script_name, "test")) return "testing.buzz";
    if (std.mem.eql(u8, script_name, "crypto")) return "crypto.buzz";
    if (std.mem.eql(u8, script_name, "ffi")) return "ffi.buzz";
    if (std.mem.eql(u8, script_name, "fs")) return "fs.buzz";
    if (std.mem.eql(u8, script_name, "io")) return "io.buzz";
    if (std.mem.eql(u8, script_name, "os")) return "os.buzz";
    if (std.mem.eql(u8, script_name, "http")) return "http.buzz";

    return null;
}

fn staticScriptNameFromUri(uri: []const u8) ?[]const u8 {
    if (isStaticScriptUri(uri, "std.buzz")) return "std";
    if (isStaticScriptUri(uri, "gc.buzz")) return "gc";
    if (isStaticScriptUri(uri, "math.buzz")) return "math";
    if (isStaticScriptUri(uri, "debug.buzz")) return "debug";
    if (isStaticScriptUri(uri, "buffer.buzz")) return "buffer";
    if (isStaticScriptUri(uri, "serialize.buzz")) return "serialize";
    if (isStaticScriptUri(uri, "errors.buzz")) return "errors";
    if (isStaticScriptUri(uri, "testing.buzz")) return "test";
    if (isStaticScriptUri(uri, "crypto.buzz")) return "crypto";
    if (isStaticScriptUri(uri, "ffi.buzz")) return "ffi";
    if (isStaticScriptUri(uri, "fs.buzz")) return "fs";
    if (isStaticScriptUri(uri, "io.buzz")) return "io";
    if (isStaticScriptUri(uri, "os.buzz")) return "os";
    if (isStaticScriptUri(uri, "http.buzz")) return "http";

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

test "scriptNameToUri converts paths to LSP URIs" {
    const allocator = std.heap.page_allocator;

    try expectScriptNameUri(allocator, "file:///tmp/already.buzz", "file:///tmp/already.buzz");
    try expectScriptNameUri(allocator, "untitled:Untitled-1", "untitled:Untitled-1");

    try expectScriptNameUri(allocator, "/tmp/buzz lsp.buzz", "file:///tmp/buzz%20lsp.buzz");

    try expectScriptNameUri(allocator, "std", "file:///tmp/buzz%20lib/std.buzz");
    try expectScriptNameUri(allocator, "test", "file:///tmp/buzz%20lib/testing.buzz");
    try std.testing.expectEqualStrings(
        "buffer",
        staticScriptNameFromUri("file:///repo/src/lib/buffer.buzz").?,
    );
    try std.testing.expectEqualStrings(
        "test",
        staticScriptNameFromUri("file:///repo/src/lib/testing.buzz").?,
    );
    try std.testing.expect(staticScriptNameFromUri("file:///repo/not-lib/buffer.buzz") == null);

    const expected_relative_path = try testRelativePath(allocator, "src/lsp.zig");
    const expected_relative_uri = try fileUri(allocator, expected_relative_path);

    try expectScriptNameUri(allocator, "src/lsp.zig", expected_relative_uri);

    const expected_weird_path = try testRelativePath(allocator, "foo:STUPIDSHIT");
    const expected_weird_uri = try fileUri(allocator, expected_weird_path);

    try expectScriptNameUri(allocator, "foo:STUPIDSHIT", expected_weird_uri);
}

fn expectScriptNameUri(allocator: std.mem.Allocator, script_name: []const u8, expected: []const u8) !void {
    const uri = try scriptNameToUri(std.testing.io, allocator, "/tmp/buzz lib", script_name);

    try std.testing.expectEqualStrings(expected, uri);
}

fn fileUri(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
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

fn testRelativePath(allocator: std.mem.Allocator, relative: []const u8) ![]u8 {
    var cwd_buffer: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const cwd_len = try std.process.currentPath(std.testing.io, &cwd_buffer);

    return std.fs.path.join(
        allocator,
        &.{ cwd_buffer[0..cwd_len], relative },
    );
}
