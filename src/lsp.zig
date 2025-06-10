const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");
const lsp = @import("lsp");
const Ast = @import("Ast.zig");
const mem = @import("memory.zig");
const Parser = @import("Parser.zig");
const Reporter = @import("Reporter.zig");
const CodeGen = @import("Codegen.zig");
const Token = @import("Token.zig");
const Renderer = @import("renderer.zig").Renderer;
const WriteableArrayList = @import("writeable_array_list.zig").WriteableArrayList;

const log = std.log.scoped(.buzz_lsp);

const Document = struct {
    pub const Definition = struct {
        def_links: []const lsp.types.DefinitionLink,
        def_node: Ast.Node.Index,
    };

    arena: std.heap.ArenaAllocator,
    src: [*:0]const u8,
    /// Not owned by this struct
    uri: []const u8,
    ast: Ast,
    errors: []const Reporter.Report,

    /// Symbols collected in the document
    symbols: std.ArrayList(lsp.types.DocumentSymbol) = .{},

    /// Cache for previous gotoDefinition
    definitions: std.AutoHashMapUnmanaged(Ast.Node.Index, ?Definition) = .{},

    /// Cache for node under position
    node_under_position: std.AutoHashMapUnmanaged(lsp.types.Position, ?Ast.Node.Index) = .{},

    /// Cache for hover
    node_hover: std.AutoHashMapUnmanaged(Ast.Node.Index, []const u8) = .{},

    /// Cache for inlay hints
    inlay_hints: std.ArrayList(lsp.types.InlayHint) = .{}, // I tried to make this a simple slice but the data was lost I don't know why

    pub fn init(parent_allocator: std.mem.Allocator, src: [*:0]const u8, uri: []const u8) !Document {
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        const allocator = arena.allocator();

        var gc = try mem.GarbageCollector.init(allocator);
        gc.type_registry = mem.TypeRegistry.init(&gc) catch return error.OutOfMemory;
        var imports = std.StringHashMapUnmanaged(Parser.ScriptImport){};

        var parser = Parser.init(
            &gc,
            &imports,
            false,
            .Ast,
        );
        var codegen = CodeGen.init(
            &gc,
            &parser,
            .Ast,
            null,
        );

        const owned_uri = try allocator.dupe(u8, uri);

        // If there's parsing error `parse` does not return the AST, but we can still use it however incomplete
        const ast = (parser.parse(std.mem.span(src), null, owned_uri) catch parser.ast) orelse
            parser.ast;

        const errors = if (parser.reporter.reports.items.len == 0 and
            (codegen.generate(ast.slice()) catch undefined) == null)
            try codegen.reporter.reports.toOwnedSlice(allocator)
        else
            try parser.reporter.reports.toOwnedSlice(allocator);

        var doc = Document{
            .arena = arena,
            .src = src,
            .uri = owned_uri,
            .ast = ast,
            .errors = errors,
        };

        if (ast.root != null) {
            doc.computeInlayHints() catch return error.OutOfMemory;
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

        pub fn processNode(self: *NodeUnderPositionContext, _: std.mem.Allocator, ast: Ast.Slice, node: Ast.Node.Index) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            const locations = ast.nodes.items(.location);
            const location = ast.tokens.get(locations[node]);
            const end_locations = ast.nodes.items(.end_location);
            const end_location = ast.tokens.get(end_locations[node]);
            const tags = ast.nodes.items(.tag);

            // Ignore root node and imports
            if (locations[node] == 0 or tags[node] == .Import) {
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
            .NamedVariable => {
                const def = components[node].NamedVariable.definition;
                const location = self.ast.nodes.items(.location)[def];
                const end_location = self.ast.nodes.items(.end_location)[def];
                const lines = self.ast.tokens.items(.line);
                const columns = self.ast.tokens.items(.column);
                const script_names = self.ast.tokens.items(.script_name);

                const tags = self.ast.nodes.items(.tag);
                log.debug(
                    "Found definition for node {} {s} at {} {s} in {s}:{}:{}-{}:{}",
                    .{
                        node,
                        @tagName(tags[node]),
                        def,
                        @tagName(tags[def]),
                        script_names[location],
                        lines[location],
                        columns[location],
                        lines[end_location],
                        columns[end_location],
                    },
                );

                try self.definitions.put(
                    allocator,
                    node,
                    .{
                        .def_links = try allocator.dupe(
                            lsp.types.DefinitionLink,
                            &.{
                                .{
                                    .targetUri = script_names[location],
                                    .targetRange = tokenToRange(ast_slice, location, end_location),
                                    .targetSelectionRange = tokenToRange(ast_slice, location, location),
                                },
                            },
                        ),
                        .def_node = def,
                    },
                );

                return self.definitions.get(node).?.?;
            },
            .Dot => {
                const comp = components[node].Dot;
                if (ast_slice.nodes.items(.type_def)[comp.callee]) |callee_type_def| {
                    switch (callee_type_def.def_type) {
                        .EnumInstance => {},
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
                                        .def_links = try allocator.dupe(
                                            lsp.types.DefinitionLink,
                                            &.{
                                                .{
                                                    .targetUri = ast_slice.tokens.items(.script_name)[field.location],
                                                    .targetRange = tokenToRange(ast_slice, field.location, field.location),
                                                    .targetSelectionRange = tokenToRange(ast_slice, field.location, field.location),
                                                },
                                            },
                                        ),
                                        .def_node = node, // FIXME: get object fields docblock
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
                                .def_links = try allocator.dupe(
                                    lsp.types.DefinitionLink,
                                    &.{
                                        .{
                                            .targetUri = ast_slice.tokens.items(.script_name)[location],
                                            .targetRange = tokenToRange(ast_slice, location, location),
                                            .targetSelectionRange = tokenToRange(ast_slice, location, location),
                                        },
                                    },
                                ),
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

        pub fn processNode(self: *InlayHintsContext, allocator: std.mem.Allocator, ast: Ast.Slice, node: Ast.Node.Index) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            switch (ast.nodes.items(.tag)[node]) {
                .VarDeclaration => {
                    const comp = ast.nodes.items(.components)[node].VarDeclaration;
                    const type_def = ast.nodes.items(.type_def)[node];
                    const name = ast.tokens.get(comp.name);

                    // If type was omitted, provide it
                    if (!comp.implicit and comp.type == null and type_def != null) {
                        var inlay = std.ArrayList(u8){};
                        var writer = inlay.writer(allocator);

                        try writer.writeAll(": ");
                        try type_def.?.toString(writer, false);

                        try self.document.inlay_hints.append(
                            allocator,
                            .{
                                .position = .{
                                    .line = @intCast(name.line),
                                    .character = @intCast(@max(1, name.column + name.lexeme.len) - 1),
                                },
                                .label = .{
                                    .string = try inlay.toOwnedSlice(allocator),
                                },
                                .kind = .Type,
                            },
                        );
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
};

extern fn getpid() std.os.linux.pid_t;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(
        .{
            .safety = builtin.mode == .Debug,
        },
    ){};
    const allocator: std.mem.Allocator = if (builtin.mode == .Debug or is_wasm)
        gpa.allocator()
    else if (BuildOptions.mimalloc)
        @import("mimalloc.zig").mim_allocator
    else
        std.heap.c_allocator;

    log.debug("Buzz LSP started with PID {}", .{getpid()});

    var read_buffer = [_]u8{undefined};
    var stdio_transport = lsp.Transport.Stdio.init(
        read_buffer[0..],
        std.fs.File.stdin(),
        std.fs.File.stdout(),
    );

    var handler = Handler{
        .allocator = allocator,
        .transport = &stdio_transport.transport,
    };

    try lsp.basic_server.run(
        allocator,
        &stdio_transport.transport,
        &handler,
        log.err,
    );
}

const Handler = struct {
    allocator: std.mem.Allocator,
    transport: *lsp.Transport,
    documents: std.StringHashMapUnmanaged(Document) = .{},
    offset_encoding: lsp.offsets.Encoding = .@"utf-16",

    pub fn initialize(
        self: *Handler,
        allocator: std.mem.Allocator,
        params: lsp.types.getRequestMetadata("initialize").?.Params.?,
    ) !lsp.types.getRequestMetadata("initialize").?.Result {
        log.info(
            "client (PID {}) {s}-{s} sent initialize request",
            .{
                params.processId orelse -1,
                if (params.clientInfo) |ci| ci.name else "???",
                if (params.clientInfo) |ci| ci.version orelse "???" else "???",
            },
        );

        var version = std.array_list.Managed(u8).init(allocator);

        try version.writer().print(
            "{f}-{s}",
            .{
                BuildOptions.version,
                BuildOptions.sha,
            },
        );

        return .{
            .serverInfo = .{
                .name = "Buzz LSP",
                .version = version.items,
            },
            .capabilities = .{
                .positionEncoding = switch (self.offset_encoding) {
                    .@"utf-8" => .@"utf-8",
                    .@"utf-16" => .@"utf-16",
                    .@"utf-32" => .@"utf-32",
                },
                .textDocumentSync = .{
                    .TextDocumentSyncOptions = .{
                        .openClose = true,
                        .change = .Full,
                        .save = .{ .bool = true },
                    },
                },

                .hoverProvider = .{
                    .HoverOptions = .{
                        .workDoneProgress = false,
                    },
                },
                .definitionProvider = .{
                    .DefinitionOptions = .{
                        .workDoneProgress = false,
                    },
                },
                .declarationProvider = .{
                    .DeclarationOptions = .{
                        .workDoneProgress = false,
                    },
                },
                .typeDefinitionProvider = .{
                    .TypeDefinitionOptions = .{
                        .workDoneProgress = false,
                    },
                },
                .implementationProvider = .{
                    .ImplementationOptions = .{
                        .workDoneProgress = false,
                    },
                },
                .documentSymbolProvider = .{
                    .DocumentSymbolOptions = .{},
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
        var res: lsp.types.PublishDiagnosticsParams = .{
            .uri = uri,
            .diagnostics = &.{},
        };

        const doc = try Document.init(
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
            var diags = std.ArrayList(lsp.types.Diagnostic){};
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

        self.allocator.free(
            try self.transport.writeNotification(
                self.allocator,
                "textDocument/publishDiagnostics",
                lsp.types.PublishDiagnosticsParams,
                res,
                .{},
            ),
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

    pub fn openDocument(
        self: *Handler,
        _: std.mem.Allocator,
        notification: lsp.types.DidOpenTextDocumentParams,
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

    pub fn changeDocument(
        self: *Handler,
        allocator: std.mem.Allocator,
        notification: lsp.types.DidChangeTextDocumentParams,
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
                .literal_1 => |change| try self.allocator.dupeZ(u8, change.text),
                .literal_0 => |change| blk: {
                    const old_text = std.mem.span(file.src);
                    const range = change.range;

                    const start_idx = lsp.offsets.maybePositionToIndex(
                        old_text,
                        range.start,
                        self.offset_encoding,
                    ) orelse {
                        log.warn(
                            "changeDocument failed: invalid start position: {any}",
                            .{
                                range.start,
                            },
                        );
                        return error.InternalError;
                    };

                    const end_idx = lsp.offsets.maybePositionToIndex(
                        old_text,
                        range.end,
                        self.offset_encoding,
                    ) orelse {
                        log.warn(
                            "changeDocument failed: invalid end position: {any}",
                            .{
                                range.end,
                            },
                        );
                        return error.InternalError;
                    };

                    var new_text = std.array_list.Managed(u8).init(self.allocator);
                    errdefer new_text.deinit();

                    try new_text.appendSlice(old_text[0..start_idx]);
                    try new_text.appendSlice(change.text);
                    try new_text.appendSlice(old_text[end_idx..]);

                    break :blk try new_text.toOwnedSliceSentinel(0);
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

    pub fn saveDocument(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.DidSaveTextDocumentParams,
    ) !void {}

    pub fn closeDocument(
        self: *Handler,
        _: std.mem.Allocator,
        notification: lsp.types.DidCloseTextDocumentParams,
    ) !void {
        var kv = self.documents.fetchRemove(notification.textDocument.uri) orelse return;
        self.allocator.free(kv.key);
        kv.value.deinit();
    }

    const DocumentSymbolContext = struct {
        document: *Document,

        pub fn processNode(self: DocumentSymbolContext, _: std.mem.Allocator, ast: Ast.Slice, node: Ast.Node.Index) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            const lexemes = ast.tokens.items(.lexeme);
            const locations = ast.nodes.items(.location);
            const end_locations = ast.nodes.items(.end_location);
            const components = ast.nodes.items(.components)[node];
            const type_def = ast.nodes.items(.type_def)[node];
            const allocator = self.document.arena.allocator();

            switch (ast.nodes.items(.tag)[node]) {
                .VarDeclaration => {
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
                },
                .Enum => {
                    var children = std.ArrayList(lsp.types.DocumentSymbol){};

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
                    var children = std.ArrayList(lsp.types.DocumentSymbol){};

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
                    var children = std.ArrayList(lsp.types.DocumentSymbol){};

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
        notification: lsp.types.getRequestMetadata("textDocument/documentSymbol").?.Params.?,
    ) !lsp.types.getRequestMetadata("textDocument/documentSymbol").?.Result {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            if (kv.value_ptr.ast.root) |root| {
                if (kv.value_ptr.symbols.items.len == 0) {
                    var document = kv.value_ptr.*;

                    const ctx = DocumentSymbolContext{
                        .document = &document,
                    };

                    document.ast.slice().walk(self.allocator, ctx, root) catch |err| {
                        log.err("textDocument/documentSymbol: {any}", .{err});

                        document.symbols = .{};
                    };

                    kv.value_ptr.* = document;
                }

                return .{
                    .array_of_DocumentSymbol = kv.value_ptr.symbols.items,
                };
            }
        }
        return null;
    }

    pub fn @"textDocument/completion"(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.getRequestMetadata("textDocument/completion").?.Params.?,
    ) !lsp.types.getRequestMetadata("textDocument/completion").?.Result {
        return .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = &.{},
            },
        };
    }

    pub fn @"textDocument/hover"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.getRequestMetadata("textDocument/hover").?.Params.?,
    ) !lsp.types.getRequestMetadata("textDocument/hover").?.Result {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            var document = kv.value_ptr.*;
            const allocator = document.arena.allocator();

            if (try document.nodeUnderPosition(notification.position)) |origin| {
                const type_def = document.ast.nodes.items(.type_def)[origin];

                if (type_def) |td| {
                    const markupEntry = try document.node_hover.getOrPut(allocator, origin);

                    if (!markupEntry.found_existing) {
                        var markup = std.array_list.Managed(u8).init(allocator);
                        const writer = markup.writer();

                        const def = try document.definition(origin);
                        if (def != null) {
                            if (document.ast.nodes.items(.docblock)[def.?.def_node]) |docblock| {
                                const doc = document.ast.tokens.items(.lexeme)[docblock];

                                var it = std.mem.tokenizeSequence(u8, doc, "/// ");
                                while (it.next()) |text| {
                                    try writer.print("{s}\n", .{text});
                                }
                            }
                        }

                        try writer.writeAll("```buzz\n");
                        td.toString(&writer, false) catch |err| {
                            log.err("textDocument/hover: {any}", .{err});
                        };
                        try writer.writeAll("\n```");

                        markupEntry.value_ptr.* = try markup.toOwnedSlice();
                    }

                    return .{
                        .contents = .{
                            .MarkupContent = .{
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
        notification: lsp.types.getRequestMetadata("textDocument/definition").?.Params.?,
    ) !lsp.types.getRequestMetadata("textDocument/definition").?.Result {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            var document = kv.value_ptr.*;

            if (try document.nodeUnderPosition(notification.position)) |origin| {
                if (kv.value_ptr.definitions.get(origin)) |result| {
                    if (result) |res| {
                        return .{
                            .array_of_DefinitionLink = res.def_links,
                        };
                    }
                }

                return .{
                    .array_of_DefinitionLink = if (try document.definition(origin)) |defs| defs.def_links else &.{},
                };
            }
        }

        return null;
    }

    pub fn @"textDocument/references"(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.getRequestMetadata("textDocument/references").?.Params.?,
    ) !lsp.types.getRequestMetadata("textDocument/references").?.Result {
        return null;
    }

    pub fn @"textDocument/formatting"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.getRequestMetadata("textDocument/formatting").?.Params.?,
    ) !lsp.types.getRequestMetadata("textDocument/formatting").?.Result {
        if (self.documents.get(notification.textDocument.uri)) |document| {
            var result = WriteableArrayList(u8).init(self.allocator);

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

            var text_edit = std.array_list.Managed(lsp.types.TextEdit).init(self.allocator);
            try text_edit.append(
                .{
                    .range = document.wholeDocumentRange(),
                    .newText = result.list.items,
                },
            );

            return try text_edit.toOwnedSlice();
        }
        return null;
    }

    pub fn @"textDocument/semanticTokens/full"(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.getRequestMetadata("textDocument/semanticTokens/full").?.Params.?,
    ) !lsp.types.getRequestMetadata("textDocument/semanticTokens/full").?.Result {
        return null;
    }

    pub fn @"textDocument/inlayHint"(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.getRequestMetadata("textDocument/inlayHint").?.Params.?,
    ) !lsp.types.getRequestMetadata("textDocument/inlayHint").?.Result {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            var document = kv.value_ptr.*;
            const allocator = document.arena.allocator();

            var result = std.ArrayList(lsp.types.InlayHint){};

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
