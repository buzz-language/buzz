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

const log = std.log.scoped(.buzz_lsp);

const Lsp = lsp.server.Server(Handler);

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
    symbols: std.ArrayListUnmanaged(lsp.types.DocumentSymbol) = .{},

    /// Cache for previous gotoDefinition
    definitions: std.AutoHashMapUnmanaged(Ast.Node.Index, ?Definition) = .{},

    /// Cache for node under position
    node_under_position: std.AutoHashMapUnmanaged(lsp.types.Position, ?Ast.Node.Index) = .{},

    /// Cache for hover
    node_hover: std.AutoArrayHashMapUnmanaged(Ast.Node.Index, []const u8) = .{},

    pub fn init(parent_allocator: std.mem.Allocator, src: [*:0]const u8, uri: []const u8) !Document {
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        const allocator = arena.allocator();

        var gc = try mem.GarbageCollector.init(allocator);
        gc.type_registry = mem.TypeRegistry.init(&gc) catch return error.OutOfMemory;
        var imports = std.StringHashMapUnmanaged(Parser.ScriptImport){};

        // FIXME: we're at least leaking their Reporter's `reports` slice
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
        const ast = (parser.parse(std.mem.span(src), null, owned_uri) catch parser.ast) orelse parser.ast;

        const errors = if (parser.reporter.reports.items.len == 0 and (codegen.generate(ast.slice()) catch undefined) == null)
            try codegen.reporter.reports.toOwnedSlice(allocator)
        else
            try parser.reporter.reports.toOwnedSlice(allocator);

        return .{
            .arena = arena,
            .src = src,
            .uri = owned_uri,
            .ast = ast,
            .errors = errors,
        };
    }

    pub fn deinit(self: *Document) void {
        self.arena.deinit();
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
                log.err("nodeUnderPosition: {!}", .{err});
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

        switch (self.ast.nodes.items(.tag)[node]) {
            .NamedVariable => {
                const def = components[node].NamedVariable.definition;
                const location = self.ast.tokens.get(self.ast.nodes.items(.location)[def]);
                const end_location = self.ast.tokens.get(self.ast.nodes.items(.end_location)[def]);

                const tags = self.ast.nodes.items(.tag);
                log.debug(
                    "Found definition for node {} {s} at {} {s} in {s}:{}:{}-{}:{}",
                    .{
                        node,
                        @tagName(tags[node]),
                        def,
                        @tagName(tags[def]),
                        location.script_name,
                        location.line,
                        location.column,
                        end_location.line,
                        end_location.column,
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
                                    .targetUri = location.script_name,
                                    .targetRange = tokenToRange(location, end_location),
                                    .targetSelectionRange = tokenToRange(location, location),
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
                if (self.ast.nodes.items(.type_def)[comp.callee]) |callee_type_def| {
                    switch (callee_type_def.def_type) {
                        .ObjectInstance, .Object => {
                            const object_def = if (callee_type_def.def_type == .ObjectInstance)
                                callee_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object
                            else
                                callee_type_def.resolved_type.?.Object;

                            if (object_def.fields.get(self.ast.tokens.items(.lexeme)[comp.identifier])) |field| {
                                try self.definitions.put(
                                    allocator,
                                    node,
                                    .{
                                        .def_links = try allocator.dupe(
                                            lsp.types.DefinitionLink,
                                            &.{
                                                .{
                                                    .targetUri = field.location.script_name,
                                                    .targetRange = tokenToRange(field.location, field.location),
                                                    .targetSelectionRange = tokenToRange(field.location, field.location),
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
                if (self.ast.nodes.items(.type_def)[node]) |type_def| {
                    if (switch (type_def.def_type) {
                        .Object => type_def.resolved_type.?.Object.location,
                        .ObjectInstance => type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.location,
                        .Enum => type_def.resolved_type.?.Enum.location,
                        .EnumInstance => type_def.resolved_type.?.EnumInstance.of.resolved_type.?.Enum.location,
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
                                            .targetUri = location.script_name,
                                            .targetRange = tokenToRange(location, location),
                                            .targetSelectionRange = tokenToRange(location, location),
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

    var transport = lsp.Transport.init(
        std.io.getStdIn().reader(),
        std.io.getStdOut().writer(),
    );
    transport.message_tracing = false;

    var server: Lsp = undefined;
    var handler: Handler = .{
        .allocator = allocator,
        .server = &server,
    };
    server = try Lsp.init(
        allocator,
        &transport,
        &handler,
    );

    try server.loop();
}

fn tokenToRange(location: Token, end_location: Token) lsp.types.Range {
    return .{
        .start = .{
            .line = @intCast(location.line),
            .character = @intCast(@max(1, location.column) - 1),
        },
        .end = .{
            .line = @intCast(end_location.line),
            .character = @intCast(@max(1, end_location.column) - 1),
        },
    };
}

const Handler = struct {
    allocator: std.mem.Allocator,
    server: *Lsp,
    documents: std.StringHashMapUnmanaged(Document) = .{},
    offset_encoding: lsp.offsets.Encoding = .@"utf-16",

    pub fn initialize(
        self: *Handler,
        allocator: std.mem.Allocator,
        params: lsp.types.InitializeParams,
        offset_encoding: lsp.offsets.Encoding,
    ) !lsp.types.InitializeResult {
        self.offset_encoding = offset_encoding;

        log.info(
            "client (PID {}) {s}-{s} sent initialize request",
            .{
                params.processId orelse -1,
                if (params.clientInfo) |ci| ci.name else "???",
                if (params.clientInfo) |ci| ci.version orelse "???" else "???",
            },
        );

        var version = std.ArrayList(u8).init(allocator);

        try version.writer().print(
            "{}-{s}",
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

                // Keeping those here so I don't forget about them

                // NYI
                .completionProvider = null,
                .referencesProvider = null,
                .documentHighlightProvider = null,
                .workspaceSymbolProvider = null,
                .documentFormattingProvider = null,
                .documentRangeFormattingProvider = null,
                .documentOnTypeFormattingProvider = null,
                .callHierarchyProvider = null,
                .inlayHintProvider = null,
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
            var diags = std.ArrayListUnmanaged(lsp.types.Diagnostic){};
            for (doc.errors) |report| {
                for (report.items) |item| {
                    if (std.mem.eql(u8, item.location.script_name, doc.uri)) {
                        try diags.append(
                            self.allocator,
                            .{
                                .range = tokenToRange(item.location, item.end_location),
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
            try self.server.sendToClientNotification(
                "textDocument/publishDiagnostics",
                res,
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

                    var new_text = std.ArrayList(u8).init(self.allocator);
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
            const location = ast.tokens.get(locations[node]);
            const end_locations = ast.nodes.items(.end_location);
            const end_location = ast.tokens.get(end_locations[node]);
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
                                try td.toStringAlloc(allocator)
                            else
                                null,
                            .kind = if (type_def != null and !type_def.?.isMutable() and components.VarDeclaration.final)
                                .Constant
                            else
                                .Variable,
                            .range = tokenToRange(location, end_location),
                            .selectionRange = tokenToRange(location, end_location),
                        },
                    );
                },
                .Enum => {
                    var children = std.ArrayListUnmanaged(lsp.types.DocumentSymbol){};

                    for (components.Enum.cases) |case| {
                        const range = tokenToRange(
                            ast.tokens.get(case.name),
                            if (case.value) |value|
                                ast.tokens.get(end_locations[value])
                            else
                                ast.tokens.get(case.name),
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
                                try td.toStringAlloc(allocator)
                            else
                                null,
                            .kind = .Enum,
                            .range = tokenToRange(location, end_location),
                            .selectionRange = tokenToRange(location, end_location),
                            .children = try children.toOwnedSlice(allocator),
                        },
                    );
                },
                .ObjectDeclaration => {
                    var children = std.ArrayListUnmanaged(lsp.types.DocumentSymbol){};

                    if (type_def) |td| {
                        var it = td.resolved_type.?.Object.fields.iterator();
                        while (it.next()) |kv| {
                            const field = kv.value_ptr.*;

                            try children.append(
                                allocator,
                                .{
                                    .name = field.name,
                                    .detail = try field.type_def.toStringAlloc(allocator),
                                    .kind = if (field.method)
                                        .Method
                                    else
                                        .Property,
                                    .range = tokenToRange(field.location, field.location),
                                    .selectionRange = tokenToRange(field.location, field.location),
                                },
                            );
                        }
                    }

                    try self.document.symbols.append(
                        allocator,
                        .{
                            .name = lexemes[components.ObjectDeclaration.name],
                            .detail = if (type_def) |td|
                                try td.toStringAlloc(allocator)
                            else
                                null,
                            .kind = .Struct,
                            .range = tokenToRange(location, end_location),
                            .selectionRange = tokenToRange(location, end_location),
                            .children = try children.toOwnedSlice(allocator),
                        },
                    );
                },
                .ProtocolDeclaration => {
                    var children = std.ArrayListUnmanaged(lsp.types.DocumentSymbol){};

                    if (type_def) |td| {
                        var it = td.resolved_type.?.Protocol.methods.iterator();
                        while (it.next()) |kv| {
                            const method = kv.value_ptr.*;

                            const method_location = td.resolved_type.?.Protocol.methods_locations.get(kv.key_ptr.*).?;
                            const range = tokenToRange(method_location, method_location);

                            try children.append(
                                allocator,
                                .{
                                    .name = kv.key_ptr.*,
                                    .detail = try method.type_def.toStringAlloc(allocator),
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
                                try td.toStringAlloc(allocator)
                            else
                                null,
                            .kind = .Interface,
                            .range = tokenToRange(location, end_location),
                            .selectionRange = tokenToRange(location, end_location),
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
                                .detail = try td.toStringAlloc(allocator),
                                .kind = .Function,
                                .range = tokenToRange(location, end_location),
                                .selectionRange = tokenToRange(location, end_location),
                            },
                        );
                    }
                },
                else => {},
            }

            return false;
        }
    };

    pub fn documentSymbol(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.DocumentSymbolParams,
    ) !lsp.server.ResultType("textDocument/documentSymbol") {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            if (kv.value_ptr.ast.root) |root| {
                if (kv.value_ptr.symbols.items.len == 0) {
                    var document = kv.value_ptr.*;

                    const ctx = DocumentSymbolContext{
                        .document = &document,
                    };

                    document.ast.slice().walk(self.allocator, ctx, root) catch |err| {
                        log.err("textDocument/documentSymbol: {!}", .{err});

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

    pub fn completion(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.CompletionParams,
    ) !lsp.server.ResultType("textDocument/completion") {
        return .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = &.{},
            },
        };
    }

    pub fn hover(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.HoverParams,
        _: lsp.offsets.Encoding,
    ) !?lsp.types.Hover {
        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            var document = kv.value_ptr.*;
            const allocator = document.arena.allocator();

            if (try document.nodeUnderPosition(notification.position)) |origin| {
                const type_def = document.ast.nodes.items(.type_def)[origin];

                if (type_def) |td| {
                    const markupEntry = try document.node_hover.getOrPut(allocator, origin);

                    if (!markupEntry.found_existing) {
                        var markup = std.ArrayList(u8).init(allocator);
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
                        td.toString(&writer) catch |err| {
                            log.err("textDocument/hover: {!}", .{err});
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

    pub fn gotoDefinition(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.DefinitionParams,
    ) !lsp.server.ResultType("textDocument/definition") {
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

    pub fn references(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.ReferenceParams,
    ) !?[]lsp.types.Location {
        return null;
    }

    pub fn formatting(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.DocumentFormattingParams,
    ) !?[]const lsp.types.TextEdit {
        return null;
    }

    pub fn semanticTokensFull(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.SemanticTokensParams,
    ) !?lsp.types.SemanticTokens {
        return null;
    }

    pub fn inlayHint(
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.InlayHintParams,
    ) !?[]lsp.types.InlayHint {
        return null;
    }

    /// Handle a response that we have received from the client.
    /// Doesn't usually happen unless we explicitly send a request to the client.
    pub fn response(_: Handler, resp: lsp.server.Message.Response) !void {
        const id: []const u8 = switch (resp.id) {
            .string => |id| id,
            .number => |id| {
                log.warn("received response from client with id '{d}' that has no handler!", .{id});
                return;
            },
        };

        if (resp.data == .@"error") {
            const err = resp.data.@"error";
            log.err("Error response for '{s}': {}, {s}", .{ id, err.code, err.message });
            return;
        }

        log.warn("received response from client with id '{s}' that has no handler!", .{id});
    }
};
