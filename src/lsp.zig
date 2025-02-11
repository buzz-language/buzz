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
    pub const DefinitionRequest = struct {
        uri: lsp.types.DocumentUri,
        position: lsp.types.Position,
    };

    src: [*:0]const u8,
    /// Not owned by this struct
    uri: []const u8,
    ast: Ast,
    errors: []const Reporter.Report,

    /// Symbols collected in the document
    symbols: std.ArrayListUnmanaged(lsp.types.DocumentSymbol) = .{},

    /// Cache for previous gotoDefinition
    definitions: std.AutoHashMapUnmanaged(DefinitionRequest, []const lsp.types.DefinitionLink) = .{},

    /// Cache for node under position
    node_under_position: std.AutoHashMapUnmanaged(lsp.types.Position, ?Ast.Node.Index) = .{},

    pub fn init(allocator: std.mem.Allocator, src: [*:0]const u8, uri: []const u8) !Document {
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
        defer {
            parser.deinit();
            codegen.deinit();
            gc.deinit();
            imports.deinit(allocator);
        }

        // If there's parsing error `parse` does not return the AST, but we can still use it however incomplete
        const ast = (parser.parse(std.mem.span(src), uri) catch parser.ast) orelse parser.ast;

        const errors = if (parser.reporter.reports.items.len == 0 and (codegen.generate(ast.slice()) catch undefined) == null)
            try codegen.reporter.reports.toOwnedSlice(allocator)
        else
            try parser.reporter.reports.toOwnedSlice(allocator);

        return .{
            .src = src,
            .uri = uri,
            .ast = ast,
            .errors = errors,
        };
    }

    pub fn deinit(self: *Document, allocator: std.mem.Allocator) void {
        for (self.errors) |*err| {
            @constCast(err).deinit(allocator);
        }
        allocator.free(self.errors);

        self.ast.deinit(allocator);

        const src = std.mem.span(self.src);
        allocator.free(self.src[0 .. src.len + 1]);

        for (self.symbols.items) |symbol| {
            if (symbol.detail) |detail| {
                allocator.free(detail);
            }

            if (symbol.children) |children| {
                for (children) |child| {
                    if (child.detail) |detail| {
                        allocator.free(detail);
                    }
                }

                allocator.free(symbol.children.?);
            }
        }
        self.symbols.deinit(allocator);

        var it = self.definitions.iterator();
        while (it.next()) |kv| {
            allocator.free(kv.value_ptr.*);
        }
        self.definitions.deinit(allocator);

        self.node_under_position.deinit(allocator);
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
        defer version.deinit();

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

        const doc = try Document.init(self.allocator, src, uri);

        log.debug("Loaded document `{s}`", .{uri});

        const gop = try self.documents.getOrPut(self.allocator, uri);
        errdefer _ = self.documents.remove(uri);

        if (gop.found_existing) {
            gop.value_ptr.deinit(self.allocator);
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

            diags.shrinkAndFree(self.allocator, diags.items.len);
            res.diagnostics = diags.items;
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
        kv.value.deinit(self.allocator);
    }

    const DocumentSymbolContext = struct {
        document: *Document,

        pub fn processNode(self: DocumentSymbolContext, allocator: std.mem.Allocator, ast: Ast.Slice, node: Ast.Node.Index) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            const lexemes = ast.tokens.items(.lexeme);
            const locations = ast.nodes.items(.location);
            const location = ast.tokens.get(locations[node]);
            const end_locations = ast.nodes.items(.end_location);
            const end_location = ast.tokens.get(end_locations[node]);
            const components = ast.nodes.items(.components)[node];
            const type_def = ast.nodes.items(.type_def)[node];

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

                    children.shrinkAndFree(allocator, children.items.len);

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
                            .children = children.items,
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

                    children.shrinkAndFree(allocator, children.items.len);

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
                            .children = children.items,
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

                    children.shrinkAndFree(allocator, children.items.len);

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
                            .children = children.items,
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

                        document.symbols.deinit(self.allocator);
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
        _: Handler,
        _: std.mem.Allocator,
        _: lsp.types.HoverParams,
        _: lsp.offsets.Encoding,
    ) !?lsp.types.Hover {
        return null;
    }

    const GotoDefinitionContext = struct {
        node: Ast.Node.Index,
        document: *Document,
        result: std.ArrayListUnmanaged(lsp.types.LocationLink) = .{},

        // pub fn processNode(self: GotoDefinitionContext, allocator: std.mem.Allocator, ast: Ast.Slice, node: Ast.Node.Index) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
        //     const lexemes = ast.tokens.items(.lexeme);
        //     const locations = ast.nodes.items(.location);
        //     const location = ast.tokens.get(locations[node]);
        //     const end_locations = ast.nodes.items(.end_location);
        //     const end_location = ast.tokens.get(end_locations[node]);
        //     const components = ast.nodes.items(.components)[node];
        //     const type_def = ast.nodes.items(.type_def)[node];

        //     switch (ast.nodes.items(.tag)[node]) {
        //         else => {},
        //     }
        // }
    };

    const NodeUnderPositionContext = struct {
        result: ?Ast.Node.Index = null,
        position: lsp.types.Position,

        pub fn processNode(self: NodeUnderPositionContext, _: std.mem.Allocator, ast: Ast.Slice, node: Ast.Node.Index) (std.mem.Allocator.Error || std.fmt.BufPrintError)!bool {
            const locations = ast.nodes.items(.location);
            const location = ast.tokens.get(locations[node]);
            const end_locations = ast.nodes.items(.end_location);
            const end_location = ast.tokens.get(end_locations[node]);

            // If outside of the node range, don't go deeper
            if (self.position.line < location.line or
                self.position.line > end_location.line or
                (self.position.line == end_location.line and self.position.character + 1 > end_location.column) or
                (self.position.line == location.line and self.position.character + 1 < self.position.column))
            {
                return true;
            }

            // Otherwise this node is a candidate: continue deeper for a more narrow match
            self.result = node;

            return false;
        }
    };

    pub fn gotoDefinition(
        self: Handler,
        _: std.mem.Allocator,
        notification: lsp.types.DefinitionParams,
    ) !lsp.server.ResultType("textDocument/definition") {
        const request = .{
            .uri = notification.textDocument.uri,
            .position = notification.position,
        };

        if (self.documents.getEntry(notification.textDocument.uri)) |kv| {
            if (kv.value_ptr.definitions.get(request)) |result| {
                return .{
                    .array_of_DefinitionLink = result,
                };
            }

            var document = kv.value_ptr.*;

            if (document.ast.root) |root| {
                // Search for node under requested position
                const nodeEntry = document.node_under_position.getOrPut(self.allocator, request.position);

                if (!nodeEntry.found_existing) {
                    const node_ctx = NodeUnderPositionContext{
                        .position = request.position,
                    };

                    document.ast.slice().walk(self.allocator, node_ctx, root) catch |err| {
                        log.err("textDocument/definition: {!}", .{err});
                    };

                    nodeEntry.value_ptr.* = node_ctx.result;
                }

                if (nodeEntry.value_ptr.* == null) {
                    return null;
                }

                // Search for definition if revelant
                const ctx = GotoDefinitionContext{
                    .node = nodeEntry.value_ptr.*,
                    .document = &document,
                };

                document.ast.slice().walk(self.allocator, ctx, root) catch |err| {
                    log.err("textDocument/definition: {!}", .{err});

                    ctx.result.deinit(self.allocator);
                };

                ctx.result.shrinkAndFree(self.allocator, ctx.result.items.len);

                document.definitions.put(request, ctx.result);

                kv.value_ptr.* = document;

                return .{
                    .array_of_DefinitionLink = ctx.result,
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
