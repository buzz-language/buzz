const std = @import("std");
const types = @import("lsp.zig");
const Transport = @import("Transport.zig");
const DocumentStore = @import("DocumentStore.zig");
const diff = @import("diff.zig");

const log = std.log.scoped(.lsp_server);

const Self = @This();

allocator: std.mem.Allocator,
document_store: DocumentStore,
transport: Transport,
job_queue: std.fifo.LinearFifo(Job, .Dynamic),
status: Status = .ready,

pub fn init(allocator: std.mem.Allocator) !Self {
    return .{
        .allocator = allocator,
        .document_store = .{
            .allocator = allocator,
        },
        .transport = Transport.init(
            std.io.getStdIn().reader(),
            std.io.getStdOut().writer(),
        ),
        .job_queue = std.fifo.LinearFifo(Job, .Dynamic).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.document_store.deinit();
    self.job_queue.deinit();
}

pub fn keepRunning(self: Self) bool {
    switch (self.status) {
        .exiting_success, .exiting_failure => return false,
        else => return true,
    }
}

pub fn loop(self: *Self) !void {
    while (self.keepRunning()) {
        // Handle new message
        const json_message = try self.transport.readJsonMessage(self.allocator);
        defer self.allocator.free(json_message);

        const job = Job{
            .incoming_message = std.json.parseFromSlice(
                types.Message,
                self.allocator,
                json_message,
                .{
                    .ignore_unknown_fields = true,
                    .max_value_len = null,
                    .allocate = .alloc_always,
                },
            ) catch return error.ParseError,
        };

        self.job_queue.writeItem(job) catch |err| {
            job.deinit(self.allocator);
            return err;
        };

        // Work on awaiting jobs
        while (self.job_queue.readItem()) |pending_job| {
            self.processJob(pending_job);
        }
    }
}

fn processJob(self: *Self, job: Job) void {
    defer job.deinit(self.allocator);

    switch (job) {
        .incoming_message => |parsed_message| {
            self.processMessageReportError(parsed_message.value);
        },
        else => unreachable, // TODO
    }
}

fn processMessageReportError(self: *Self, message: types.Message) void {
    self.processMessage(message) catch |err| {
        log.err("failed to process {}: {}", .{ message, err });
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }

        switch (message) {
            .request => |request| _ = self.sendToClientResponseError(
                request.id,
                types.Message.Response.Error{
                    .code = @enumFromInt(switch (err) {
                        error.OutOfMemory => @intFromEnum(types.ErrorCodes.InternalError),
                        error.ParseError => @intFromEnum(types.ErrorCodes.ParseError),
                        error.InvalidRequest => @intFromEnum(types.ErrorCodes.InvalidRequest),
                        error.MethodNotFound => @intFromEnum(types.ErrorCodes.MethodNotFound),
                        error.InvalidParams => @intFromEnum(types.ErrorCodes.InvalidParams),
                        error.InternalError => @intFromEnum(types.ErrorCodes.InternalError),
                        error.ServerNotInitialized => @intFromEnum(types.ErrorCodes.ServerNotInitialized),
                        error.RequestFailed => @intFromEnum(types.LSPErrorCodes.RequestFailed),
                        error.ServerCancelled => @intFromEnum(types.LSPErrorCodes.ServerCancelled),
                        error.ContentModified => @intFromEnum(types.LSPErrorCodes.ContentModified),
                        error.RequestCancelled => @intFromEnum(types.LSPErrorCodes.RequestCancelled),
                    }),
                    .message = @errorName(err),
                },
            ) catch {},
            .notification, .response => {},
        }
    };
}

fn processMessage(self: *Self, message: types.Message) Error!void {
    var arena_allocator = std.heap.ArenaAllocator.init(self.allocator);
    defer arena_allocator.deinit();

    @setEvalBranchQuota(5_000);
    switch (message) {
        .request => |request| {
            const handled_method = std.meta.stringToEnum(HandledRequestMethods, request.method) orelse {
                try self.sendToClientResponse(request.id, null);
                return;
            };

            switch (handled_method) {
                inline else => |method| {
                    const Params = ParamsType(@tagName(method));
                    const params = if (Params == void) {} else std.json.parseFromValueLeaky(
                        Params,
                        arena_allocator.allocator(),
                        request.params orelse .null,
                        .{ .ignore_unknown_fields = true },
                    ) catch return error.ParseError;

                    const result = try self.sendRequest(arena_allocator.allocator(), @tagName(method), params);
                    return try self.sendToClientResponse(request.id, result);
                },
            }
        },
        .notification => unreachable, // TODO
        .response => unreachable, // TODO
    }
}

pub fn sendRequest(self: *Self, _: std.mem.Allocator, comptime method: []const u8, _: ParamsType(method)) Error!ResultType(method) {
    comptime std.debug.assert(isRequestMethod(method));

    return switch (comptime std.meta.stringToEnum(HandledRequestMethods, method) orelse return null) {
        .initialize => unreachable, // TODO
        .shutdown => self.status = .shutdown,
        .@"textDocument/willSaveWaitUntil" => unreachable, // TODO
        .@"textDocument/semanticTokens/full" => unreachable, // TODO
        .@"textDocument/semanticTokens/range" => unreachable, // TODO
        .@"textDocument/inlayHint" => unreachable, // TODO
        .@"textDocument/completion" => unreachable, // TODO
        .@"textDocument/signatureHelp" => unreachable, // TODO
        .@"textDocument/definition" => unreachable, // TODO
        .@"textDocument/typeDefinition" => unreachable, // TODO
        .@"textDocument/implementation" => unreachable, // TODO
        .@"textDocument/declaration" => unreachable, // TODO
        .@"textDocument/hover" => unreachable, // TODO
        .@"textDocument/documentSymbol" => unreachable, // TODO
        .@"textDocument/formatting" => unreachable, // TODO
        .@"textDocument/rename" => unreachable, // TODO
        .@"textDocument/references" => unreachable, // TODO
        .@"textDocument/documentHighlight" => unreachable, // TODO
        .@"textDocument/codeAction" => unreachable, // TODO
        .@"textDocument/foldingRange" => unreachable, // TODO
        .@"textDocument/selectionRange" => unreachable, // TODO
    };
}

pub fn sendNotification(self: *Self, arena: std.mem.Allocator, comptime method: []const u8, params: ParamsType(method)) Error!void {
    return switch (comptime std.meta.stringToEnum(HandledNotificationMethods, method) orelse return) {
        .initialized => {},
        .exit => try self.exitHandler(arena, params),
        .@"$/cancelRequest" => {},
        .@"$/setTrace" => {},
        .@"textDocument/didOpen" => try self.openDocumentHandler(arena, params),
        .@"textDocument/didChange" => try self.changeDocumentHandler(arena, params),
        .@"textDocument/didSave" => try self.saveDocumentHandler(arena, params),
        .@"textDocument/didClose" => try self.closeDocumentHandler(arena, params),
        .@"workspace/didChangeWorkspaceFolders" => try self.didChangeWorkspaceFoldersHandler(arena, params),
        .@"workspace/didChangeConfiguration" => {},
    };
}

fn openDocumentHandler(self: *Self, _: std.mem.Allocator, notification: types.DidOpenTextDocumentParams) Error!void {
    if (notification.textDocument.text.len > DocumentStore.max_document_size) {
        log.err("open document `{s}` failed: text size ({d}) is above maximum length ({d})", .{
            notification.textDocument.uri,
            notification.textDocument.text.len,
            DocumentStore.max_document_size,
        });
        return error.InternalError;
    }

    try self.document_store.openDocument(notification.textDocument.uri, notification.textDocument.text);

    if (self.client_capabilities.supports_publish_diagnostics) {
        try self.pushJob(.{
            .generate_diagnostics = try self.allocator.dupe(u8, notification.textDocument.uri),
        });
    }
}

fn changeDocumentHandler(self: *Self, _: std.mem.Allocator, notification: types.DidChangeTextDocumentParams) Error!void {
    const handle = self.document_store.getHandle(notification.textDocument.uri) orelse return;

    const new_text = try diff.applyContentChanges(
        self.allocator,
        handle.tree.source,
        notification.contentChanges,
        self.offset_encoding,
    );

    if (new_text.len > DocumentStore.max_document_size) {
        log.err("change document `{s}` failed: text size ({d}) is above maximum length ({d})", .{
            notification.textDocument.uri,
            new_text.len,
            DocumentStore.max_document_size,
        });
        return error.InternalError;
    }

    try self.document_store.refreshDocument(handle.uri, new_text);

    if (self.client_capabilities.supports_publish_diagnostics) {
        try self.pushJob(
            .{
                .generate_diagnostics = try self.allocator.dupe(u8, handle.uri),
            },
        );
    }
}

fn saveDocumentHandler(self: *Self, arena: std.mem.Allocator, notification: types.DidSaveTextDocumentParams) Error!void {
    const uri = notification.textDocument.uri;

    if (std.process.can_spawn and DocumentStore.isBuildFile(uri)) {
        try self.document_store.invalidateBuildFile(uri);
    }

    if (std.process.can_spawn and self.config.enable_build_on_save) {
        try self.pushJob(.run_build_on_save);
    }

    if (self.getAutofixMode() == .on_save) {
        const handle = self.document_store.getHandle(uri) orelse return;
        var text_edits = try self.autofix(arena, handle);

        var workspace_edit = types.WorkspaceEdit{ .changes = .{} };
        try workspace_edit.changes.?.map.putNoClobber(arena, uri, try text_edits.toOwnedSlice(arena));

        const json_message = try self.sendToClientRequest(
            .{ .string = "apply_edit" },
            "workspace/applyEdit",
            types.ApplyWorkspaceEditParams{
                .label = "autofix",
                .edit = workspace_edit,
            },
        );
        self.allocator.free(json_message);
    }
}

fn closeDocumentHandler(self: *Self, _: std.mem.Allocator, notification: types.DidCloseTextDocumentParams) error{}!void {
    self.document_store.closeDocument(notification.textDocument.uri);

    if (self.client_capabilities.supports_publish_diagnostics) {
        // clear diagnostics on closed file
        const json_message = self.sendToClientNotification("textDocument/publishDiagnostics", .{
            .uri = notification.textDocument.uri,
            .diagnostics = &.{},
        }) catch return;
        self.allocator.free(json_message);
    }
}

fn shutdownHandler(self: *Self, _: std.mem.Allocator, _: void) Error!?void {
    defer self.status = .shutdown;
    if (self.status != .initialized) return error.InvalidRequest; // received a shutdown request but the server is not initialized!
}

fn exitHandler(self: *Self, _: std.mem.Allocator, _: void) Error!void {
    self.status = switch (self.status) {
        .initialized => .exiting_failure,
        .shutdown => .exiting_success,
        else => unreachable,
    };
}

fn sendToClientResponse(self: *Self, id: types.Message.ID, result: anytype) error{OutOfMemory}!void {
    try self.sendToClientInternal(
        id,
        null,
        null,
        "result",
        result,
    );
}

fn sendToClientRequest(self: *Self, id: types.Message.ID, method: []const u8, params: anytype) error{OutOfMemory}!void {
    try self.sendToClientInternal(
        id,
        method,
        null,
        "params",
        params,
    );
}

fn sendToClientNotification(self: *Self, method: []const u8, params: anytype) error{OutOfMemory}!void {
    try self.sendToClientInternal(
        null,
        method,
        null,
        "params",
        params,
    );
}

fn sendToClientResponseError(self: *Self, id: types.Message.ID, err: ?types.Message.Response.Error) error{OutOfMemory}!void {
    try self.sendToClientInternal(
        id,
        null,
        err,
        "",
        null,
    );
}

fn sendToClientInternal(
    self: *Self,
    maybe_id: ?types.Message.ID,
    maybe_method: ?[]const u8,
    maybe_err: ?types.Message.Response.Error,
    extra_name: []const u8,
    extra: anytype,
) error{OutOfMemory}!void {
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(self.allocator);

    var writer = buffer.writer(self.allocator);
    try writer.writeAll(
        \\{"jsonrpc":"2.0"
    );
    if (maybe_id) |id| {
        try writer.writeAll(
            \\,"id":
        );
        try std.json.stringify(id, .{}, writer);
    }
    if (maybe_method) |method| {
        try writer.writeAll(
            \\,"method":
        );
        try std.json.stringify(method, .{}, writer);
    }
    switch (@TypeOf(extra)) {
        void => {},
        ?void => {
            try writer.print(
                \\,"{s}":null
            , .{extra_name});
        },
        else => {
            try writer.print(
                \\,"{s}":
            , .{extra_name});
            try std.json.stringify(extra, .{ .emit_null_optional_fields = false }, writer);
        },
    }
    if (maybe_err) |err| {
        try writer.writeAll(
            \\,"error":
        );
        try std.json.stringify(err, .{}, writer);
    }
    try writer.writeByte('}');

    self.transport.writeJsonMessage(buffer.items) catch |err| {
        log.err("failed to write response: {}", .{err});
    };
}

const Job = union(enum) {
    incoming_message: std.json.Parsed(types.Message),
    generate_diagnostics: DocumentStore.Uri,
    run_build_on_save,

    fn deinit(self: Job, allocator: std.mem.Allocator) void {
        switch (self) {
            .incoming_message => |parsed_message| parsed_message.deinit(),
            .generate_diagnostics => |uri| allocator.free(uri),
            .run_build_on_save => {},
        }
    }
};

pub const Error = error{
    OutOfMemory,
    ParseError,
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
    /// Error code indicating that a server received a notification or
    /// request before the server has received the `initialize` request.
    ServerNotInitialized,
    /// A request failed but it was syntactically correct, e.g the
    /// method name was known and the parameters were valid. The error
    /// message should contain human readable information about why
    /// the request failed.
    ///
    /// @since 3.17.0
    RequestFailed,
    /// The server cancelled the request. This error code should
    /// only be used for requests that explicitly support being
    /// server cancellable.
    ///
    /// @since 3.17.0
    ServerCancelled,
    /// The server detected that the content of a document got
    /// modified outside normal conditions. A server should
    /// NOT send this error code if it detects a content change
    /// in it unprocessed messages. The result even computed
    /// on an older state might still be useful for the client.
    ///
    /// If a client decides that a result is not of any use anymore
    /// the client should cancel the request.
    ContentModified,
    /// The client has canceled a request and a server as detected
    /// the cancel.
    RequestCancelled,
};

pub const Status = enum {
    ready,
    shutdown,
    exiting_success,
    exiting_failure,
};

const HandledRequestMethods = enum {
    initialize,
    shutdown,
    @"textDocument/willSaveWaitUntil",
    @"textDocument/semanticTokens/full",
    @"textDocument/semanticTokens/range",
    @"textDocument/inlayHint",
    @"textDocument/completion",
    @"textDocument/signatureHelp",
    @"textDocument/definition",
    @"textDocument/typeDefinition",
    @"textDocument/implementation",
    @"textDocument/declaration",
    @"textDocument/hover",
    @"textDocument/documentSymbol",
    @"textDocument/formatting",
    @"textDocument/rename",
    @"textDocument/references",
    @"textDocument/documentHighlight",
    @"textDocument/codeAction",
    @"textDocument/foldingRange",
    @"textDocument/selectionRange",
};

const HandledNotificationMethods = enum {
    initialized,
    exit,
    @"$/cancelRequest",
    @"$/setTrace",
    @"textDocument/didOpen",
    @"textDocument/didChange",
    @"textDocument/didSave",
    @"textDocument/didClose",
    @"workspace/didChangeWorkspaceFolders",
    @"workspace/didChangeConfiguration",
};

//
// LSP helper functions
//

pub fn ResultType(comptime method: []const u8) type {
    if (getRequestMetadata(method)) |meta| return meta.Result;
    if (isNotificationMethod(method)) return void;
    @compileError("unknown method '" ++ method ++ "'");
}

pub fn ParamsType(comptime method: []const u8) type {
    if (getRequestMetadata(method)) |meta| return meta.Params orelse void;
    if (getNotificationMetadata(method)) |meta| return meta.Params orelse void;
    @compileError("unknown method '" ++ method ++ "'");
}

fn getRequestMetadata(comptime method: []const u8) ?types.RequestMetadata {
    for (types.request_metadata) |meta| {
        if (std.mem.eql(u8, method, meta.method)) {
            return meta;
        }
    }
    return null;
}

fn getNotificationMetadata(comptime method: []const u8) ?types.NotificationMetadata {
    for (types.notification_metadata) |meta| {
        if (std.mem.eql(u8, method, meta.method)) {
            return meta;
        }
    }
    return null;
}

const RequestMethodSet = blk: {
    @setEvalBranchQuota(5000);
    var kvs_list: [types.request_metadata.len]struct { []const u8 } = undefined;
    for (types.request_metadata, &kvs_list) |meta, *kv| {
        kv.* = .{meta.method};
    }
    break :blk std.ComptimeStringMap(void, &kvs_list);
};

const NotificationMethodSet = blk: {
    @setEvalBranchQuota(5000);
    var kvs_list: [types.notification_metadata.len]struct { []const u8 } = undefined;
    for (types.notification_metadata, &kvs_list) |meta, *kv| {
        kv.* = .{meta.method};
    }
    break :blk std.ComptimeStringMap(void, &kvs_list);
};

/// return true if there is a request with the given method name
pub fn isRequestMethod(method: []const u8) bool {
    return RequestMethodSet.has(method);
}

/// return true if there is a notification with the given method name
pub fn isNotificationMethod(method: []const u8) bool {
    return NotificationMethodSet.has(method);
}
