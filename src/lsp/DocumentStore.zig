const std = @import("std");
const URI = @import("uri.zig");
const log = std.log.scoped(.lsp_store);
const Ast = @import("../Ast.zig");
const ImportRegistry = @import("../vm.zig").ImportRegistry;
const GarbageCollector = @import("../memory.zig").GarbageCollector;
const TypeRegistry = @import("../memory.zig").TypeRegistry;
const ObjTypeDef = @import("../obj.zig").ObjTypeDef;
const Parser = @import("../Parser.zig");

pub const Uri = []const u8;
pub const max_document_size = std.math.maxInt(u32);

const Self = @This();

allocator: std.mem.Allocator,
handles: std.StringArrayHashMapUnmanaged(*Handle) = .{},

pub fn deinit(self: *Self) void {
    self.handles.deinit(self.allocator);
}

pub const Handle = struct {
    uri: Uri,
    ast: Ast,

    pub fn init(allocator: std.mem.Allocator, uri: Uri, text: [:0]const u8) error{OutOfMemory}!Handle {
        var handle = Handle{
            .uri = uri,
            .ast = undefined,
        };

        handle.ast = try handle.parse(allocator, uri, text);

        return handle;
    }

    pub fn refresh(self: *Handle, allocator: std.mem.Allocator, new_text: [:0]const u8) error{OutOfMemory}!void {
        self.ast = try self.parse(
            allocator,
            self.uri,
            new_text,
        );
    }

    fn parse(allocator: std.mem.Allocator, uri: Uri, text: [:0]const u8) error{OutOfMemory}!Ast {
        var import_registry = ImportRegistry.init(allocator);
        defer import_registry.deinit();
        var gc = GarbageCollector.init(allocator);
        gc.type_registry = TypeRegistry{
            .gc = &gc,
            .registry = std.StringHashMap(*ObjTypeDef).init(allocator),
        };
        defer gc.deinit();
        var imports = std.StringHashMap(Parser.ScriptImport).init(allocator);
        defer imports.deinit();
        var parser = Parser.init(
            &gc,
            &imports,
            false,
            .Lsp,
        );
        defer parser.deinit();

        return (try parser.parse(text, uri)) orelse Ast.init(allocator);
    }
};

pub fn getHandle(self: *Self, uri: Uri) ?*Handle {
    return self.handles.get(uri);
}

pub fn getOrLoadHandle(self: *Self, uri: Uri) ?*Handle {
    if (self.getHandle(uri)) |handle|
        return handle;

    const file_path = URI.parse(self.allocator, uri) catch |err| {
        log.err("failed to parse URI `{s}`: {}", .{ uri, err });
        return null;
    };
    defer self.allocator.free(file_path);

    if (!std.fs.path.isAbsolute(file_path)) {
        log.err("file path is not absolute `{s}`", .{file_path});
        return null;
    }
    const file_contents = std.fs.cwd().readFileAllocOptions(
        self.allocator,
        file_path,
        max_document_size,
        null,
        @alignOf(u8),
        0,
    ) catch |err| {
        log.err("failed to load document `{s}`: {}", .{ file_path, err });
        return null;
    };

    return self.createAndStoreDocument(uri, file_contents, false) catch return null;
}

fn createAndStoreDocument(self: *Self, uri: Uri, text: [:0]const u8, open: bool) error{OutOfMemory}!*Handle {
    const handle_ptr: *Handle = try self.allocator.create(Handle);
    errdefer self.allocator.destroy(handle_ptr);

    handle_ptr.* = try self.createDocument(uri, text, open);
    errdefer handle_ptr.deinit();

    const gop = try self.handles.getOrPutValue(self.allocator, handle_ptr.uri, handle_ptr);

    if (gop.found_existing) {
        handle_ptr.deinit();
        self.allocator.destroy(handle_ptr);
    }

    log.debug("Opened document `{s}`", .{gop.value_ptr.*.uri});

    return gop.value_ptr.*;
}

pub fn refreshDocument(self: *Self, uri: Uri, new_text: [:0]const u8) !void {
    try self.handles.get(uri).?.refresh(self.allocator, new_text);
}

fn createDocument(self: *Self, uri: Uri, text: [:0]const u8) error{OutOfMemory}!Handle {
    return try Handle.init(self.allocator, uri, text);
}
