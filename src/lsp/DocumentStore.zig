const std = @import("std");
const node = @import("../node.zig");

const Self = @This();

pub const Uri = []const u8;

pub const Document = struct {
    uri: Uri,
    root: *node.ParseNode,

    // TODO: add here things like, already computed list of symbols etc.
};

allocator: std.mem.Allocator,
documents: std.StringArrayHashMap(*Document),

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .documents = std.StringArrayHashMap(*Document).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.documents.deinit();
}
