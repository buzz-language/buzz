const std = @import("std");
const assert = std.debug.assert;
const BuildOptions = @import("build_options");
const Token = @import("./token.zig").Token;
const o = @import("obj.zig");
const ObjTypeDef = o.ObjTypeDef;
const PlaceholderDef = o.PlaceholderDef;

const Self = @This();

allocator: std.mem.Allocator,
panic_mode: bool = false,
had_error: bool = false,

pub fn report(self: *Self, token: Token, message: []const u8) !void {
    const lines: std.ArrayList([]const u8) = try token.getLines(self.allocator, 3);
    defer lines.deinit();
    var report_line = std.ArrayList(u8).init(self.allocator);
    defer report_line.deinit();
    var writer = report_line.writer();

    try writer.print("\n", .{});
    var l: usize = if (token.line > 0) token.line - 1 else 0;
    for (lines.items) |line| {
        if (l != token.line) {
            try writer.print("\u{001b}[2m", .{});
        }

        var prefix_len: usize = report_line.items.len;
        try writer.print(" {: >5} │", .{l + 1});
        prefix_len = report_line.items.len - prefix_len;
        try writer.print(" {s}\n\u{001b}[0m", .{line});

        if (l == token.line) {
            try writer.print("       \u{001b}[2m┆\u{001b}[0m", .{});
            try writer.writeByteNTimes(' ', (if (token.column > 0) token.column else 0));
            try writer.print("\u{001b}[31m┬\u{001b}[0m\n", .{});

            try writer.print("       \u{001b}[2m┆\u{001b}[0m", .{});
            try writer.writeByteNTimes(' ', (if (token.column > 0) token.column else 0));
            try writer.print("\u{001b}[31m╰─ {s}\u{001b}[0m\n", .{message});
        }

        l += 1;
    }
    std.debug.print("{s}:{}:{}: \u{001b}[31mSyntax error:\u{001b}[0m {s}{s}", .{
        token.script_name,
        token.line + 1,
        token.column + 1,
        message,
        report_line.items,
    });

    if (BuildOptions.stop_on_report) {
        unreachable;
    }
}

pub fn reportErrorAt(self: *Self, token: Token, message: []const u8) !void {
    if (self.panic_mode) {
        return;
    }

    self.panic_mode = true;
    self.had_error = true;

    try self.report(token, message);
}

pub fn reportErrorFmt(self: *Self, token: Token, comptime fmt: []const u8, args: anytype) !void {
    var message = std.ArrayList(u8).init(self.allocator);
    defer message.deinit();

    var writer = message.writer();
    try writer.print(fmt, args);

    try self.reportErrorAt(token, message.items);
}

pub fn reportTypeCheckAt(self: *Self, expected_type: *ObjTypeDef, actual_type: *ObjTypeDef, message: []const u8, at: Token) !void {
    var error_message = std.ArrayList(u8).init(self.allocator);
    var writer = &error_message.writer();

    try writer.print("{s}: expected type `", .{message});
    try expected_type.toString(writer);
    try writer.writeAll("`, got `");
    try actual_type.toString(writer);
    try writer.writeAll("`");

    try self.reportErrorAt(at, error_message.items);
}

// Got to the root placeholder and report it
pub fn reportPlaceholder(self: *Self, placeholder: PlaceholderDef) anyerror!void {
    if (placeholder.parent) |parent| {
        if (parent.def_type == .Placeholder) {
            try self.reportPlaceholder(parent.resolved_type.?.Placeholder);
        }
    } else {
        // Should be a root placeholder with a name
        assert(placeholder.name != null);
        try self.reportErrorFmt(placeholder.where, "`{s}` is not defined", .{placeholder.name.?.string});
    }
}
