const std = @import("std");
const assert = std.debug.assert;
const Token = @import("Token.zig");
const o = @import("obj.zig");
const ObjTypeDef = o.ObjTypeDef;
const PlaceholderDef = o.PlaceholderDef;
const Scanner = @import("Scanner.zig");
const Ast = @import("Ast.zig");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const io = @import("io.zig");

const Self = @This();

// Do not reorder whitout updating documentation, values are explicit so they can be retrieved easily
pub const Error = enum(u8) {
    already_conforming_protocol = 0,
    arguments_count = 1,
    arithmetic_operand_type = 2,
    assignable = 3,
    assignment_value_type = 4,
    binary_operand_type = 5,
    bitwise_operand_type = 6,
    block_too_large = 7,
    call_argument_type = 8,
    call_arguments = 9,
    callable = 10,
    closures_count = 11,
    comparison_operand_type = 12,
    compile = 13,
    constant_default = 14,
    constant = 15,
    do_condition_type = 16,
    enum_argument = 17,
    enum_case_type = 18,
    enum_case = 19,
    enum_cases_count = 20,
    error_not_handled = 21,
    error_type = 22,
    expected_object = 23,
    export_count = 24,
    extern_main = 25,
    fiber_call_not_allowed = 26,
    fiber = 27,
    field_access = 28,
    for_condition_type = 29,
    foreach_iterable = 30,
    foreach_key_type = 31,
    foreach_value_type = 32,
    generic_type = 33,
    global_initializer = 34,
    globals_count = 35,
    if_condition_type = 36,
    inline_catch_type = 37,
    inline_if_body_type = 38,
    inline_if_else_type = 39,
    jump_too_large = 40,
    library_not_found = 41,
    list_item_type = 42,
    local_initializer = 43,
    locals_count = 44,
    logical_operand_type = 45,
    loop_body_too_large = 46,
    main_signature = 47,
    map_key_type = 48,
    map_value_type = 49,
    missing_return = 50,
    nested_try = 51,
    no_error = 52,
    optional = 53,
    pattern = 54,
    property_already_exists = 55,
    property_default_value = 56,
    property_does_not_exists = 57,
    property_not_initialized = 58,
    property_type = 59,
    protocol_conforming = 60,
    protocols_count = 61,
    range_type = 62,
    raw_char = 63,
    resolvable = 64,
    resumable = 65,
    return_type = 66,
    runtime = 67,
    script_not_found = 68,
    shadowed_global = 69,
    subscript_key_type = 70,
    subscript_value_type = 71,
    subscriptable = 72,
    symbol_not_found = 73,
    syntax = 74,
    undefined = 75,
    unexpected_error_type = 76,
    unknown_import = 77,
    unknown = 78,
    variable_already_exists = 79,
    while_condition_type = 80,
    yield_not_allowed = 81,
    yield_type = 82,
    zdef = 83,
    any_generic = 84,
    collect_signature = 85,
    tostring_signature = 86,
    gc = 87,
    discarded_value = 88,
    unused_argument = 89,
    inferred_type = 90,
    empty_import = 91,
    import_already_exists = 92,
    code_after_return = 93,
    unused_import = 94,
    label_does_not_exists = 95,
    constant_property = 96,
    tuple_limit = 97,
    mix_tuple = 98,
};

// Inspired by https://github.com/zesterer/ariadne
pub const ReportKind = enum {
    @"error",
    warning,
    hint,

    pub fn color(self: ReportKind) u8 {
        return switch (self) {
            .@"error" => 31,
            .warning => 33,
            .hint => 34,
        };
    }

    pub fn name(self: ReportKind) []const u8 {
        return switch (self) {
            .@"error" => "Error",
            .warning => "Warning",
            .hint => "Note",
        };
    }

    pub fn nameLower(self: ReportKind) []const u8 {
        return switch (self) {
            .@"error" => " error",
            .warning => " warning",
            .hint => " note",
        };
    }

    pub fn prefix(self: ReportKind) []const u8 {
        return switch (self) {
            .@"error" => "E",
            .warning => "W",
            .hint => "I",
        };
    }
};

pub const Note = struct {
    kind: ReportKind = .hint,
    message: []const u8,
    show_prefix: bool = true,
};

pub const ReportItem = struct {
    location: Token,
    kind: ReportKind = .@"error",
    message: []const u8,

    pub const SortContext = struct {};

    pub fn lessThan(_: SortContext, lhs: ReportItem, rhs: ReportItem) bool {
        return lhs.location.line < rhs.location.line or (lhs.location.line == rhs.location.line and lhs.location.column < rhs.location.column);
    }
};

pub const ReportOptions = struct {
    surrounding_lines: usize = 2,
    color: bool = true,
};

pub const Report = struct {
    message: []const u8,
    error_type: Error,
    items: []const ReportItem,
    notes: []const Note = &[_]Note{},
    options: ReportOptions = .{},

    pub inline fn reportStderr(self: *Report, reporter: *Self) !void {
        return self.report(reporter, io.stdErrWriter);
    }

    pub fn report(self: *Report, reporter: *Self, out: anytype) !void {
        assert(self.items.len > 0);
        var env_map = try std.process.getEnvMap(reporter.allocator);
        defer env_map.deinit();

        const colorterm = env_map.get("COLORTERM");
        const true_color = if (colorterm) |ct|
            std.mem.eql(u8, ct, "24bit") or std.mem.eql(u8, ct, "truecolor")
        else
            false;

        // Print main error message
        const main_item = self.items[0];

        if (self.options.color) {
            try out.print(
                "\n{s}:{}:{}: \x1b[{d}m[{s}{d}] {s}{s}:\x1b[0m {s}\n",
                .{
                    main_item.location.script_name,
                    main_item.location.line + 1,
                    main_item.location.column,
                    main_item.kind.color(),
                    main_item.kind.prefix(),
                    @intFromEnum(self.error_type),
                    if (reporter.error_prefix) |prefix|
                        prefix
                    else
                        "",
                    if (reporter.error_prefix != null)
                        main_item.kind.nameLower()
                    else
                        main_item.kind.name(),
                    self.message,
                },
            );
        } else {
            try out.print(
                "\n{s}:{}:{}: [{s}{d}] {s}{s}: {s}\n",
                .{
                    main_item.location.script_name,
                    main_item.location.line + 1,
                    main_item.location.column,
                    main_item.kind.prefix(),
                    @intFromEnum(self.error_type),
                    if (reporter.error_prefix) |prefix|
                        prefix
                    else
                        "",
                    if (reporter.error_prefix != null)
                        main_item.kind.nameLower()
                    else
                        main_item.kind.name(),
                    self.message,
                },
            );
        }

        // Print items

        // Group items by files
        var reported_files = std.StringArrayHashMap(std.ArrayList(ReportItem)).init(reporter.allocator);
        defer {
            var it = reported_files.iterator();
            while (it.next()) |kv| {
                kv.value_ptr.*.deinit();
            }
            reported_files.deinit();
        }

        for (self.items) |item| {
            if (reported_files.get(item.location.script_name) == null) {
                try reported_files.put(
                    item.location.script_name,
                    std.ArrayList(ReportItem).init(reporter.allocator),
                );
            }

            try reported_files.getEntry(item.location.script_name).?.value_ptr.append(item);
        }

        var file_it = reported_files.iterator();
        while (file_it.next()) |file_entry| {
            if (reported_files.count() > 1) {
                if (self.options.color) {
                    try out.print("       \x1b[2m╭─\x1b[0m \x1b[4m{s}\x1b[0m\n", .{file_entry.key_ptr.*});
                } else {
                    try out.print("       ╭─ {s}\n", .{file_entry.key_ptr.*});
                }
            }

            // Sort items by location in the source
            std.sort.insertion(
                ReportItem,
                file_entry.value_ptr.items,
                ReportItem.SortContext{},
                ReportItem.lessThan,
            );

            var reported_lines = std.AutoArrayHashMap(usize, std.ArrayList(ReportItem)).init(reporter.allocator);
            defer {
                var it = reported_lines.iterator();
                while (it.next()) |kv| {
                    kv.value_ptr.*.deinit();
                }
                reported_lines.deinit();
            }

            for (file_entry.value_ptr.items) |item| {
                if (reported_lines.get(item.location.line) == null) {
                    try reported_lines.put(
                        item.location.line,
                        std.ArrayList(ReportItem).init(reporter.allocator),
                    );
                }

                try reported_lines.getEntry(item.location.line).?.value_ptr.append(item);
            }

            var previous_line: ?usize = null;
            const keys = reported_lines.keys();
            for (keys, 0..) |line, index| {
                const next_line = if (index < keys.len - 1) keys[index + 1] else null;
                const report_items = reported_lines.get(line).?;

                assert(report_items.items.len > 0);

                // Does it overlap with previous reports, if so don't show lines before again
                var overlapping_before: i64 = if (previous_line) |previous|
                    @as(i64, @intCast(previous + self.options.surrounding_lines)) - @as(i64, @intCast(line - @min(line, self.options.surrounding_lines))) + 1
                else
                    0;

                // Is there a gap between two report items?
                if (overlapping_before < 0) {
                    if (self.options.color) {
                        try out.print("       \x1b[2m ...\x1b[0m\n", .{});
                    } else {
                        try out.print("        ...\n", .{});
                    }
                }

                overlapping_before = @max(overlapping_before, 0);

                var before = @as(i64, @intCast(self.options.surrounding_lines)) - overlapping_before;
                before = @max(0, before);

                const after = if (next_line) |next|
                    if (next <= (line + self.options.surrounding_lines))
                        (line + self.options.surrounding_lines) - next + 1
                    else
                        self.options.surrounding_lines
                else
                    self.options.surrounding_lines;

                const lines = try report_items.items[0].location.getLines(
                    reporter.allocator,
                    @intCast(before),
                    after,
                );
                defer lines.deinit();

                var l: usize = line - @min(line, @as(usize, @intCast(before)));
                for (lines.items, 0..) |src_line, line_index| {
                    if (l != line) {
                        if (self.options.color) {
                            try out.print("\x1b[2m", .{});
                        }
                    }

                    try out.print(
                        " {: >5} {s} ",
                        .{
                            l + 1,
                            if (line_index == 0 and (reported_files.count() == 1 or index > 0))
                                "╭─"
                            else if (line_index == lines.items.len - 1)
                                "╰─"
                            else
                                "│ ",
                        },
                    );

                    if (l == line) {
                        if (self.options.color) {
                            var scanner = Scanner.init(
                                reporter.allocator,
                                "reporter",
                                src_line,
                            );
                            scanner.highlight(out, true_color);
                        } else {
                            try out.writeAll(src_line);
                        }
                    } else {
                        try out.writeAll(src_line);
                    }

                    if (self.options.color) {
                        try out.writeAll("\n\x1b[0m");
                    } else {
                        try out.writeAll("\n");
                    }

                    if (l == line) {
                        // Print error cursors
                        if (self.options.color) {
                            try out.print("       \x1b[2m┆ \x1b[0m ", .{});
                        } else {
                            try out.print("       ┆  ", .{});
                        }
                        var column: usize = 0;
                        for (report_items.items) |item| {
                            const indent = if (item.location.column > 0)
                                item.location.column - 1 - @min(column, item.location.column - 1)
                            else
                                0;
                            try out.writeByteNTimes(' ', indent);

                            if (self.options.color) {
                                try out.print("\x1b[{d}m", .{item.kind.color()});
                            }

                            try out.print(
                                "{s}",
                                .{
                                    if (item.location.lexeme.len > 1)
                                        "╭"
                                    else
                                        "┬",
                                },
                            );

                            if (item.location.lexeme.len > 1) {
                                var i: usize = 0;
                                while (i < item.location.lexeme.len - 1) : (i += 1) {
                                    try out.print("─", .{});
                                }
                            } else {
                                try out.print("─", .{});
                            }

                            if (self.options.color) {
                                try out.print("\x1b[0m", .{});
                            }

                            column += indent + item.location.lexeme.len;
                        }

                        _ = try out.write("\n");

                        // Print error messages
                        for (report_items.items) |item| {
                            if (self.options.color) {
                                try out.print("       \x1b[2m┆ \x1b[0m ", .{});
                            } else {
                                try out.print("       ┆  ", .{});
                            }
                            try out.writeByteNTimes(' ', if (item.location.column > 0)
                                item.location.column - 1
                            else
                                0);
                            if (self.options.color) {
                                try out.print(
                                    "\x1b[{d}m╰─ {s}\x1b[0m\n",
                                    .{
                                        item.kind.color(),
                                        item.message,
                                    },
                                );
                            } else {
                                try out.print(
                                    "╰─ {s}\n",
                                    .{
                                        item.message,
                                    },
                                );
                            }
                        }
                    }

                    l += 1;
                }

                previous_line = line;
            }
        }

        // Print notes
        for (self.notes) |note| {
            if (self.options.color) {
                try out.print(
                    "\x1b[{d}m{s}{s}\x1b[0m {s}\n",
                    .{
                        note.kind.color(),
                        if (note.show_prefix) note.kind.name() else "",
                        if (note.show_prefix) ":" else "",
                        note.message,
                    },
                );
            } else {
                try out.print(
                    "{s}{s} {s}\n",
                    .{
                        if (note.show_prefix) note.kind.name() else "",
                        if (note.show_prefix) ":" else "",
                        note.message,
                    },
                );
            }
        }
    }
};

allocator: std.mem.Allocator,
panic_mode: bool = false,
had_error: bool = false,
error_prefix: ?[]const u8 = null,

pub fn warn(self: *Self, error_type: Error, token: Token, message: []const u8) void {
    var error_report = Report{
        .message = message,
        .error_type = error_type,
        .items = &[_]ReportItem{
            ReportItem{
                .kind = .warning,
                .location = token,
                .message = message,
            },
        },
        .notes = &[_]Note{},
    };

    error_report.reportStderr(self) catch @panic("Unable to report error");
}

pub fn report(self: *Self, error_type: Error, token: Token, message: []const u8) void {
    self.panic_mode = true;
    self.had_error = true;

    var error_report = Report{
        .message = message,
        .error_type = error_type,
        .items = &[_]ReportItem{
            ReportItem{
                .location = token,
                .message = message,
            },
        },
        .notes = &[_]Note{},
    };

    error_report.reportStderr(self) catch @panic("Unable to report error");
}

pub fn reportErrorAt(self: *Self, error_type: Error, token: Token, message: []const u8) void {
    if (self.panic_mode) {
        return;
    }

    self.report(error_type, token, message);
}

pub fn warnAt(self: *Self, error_type: Error, token: Token, message: []const u8) void {
    self.warn(error_type, token, message);
}

pub fn reportErrorFmt(self: *Self, error_type: Error, token: Token, comptime fmt: []const u8, args: anytype) void {
    var message = std.ArrayList(u8).init(self.allocator);
    defer message.deinit();

    var writer = message.writer();
    writer.print(fmt, args) catch @panic("Unable to report error");

    self.reportErrorAt(error_type, token, message.items);
}

pub fn warnFmt(self: *Self, error_type: Error, token: Token, comptime fmt: []const u8, args: anytype) void {
    var message = std.ArrayList(u8).init(self.allocator);
    defer message.deinit();

    var writer = message.writer();
    writer.print(fmt, args) catch @panic("Unable to report error");

    self.warnAt(error_type, token, message.items);
}

pub fn reportWithOrigin(
    self: *Self,
    error_type: Error,
    at: Token,
    decl_location: Token,
    comptime fmt: []const u8,
    args: anytype,
    declared_message: ?[]const u8,
) void {
    var message = std.ArrayList(u8).init(self.allocator);
    defer message.deinit();

    var writer = message.writer();
    writer.print(fmt, args) catch @panic("Unable to report error");

    var decl_report = Report{
        .message = message.items,
        .error_type = error_type,
        .items = &[_]ReportItem{
            .{
                .location = at,
                .kind = .@"error",
                .message = message.items,
            },
            .{
                .location = decl_location,
                .kind = .hint,
                .message = declared_message orelse "declared here",
            },
        },
    };

    self.panic_mode = true;
    self.had_error = true;

    decl_report.reportStderr(self) catch @panic("Could not report error");
}

pub fn reportTypeCheck(
    self: *Self,
    error_type: Error,
    expected_location: ?Token,
    expected_type: *ObjTypeDef,
    actual_location: Token,
    actual_type: *ObjTypeDef,
    message: []const u8,
) void {
    var actual_message = std.ArrayList(u8).init(self.allocator);
    defer actual_message.deinit();
    var writer = &actual_message.writer();

    writer.print("{s}: got type `", .{message}) catch @panic("Unable to report error");
    actual_type.toString(writer) catch @panic("Unable to report error");
    writer.writeAll("`") catch @panic("Unable to report error");

    var expected_message = std.ArrayList(u8).init(self.allocator);
    defer expected_message.deinit();

    if (expected_location != null) {
        writer = &expected_message.writer();
    }

    writer.writeAll("expected `") catch @panic("Unable to report error");

    expected_type.toString(writer) catch @panic("Unable to report error");
    writer.writeAll("`") catch @panic("Unable to report error");

    var full_message = if (expected_location == null) actual_message else std.ArrayList(u8).init(self.allocator);
    defer {
        if (expected_location != null) {
            full_message.deinit();
        }
    }
    if (expected_location != null) {
        full_message.writer().print("{s}, {s}", .{ actual_message.items, expected_message.items }) catch @panic("Unable to report error");
    }

    var check_report = if (expected_location) |location|
        Report{
            .message = full_message.items,
            .error_type = error_type,
            .items = &[_]ReportItem{
                .{
                    .location = actual_location,
                    .kind = .@"error",
                    .message = actual_message.items,
                },
                .{
                    .location = location,
                    .kind = .hint,
                    .message = expected_message.items,
                },
            },
        }
    else
        Report{
            .message = full_message.items,
            .error_type = error_type,
            .items = &[_]ReportItem{
                .{
                    .location = actual_location,
                    .kind = .hint,
                    .message = actual_message.items,
                },
            },
        };

    self.panic_mode = true;
    self.had_error = true;

    check_report.reportStderr(self) catch @panic("Could not report error");
}

// Got to the root placeholder and report it
pub fn reportPlaceholder(self: *Self, ast: Ast, placeholder: PlaceholderDef) void {
    if (placeholder.parent) |parent| {
        if (parent.def_type == .Placeholder) {
            self.reportPlaceholder(ast, parent.resolved_type.?.Placeholder);
        }
    } else {
        // Should be a root placeholder with a name
        self.reportErrorFmt(
            .undefined,
            ast.tokens.get(placeholder.where),
            "`{s}` is not defined",
            .{ast.tokens.items(.lexeme)[placeholder.where]},
        );
    }
}

test "multiple error on one line" {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};

    var reporter = Self{
        .allocator = gpa.allocator(),
    };

    const source =
        \\| Say hello
        \\fun hello() > void {
        \\    callSomething(true, complex: 12);
        \\    return null;
        \\}
        \\
        \\fun something() > int {
        \\    foreach (int i, str char in "hello") {
        \\        if (i % 2 == 0) {
        \\            print("yes");
        \\            return i;
        \\        }
        \\    }
        \\
        \\    return -1;
        \\}
    ;

    var bad = Report{
        .message = "This could have been avoided if you were not a moron",
        .error_type = .runtime,
        .notes = &[_]Note{
            .{ .message = "This could have been avoided if you were not a moron" },
        },
        .items = &[_]ReportItem{
            .{
                .location = Token{
                    .source = source,
                    .script_name = "test",
                    .tag = .Identifier,
                    .lexeme = "callSomething",
                    .line = 2,
                    .column = 5,
                },
                .kind = .@"error",
                .message = "This is so wrong",
            },
            .{
                .location = Token{
                    .source = source,
                    .script_name = "test",
                    .tag = .Identifier,
                    .lexeme = "true",
                    .line = 2,
                    .column = 19,
                },
                .kind = .hint,
                .message = "This is also wrong",
            },
            .{
                .location = Token{
                    .source = source,
                    .script_name = "test",
                    .tag = .Identifier,
                    .lexeme = "complex",
                    .line = 2,
                    .column = 25,
                },
                .kind = .warning,
                .message = "This is terribly wrong",
            },
            .{
                .location = Token{
                    .source = source,
                    .script_name = "test",
                    .tag = .Identifier,
                    .lexeme = "print",
                    .line = 9,
                    .column = 13,
                },
                .kind = .hint,
                .message = "This was correct here",
            },
        },
    };

    try bad.reportStderr(&reporter);
}
