const std = @import("std");
const Parser = @import("Parser.zig");
const Scanner = @import("scanner.zig").Scanner;
const obj = @import("obj.zig");
const Value = @import("value.zig").Value;
const Token = @import("Token.zig");
const Ast = @import("Ast.zig");

pub const StringParser = struct {
    const Self = @This();

    script_name: []const u8,
    source: []const u8,

    parser: *Parser,
    current: ?u8 = null,
    // TODO: this memory is never freed: it end up as key of the `strings` hashmap
    //       and since not all of its keys come from here, we don't know which we can
    //       free when we deinit strings.
    current_chunk: std.ArrayList(u8),
    offset: usize = 0,
    previous_interp: ?usize = null,
    chunk_count: usize = 0,
    elements: std.ArrayList(Ast.Node.Index),
    line_offset: usize,
    column_offset: usize,

    pub fn init(
        parser: *Parser,
        source: []const u8,
        script_name: []const u8,
        line_offset: usize,
        column_offset: usize,
    ) Self {
        return Self{
            .parser = parser,
            .source = source,
            .current_chunk = std.ArrayList(u8).init(parser.gc.allocator),
            .elements = std.ArrayList(Ast.Node.Index).init(parser.gc.allocator),
            .line_offset = line_offset,
            .column_offset = column_offset,
            .script_name = script_name,
        };
    }

    fn advance(self: *Self) ?u8 {
        if (self.offset >= self.source.len) {
            return null;
        }

        self.current = self.source[self.offset];
        self.offset += 1;

        if (self.current != null and self.current.? == '\n') {
            self.line_offset += 1;
            self.column_offset = 0;
        } else {
            self.column_offset += 1;
        }

        return self.current;
    }

    pub fn parse(self: *Self) !Ast.Node.Index {
        while (self.offset < self.source.len) {
            const char: ?u8 = self.advance();
            if (char == null) {
                break;
            }

            switch (char.?) {
                '\\' => try self.escape(),
                '{' => {
                    if (self.previous_interp == null or self.previous_interp.? < self.offset - 1) {
                        if (self.current_chunk.items.len > 0) {
                            try self.push(self.current_chunk.items);
                            // The previous `current_chunk` memory is owned by the parser
                            self.current_chunk = std.ArrayList(u8).init(self.parser.gc.allocator);

                            try self.inc();
                        }
                    }

                    try self.interpolation();

                    try self.inc();
                },
                else => try self.current_chunk.append(char.?),
            }
        }

        // Trailing string
        if ((self.previous_interp == null or self.previous_interp.? < self.offset) and self.current_chunk.items.len > 0) {
            try self.push(self.current_chunk.items);

            // The previous `current_chunk` memory is owned by the parser
            self.current_chunk = std.ArrayList(u8).init(self.parser.gc.allocator);
        }

        self.elements.shrinkAndFree(self.elements.items.len);

        return try self.parser.ast.appendNode(
            .{
                .tag = .String,
                .location = self.parser.ast.nodes.items(.location)[self.elements.items[0]],
                .end_location = self.parser.ast.nodes.items(.location)[self.elements.getLast()],
                .type_def = try self.parser.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .String,
                    },
                ),
                .components = .{
                    .String = self.elements.items,
                },
            },
        );
    }

    fn push(self: *Self, chars: []const u8) !void {
        try self.elements.append(
            try self.parser.ast.appendNode(
                .{
                    .tag = .StringLiteral,
                    .location = self.parser.current_token.? - 1,
                    .end_location = self.parser.current_token.? - 1,
                    .type_def = try self.parser.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .String,
                        },
                    ),
                    .components = .{
                        .StringLiteral = try self.parser.gc.copyString(chars),
                    },
                },
            ),
        );
    }

    fn inc(self: *Self) !void {
        self.chunk_count += 1;
    }

    fn interpolation(self: *Self) !void {
        const expr = self.source[self.offset..];

        var expr_scanner = Scanner.init(
            self.parser.gc.allocator,
            self.parser.script_name,
            expr,
        );
        expr_scanner.line_offset = self.line_offset;
        expr_scanner.column_offset = self.column_offset;

        // Replace parser scanner with one that only looks at that substring
        const scanner = self.parser.scanner;
        self.parser.scanner = expr_scanner;

        try self.parser.advance();

        // Parse expression
        try self.elements.append(try self.parser.expression(false));

        self.offset += self.parser.scanner.?.current.offset - 1;
        self.previous_interp = self.offset;

        // Put back parser's scanner
        self.parser.scanner = scanner;

        // Consume closing `}`
        _ = self.advance();
    }

    fn escape(self: *Self) !void {
        const char: ?u8 = self.advance();
        if (char == null) {
            return;
        }
        switch (char.?) {
            'n' => try self.current_chunk.append('\n'),
            't' => try self.current_chunk.append('\t'),
            'r' => try self.current_chunk.append('\r'),
            '"' => try self.current_chunk.append('"'),
            '\\' => try self.current_chunk.append('\\'),
            '{' => try self.current_chunk.append('{'),
            else => try self.rawChar(),
        }
    }

    fn rawChar(self: *Self) !void {
        const start: usize = self.offset - 1;
        while (self.offset + 1 < self.source.len and self.source[self.offset + 1] >= '0' and self.source[self.offset + 1] <= '9') {
            _ = self.advance();
        }

        const num_str: []const u8 = self.source[start..@min(self.offset + 1, self.source.len)];
        _ = self.advance();
        const number: ?u8 = std.fmt.parseInt(u8, num_str, 10) catch null;

        if (number) |unumber| {
            try self.current_chunk.append(unumber);
        } else {
            self.parser.reportError(.raw_char, "Raw char should be between 0 and 255.");
        }
    }
};
