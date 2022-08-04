const std = @import("std");
const _parser = @import("./parser.zig");
const _node = @import("./node.zig");
const Parser = _parser.Parser;
const ParseNode = _node.ParseNode;
const StringNode = _node.StringNode;
const StringLiteralNode = _node.StringLiteralNode;
const ParserState = _parser.ParserState;
const Scanner = @import("./scanner.zig").Scanner;
const _obj = @import("./obj.zig");
const _value = @import("./value.zig");
const Token = @import("./token.zig").Token;

const Value = _value.Value;
const ObjTypeDef = _obj.ObjTypeDef;
const copyStringRaw = _obj.copyStringRaw;

pub const StringParser = struct {
    const Self = @This();

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
    elements: std.ArrayList(*ParseNode),
    line_offset: usize,
    column_offset: usize,

    pub fn init(parser: *Parser, source: []const u8, line_offset: usize, column_offset: usize) Self {
        return Self{
            .parser = parser,
            .source = source,
            .current_chunk = std.ArrayList(u8).init(parser.allocator),
            .elements = std.ArrayList(*ParseNode).init(parser.allocator),
            .line_offset = line_offset,
            .column_offset = column_offset,
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
            self.column_offset += 0;
        } else {
            self.column_offset += 1;
        }

        return self.current;
    }

    pub fn parse(self: *Self) !*StringNode {
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
                            self.current_chunk = std.ArrayList(u8).init(self.parser.allocator);

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
            self.current_chunk = std.ArrayList(u8).init(self.parser.allocator);
        }

        var node = try self.parser.allocator.create(StringNode);
        node.* = .{ .elements = self.elements.items };
        node.node.type_def = try self.parser.type_registry.getTypeDef(.{ .def_type = .String });

        return node;
    }

    fn push(self: *Self, chars: []const u8) !void {
        var node = try self.parser.allocator.create(StringLiteralNode);
        node.* = .{
            .constant = try copyStringRaw(
                self.parser.strings,
                self.parser.allocator,
                chars,
                true, // The substring we built is now owned by parser
            ),
        };
        node.node.type_def = try self.parser.type_registry.getTypeDef(.{ .def_type = .String });
        node.node.location = self.parser.parser.previous_token.?;

        try self.elements.append(node.toNode());
    }

    fn inc(self: *Self) !void {
        self.chunk_count += 1;
    }

    fn interpolation(self: *Self) !void {
        var expr: []const u8 = self.source[self.offset..];

        var expr_scanner = Scanner.init(self.parser.allocator, expr);
        expr_scanner.line_offset = self.line_offset;
        expr_scanner.column_offset = self.column_offset;

        // Replace parser scanner with one that only looks at that substring
        var scanner = self.parser.scanner;
        self.parser.scanner = expr_scanner;
        var parser = self.parser.parser;
        self.parser.parser = ParserState.init(self.parser.allocator);

        try self.parser.advance();

        // Parse expression
        try self.elements.append(try self.parser.expression(false));

        const current: Token = self.parser.parser.current_token.?; // }
        var delta: usize = self.parser.scanner.?.current.offset;

        if (self.parser.parser.ahead.items.len > 0) {
            const next = self.parser.parser.ahead.items[self.parser.parser.ahead.items.len - 1];
            delta = delta - next.lexeme.len - next.offset + current.offset;
        }

        self.offset += delta - 1;
        self.previous_interp = self.offset;

        // Put back parser's scanner
        self.parser.scanner = scanner;
        self.parser.parser.deinit();
        self.parser.parser = parser;

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

        const num_str: []const u8 = self.source[start .. self.offset + 1];
        _ = self.advance();
        const number: ?u8 = std.fmt.parseInt(u8, num_str, 10) catch null;

        if (number) |unumber| {
            try self.current_chunk.append(unumber);
        } else {
            try self.parser.reportError("Raw char should be between 0 and 255.");
        }
    }
};
