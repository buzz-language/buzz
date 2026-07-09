const std = @import("std");
const Parser = @import("Parser.zig");
const Scanner = @import("Scanner.zig");
const obj = @import("obj.zig");
const Value = @import("value.zig").Value;
const Token = @import("Token.zig");
const Ast = @import("Ast.zig");

const Self = @This();

script_name: []const u8,
source: []const u8,

parser: *Parser,
current: ?u8 = null,
delimiter: u8,
current_chunk: std.ArrayList(u8) = .empty,
offset: usize = 0,
previous_interp: ?usize = null,
chunk_count: usize = 0,
elements: std.ArrayList(Ast.Node.Index) = .empty,
line_offset: usize,
column_offset: usize,
host_offset: usize,

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
    const start_location = self.parser.current_token.? - 1;
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
                        self.current_chunk.deinit(self.parser.gc.allocator);
                        self.current_chunk = .empty;

                        try self.inc();
                    }
                }

                try self.interpolation();

                try self.inc();
            },
            else => try self.current_chunk.append(self.parser.gc.allocator, char.?),
        }
    }

    // Trailing string
    if ((self.previous_interp == null or self.previous_interp.? < self.offset) and self.current_chunk.items.len > 0) {
        try self.push(self.current_chunk.items);

        // The previous `current_chunk` memory is owned by the parser
        self.current_chunk = .empty;
    }

    return try self.parser.ast.appendNode(
        .{
            .tag = .String,
            .location = start_location,
            .end_location = self.parser.current_token.? - 1,
            .type_def = self.parser.gc.type_registry.str_type,
            .components = .{
                .String = try self.elements.toOwnedSlice(self.parser.gc.allocator),
            },
        },
    );
}

fn push(self: *Self, chars: []const u8) !void {
    try self.elements.append(
        self.parser.gc.allocator,
        try self.parser.ast.appendNode(
            .{
                .tag = .StringLiteral,
                .location = self.parser.current_token.? - 1,
                .end_location = self.parser.current_token.? - 1,
                .type_def = self.parser.gc.type_registry.str_type,
                .components = .{
                    .StringLiteral = .{
                        .delimiter = self.delimiter,
                        .literal = try self.parser.gc.copyString(chars),
                    },
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

    const previous_current_token = self.parser.current_token.?;
    const previous_offset = self.offset + 1;

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
    self.parser.in_string_interpolation += 1;
    defer self.parser.in_string_interpolation -= 1;

    try self.parser.advance();

    // Parse expression
    try self.elements.append(self.parser.gc.allocator, try self.parser.expression(false));

    self.offset += self.parser.scanner.?.current.offset - 1;
    self.previous_interp = self.offset;

    // Fix location of tokens created by this parsing
    // Include the closing `}` token so nested interpolations keep monotonic token offsets.
    for (previous_current_token + 1..self.parser.current_token.? + 1) |i| {
        self.parser.ast.tokens.items(.source)[i] = scanner.?.source;
        self.parser.ast.tokens.items(.offset)[i] += self.host_offset + previous_offset;
    }

    // Put back parser's scanner
    self.parser.scanner = scanner;

    // Consume closing `}`
    _ = self.advance();
}

fn escape(self: *Self) !void {
    const char = self.advance();
    if (char == null) {
        return;
    }
    switch (char.?) {
        'a' => try self.current_chunk.append(self.parser.gc.allocator, 0x07),
        'b' => try self.current_chunk.append(self.parser.gc.allocator, 0x08),
        'f' => try self.current_chunk.append(self.parser.gc.allocator, 0x0c),
        'n' => try self.current_chunk.append(self.parser.gc.allocator, '\n'),
        'r' => try self.current_chunk.append(self.parser.gc.allocator, '\r'),
        't' => try self.current_chunk.append(self.parser.gc.allocator, '\t'),
        'v' => try self.current_chunk.append(self.parser.gc.allocator, 0x0b),
        '"' => try self.current_chunk.append(self.parser.gc.allocator, '"'),
        '\'' => try self.current_chunk.append(self.parser.gc.allocator, '\''),
        '\\' => try self.current_chunk.append(self.parser.gc.allocator, '\\'),
        '{' => try self.current_chunk.append(self.parser.gc.allocator, '{'),
        else => try self.rawChar(),
    }
}

fn rawChar(self: *Self) !void {
    const start = self.offset - 1;
    while (self.offset < self.source.len and self.source[self.offset] >= '0' and self.source[self.offset] <= '9') {
        _ = self.advance();
    }

    const num_str = self.source[start..self.offset];
    const number = std.fmt.parseInt(u8, num_str, 10) catch null;

    if (number) |unumber| {
        try self.current_chunk.append(self.parser.gc.allocator, unumber);
    } else {
        const location = self.parser.ast.tokens.get(self.parser.current_token.? - 1);
        self.parser.reporter.reportErrorAt(
            .raw_char,
            location,
            location,
            "Raw char should be between 0 and 255.",
        );
    }
}
