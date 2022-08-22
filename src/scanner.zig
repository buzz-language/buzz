const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const tk = @import("./token.zig");
const Token = tk.Token;
const TokenType = tk.TokenType;

pub const SourceLocation = struct {
    start: usize,
    start_line: usize,
    start_column: usize,
    line: usize,
    column: usize,
    offset: usize,
};

// TODO: iterate over utf8 grapheme instead of ascii characters
pub const Scanner = struct {
    const Self = @This();

    allocator: Allocator,
    source: []const u8,
    current: SourceLocation = .{
        .start = 0,
        .start_line = 0,
        .start_column = 0,
        .line = 0,
        .column = 0,
        .offset = 0,
    },
    // When scanning an interpolation, we want to remember the scanned string line
    line_offset: usize = 0,
    column_offset: usize = 0,
    script_name: []const u8,

    pub fn init(allocator: Allocator, script_name: []const u8, source: []const u8) Self {
        return Self{
            .allocator = allocator,
            .source = source,
            .script_name = script_name,
        };
    }

    pub fn scanToken(self: *Self) !Token {
        self.skipWhitespaces();

        self.current.start = self.current.offset;
        self.current.start_line = self.current.line;
        self.current.start_column = self.current.column;

        if (self.isEOF()) {
            return self.makeToken(.Eof, null, null, null);
        }

        var char: u8 = self.advance();
        return try switch (char) {
            'b' => return self.identifier(),
            'a', 'c'...'z', 'A'...'Z' => return self.identifier(),
            '0' => {
                if (self.match('x')) {
                    return try self.hexa();
                } else if (self.match('b')) {
                    return try self.binary();
                } else {
                    return try self.number();
                }
            },
            '1'...'9' => return try self.number(),

            '[' => return self.makeToken(.LeftBracket, null, null, null),
            ']' => return self.makeToken(.RightBracket, null, null, null),
            '(' => return self.makeToken(.LeftParen, null, null, null),
            ')' => return self.makeToken(.RightParen, null, null, null),
            '{' => return self.makeToken(.LeftBrace, null, null, null),
            '}' => return self.makeToken(.RightBrace, null, null, null),
            ',' => return self.makeToken(.Comma, null, null, null),
            ';' => return self.makeToken(.Semicolon, null, null, null),
            '.' => return self.makeToken(.Dot, null, null, null),
            '>' => {
                if (self.match('>')) {
                    return self.makeToken(.ShiftRight, null, null, null);
                } else if (self.match('=')) {
                    return self.makeToken(.GreaterEqual, null, null, null);
                } else {
                    return self.makeToken(.Greater, null, null, null);
                }
            },
            '<' => {
                if (self.match('<')) {
                    return self.makeToken(.ShiftLeft, null, null, null);
                } else if (self.match('=')) {
                    return self.makeToken(.LessEqual, null, null, null);
                } else {
                    return self.makeToken(.Less, null, null, null);
                }
            },
            '~' => return self.makeToken(.Bnot, null, null, null),
            '^' => return self.makeToken(.Xor, null, null, null),
            '\\' => return self.makeToken(.Bor, null, null, null),
            '+' => {
                if (self.match('+')) {
                    return self.makeToken(.Increment, null, null, null);
                } else if (self.match('=')) {
                    return self.makeToken(.PlusEqual, null, null, null);
                } else {
                    return self.makeToken(.Plus, null, null, null);
                }
            },
            '-' => {
                if (self.match('-')) {
                    return self.makeToken(.Decrement, null, null, null);
                } else if (self.match('=')) {
                    return self.makeToken(.MinusEqual, null, null, null);
                } else if (self.match('>')) {
                    return self.makeToken(.Arrow, null, null, null);
                } else {
                    return self.makeToken(.Minus, null, null, null);
                }
            },
            '&' => return self.makeToken(.Ampersand, null, null, null),
            '*' => return self.makeToken(if (self.match('=')) .StarEqual else .Star, null, null, null),
            '/' => return self.makeToken(if (self.match('=')) .SlashEqual else .Slash, null, null, null),
            '%' => return self.makeToken(.Percent, null, null, null),
            '?' => return self.makeToken(if (self.match('?')) .QuestionQuestion else .Question, null, null, null),
            '!' => return self.makeToken(if (self.match('=')) .BangEqual else .Bang, null, null, null),
            ':' => return self.makeToken(.Colon, null, null, null),
            '=' => return self.makeToken(if (self.match('=')) .EqualEqual else .Equal, null, null, null),
            '\"' => return self.string(),
            '|' => return try self.docblock(),
            '_' => return try self.pattern(),

            else => return self.makeToken(.Error, "Unexpected character.", null, null),
        };
    }

    fn skipWhitespaces(self: *Self) void {
        while (true) {
            var char: u8 = self.peek();

            switch (char) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.current.line += 1;
                    self.current.column = 0;
                    _ = self.advance();
                },
                '|' => {
                    // It's a docblock, we don't skip it
                    if (self.peekNext() == '|') {
                        return;
                    }

                    while (self.peek() != '\n' and !self.isEOF()) {
                        _ = self.advance();
                    }
                },
                else => return,
            }
        }
    }

    fn isNumber(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn isLetter(char: u8) bool {
        return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z');
    }

    fn docblock(self: *Self) !Token {
        _ = self.advance(); // Skip second `|`

        var block = std.ArrayList(u8).init(self.allocator);

        while (!self.isEOF()) {
            while (!self.isEOF()) {
                var char: u8 = self.peek();

                if (char == '\n') {
                    self.current.line += 1;
                    self.current.column = 0;
                    _ = self.advance();

                    try block.append('\n');
                    break;
                } else {
                    try block.append(char);
                }

                _ = self.advance();
            }

            self.skipWhitespaces();

            if (self.peek() != '|' or self.peekNext() != '|') {
                break;
            } else {
                _ = self.advance();
                _ = self.advance();
            }
        }

        return self.makeToken(.Docblock, std.mem.trim(u8, block.items, " "), null, null);
    }

    fn identifier(self: *Self) !Token {
        while (isLetter(self.peek()) or isNumber(self.peek())) {
            _ = self.advance();
        }

        const literal = self.source[self.current.start..self.current.offset];
        const keywordOpt = tk.isKeyword(literal);

        if (keywordOpt) |keyword| {
            return self.makeToken(keyword, literal, null, null);
        } else {
            return self.makeToken(.Identifier, literal, null, null);
        }
    }

    fn number(self: *Self) !Token {
        while (isNumber(self.peek())) {
            _ = self.advance();
        }

        var is_float: bool = false;
        if (self.peek() == '.' and isNumber(self.peekNext())) {
            is_float = true;
            _ = self.advance(); // Consume .

            while (isNumber(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(
            .Number,
            null,
            if (is_float) try std.fmt.parseFloat(f64, self.source[self.current.start..self.current.offset]) else null,
            if (!is_float) try std.fmt.parseInt(i64, self.source[self.current.start..self.current.offset], 10) else null,
        );
    }

    fn binary(self: *Self) !Token {
        var peeked: u8 = self.peek();
        while (peeked == '0' or peeked == '1') {
            _ = self.advance();

            peeked = self.peek();
        }

        return self.makeToken(
            .Number,
            null,
            null,
            try std.fmt.parseInt(i64, self.source[self.current.start + 2 .. self.current.offset], 2),
        );
    }

    fn hexa(self: *Self) !Token {
        _ = self.advance(); // Consume 'x'

        var peeked: u8 = self.peek();
        while (isNumber(peeked) or (peeked >= 'A' and peeked <= 'F')) {
            _ = self.advance();

            peeked = self.peek();
        }

        return self.makeToken(
            .Number,
            null,
            null,
            try std.fmt.parseInt(i64, self.source[self.current.start + 2 .. self.current.offset], 16),
        );
    }

    fn pattern(self: *Self) !Token {
        while ((self.peek() != '_' or self.peekNext() == '_') and !self.isEOF()) {
            if (self.peek() == '\n') {
                return self.makeToken(.Error, "Unterminated pattern.", null, null);
            } else if (self.peek() == '_' and self.peekNext() == '_') {
                // Escaped pattern delimiter, go past it
                _ = self.advance();
            }

            _ = self.advance();
        }

        if (self.isEOF()) {
            return self.makeToken(.Error, "Unterminated pattern.", null, null);
        } else {
            _ = self.advance();
        }

        return self.makeToken(
            .Pattern,
            if (self.current.offset - self.current.start > 0)
                self.source[(self.current.start + 1)..(self.current.offset - 1)]
            else
                null,
            null,
            null,
        );
    }

    fn string(self: *Self) !Token {
        var in_interp: bool = false;
        var interp_depth: usize = 0;
        while ((self.peek() != '"' or in_interp) and !self.isEOF()) {
            if (self.peek() == '\n') {
                return self.makeToken(.Error, "Unterminated string.", null, null);
            } else if (self.peek() == '{') {
                if (!in_interp) {
                    in_interp = true;
                } else {
                    interp_depth += 1;
                }
            } else if (self.peek() == '}') {
                if (in_interp) {
                    if (interp_depth > 0) {
                        interp_depth -= 1;
                    }

                    if (interp_depth == 0) {
                        in_interp = false;
                    }
                }
            } else if (self.peek() == '\\' and self.peekNext() == '"') {
                // Escaped string delimiter, go past it
                _ = self.advance();
            } else if (self.peek() == '\\' and self.peekNext() == '{') {
                // Escaped interpolation delimiter, go past it
                _ = self.advance();
            } else if (self.peek() == '\\' and self.peekNext() == '\\') {
                // Escaped backslash, go past it
                _ = self.advance();
            }

            _ = self.advance();
        }

        if (self.isEOF()) {
            return self.makeToken(.Error, "Unterminated string.", null, null);
        } else {
            _ = self.advance();
        }

        return self.makeToken(
            .String,
            if (self.current.offset - self.current.start > 0)
                self.source[(self.current.start + 1)..(self.current.offset - 1)]
            else
                null,
            null,
            null,
        );
    }

    fn isEOF(self: *Self) bool {
        return self.current.offset >= self.source.len or self.source[self.current.offset] == 0;
    }

    fn peek(self: *Self) u8 {
        if (self.isEOF()) {
            return '\x00';
        }

        return self.source[self.current.offset];
    }

    fn peekNext(self: *Self) u8 {
        if (self.current.offset + 1 >= self.source.len) {
            return '\x00';
        }

        return self.source[self.current.offset + 1];
    }

    fn advance(self: *Self) u8 {
        const char = self.source[self.current.offset];

        self.current.offset += 1;
        self.current.column += 1;

        return char;
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isEOF()) {
            return false;
        }

        if (self.source[self.current.offset] != expected) {
            return false;
        }

        self.current.offset += 1;
        return true;
    }

    fn makeToken(self: *Self, token_type: TokenType, literal_string: ?[]const u8, literal_float: ?f64, literal_integer: ?i64) Token {
        return Token{
            .token_type = token_type,
            .lexeme = self.source[self.current.start..self.current.offset],
            .literal_string = literal_string,
            .literal_float = literal_float,
            .literal_integer = literal_integer,
            .offset = self.current.start,
            .line = self.line_offset + self.current.start_line,
            .column = self.column_offset + self.current.start_column,
            .source = self.source,
            .script_name = self.script_name,
        };
    }
};
