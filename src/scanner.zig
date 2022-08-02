const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const tk = @import("./token.zig");
const Token = tk.Token;
const TokenType = tk.TokenType;

pub const SourceLocation = struct {
    start: usize,
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
        .line = 0,
        .column = 0,
        .offset = 0,
    },

    pub fn init(allocator: Allocator, source: []const u8) Self {
        return Self{
            .allocator = allocator,
            .source = source,
        };
    }

    pub fn getLines(self: *Self, allocator: Allocator, index: usize, n: usize) !std.ArrayList([]const u8) {
        var lines = std.ArrayList([]const u8).init(allocator);

        var it = std.mem.split(u8, self.source, "\n");
        var count: usize = 0;
        while (it.next()) |line| : (count += 1) {
            if (count >= index and count < index + n) {
                try lines.append(line);
            }

            if (count > index + n) {
                break;
            }
        }

        return lines;
    }

    pub fn scanToken(self: *Self) !Token {
        self.skipWhitespaces();

        self.current.start = self.current.offset;

        if (self.isEOF()) {
            return self.makeToken(.Eof, null, null);
        }

        var char: u8 = self.advance();
        return try switch (char) {
            'b' => return if (isNumber(self.peek())) self.binary() else self.identifier(),
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

            '[' => return self.makeToken(.LeftBracket, null, null),
            ']' => return self.makeToken(.RightBracket, null, null),
            '(' => return self.makeToken(.LeftParen, null, null),
            ')' => return self.makeToken(.RightParen, null, null),
            '{' => return self.makeToken(.LeftBrace, null, null),
            '}' => return self.makeToken(.RightBrace, null, null),
            ',' => return self.makeToken(.Comma, null, null),
            ';' => return self.makeToken(.Semicolon, null, null),
            '.' => return self.makeToken(.Dot, null, null),
            '>' => {
                if (self.match('>')) {
                    return self.makeToken(.ShiftRight, null, null);
                } else if (self.match('=')) {
                    return self.makeToken(.GreaterEqual, null, null);
                } else {
                    return self.makeToken(.Greater, null, null);
                }
            },
            '<' => {
                if (self.match('<')) {
                    return self.makeToken(.ShiftLeft, null, null);
                } else if (self.match('=')) {
                    return self.makeToken(.LessEqual, null, null);
                } else {
                    return self.makeToken(.Less, null, null);
                }
            },
            '+' => {
                if (self.match('+')) {
                    return self.makeToken(.Increment, null, null);
                } else if (self.match('=')) {
                    return self.makeToken(.PlusEqual, null, null);
                } else {
                    return self.makeToken(.Plus, null, null);
                }
            },
            '-' => {
                if (self.match('-')) {
                    return self.makeToken(.Decrement, null, null);
                } else if (self.match('=')) {
                    return self.makeToken(.MinusEqual, null, null);
                } else if (self.match('>')) {
                    return self.makeToken(.Arrow, null, null);
                } else {
                    return self.makeToken(.Minus, null, null);
                }
            },
            '*' => return self.makeToken(if (self.match('=')) .StarEqual else .Star, null, null),
            '/' => return self.makeToken(if (self.match('=')) .SlashEqual else .Slash, null, null),
            '%' => return self.makeToken(.Percent, null, null),
            '?' => return self.makeToken(if (self.match('?')) .QuestionQuestion else .Question, null, null),
            '!' => return self.makeToken(if (self.match('=')) .BangEqual else .Bang, null, null),
            ':' => return self.makeToken(.Colon, null, null),
            '=' => return self.makeToken(if (self.match('=')) .EqualEqual else .Equal, null, null),
            '\"' => return self.string(),
            '|' => return try self.docblock(),
            '_' => return try self.pattern(),

            else => return self.makeToken(.Error, "Unexpected character.", null),
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

                    try block.append(' ');
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

        return self.makeToken(.Docblock, std.mem.trim(u8, block.items, " "), null);
    }

    fn identifier(self: *Self) !Token {
        while (isLetter(self.peek())) {
            _ = self.advance();
        }

        const literal = self.source[self.current.start..self.current.offset];
        const keywordOpt = tk.isKeyword(literal);

        if (keywordOpt) |keyword| {
            return self.makeToken(keyword, literal, null);
        } else {
            return self.makeToken(.Identifier, literal, null);
        }
    }

    fn number(self: *Self) !Token {
        while (isNumber(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and isNumber(self.peekNext())) {
            _ = self.advance(); // Consume .

            while (isNumber(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.Number, null, try std.fmt.parseFloat(f64, self.source[self.current.start..self.current.offset]));
    }

    fn binary(self: *Self) !Token {
        var peeked: u8 = self.peek();
        while (peeked == '0' or peeked == '1') {
            _ = self.advance();

            peeked = self.peek();
        }

        if (self.current.offset - self.current.start != 10) {
            return self.makeToken(.Error, "Malformed binary number.", null);
        }

        return self.makeToken(.Number, null, try std.fmt.parseFloat(f64, self.source[self.current.start..self.current.offset]));
    }

    fn hexa(self: *Self) !Token {
        _ = self.advance(); // Consume 'x'

        var peeked: u8 = self.peek();
        while (isNumber(peeked) or (peeked >= 'A' and peeked <= 'F')) {
            _ = self.advance();

            peeked = self.peek();
        }

        return self.makeToken(.Number, null, try std.fmt.parseFloat(f64, self.source[self.current.start..self.current.offset]));
    }

    fn pattern(self: *Self) !Token {
        while (self.peek() != '_' and !self.isEOF()) {
            if (self.peek() == '\n') {
                return self.makeToken(.Error, "Unterminated pattern.", null);
            } else if (self.peek() == '_' and self.peekNext() == '_') {
                // Escaped pattern delimiter, go past it
                _ = self.advance();
            }

            _ = self.advance();
        }

        if (self.isEOF()) {
            return self.makeToken(.Error, "Unterminated pattern.", null);
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
        );
    }

    fn string(self: *Self) !Token {
        var in_interp: bool = false;
        var interp_depth: usize = 0;
        while ((self.peek() != '"' or in_interp) and !self.isEOF()) {
            if (self.peek() == '\n') {
                return self.makeToken(.Error, "Unterminated string.", null);
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
            return self.makeToken(.Error, "Unterminated string.", null);
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

    fn makeToken(self: *Self, token_type: TokenType, literal_string: ?[]const u8, literal_number: ?f64) Token {
        return Token{
            .token_type = token_type,
            .lexeme = self.source[self.current.start..self.current.offset],
            .literal_string = literal_string,
            .literal_number = literal_number,
            .offset = self.current.start,
            .line = self.current.line,
            .column = self.current.column,
        };
    }
};
