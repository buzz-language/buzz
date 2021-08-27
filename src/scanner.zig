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

    source: []u8,
    current: SourceLocation = .{
        .start = 0,
        .line = 0,
        .column = 0,
        .offset = 0,
    },

    pub fn init(allocator: *Allocator, source: []u8) Self {
        return Self{
            .source = source,
        };
    }

    pub fn scanToken(self: *Self) void {
        self.skipWhitespaces();

        self.current.start = self.current.offset;

        if (self.isEOF()) {
            return self.makeToken(.Eof, null, null, null);
        }

        switch (self.advance()) {
            'b' => if (isNumber(self.peek())) self.binary() else self.identifier(),
            'a', 'c'...'z', 'A'...'Z' => self.identifier(),
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
            '*' => return self.makeToken(if (self.match('=')) .StarEqual else .Star, null, null, null),
            '/' => return self.makeToken(if (self.match('=')) .SlashEqual else .Slash, null, null, null),
            '%' => return self.makeToken(.Percent, null, null, null),
            '?' => return self.makeToken(if (self.match('?')) .QuestionQuestion else .Question, null, null, null),
            '!' => return self.makeToken(if (self.match('=')) .BangEqual else .Bang, null, null, null),
            ':' => return self.makeToken(.Colon, null, null, null),
            '=' => return self.makeToken(if (self.match('=')) .EqualEqual else .Equal, null, null, null),
            '\"' => return self.string(),

            else => return self.makeToken(.Error, "Unexpected character.".*, null, null)
        }
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
                    while(self.peek() != '\n' and !self.isEOF()) {
                        _ = self.advance();
                    }
                },
                else => return
            }
        }
    }

    fn isNumber(char:  u8) bool { 
        return char >= '0' and char <= '9';
    }

    fn isLetter(char:  u8) bool { 
        return (char >= 'a' and char <= 'z')
            or (char >= 'A' and char <= 'Z');
    }

    fn identifier(self: *Self) !Token {
        while (isLetter(self.peek())) {
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

        if (self.peek() == '.' and isNumber(self.peekNext())) {
            _ = self.advance(); // Consume .

            while (isNumber(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.Number, null, try std.fmt.parseFloat(f64, self.source[self.current.start..self.current.offset]), null);
    }

    fn binary(self: *Self) !Token {
        var peeked: u8 = self.peek();
        while (peeked == '0' or peeked == '1') {
            _ = self.advance();

            peeked = self.peek();
        }

        if (self.current.offset - self.current.start != 10) {
            return self.makeToken(.Error, "Malformed binary number.".*, null, null);
        }

        return self.makeToken(.Byte, null, null, try std.fmt.parseInt(u8, self.source[self.current.start..self.current.offset], 0));
    }

    fn hexa(self: *Self) !Token {
        _ = self.advance(); // Consume 'x'

        var peeked: u8 = self.peek();
        while (isNumber(peeked) or (peeked >= 'A' and peeked <= 'F')) {
            _ = self.advance();

            peeked = self.peek();
        }

        if (self.current.offset - self.current.start != 4) {
            return self.makeToken(.Error, "Malformed hexadecimal number.".*, null, null);
        }

        return self.makeToken(.Byte, null, null, try std.fmt.parseInt(u8, self.source[self.current.start..self.current.offset], 0));
    }

    fn string(self: *Self) !Token {
        while (self.peek() != '"' and !self.isEOF()) {
            if (self.peek() == '\n') {
                return self.makeToken(.Error, "Unterminated string.".*, null, null);
            }

            _ = self.advance();
        }

        if (self.isEOF()) {
            return self.makeToken(.Error, "Unterminated string.".*, null, null);
        } else {
            _ = self.advance();
        }

        return self.makeToken(
            .String,
            if (self.current.offset - self.current.start > 0)
                self.source[(self.current.start + 1)..(self.current.offset - 1)]
            else null,
            null,
            null
        );
    }

    fn isEOF(self: *Self) bool {
        return self.current.offset >= self.source.len;
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

    fn makeToken(self: *Self, token_type: TokenType, literal_string: ?[]u8, literal_number: ?f64, literal_byte: ?u8) Token {
        return Token {
            .token_type = token_type,
            .lexeme = self.source[self.current.start..self.current.offset],
            .literal_string = literal_string,
            .literal_number = literal_number,
            .literal_byte = literal_byte,
            .line = self.current.line,
            .column = self.current.column,
        };
    }
};