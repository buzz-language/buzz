const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const tk = @import("./token.zig");
const Token = tk.Token;
const TokenType = tk.TokenType;

pub const LexicalError = error {
    UnterminatedString,
    UnexpectedCharacter,
    MalformedBinary,
    MalformedHexa,
};

pub const SourceLocation = struct {
    start: usize,
    line: usize,
    column: usize,
    offset: usize,
};

// TODO: utf8
pub const Scanner = struct {
    const Self = @This();

    source: []u8,
    errors: std.ArrayList(LexicalError),
    tokens: std.ArrayList(Token),
    current: SourceLocation = .{
        .start = 0,
        .line = 0,
        .column = 0,
        .offset = 0,
    },
    allocator: *Allocator,

    pub fn init(allocator: *Allocator, source: []u8) Self {
        return Self{
            .allocator = allocator,
            .source = source,
            .errors = std.ArrayList(LexicalError).init(allocator),
            .tokens = std.ArrayList(Token).init(allocator),
        };
    }
    

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
        self.tokens.deinit();
    }

    pub fn scan(self: *Self) !void {
        while (!self.isEOF()) {
            self.current.start = self.current.offset;
            try self.scanToken();
        }

        // TODO: print errors
    }

    fn scanToken(self: *Self) !void {
        try switch (self.advance()) {
            '|' => {
                while(self.peek() != '\n' and !self.isEOF()) {
                    _ = self.advance();
                }
            },
            '[' => try self.addToken(.LeftBracket, null, null, null),
            ']' => try self.addToken(.RightBracket, null, null, null),
            '(' => try self.addToken(.LeftParen, null, null, null),
            ')' => try self.addToken(.RightParen, null, null, null),
            '{' => try self.addToken(.LeftBrace, null, null, null),
            '}' => try self.addToken(.RightBrace, null, null, null),
            ',' => try self.addToken(.Comma, null, null, null),
            ';' => try self.addToken(.Semicolon, null, null, null),
            '.' => try self.addToken(.Dot, null, null, null),
            '>' => {
                if (self.match('>')) {
                    try self.addToken(.ShiftRight, null, null, null);
                } else if (self.match('=')) {
                    try self.addToken(.GreaterEqual, null, null, null);
                } else {
                    try self.addToken(.Greater, null, null, null);
                }
            },
            '<' => {
                if (self.match('<')) {
                    try self.addToken(.ShiftLeft, null, null, null);
                } else if (self.match('=')) {
                    try self.addToken(.LessEqual, null, null, null);
                } else {
                    try self.addToken(.Less, null, null, null);
                }
            },
            '+' => {
                if (self.match('+')) {
                    try self.addToken(.Increment, null, null, null);
                } else if (self.match('=')) {
                    try self.addToken(.PlusEqual, null, null, null);
                } else {
                    try self.addToken(.Plus, null, null, null);
                }
            },
            '-' => {
                if (self.match('-')) {
                    try self.addToken(.Decrement, null, null, null);
                } else if (self.match('=')) {
                    try self.addToken(.MinusEqual, null, null, null);
                } else if (self.match('>')) {
                    try self.addToken(.Arrow, null, null, null);
                } else {
                    try self.addToken(.Minus, null, null, null);
                }
            },
            '*' => try self.addToken(if (self.match('=')) .StarEqual else .Star, null, null, null),
            '/' => try self.addToken(if (self.match('=')) .SlashEqual else .Slash, null, null, null),
            '%' => try self.addToken(.Percent, null, null, null),
            '?' => try self.addToken(if (self.match('?')) .QuestionQuestion else .Question, null, null, null),
            '!' => try self.addToken(if (self.match('=')) .BangEqual else .Bang, null, null, null),
            ':' => try self.addToken(.Colon, null, null, null),
            '=' => try self.addToken(if (self.match('=')) .EqualEqual else .Equal, null, null, null),
            '\n' => {
                self.current.line += 1;
                self.current.column = 0;
            },
            '\"' => self.string(),
            'b' => if (isNumber(self.peek())) self.binary() else self.identifier(),
            'a', 'c'...'z', 'A'...'Z' => self.identifier(),
            '0' => {
                if (self.match('x')) {
                    try self.hexa();
                } else if (self.match('b')) {
                    try self.binary();
                } else {
                    try self.number();
                }
            },
            '1'...'9' => try self.number(),
            else => try self.errors.append(LexicalError.UnexpectedCharacter)
        };
    }

    fn isNumber(char:  u8) bool { 
        return char >= '0' and char <= '9';
    }

    fn isLetter(char:  u8) bool { 
        return (char >= 'a' and char <= 'z')
            or (char >= 'A' and char <= 'Z');
    }

    fn identifier(self: *Self) !void {
        while (isLetter(self.peek())) {
            _ = self.advance();
        }

        const literal = self.source[self.current.start..self.current.offset];
        const keywordOpt = tk.isKeyword(literal);

        if (keywordOpt) |keyword| {
            try self.addToken(keyword, literal, null, null);
        } else {
            try self.addToken(.Identifier, literal, null, null);
        }
    }
    
    fn number(self: *Self) !void {
        while (isNumber(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and isNumber(self.peekNext())) {
            _ = self.advance(); // Consume .

            while (isNumber(self.peek())) {
                _ = self.advance();
            }
        }

        try self.addToken(.Number, null, try std.fmt.parseFloat(f64, self.source[self.current.start..self.current.offset]), null);
    }

    fn binary(self: *Self) !void {
        var peeked: u8 = self.peek();
        while (peeked == '0' or peeked == '1') {
            _ = self.advance();

            peeked = self.peek();
        }

        if (self.current.offset - self.current.start != 10) {
            try self.errors.append(LexicalError.MalformedBinary);

            return;
        }

        try self.addToken(.Byte, null, null, try std.fmt.parseInt(u8, self.source[self.current.start..self.current.offset], 0));
    }

    fn hexa(self: *Self) !void {
        _ = self.advance(); // Consume 'x'

        var peeked: u8 = self.peek();
        while (isNumber(peeked) or (peeked >= 'A' and peeked <= 'F')) {
            _ = self.advance();

            peeked = self.peek();
        }

        if (self.current.offset - self.current.start != 4) {
            try self.errors.append(LexicalError.MalformedHexa);

            return;
        }

        try self.addToken(.Byte, null, null, try std.fmt.parseInt(u8, self.source[self.current.start..self.current.offset], 0));
    }

    fn string(self: *Self) !void {
        while (self.peek() != '"' and !self.isEOF()) {
            if (self.peek() == '\n') {
                try self.errors.append(LexicalError.UnterminatedString);
            }

            _ = self.advance();
        }

        if (self.isEOF()) {
            try self.errors.append(LexicalError.UnterminatedString);
        } else {
            _ = self.advance();
        }

        try self.addToken(
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

    fn addToken(self: *Self, token_type: TokenType, literal_string: ?[]u8, literal_number: ?f64, literal_byte: ?u8) !void {
        const token = Token {
            .token_type = token_type,
            .lexeme = self.source[self.current.start..self.current.offset],
            .literal_string = literal_string,
            .literal_number = literal_number,
            .literal_byte = literal_byte,
            .line = self.current.line,
            .column = self.current.column,
        };

        try self.tokens.append(token);
    }
};