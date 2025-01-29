const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const Token = @import("Token.zig");

pub const SourceLocation = struct {
    start: usize,
    start_line: usize,
    start_column: usize,
    line: usize,
    column: usize,
    offset: usize,
};

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
token_index: usize = 0,

pub fn init(allocator: Allocator, script_name: []const u8, source: []const u8) Self {
    return Self{
        .allocator = allocator,
        .source = source,
        .script_name = script_name,
    };
}

pub fn scanToken(self: *Self) Allocator.Error!Token {
    self.skipWhitespaces();

    self.current.start = self.current.offset;
    self.current.start_line = self.current.line;
    self.current.start_column = self.current.column;

    if (self.isEOF()) {
        return self.makeToken(.Eof, null, null, null);
    }

    const char: u8 = self.advance();
    return switch (char) {
        'a'...'z', 'A'...'Z' => self.identifier(),
        '_' => self.makeToken(
            .Identifier,
            self.source[self.current.start..self.current.offset],
            null,
            null,
        ),
        '0' => if (self.match('x'))
            self.hexa()
        else if (self.match('b'))
            self.binary()
        else
            self.number(),
        '1'...'9' => self.number(),

        '[' => self.makeToken(.LeftBracket, null, null, null),
        ']' => self.makeToken(.RightBracket, null, null, null),
        '(' => self.makeToken(.LeftParen, null, null, null),
        ')' => self.makeToken(.RightParen, null, null, null),
        '{' => self.makeToken(.LeftBrace, null, null, null),
        '}' => self.makeToken(.RightBrace, null, null, null),
        ',' => self.makeToken(.Comma, null, null, null),
        ';' => self.makeToken(.Semicolon, null, null, null),
        '.' => if (self.match('.'))
            self.makeToken(.Spread, null, null, null)
        else
            self.makeToken(.Dot, null, null, null),
        '>' => if (self.match('>'))
            if (self.match('='))
                self.makeToken(.ShiftRightEqual, null, null, null)
            else
                self.makeToken(.ShiftRight, null, null, null)
        else if (self.match('='))
            self.makeToken(.GreaterEqual, null, null, null)
        else
            self.makeToken(.Greater, null, null, null),
        '<' => if (self.match('<'))
            if (self.match('='))
                self.makeToken(.ShiftLeftEqual, null, null, null)
            else
                self.makeToken(.ShiftLeft, null, null, null)
        else if (self.match('='))
            self.makeToken(.LessEqual, null, null, null)
        else
            self.makeToken(.Less, null, null, null),
        '~' => if (self.match('='))
            self.makeToken(.BnotEqual, null, null, null)
        else
            self.makeToken(.Bnot, null, null, null),
        '^' => if (self.match('='))
            self.makeToken(.XorEqual, null, null, null)
        else
            self.makeToken(.Xor, null, null, null),
        '|' => if (self.match('='))
            self.makeToken(.BorEqual, null, null, null)
        else
            self.makeToken(.Bor, null, null, null),
        '+' => if (self.match('='))
            self.makeToken(.PlusEqual, null, null, null)
        else
            self.makeToken(.Plus, null, null, null),
        '-' => if (self.match('='))
            self.makeToken(.MinusEqual, null, null, null)
        else if (self.match('>'))
            self.makeToken(.Arrow, null, null, null)
        else
            self.makeToken(.Minus, null, null, null),
        '&' => if (self.match('='))
            self.makeToken(.AmpersandEqual, null, null, null)
        else
            self.makeToken(.Ampersand, null, null, null),
        '*' => if (self.match('='))
            self.makeToken(.StarEqual, null, null, null)
        else
            self.makeToken(.Star, null, null, null),
        '/' => if (self.match('='))
            self.makeToken(.SlashEqual, null, null, null)
        else if (self.match('/'))
            try self.docblock()
        else
            self.makeToken(.Slash, null, null, null),
        '%' => if (self.match('='))
            self.makeToken(.PercentEqual, null, null, null)
        else
            self.makeToken(.Percent, null, null, null),
        '?' => self.makeToken(if (self.match('?')) .QuestionQuestion else .Question, null, null, null),
        '!' => if (self.match('='))
            self.makeToken(.BangEqual, null, null, null)
        else if (self.match('>'))
            self.makeToken(.BangGreater, null, null, null)
        else
            self.makeToken(.Bang, null, null, null),
        ':' => if (self.match(':'))
            self.makeToken(.DoubleColon, null, null, null)
        else
            self.makeToken(.Colon, null, null, null),
        '=' => if (self.match('>'))
            self.makeToken(
                .DoubleArrow,
                null,
                null,
                null,
            )
        else
            self.makeToken(
                if (self.match('=')) .EqualEqual else .Equal,
                null,
                null,
                null,
            ),
        '"' => self.string(false),
        '`' => self.string(true),
        '\'' => self.byte(),
        '@' => self.atIdentifier(),
        '$' => self.pattern(),
        '\\' => self.makeToken(.AntiSlash, null, null, null),

        else => self.makeToken(
            .Error,
            "Unexpected character.",
            null,
            null,
        ),
    };
}

fn skipWhitespaces(self: *Self) void {
    while (true) {
        const char: u8 = self.peek();

        switch (char) {
            ' ', '\r', '\t' => _ = self.advance(),
            '\n' => {
                self.current.line += 1;
                self.current.column = 0;
                _ = self.advance();
            },
            '#' => { // Shebang
                if (self.token_index == 0 and self.peekNext() == '!') {
                    while (self.peek() != '\n' and !self.isEOF()) {
                        _ = self.advance();
                    }
                } else {
                    return;
                }
            },
            '/' => {
                // It's a / operator
                if (self.peekNext() != '/') {
                    return;
                }

                // It's a docblock
                if (self.peekNextNext() == '/') {
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
    _ = self.advance(); // Skip third `/`

    var block = std.ArrayList(u8).init(self.allocator);

    while (!self.isEOF()) {
        while (!self.isEOF()) {
            const char: u8 = self.peek();

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

        if (self.peek() != '/' or self.peekNext() != '/' or self.peekNextNext() != '/') {
            break;
        } else {
            _ = self.advance();
            _ = self.advance();
            _ = self.advance();
        }
    }

    return self.makeToken(.Docblock, std.mem.trim(u8, block.items, " "), null, null);
}

fn atIdentifier(self: *Self) Token {
    self.current.start = self.current.offset;
    self.current.start_line = self.current.line;
    self.current.start_column = self.current.column;

    if (self.advance() != '"') {
        return self.makeToken(.Error, "Unterminated identifier.", null, null);
    }

    const string_token = self.string(false);

    self.token_index += 1;
    return .{
        .tag = .Identifier,
        .lexeme = string_token.literal_string.?,
        .literal_string = string_token.literal_string.?,
        .literal_float = null,
        .literal_integer = null,
        .offset = self.current.start - 1,
        .line = self.line_offset + self.current.start_line,
        .column = self.column_offset + self.current.start_column,
        .source = self.source,
        .script_name = self.script_name,
    };
}

fn identifier(self: *Self) Token {
    while (isLetter(self.peek()) or isNumber(self.peek()) or self.peek() == '_') {
        _ = self.advance();
    }

    const literal = self.source[self.current.start..self.current.offset];
    const keywordOpt = Token.keywords.get(literal);

    if (keywordOpt) |keyword| {
        if (keyword == .As and self.match('?')) {
            return self.makeToken(.AsQuestion, null, null, null);
        }

        return self.makeToken(keyword, literal, null, null);
    } else {
        return self.makeToken(.Identifier, literal, null, null);
    }
}

fn number(self: *Self) Token {
    var peeked: u8 = self.peek();
    while (isNumber(peeked) or peeked == '_') {
        _ = self.advance();

        peeked = self.peek();
    }

    if (self.source[self.current.offset - 1] == '_') {
        return self.makeToken(.Error, "'_' must be between digits", null, null);
    }

    var is_float: bool = false;
    if (self.peek() == '.' and isNumber(self.peekNext())) {
        is_float = true;
        _ = self.advance(); // Consume .

        peeked = self.peek();
        while (isNumber(peeked) or peeked == '_') {
            _ = self.advance();

            peeked = self.peek();
        }
    }

    if (self.source[self.current.offset - 1] == '_') {
        return self.makeToken(.Error, "'_' must be between digits", null, null);
    }

    const double = if (is_float)
        std.fmt.parseFloat(f64, self.source[self.current.start..self.current.offset]) catch {
            return self.makeToken(
                .Error,
                "double overflow",
                null,
                null,
            );
        }
    else
        null;

    const int = if (!is_float)
        std.fmt.parseInt(i32, self.source[self.current.start..self.current.offset], 10) catch {
            return self.makeToken(
                .Error,
                "int overflow",
                null,
                null,
            );
        }
    else
        null;

    return self.makeToken(
        if (is_float) .FloatValue else .IntegerValue,
        null,
        double,
        int,
    );
}

fn byte(self: *Self) Token {
    const is_escape_sequence = self.match('\\');
    const literal_integer = if (!self.isEOF())
        self.advance()
    else
        null;

    if (is_escape_sequence and literal_integer != '\\' and literal_integer != '\'') {
        return self.makeToken(
            .Error,
            "Invalid escape sequence in char literal.",
            null,
            null,
        );
    }

    // Skip closing
    if (self.isEOF() or self.peek() != '\'') {
        return self.makeToken(
            .Error,
            "Unterminated char literal.",
            null,
            null,
        );
    } else {
        _ = self.advance();
    }

    return self.makeToken(
        .IntegerValue,
        null,
        null,
        @intCast(literal_integer.?),
    );
}

fn binary(self: *Self) Token {
    var peeked: u8 = self.peek();
    if (peeked == '_') {
        return self.makeToken(.Error, "'_' must be between digits", null, null);
    }

    while (peeked == '0' or peeked == '1' or peeked == '_') {
        _ = self.advance();

        peeked = self.peek();
    }

    if (self.source[self.current.offset - 1] == '_') {
        return self.makeToken(.Error, "'_' must be between digits", null, null);
    }

    return self.makeToken(
        .IntegerValue,
        null,
        null,
        std.fmt.parseInt(i32, self.source[self.current.start + 2 .. self.current.offset], 2) catch {
            return self.makeToken(
                .Error,
                "int overflow",
                null,
                null,
            );
        },
    );
}

fn hexa(self: *Self) Token {
    if (self.peek() == '_') {
        return self.makeToken(.Error, "'_' must be between digits", null, null);
    }

    _ = self.advance(); // Consume 'x'
    var peeked: u8 = self.peek();
    while (isNumber(peeked) or (peeked >= 'A' and peeked <= 'F') or (peeked >= 'a' and peeked <= 'f') or peeked == '_') {
        _ = self.advance();

        peeked = self.peek();
    }

    if (self.source[self.current.offset - 1] == '_') {
        return self.makeToken(.Error, "'_' must be between digits", null, null);
    }

    return self.makeToken(
        .IntegerValue,
        null,
        null,
        std.fmt.parseInt(i32, self.source[self.current.start + 2 .. self.current.offset], 16) catch {
            return self.makeToken(
                .Error,
                "int overflow",
                null,
                null,
            );
        },
    );
}

fn pattern(self: *Self) Token {
    if (self.advance() != '"') {
        return self.makeToken(.Error, "Unterminated pattern.", null, null);
    }

    while ((self.peek() != '"' or self.peekNext() == '"') and !self.isEOF()) {
        if (self.peek() == '\n') {
            return self.makeToken(.Error, "Unterminated pattern.", null, null);
        } else if (self.peek() == '\\' and self.peekNext() == '"') {
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
            self.source[(self.current.start + 2)..(self.current.offset - 1)]
        else
            null,
        null,
        null,
    );
}

fn string(self: *Self, multiline: bool) Token {
    const delimiter: u8 = if (multiline) '`' else '"';
    var in_interp: bool = false;
    var interp_depth: usize = 0;
    while ((self.peek() != delimiter or in_interp) and !self.isEOF()) {
        if (self.peek() == '\n' and !multiline) {
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
        } else if (self.peek() == '\\' and self.peekNext() == delimiter) {
            // Escaped string delimiter, go past it
            _ = self.advance();
        } else if (self.peek() == '\\' and self.peekNext() == '{') {
            // Escaped interpolation delimiter, go past it
            _ = self.advance();
        } else if (self.peek() == '\\' and self.peekNext() == '\\') {
            // Escaped backslash, go past it
            _ = self.advance();
        } else if (self.peek() == '\n') {
            self.current.line += 1;
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

fn peekNextNext(self: *Self) u8 {
    if (self.current.offset + 2 >= self.source.len) {
        return '\x00';
    }

    return self.source[self.current.offset + 2];
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

fn makeToken(self: *Self, tag: Token.Type, literal_string: ?[]const u8, literal_float: ?f64, literal_integer: ?i32) Token {
    self.token_index += 1;
    return Token{
        .tag = tag,
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

pub fn highlight(self: *Self, out: anytype, true_color: bool) void {
    var previous_offset: usize = 0;
    var token = self.scanToken() catch unreachable;
    while (token.tag != .Eof and token.tag != .Error) {
        // If there some whitespace or comments between tokens?
        // In gray because either whitespace or comment
        if (token.offset > previous_offset) {
            if (true_color) {
                out.print(
                    "{s}{s}{s}",
                    .{
                        Color.comment,
                        self.source[previous_offset..token.offset],
                        Color.reset,
                    },
                ) catch unreachable;
            } else {
                out.print(
                    "{s}{s}{s}{s}",
                    .{
                        Color.dim,
                        Color.black,
                        self.source[previous_offset..token.offset],
                        Color.reset,
                    },
                ) catch unreachable;
            }
        }

        out.print(
            "{s}{s}{s}",
            .{
                switch (token.tag) {
                    // Operators
                    .Greater,
                    .Less,
                    .Plus,
                    .Minus,
                    .Star,
                    .Slash,
                    .AntiSlash,
                    .Percent,
                    .Equal,
                    .EqualEqual,
                    .BangEqual,
                    .BangGreater,
                    .GreaterEqual,
                    .LessEqual,
                    .QuestionQuestion,
                    .ShiftLeft,
                    .ShiftRight,
                    .Xor,
                    .Bor,
                    .Bnot,
                    .Ud,
                    .Void,
                    .True,
                    .False,
                    .Null,
                    .Or,
                    .And,
                    .As,
                    .Return,
                    .If,
                    .Else,
                    .While,
                    .For,
                    .ForEach,
                    .Break,
                    .Continue,
                    .Final,
                    .Fun,
                    .In,
                    .Str,
                    .Int,
                    .Double,
                    .Bool,
                    .Pat,
                    .Do,
                    .Until,
                    .Is,
                    .Object,
                    .Obj,
                    .Static,
                    .Protocol,
                    .Enum,
                    .Throw,
                    .Catch,
                    .Try,
                    .Test,
                    .Function,
                    .Import,
                    .Export,
                    .Extern,
                    .From,
                    .Fib,
                    .Resume,
                    .Resolve,
                    .Yield,
                    .Any,
                    .Zdef,
                    .Type,
                    .TypeOf,
                    .Var,
                    .Question,
                    .AsQuestion,
                    .Out,
                    .Namespace,
                    .Range,
                    .Mut,
                    .PlusEqual,
                    .MinusEqual,
                    .StarEqual,
                    .SlashEqual,
                    .ShiftRightEqual,
                    .ShiftLeftEqual,
                    .XorEqual,
                    .BorEqual,
                    .BnotEqual,
                    .PercentEqual,
                    .AmpersandEqual,
                    => if (true_color) Color.keyword else Color.magenta,
                    // Punctuation
                    .LeftBracket,
                    .RightBracket,
                    .LeftParen,
                    .RightParen,
                    .LeftBrace,
                    .RightBrace,
                    .Dot,
                    .Comma,
                    .Semicolon,
                    .Bang,
                    .Colon,
                    .DoubleColon,
                    .Arrow,
                    .DoubleArrow,
                    .Ampersand,
                    .Spread,
                    => if (true_color) Color.punctuation else Color.bright_white,
                    .IntegerValue,
                    .FloatValue,
                    => if (true_color) Color.number else Color.yellow,
                    .String, .Pattern => if (true_color) Color.string else Color.green,
                    .Identifier => "",
                    .Docblock => if (true_color) Color.comment else Color.dim,
                    .Eof, .Error => unreachable,
                },
                token.lexeme,
                Color.reset,
            },
        ) catch unreachable;

        previous_offset = token.offset + token.lexeme.len;

        token = self.scanToken() catch unreachable;
    }

    // Is there some comments or whitespace after last token?
    if (previous_offset < self.source.len) {
        out.print(
            "{s}{s}{s}{s}",
            .{
                Color.dim,
                Color.black,
                self.source[previous_offset..],
                Color.reset,
            },
        ) catch unreachable;
    }
}

pub const Color = struct {
    pub const black = "\x1b[30m";
    pub const red = "\x1b[31m";
    pub const green = "\x1b[32m";
    pub const yellow = "\x1b[33m";
    pub const blue = "\x1b[34m";
    pub const magenta = "\x1b[35m";
    pub const cyan = "\x1b[36m";
    pub const white = "\x1b[37m";
    pub const bright_black = "\x1b[90m";
    pub const bright_red = "\x1b[91m";
    pub const bright_green = "\x1b[92m";
    pub const bright_yellow = "\x1b[93m";
    pub const bright_blue = "\x1b[94m";
    pub const bright_magenta = "\x1b[95m";
    pub const bright_cyan = "\x1b[96m";
    pub const bright_white = "\x1b[97m";
    pub const dim = "\x1b[1m";
    pub const bold = "\x1b[2m";
    pub const reset = "\x1b[0m";

    pub const comment = "\x1b[38;2;99;106;114m";
    pub const keyword = "\x1b[38;2;249;140;63m";
    pub const punctuation = "\x1b[38;2;255;215;0m";
    pub const number = "\x1b[38;2;249;175;79m";
    pub const string = "\x1b[38;2;127;217;98m";
};
