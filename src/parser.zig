const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const Scanner = @import("./scanner.zig").Scanner;
const tk = @import("./token.zig");
const Token = tk.Token;
const TokenType = tk.TokenType;

pub const ParseError = error {
    UnexpectedToken,
};

pub const Parser = struct {
    const Self = @This();

    scanner: Scanner,
    current: usize = 0,

    pub fn init(allocator: *Allocator, source: []u8) Self {
        var self = Self {
            .scanner = Scanner.init(allocator, source)
        };

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.scanner.deinit();
    }

    pub fn parse(self: *Self) !void {
        try self.scanner.scan();

        // If not lexical sound, exit
        if (self.scanner.errors.items.len > 0) {
            return;
        }
    }

    fn match(self: *Self, types: []TokenType) bool {
        for (types) |token_type| {
            if (self.check(token_type)) {
                _ = self.advance();

                return true;
            }
        }

        return false;
    }

    fn consume(self: *Self, expected: TokenType, err_message: []u8) !Token {
        if (self.check(expected)) {
            return self.advance();
        }

        std.debug.warn("Syntax error: {s}\n", err_message);

        return ParseError.UnexpectedToken;
    }

    fn check(self: *Self, expected: TokenType) bool {
        if (self.isAtEnd()) {
            return false;
        }

        return self.peek().token_type == expected;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }

        return self.previous();
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().token_type == .Eof or self.current >= self.scanner.tokens.items.len;
    }

    fn peek(self: *Self) Token {
        return self.scanner.tokens.items[self.current];
    }

    fn previous(self: *Self) Token {
        return self.scanner.tokens.items[self.current - 1];
    }

    fn synchronize(self: *Self) void {
        self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().token_type  == .Semicolon) {
                return;
            }

            switch (self.peek().token_type) {
                .Fun, .For, .If, .While, .Return => return,
                else => self.advance()
            }
        }
    }
};