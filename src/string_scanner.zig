const std = @import("std");
const _compiler = @import("./compiler.zig");
const Compiler = _compiler.Compiler;
const ParserState = _compiler.ParserState;
const Scanner = @import("./scanner.zig").Scanner;
const _obj = @import("./obj.zig");
const _value = @import("./value.zig");
const Token = @import("./token.zig").Token;

const Value = _value.Value;
const ObjTypeDef = _obj.ObjTypeDef;
const copyStringRaw = _obj.copyStringRaw;

pub const StringScanner = struct {
    const Self = @This();

    source: []const u8,

    compiler: *Compiler,
    current: ?u8 = null,
    // TODO: this memory is never freed: it end up as key of the `strings` hashmap
    //       and since not all of its keys come from here, we don't know which we can
    //       free when we deinit strings.
    current_chunk: std.ArrayList(u8),
    offset: usize = 0,
    previous_interp: ?usize = null,
    chunk_count: usize = 0,

    pub fn init(compiler: *Compiler, source: []const u8) Self {
        return Self {
            .compiler = compiler,
            .source = source,
            .current_chunk = std.ArrayList(u8).init(compiler.allocator)
        };
    }

    fn advance(self: *Self) ?u8 {
        if (self.offset >= self.source.len) {
            return null;
        }

        self.current = self.source[self.offset];
        self.offset += 1;

        return self.current;
    }

    pub fn parse(self: *Self) !void {
        if (self.source.len == 0) return;

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
                            // The previous `current_chunk` memory is owned by the compiler
                            self.current_chunk = std.ArrayList(u8).init(self.compiler.allocator);

                            try self.inc();
                        }
                    }

                    try self.interpolation();

                    try self.inc();
                },
                else => try self.current_chunk.append(char.?)
            }
        }

        // Trailing string
        if ((self.previous_interp == null or self.previous_interp.? < self.offset)
            and self.current_chunk.items.len > 0) {
            try self.push(self.current_chunk.items);
            // The previous `current_chunk` memory is owned by the compiler
            self.current_chunk = std.ArrayList(u8).init(self.compiler.allocator);

            if (self.previous_interp != null) {
                try self.compiler.emitOpCode(.OP_ADD);
            }
        }
    }

    fn push(self: *Self, chars: []const u8) !void {
        try self.compiler.emitConstant(Value {
            .Obj = (try copyStringRaw(
                self.compiler.strings,
                self.compiler.allocator,
                chars,
                true // The substring we built is now owned by compiler
            )).toObj()
        });
    }

    fn inc(self: *Self) !void {
        self.chunk_count += 1;

        if (self.chunk_count == 2 or self.chunk_count > 2) {
            try self.compiler.emitOpCode(.OP_ADD);
        }
    }

    fn interpolation(self: *Self) !void {
        var expr: []const u8 = self.source[self.offset..];

        var expr_scanner = Scanner.init(expr);

        // Replace compiler scanner with one that only looks at that substring
        var scanner = self.compiler.scanner;
        self.compiler.scanner = expr_scanner;
        var parser = self.compiler.parser;
        self.compiler.parser = ParserState.init(self.compiler.allocator);

        try self.compiler.advance();

        // Parse expression
        var expr_type: *ObjTypeDef = try self.compiler.expression(false);
        // if placeholder we emit a useless OP_TO_STRING!
        if (expr_type.def_type != .String or expr_type.optional) {
            try self.compiler.emitOpCode(.OP_TO_STRING);
        }

        const current: Token = self.compiler.parser.current_token.?; // }
        var delta: usize = self.compiler.scanner.?.current.offset;

        if (self.compiler.parser.ahead.items.len > 0) {
            const next = self.compiler.parser.ahead.items[self.compiler.parser.ahead.items.len - 1];
            delta = delta - next.lexeme.len - next.offset + current.offset;
        }
        
        self.offset += delta - 1;
        self.previous_interp = self.offset;

        // Put back compiler's scanner
        self.compiler.scanner = scanner;
        self.compiler.parser.deinit();
        self.compiler.parser = parser;

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

        const num_str: []const u8 = self.source[start..self.offset+1];
        _ = self.advance();
        const number: ?u8 = std.fmt.parseInt(u8, num_str, 10) catch null;

        if (number) |unumber| {
            try self.current_chunk.append(unumber);
        } else {
            try self.compiler.reportError("Raw char should be between 0 and 255.");
        }
    }
};