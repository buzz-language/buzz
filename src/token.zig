const std = @import("std");
const mem = std.mem;

pub const Token = struct {
    const Self = @This();

    // Since we can parse multiple file, we have to keep a reference to the source in which this token occured
    source: []const u8,
    script_name: []const u8,
    token_type: TokenType,
    lexeme: []const u8,
    // Literal is either a string or a number
    literal_string: ?[]const u8 = null,
    literal_float: ?f64 = null,
    literal_integer: ?i64 = null,
    line: usize,
    column: usize,
    offset: usize = 0,

    pub fn eql(self: Self, other: Self) bool {
        // zig fmt: off
        return self.token_type == other.token_type
            and self.line == other.line
            and self.column == other.column
            and self.offset == other.offset
            and std.mem.eql(u8, self.source, other.source)
            and std.mem.eql(u8, self.script_name, other.script_name);
        // zig fmt: on
    }

    pub fn identifier(name: []const u8) Self {
        return .{
            .token_type = .Identifier,
            .lexeme = name,
            .line = 0,
            .column = 0,
            .source = "",
            .script_name = "",
        };
    }

    pub fn clone(self: Self) Self {
        return .{
            .token_type = self.token_type,
            .lexeme = self.lexeme,
            .source = self.source,
            .script_name = self.script_name,
            .literal_string = self.literal_string,
            .literal_float = self.literal_float,
            .literal_integer = self.literal_integer,
            .line = self.line,
            .column = self.column,
        };
    }

    // Return `n` lines around the token line in its source
    pub fn getLines(self: Self, allocator: mem.Allocator, n: usize) !std.ArrayList([]const u8) {
        var lines = std.ArrayList([]const u8).init(allocator);
        const index = if (self.line > 0) self.line - 1 else 0;

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
};

// WARNING: don't reorder without reordering `rules` in parser.zig
pub const TokenType = enum {
    Pipe, // |
    LeftBracket, // [
    RightBracket, // ]
    LeftParen, // (
    RightParen, // )
    LeftBrace, // {
    RightBrace, // }
    Dot, // .
    Comma, // ,
    Semicolon, // ;
    Greater, // >
    Less, // <
    Plus, // +
    Minus, // -
    Star, // *
    Slash, // /
    Percent, // %
    Question, // ?
    Bang, // !
    Colon, // :
    Equal, // =
    EqualEqual, // ==
    BangEqual, // !=
    GreaterEqual, // >=
    LessEqual, // <=
    QuestionQuestion, // ??
    PlusEqual, // +=
    MinusEqual, // -=
    StarEqual, // *=
    SlashEqual, // /=
    Increment, // ++
    Decrement, // --
    Arrow, // ->
    True, // true
    False, // false
    Null, // null
    Str, // str
    Ud, // ud
    Num, // num
    Type, // type
    Bool, // bool
    Function, // Function

    ShiftRight, // >>
    ShiftLeft, // <<
    Xor, // ^
    Bor, // \
    Bnot, // ~

    Or, // or
    And, // and
    Return, // return
    If, // if
    Else, // else
    Do, // do
    Until, // until
    While, // while
    For, // for
    ForEach, // foreach
    Switch, // switch
    Break, // break
    Continue, // continue
    Default, // default
    In, // in
    Is, // is
    Number, // 123
    String, // "hello"
    Identifier, // anIdentifier
    Fun, // fun
    Object, // object
    Class, // class
    Enum, // enum
    Throw, // throw
    Catch, // catch
    Test, // test
    Import, // import
    Export, // export
    Const, // const
    Static, // static
    Super, // super
    From, // from
    As, // as
    Extern, // extern
    Eof, // EOF
    Error, // Error
    Void, // void
    Docblock, // Docblock
    Pattern, // Pattern
    Pat, // pat
    Fib, // fib
    Ampersand, // async or band
    Resume, // resume
    Resolve, // resolve
    Yield, // yield
};

pub const Keywords = [_]TokenType{
    .True,
    .False,
    .Null,
    .Or,
    .And,
    .Return,
    .If,
    .Else,
    .While,
    .For,
    .ForEach,
    .Switch,
    .Break,
    .Continue,
    .Default,
    .Fun,
    .In,
    .Function,
    .Print,
    .Throw,
    .Catch,
    .Test,
    .Import,
    .Export,
    .Const,
    .Static,
    .Super,
    .From,
    .As,
    .Continue,
    .Extern,
    .Void,
    .Pat,
    .Fib,
    .Resume,
    .Resolve,
    .Yield,
};

// TODO: must be a way to write that more elegantly
pub fn isKeyword(literal: []const u8) ?TokenType {
    if (mem.eql(u8, literal, "ud")) {
        return .Ud;
    }

    if (mem.eql(u8, literal, "void")) {
        return .Void;
    }

    if (mem.eql(u8, literal, "true")) {
        return .True;
    }

    if (mem.eql(u8, literal, "false")) {
        return .False;
    }

    if (mem.eql(u8, literal, "null")) {
        return .Null;
    }

    if (mem.eql(u8, literal, "or")) {
        return .Or;
    }

    if (mem.eql(u8, literal, "and")) {
        return .And;
    }

    if (mem.eql(u8, literal, "as")) {
        return .As;
    }

    if (mem.eql(u8, literal, "return")) {
        return .Return;
    }

    if (mem.eql(u8, literal, "if")) {
        return .If;
    }

    if (mem.eql(u8, literal, "else")) {
        return .Else;
    }

    if (mem.eql(u8, literal, "while")) {
        return .While;
    }

    if (mem.eql(u8, literal, "for")) {
        return .For;
    }

    if (mem.eql(u8, literal, "foreach")) {
        return .ForEach;
    }

    if (mem.eql(u8, literal, "switch")) {
        return .Switch;
    }

    if (mem.eql(u8, literal, "break")) {
        return .Break;
    }

    if (mem.eql(u8, literal, "continue")) {
        return .Continue;
    }

    if (mem.eql(u8, literal, "default")) {
        return .Default;
    }

    if (mem.eql(u8, literal, "const")) {
        return .Const;
    }

    if (mem.eql(u8, literal, "super")) {
        return .Super;
    }

    if (mem.eql(u8, literal, "fun")) {
        return .Fun;
    }

    if (mem.eql(u8, literal, "in")) {
        return .In;
    }

    if (mem.eql(u8, literal, "str")) {
        return .Str;
    }

    if (mem.eql(u8, literal, "num")) {
        return .Num;
    }

    if (mem.eql(u8, literal, "type")) {
        return .Type;
    }

    if (mem.eql(u8, literal, "bool")) {
        return .Bool;
    }

    if (mem.eql(u8, literal, "pat")) {
        return .Pat;
    }

    if (mem.eql(u8, literal, "xor")) {
        return .Xor;
    }

    if (mem.eql(u8, literal, "do")) {
        return .Do;
    }

    if (mem.eql(u8, literal, "until")) {
        return .Until;
    }

    if (mem.eql(u8, literal, "is")) {
        return .Is;
    }

    if (mem.eql(u8, literal, "object")) {
        return .Object;
    }

    if (mem.eql(u8, literal, "static")) {
        return .Static;
    }

    if (mem.eql(u8, literal, "class")) {
        return .Class;
    }

    if (mem.eql(u8, literal, "enum")) {
        return .Enum;
    }

    if (mem.eql(u8, literal, "throw")) {
        return .Throw;
    }

    if (mem.eql(u8, literal, "catch")) {
        return .Catch;
    }

    if (mem.eql(u8, literal, "test")) {
        return .Test;
    }

    if (mem.eql(u8, literal, "Function")) {
        return .Function;
    }

    if (mem.eql(u8, literal, "import")) {
        return .Import;
    }

    if (mem.eql(u8, literal, "export")) {
        return .Export;
    }

    if (mem.eql(u8, literal, "extern")) {
        return .Extern;
    }

    if (mem.eql(u8, literal, "from")) {
        return .From;
    }

    if (mem.eql(u8, literal, "fib")) {
        return .Fib;
    }

    if (mem.eql(u8, literal, "resume")) {
        return .Resume;
    }
    if (mem.eql(u8, literal, "resolve")) {
        return .Resolve;
    }
    if (mem.eql(u8, literal, "yield")) {
        return .Yield;
    }

    return null;
}
