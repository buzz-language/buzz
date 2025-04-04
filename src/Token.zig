const std = @import("std");
const mem = std.mem;
const v = @import("value.zig");

const Self = @This();

pub const LiteralTag = enum {
    String,
    Double,
    Integer,
    None,
};

pub const Literal = union(LiteralTag) {
    String: []const u8,
    Double: v.Double,
    Integer: v.Integer,
    None: void,
};

pub const NoLiteral = Literal{
    .None = {},
};

tag: Type,
/// When true, token inserted by the parser
utility_token: bool = false,
source: []const u8,
script_name: []const u8,
lexeme: []const u8,
literal: Literal = NoLiteral,
line: usize,
column: usize,
offset: usize = 0,

pub fn eql(self: Self, other: Self) bool {
    return self.tag == other.tag and
        self.line == other.line and
        self.column == other.column and
        self.offset == other.offset and
        std.mem.eql(u8, self.source, other.source) and
        std.mem.eql(u8, self.script_name, other.script_name);
}

pub fn identifier(name: []const u8) Self {
    return .{
        .tag = .Identifier,
        .lexeme = name,
        .line = 0,
        .column = 0,
        .source = "",
        .script_name = "",
        .literal = .{
            .String = name,
        },
    };
}

pub fn clone(self: Self) Self {
    return .{
        .tag = self.tag,
        .lexeme = self.lexeme,
        .source = self.source,
        .script_name = self.script_name,
        .literal = self.literal,
        .line = self.line,
        .column = self.column,
        .offset = self.offset,
    };
}

// Return `n` lines around the token line in its source
pub fn getLines(self: Self, allocator: mem.Allocator, before: usize, after: usize) ![][]const u8 {
    var lines = std.ArrayList([]const u8).init(allocator);
    const before_index = if (self.line > 0) self.line - @min(before, self.line) else self.line;
    const after_index = if (self.line > 0) self.line + after else self.line;

    var it = std.mem.splitScalar(u8, self.source, '\n');
    var current: usize = 0;
    while (it.next()) |line| : (current += 1) {
        if (current >= before_index and current <= after_index) {
            try lines.append(line);
        }

        if (current > after_index) {
            return try lines.toOwnedSlice();
        }
    }

    return try lines.toOwnedSlice();
}

// WARNING: don't reorder without reordering `rules` in parser.zig
pub const Type = enum {
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
    AntiSlash, // \
    Percent, // %
    Question, // ?
    Bang, // !
    Colon, // :
    DoubleColon, // ::
    Equal, // =
    EqualEqual, // ==
    BangEqual, // !=
    BangGreater, // !>
    GreaterEqual, // >=
    LessEqual, // <=
    QuestionQuestion, // ??
    Arrow, // ->
    DoubleArrow, // =>
    True, // true
    False, // false
    Null, // null
    Str, // str
    Ud, // ud
    Int, // int
    Double, // double
    Type, // type
    Bool, // bool
    Function, // Function

    ShiftRight, // >>
    ShiftLeft, // <<
    Xor, // ^
    Bor, // |
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
    Break, // break
    Continue, // continue
    In, // in
    Is, // is
    IntegerValue, // 123
    DoubleValue, // 123.2
    String, // "hello"
    Identifier, // anIdentifier
    Fun, // fun
    Object, // object
    Obj, // obj
    Protocol, // protocol
    Enum, // enum
    Throw, // throw
    Try, // try
    Catch, // catch
    Test, // test
    Import, // import
    Export, // export
    Final, // final
    Static, // static
    From, // from
    As, // as
    AsQuestion, // as?
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
    Spread, // ..
    Any, // any
    Zdef, // zdef
    TypeOf, // typeof
    Var, // var
    Out, // out
    Namespace, // namespace
    Range, // range
    Mut, // mut
    PlusEqual, // +=
    MinusEqual, // -=
    StarEqual, // *=
    SlashEqual, // /=
    ShiftRightEqual, // >>=
    ShiftLeftEqual, // <<=
    XorEqual, // ^=
    BorEqual, // |=
    BnotEqual, // ~=
    AmpersandEqual, // &=
    PercentEqual, // %=
};

// FIXME if case had the same name as the actual token we could simply use @tagName
pub const keywords = std.StaticStringMap(Type).initComptime(
    .{
        .{ "and", .And },
        .{ "any", .Any },
        .{ "as", .As },
        .{ "bool", .Bool },
        .{ "break", .Break },
        .{ "catch", .Catch },
        .{ "final", .Final },
        .{ "continue", .Continue },
        .{ "do", .Do },
        .{ "else", .Else },
        .{ "enum", .Enum },
        .{ "export", .Export },
        .{ "extern", .Extern },
        .{ "false", .False },
        .{ "fib", .Fib },
        .{ "double", .Double },
        .{ "for", .For },
        .{ "foreach", .ForEach },
        .{ "from", .From },
        .{ "fun", .Fun },
        .{ "Function", .Function },
        .{ "if", .If },
        .{ "import", .Import },
        .{ "in", .In },
        .{ "int", .Int },
        .{ "is", .Is },
        .{ "namespace", .Namespace },
        .{ "mut", .Mut },
        .{ "null", .Null },
        .{ "obj", .Obj },
        .{ "object", .Object },
        .{ "or", .Or },
        .{ "out", .Out },
        .{ "pat", .Pat },
        .{ "protocol", .Protocol },
        .{ "rg", .Range },
        .{ "resolve", .Resolve },
        .{ "resume", .Resume },
        .{ "return", .Return },
        .{ "static", .Static },
        .{ "str", .Str },
        .{ "test", .Test },
        .{ "throw", .Throw },
        .{ "true", .True },
        .{ "try", .Try },
        .{ "type", .Type },
        .{ "typeof", .TypeOf },
        .{ "ud", .Ud },
        .{ "until", .Until },
        .{ "var", .Var },
        .{ "void", .Void },
        .{ "while", .While },
        .{ "yield", .Yield },
        .{ "zdef", .Zdef },
    },
);
