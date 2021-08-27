const std = @import("std");
const mem = std.mem;

pub const TokenType = enum {
    Pipe,             // |
    LeftBracket,      // [
    RightBracket,     // ]
    LeftParen,        // (
    RightParen,       // )
    LeftBrace,        // {
    RightBrace,       // }
    Dot,              // .
    Comma,            // ,
    Semicolon,        // ;
    Greater,          // >
    Less,             // <
    Plus,             // +
    Minus,            // -
    Star,             // *
    Slash,            // /
    Percent,          // %
    Question,         // ?
    Bang,             // !
    Colon,            // :
    Equal,            // =

    EqualEqual,       // ==
    BangEqual,        // !=
    GreaterEqual,     // >=
    LessEqual,        // <=
    QuestionQuestion, // ??
    PlusEqual,        // +=
    MinusEqual,       // -=
    StarEqual,        // *=
    SlashEqual,       // /=
    Increment,        // ++
    Decrement,        // --
    
    Arrow,            // ->
    
    True,             // true
    False,            // false
    Null,             // null

    Str,              // str
    Num,              // num
    Byte,             // byte
    Type,             // type
    Bool,             // bool
    Function,         // Function

    ShiftRight,       // >>
    ShiftLeft,        // <<
    Xor,              // xor
    Or,               // or
    And,              // and

    Return,           // return
    If,               // if
    Else,             // else
    Do,               // do
    Until,            // until
    While,            // while
    For,              // for
    Switch,           // switch
    Break,            // break
    Default,          // default
    In,               // in
    Is,               // is
    Number,           // 123
    String,           // "hello"
    Identifier,       // anIdentifier

    Fun,              // fun
    Object,           // object
    Class,            // class
    Enum,             // enum

    Eof,              // EOF
    Error,            // Error
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
    .Switch,
    .Break,
    .Continue,
    .Default,
    .Fun,
    .In,
    .Function,
};

// TODO: must be a way to write that more elegantly
pub fn isKeyword(literal: []u8) ?TokenType {
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

    if (mem.eql(u8, literal, "switch")) {
        return .Switch;
    }

    if (mem.eql(u8, literal, "break")) {
        return .Break;
    }

    if (mem.eql(u8, literal, "default")) {
        return .Default;
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

    if (mem.eql(u8, literal, "byte")) {
        return .Byte;
    }
    
    if (mem.eql(u8, literal, "type")) {
        return .Type;
    }

    if (mem.eql(u8, literal, "bool")) {
        return .Bool;
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

    if (mem.eql(u8, literal, "class")) {
        return .Class;
    }

    if (mem.eql(u8, literal, "enum")) {
        return .Enum;
    }

    return null;
}

pub const Token = struct {
    token_type: TokenType,
    lexeme: []u8,
    // Literal is either a string or a number
    literal_string: ?[]u8 = null,
    literal_number: ?f64 = null,
    literal_byte: ?u8 = null,
    line: usize,
    column: usize,
};