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
    Or,               // or
    And,              // and
    Return,           // return
    If,               // if
    Else,             // else
    While,            // while
    For,              // for
    Switch,           // switch
    Break,            // break
    Continue,         // continue
    Default,          // default
    Fun,              // fun
    In,               // in
    Number,           // 123
    String,           // "hello"
    Identifier,       // anIdentifier
    Eof,              // EOF
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
};

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

    if (mem.eql(u8, literal, "continue")) {
        return .Continue;
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
    

    return null;
}

pub const Token = struct {
    token_type: TokenType,
    lexeme: []u8,
    // Literal is either a string or a number
    literal_string: ?[]u8,
    literal_number: ?f64,
    line: usize,
    column: usize,
};