use std::any::Any;

pub enum TokenType {
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
}

pub const KEYWORDS: [TokenType; 16] = [
    TokenType::True,
    TokenType::False,
    TokenType::Null,
    TokenType::Or,
    TokenType::And,
    TokenType::Return,
    TokenType::If,
    TokenType::Else,
    TokenType::While,
    TokenType::For,
    TokenType::Switch,
    TokenType::Break,
    TokenType::Continue,
    TokenType::Default,
    TokenType::Fun,
    TokenType::In,
];

pub struct Token<'a> {
    pub token: TokenType,
    pub lexeme: &'a [&'a str],
    pub literal: Option<Box<dyn Any>>,
    pub location: (usize, usize),
}
