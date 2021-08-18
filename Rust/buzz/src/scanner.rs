use std::any::Any;

use crate::token::{Token, TokenType};
use unicode_segmentation::UnicodeSegmentation;

struct Location {
    start: usize,
    line: usize,
    column: usize,
    offset: usize,
}

pub struct Scanner<'a> {
    source: Vec<&'a str>,
    tokens: Vec<Token<'a>>,
    current: Location,
}

impl<'a> Scanner<'a> {
    fn scan(&'a mut self) {
        while !self.is_eof() {
            self.current.start = self.current.offset;
            self.scan_token();
        }

        self.tokens.push(Token {
            token: TokenType::Eof,
            lexeme: &self.source[self.source.len()..self.source.len() - 1],
            literal: Some(Box::new("\0")),
            location: (self.current.line, self.current.column),
        })
    }

    fn scan_token(&'a mut self) {
        let character = self.advance();
        match character {
            "|" => {
                while !self.peek().eq("\n") && !self.is_eof() {
                    self.advance();
                }
            }
            "[" => self.add_token(TokenType::LeftBracket, None),
            "]" => self.add_token(TokenType::RightBracket, None),
            "(" => self.add_token(TokenType::LeftParen, None),
            ")" => self.add_token(TokenType::LeftParen, None),
            "{" => self.add_token(TokenType::LeftBrace, None),
            "}" => self.add_token(TokenType::RightBrace, None),
            "," => self.add_token(TokenType::Comma, None),
            ";" => self.add_token(TokenType::Semicolon, None),
            ":" => self.add_token(TokenType::Colon, None),
            "%" => self.add_token(TokenType::Percent, None),
            ">" => {
                if self.match_char("=") {
                    self.add_token(TokenType::GreaterEqual, None);
                } else {
                    self.add_token(TokenType::Greater, None);
                }
            }
            "<" => {
                if self.match_char("=") {
                    self.add_token(TokenType::LessEqual, None);
                } else {
                    self.add_token(TokenType::Less, None);
                }
            }
            "+" => {
                if self.match_char("+") {
                    self.add_token(TokenType::Increment, None);
                } else if self.match_char("=") {
                    self.add_token(TokenType::PlusEqual, None);
                } else {
                    self.add_token(TokenType::Plus, None);
                }
            }
            "-" => {
                if self.match_char(">") {
                    self.add_token(TokenType::Arrow, None);
                } else if self.match_char("-") {
                    self.add_token(TokenType::Decrement, None);
                } else if self.match_char("=") {
                    self.add_token(TokenType::MinusEqual, None);
                } else {
                    self.add_token(TokenType::Minus, None);
                }
            }
            "*" => {
                if self.match_char("=") {
                    self.add_token(TokenType::StarEqual, None);
                } else {
                    self.add_token(TokenType::Star, None);
                }
            }
            "*" => {
                if self.match_char("=") {
                    self.add_token(TokenType::StarEqual, None);
                } else {
                    self.add_token(TokenType::Star, None);
                }
            }
            "/" => {
                if self.match_char("=") {
                    self.add_token(TokenType::SlashEqual, None);
                } else {
                    self.add_token(TokenType::Slash, None);
                }
            }
            "?" => {
                if self.match_char("?") {
                    self.add_token(TokenType::QuestionQuestion, None);
                } else {
                    self.add_token(TokenType::Question, None);
                }
            }
            "!" => {
                if self.match_char("=") {
                    self.add_token(TokenType::BangEqual, None);
                } else {
                    self.add_token(TokenType::Bang, None);
                }
            }
            "=" => {
                if self.match_char("=") {
                    self.add_token(TokenType::EqualEqual, None);
                } else {
                    self.add_token(TokenType::Equal, None);
                }
            }
            "\n" => {
                self.current.line += 1;
                self.current.column = 0;
            }
            "\"" => self.string(),
            _ => {
                eprintln!(
                    "Unexpected character at {}:{}",
                    self.current.line, self.current.column
                );
            }
        }
    }

    fn string(&'a mut self) {
        while !self.peek().eq("\"") && !self.is_eof() {
            if self.peek().eq("\n") {
                eprintln!(
                    "Unterminated string at {}:{}",
                    self.current.line, self.current.column
                );
            }

            self.advance();
        }

        if self.is_eof() {
            eprintln!(
                "Unterminated string at {}:{}",
                self.current.line, self.current.column
            );
        } else {
            // Closing "
            self.advance();
        }

        // self.add_token(
        //     TokenType::String,
        //     Some(Box::new(
        //         &self.source[(self.current.start + 1)..(self.current.offset - 1)],
        //     )),
        // )
    }

    fn is_eof(&self) -> bool {
        return self.current.offset > self.source.len();
    }

    fn peek(&self) -> &str {
        if self.is_eof() {
            return "\0";
        }

        return self.char_at(self.current.offset);
    }

    fn peek_next(&self) -> &str {
        if self.current.offset + 1 > self.source.len() {
            return "\0";
        }

        return self.char_at(self.current.offset);
    }

    fn advance(&mut self) -> &str {
        self.current.offset += 1;
        self.current.column += 1;

        return self.char_at(self.current.offset - 1);
    }

    fn char_at(&self, index: usize) -> &str {
        return self.source[index];
    }

    fn match_char(&mut self, expected: &str) -> bool {
        if self.is_eof() {
            return false;
        }

        if !self.char_at(self.current.offset).eq(expected) {
            return false;
        }

        self.current.offset += 1;
        return true;
    }

    fn add_token(&'a mut self, token_type: TokenType, literal: Option<Box<dyn Any>>) {
        let token = Token {
            token: token_type,
            lexeme: &self.source[self.current.start..(self.current.start + self.current.offset)],
            literal,
            location: (self.current.line, self.current.column),
        };

        self.tokens.push(token);
    }
}

pub fn scanner_from_string<'a>(source: &'a String) -> Scanner<'a> {
    return Scanner {
        source: source.graphemes(true).collect::<Vec<&str>>(),
        tokens: vec![],
        current: Location {
            start: 0,
            line: 0,
            column: 0,
            offset: 0,
        },
    };
}
