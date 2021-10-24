use std::{iter::Peekable, str::Chars};

use super::token::{Location, Token, TokenVariant};

pub struct Lexer<'source> {
    location: Location,
    chars: Peekable<Chars<'source>>,
    done: bool,
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        use super::token::TokenVariant::*;

        if self.done {
            return None;
        }

        self.skip_non_code();

        if self.peek().is_none() {
            self.done = true;
            return Some(Token {
                location: self.location,
                variant: Eof,
            });
        }

        let c = self.peek()?;

        let token = if c.is_numeric() {
            self.number()
        } else if c.is_alphabetic() || c == '_' {
            self.identifier()
        } else if c == '"' {
            self.str()
        } else {
            let location = self.location;

            // Fixed length tokens
            let variant = match self.advance().unwrap() {
                '+' => OpPlus,
                '-' => {
                    if self.advance_if('>') {
                        Arrow
                    } else {
                        OpMinus
                    }
                }
                '*' => OpStar,
                '/' => OpSlash,
                '=' => {
                    if self.advance_if('=') {
                        OpEq
                    } else {
                        Assign
                    }
                }
                '!' => {
                    if self.advance_if('=') {
                        OpNeq
                    } else {
                        OpNot
                    }
                }
                '<' => {
                    if self.advance_if('=') {
                        OpLte
                    } else {
                        OpLt
                    }
                }
                '>' => {
                    if self.advance_if('=') {
                        OpGte
                    } else {
                        OpGt
                    }
                }
                '(' => GroupOpen,
                ')' => GroupClose,
                '{' => BlockOpen,
                '}' => BlockClose,
                '[' => ArrayOpen,
                ']' => ArrayClose,
                '.' => Dot,
                ',' => Comma,
                ':' => {
                    if self.advance_if('=') {
                        ConstAssign
                    } else {
                        Colon
                    }
                }
                ';' => SemiColon,
                _ => Unknown(c),
            };

            Token { location, variant }
        };

        Some(token)
    }
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Self {
        Lexer {
            location: Location { col: 0, row: 0 },
            chars: source.chars().peekable(),
            done: false,
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn advance(&mut self) -> Option<char> {
        let next = self.chars.next();
        if let Some(c) = next {
            if c == '\n' {
                self.location.row += 1;
                self.location.col = 0;
            } else {
                self.location.col += 1;
            }
        }

        next
    }

    fn advance_if(&mut self, c: char) -> bool {
        self.chars.next_if_eq(&c).is_some()
    }

    fn skip_non_code(&mut self) {
        let mut is_in_comment = false;
        while let Some(c) = self.peek() {
            if is_in_comment {
                if c == '\n' {
                    is_in_comment = false;
                }
            } else if c == '#' {
                is_in_comment = true;
            } else if !c.is_whitespace() && c != '\n' {
                break;
            }
            self.advance();
        }
    }

    fn number(&mut self) -> Token {
        let location = self.location;

        let mut is_integer = true;
        let mut buffer = String::new();

        while self.peek().map_or(false, |c| c.is_numeric() || c == '.') {
            let c = self.advance().unwrap();
            if c == '.' {
                is_integer = false;
            }
            buffer.push(c);
        }

        let variant = if is_integer {
            let int = buffer.parse().expect("Failed lexing integer token.");
            TokenVariant::Int(int)
        } else {
            let float = buffer.parse().expect("Failed lexing float token.");
            TokenVariant::Float(float)
        };

        Token { location, variant }
    }

    fn identifier(&mut self) -> Token {
        let location = self.location;

        let mut buffer = String::new();

        while self.peek().map_or(false, |c| c.is_alphabetic() || c == '_') {
            let c = self.advance().unwrap();
            buffer.push(c);
        }

        let variant = match buffer.as_str() {
            "fn" => TokenVariant::KeywordFn,
            "if" => TokenVariant::KeywordIf,
            "elif" => TokenVariant::KeywordElif,
            "else" => TokenVariant::KeywordElse,
            "loop" => TokenVariant::KeywordLoop,
            "type" => TokenVariant::KeywordType,
            "form" => TokenVariant::KeywordForm,
            "self" => TokenVariant::KeywordSelf,
            "true" => TokenVariant::KeywordTrue,
            "false" => TokenVariant::KeywordFalse,
            "return" => TokenVariant::KeywordReturn,
            "break" => TokenVariant::KeywordBreak,
            "continue" => TokenVariant::KeywordContinue,
            "print" => TokenVariant::KeywordPrint,
            _ => TokenVariant::Ident(buffer),
        };

        Token { location, variant }
    }

    fn str(&mut self) -> Token {
        let location = self.location;

        // Remove first "
        self.advance().unwrap();

        let mut buffer = String::new();
        loop {
            let c = self.advance().expect("Expected Str literal to be closed");
            if c == '"' {
                break;
            }
            buffer.push(c);
        }

        Token {
            location,
            variant: TokenVariant::Str(buffer),
        }
    }
}
