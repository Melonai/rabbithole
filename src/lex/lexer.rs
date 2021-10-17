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
        if self.done {
            return None;
        }

        if let None = self.chars.peek() {
            self.done = true;
            return Some(Token {
                location: self.location,
                variant: TokenVariant::Eof,
            });
        }

        self.skip_whitespace();

        let c = *self.chars.peek()?;

        let token = if c.is_numeric() {
            self.number()
        } else if c == '+' {
            self.char_token(TokenVariant::OpPlus)
        } else if c == '-' {
            self.char_token(TokenVariant::OpMinus)
        } else if c == '*' {
            self.char_token(TokenVariant::OpStar)
        } else if c == '/' {
            self.char_token(TokenVariant::OpSlash)
        } else if c == '!' {
            self.char_token(TokenVariant::OpNot)
        } else if c == '(' {
            self.char_token(TokenVariant::GroupOpen)
        } else if c == ')' {
            self.char_token(TokenVariant::GroupClose)
        } else {
            self.char_token(TokenVariant::Unknown(c))
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

    fn advance(&mut self) -> Option<char> {
        let next = self.chars.next();
        if let Some(c) = next {
            if c == '\n' {
                self.location.row += 1;
                self.location.col = 0;
            } else {
                self.location.row += 1;
            }
        }

        next
    }

    fn skip_whitespace(&mut self) {
        while self
            .chars
            .peek()
            .map_or(false, |x| x.is_whitespace() && *x != '\n')
        {
            self.advance();
        }
    }

    fn char_token(&mut self, variant: TokenVariant) -> Token {
        let token = Token {
            location: self.location,
            variant,
        };
        self.advance();
        token
    }

    fn number(&mut self) -> Token {
        let location = self.location;

        let mut is_integer = true;
        let mut buffer = String::new();

        while self
            .chars
            .peek()
            .map_or(false, |&c| c.is_numeric() || c == '.')
        {
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
}
