pub mod ast;

use crate::parse::ast::Expression;
use std::iter::Peekable;

use crate::lex::token::Token;

pub fn parse(tokens: Peekable<impl Iterator<Item = Token>>) -> Expression {
    todo!()
}
