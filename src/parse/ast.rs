use std::fmt::Display;

use crate::lex::token::Token;

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        op: Token,
        right: Box<Expression>,
    },
    Unary {
        op: Token,
        right: Box<Expression>,
    },
    Group(Box<Expression>),
    Literal(Token),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.nested_fmt(f, 0)
    }
}

impl Expression {
    fn nested_fmt(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        let pad = "  ".repeat(depth);
        match self {
            Expression::Binary { left, op, right } => {
                writeln!(f, "{}Binary:", pad)?;
                writeln!(f, "{}- Left:", pad)?;
                left.nested_fmt(f, depth + 1)?;
                writeln!(f, "{}- Operator: {:?}", pad, op.variant)?;
                writeln!(f, "{}- Right:", pad)?;
                right.nested_fmt(f, depth + 1)?;
            }
            Expression::Unary { op, right } => {
                writeln!(f, "{}Unary:", pad)?;
                writeln!(f, "{}- Operator: {:?}", pad, op.variant)?;
                writeln!(f, "{}- Right:", pad)?;
                right.nested_fmt(f, depth + 1)?;
            }
            Expression::Group(node) => {
                writeln!(f, "{}Group:", pad)?;
                node.nested_fmt(f, depth + 1)?;
            }
            Expression::Literal(token) => {
                writeln!(f, "{}Literal: {:?}", pad, token.variant)?;
            }
        }

        Ok(())
    }
}
