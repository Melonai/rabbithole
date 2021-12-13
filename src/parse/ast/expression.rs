use std::fmt::{self, Display, Formatter};

use crate::lex::token::Location;

use super::nodes::{
    ArrayAccessExpression, ArrayExpression, BinaryOperatorNode, BlockExpression, CallExpression,
    FnExpression, Identifier, IfExpression, LoopExpression, MemberAccessExpression,
    SimpleLiteralNode, StrExpression, StrPartKind, UnaryOperatorNode,
};

#[derive(Debug, Clone)]
pub struct Expression {
    // TODO: Sometimes this location is duplicated for no reason,
    // i.e. in the UnaryExpression, since the token for the UnaryOperatorNode
    // will be the same as the location for the expression...
    // Can we do something about it or is some duplication fine?
    pub at: Location,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Binary {
        left: Box<Expression>,
        op: BinaryOperatorNode,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOperatorNode,
        right: Box<Expression>,
    },
    Call(Box<CallExpression>),
    ArrayAccess(Box<ArrayAccessExpression>),
    MemberAccess(Box<MemberAccessExpression>),
    Group(Box<Expression>),
    Block(Box<BlockExpression>),
    If(Box<IfExpression>),
    Loop(Box<LoopExpression>),
    StrLiteral(Box<StrExpression>),
    FnLiteral(Box<FnExpression>),
    ArrayLiteral(ArrayExpression),
    SimpleLiteral(SimpleLiteralNode),
    Identifier(Identifier),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.nested_fmt(f, 0)
    }
}

impl Expression {
    pub(crate) fn nested_fmt(&self, f: &mut Formatter<'_>, depth: usize) -> fmt::Result {
        let pad = "  ".repeat(depth);
        match self.kind {
            ExpressionKind::Binary { left, op, right } => {
                writeln!(f, "{}Binary:", pad)?;
                writeln!(f, "{}- Left:", pad)?;
                left.nested_fmt(f, depth + 1)?;
                writeln!(f, "{}- Operator: {:?}", pad, op)?;
                writeln!(f, "{}- Right:", pad)?;
                right.nested_fmt(f, depth + 1)?;
            }
            ExpressionKind::Unary { op, right } => {
                writeln!(f, "{}Unary:", pad)?;
                writeln!(f, "{}- Operator: {:?}", pad, op)?;
                writeln!(f, "{}- Right:", pad)?;
                right.nested_fmt(f, depth + 1)?;
            }
            ExpressionKind::Call(expr) => {
                writeln!(f, "{}Function Call:", pad)?;
                writeln!(f, "{}- Called:", pad)?;
                expr.called.nested_fmt(f, depth + 1)?;
                for (i, e) in expr.arguments.iter().enumerate() {
                    writeln!(f, "{}- Argument {}:", pad, i)?;
                    e.nested_fmt(f, depth + 1)?;
                }
            }
            ExpressionKind::ArrayAccess(expr) => {
                writeln!(f, "{}Array Access:", pad)?;
                writeln!(f, "{}- Array:", pad)?;
                expr.array.nested_fmt(f, depth + 1)?;
                writeln!(f, "{}- Index:", pad)?;
                expr.index.nested_fmt(f, depth + 1)?;
            }
            ExpressionKind::MemberAccess(expr) => {
                writeln!(f, "{}Member Access:", pad)?;
                writeln!(f, "{}- Object:", pad)?;
                expr.object.nested_fmt(f, depth + 1)?;
                writeln!(f, "{}- Member Name: {}", pad, expr.member_name)?;
            }
            ExpressionKind::Group(expr) => {
                writeln!(f, "{}Group:", pad)?;
                expr.nested_fmt(f, depth + 1)?;
            }
            ExpressionKind::Block(expr) => {
                Self::block_fmt(f, &expr, depth + 1)?;
            }
            ExpressionKind::StrLiteral(expr) => {
                writeln!(f, "{}Str:", pad)?;
                for (i, statement) in expr.parts.iter().enumerate() {
                    writeln!(f, "{}- {}:", pad, i)?;
                    match statement.kind {
                        StrPartKind::Literal(literal) => {
                            writeln!(f, "{}{}", "  ".repeat(depth + 1), literal.clone())
                        }
                        StrPartKind::Embed(block) => block.nested_fmt(f, depth + 1),
                    }?;
                }
            }
            ExpressionKind::FnLiteral(expr) => {
                write!(f, "{}Fn (", pad)?;

                // Write self receiver
                if expr.header.has_self_receiver {
                    write!(f, "self, ")?;
                }

                // Write parameters
                for p in expr.header.parameters.iter() {
                    write!(
                        f,
                        "{}: {}, ",
                        p.identifier,
                        p.type_constraint.as_ref().unwrap_or(&"_".into())
                    )?;
                }

                // Write return type
                writeln!(
                    f,
                    ") -> {}:",
                    expr.header.return_type.as_ref().unwrap_or(&"_".into())
                )?;
                Self::block_fmt(f, &expr.body, depth + 1)?;
            }
            ExpressionKind::ArrayLiteral(expr) => {
                writeln!(f, "{}Array Literal:", pad)?;
                for (i, c) in expr.elements.iter().enumerate() {
                    writeln!(f, "{}- Element {}:", pad, i)?;
                    c.nested_fmt(f, depth + 1)?;
                }
            }
            ExpressionKind::SimpleLiteral(literal) => {
                writeln!(f, "{}Literal: {:?}", pad, literal)?;
            }
            ExpressionKind::Identifier(identifier) => {
                writeln!(f, "{}Identifier: {:?}", pad, identifier)?;
            }
            ExpressionKind::If(expr) => {
                writeln!(f, "{}If:", pad)?;
                for (i, c) in expr.conditionals.iter().enumerate() {
                    writeln!(f, "{}- Condition {}:", pad, i)?;
                    c.condition.nested_fmt(f, depth + 1)?;
                    writeln!(f, "{}- Body {}:", pad, i)?;
                    Self::block_fmt(f, &c.block, depth + 1)?;
                }
                if let Some(e) = &expr.else_block {
                    writeln!(f, "{}- Else:", pad)?;
                    Self::block_fmt(f, e, depth + 1)?;
                }
            }
            ExpressionKind::Loop(expr) => {
                writeln!(f, "{}Loop:", pad)?;
                if let Some(loop_condition) = &expr.condition {
                    writeln!(f, "{}- Condition:", pad)?;
                    loop_condition.nested_fmt(f, depth + 1)?;
                }

                writeln!(f, "{}- Body:", pad)?;
                Self::block_fmt(f, &expr.body, depth + 1)?;
            }
        }

        Ok(())
    }

    fn block_fmt(f: &mut Formatter<'_>, block: &BlockExpression, depth: usize) -> fmt::Result {
        let pad = "  ".repeat(depth);
        writeln!(f, "{}Block:", pad)?;
        for (i, statement) in block.statements.iter().enumerate() {
            writeln!(f, "{}- {}:", pad, i)?;
            statement.nested_fmt(f, depth + 1)?;
        }
        if let Some(tail_expression) = &block.tail_expression {
            writeln!(f, "{}- Tail:", pad)?;
            tail_expression.nested_fmt(f, depth + 1)?;
        }

        Ok(())
    }
}
