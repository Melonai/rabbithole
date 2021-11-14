use std::fmt::{self, Display, Formatter};

use crate::parse::ast::nodes::StrPart;

use super::nodes::{
    ArrayAccessNode, ArrayNode, BinaryOperator, BlockNode, CallNode, FnNode, Identifier, IfNode,
    LoopNode, MemberAccessNode, SimpleLiteral, StrNode, UnaryOperator,
};

#[derive(Debug, Clone)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOperator,
        right: Box<Expression>,
    },
    Call(Box<CallNode>),
    ArrayAccess(Box<ArrayAccessNode>),
    MemberAccess(Box<MemberAccessNode>),
    Group(Box<Expression>),
    Block(Box<BlockNode>),
    If(Box<IfNode>),
    Loop(Box<LoopNode>),
    StrLiteral(Box<StrNode>),
    FnLiteral(Box<FnNode>),
    ArrayLiteral(ArrayNode),
    SimpleLiteral(SimpleLiteral),
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
        match self {
            Expression::Binary { left, op, right } => {
                writeln!(f, "{}Binary:", pad)?;
                writeln!(f, "{}- Left:", pad)?;
                left.nested_fmt(f, depth + 1)?;
                writeln!(f, "{}- Operator: {:?}", pad, op)?;
                writeln!(f, "{}- Right:", pad)?;
                right.nested_fmt(f, depth + 1)?;
            }
            Expression::Unary { op, right } => {
                writeln!(f, "{}Unary:", pad)?;
                writeln!(f, "{}- Operator: {:?}", pad, op)?;
                writeln!(f, "{}- Right:", pad)?;
                right.nested_fmt(f, depth + 1)?;
            }
            Expression::Call(node) => {
                writeln!(f, "{}Function Call:", pad)?;
                writeln!(f, "{}- Called:", pad)?;
                node.called.nested_fmt(f, depth + 1)?;
                for (i, e) in node.arguments.iter().enumerate() {
                    writeln!(f, "{}- Argument {}:", pad, i)?;
                    e.nested_fmt(f, depth + 1)?;
                }
            }
            Expression::ArrayAccess(node) => {
                writeln!(f, "{}Array Access:", pad)?;
                writeln!(f, "{}- Array:", pad)?;
                node.array.nested_fmt(f, depth + 1)?;
                writeln!(f, "{}- Index:", pad)?;
                node.index.nested_fmt(f, depth + 1)?;
            }
            Expression::MemberAccess(node) => {
                writeln!(f, "{}Member Access:", pad)?;
                writeln!(f, "{}- Object:", pad)?;
                node.object.nested_fmt(f, depth + 1)?;
                writeln!(f, "{}- Member Name: {}", pad, node.member_name)?;
            }
            Expression::Group(node) => {
                writeln!(f, "{}Group:", pad)?;
                node.nested_fmt(f, depth + 1)?;
            }
            Expression::Block(block) => {
                Self::block_fmt(f, block, depth + 1)?;
            }
            Expression::StrLiteral(node) => {
                writeln!(f, "{}Str:", pad)?;
                for (i, statement) in node.parts.iter().enumerate() {
                    writeln!(f, "{}- {}:", pad, i)?;
                    match statement {
                        StrPart::Literal(literal) => {
                            writeln!(f, "{}{}", "  ".repeat(depth + 1), literal.clone())
                        }
                        StrPart::Embed(block) => block.nested_fmt(f, depth + 1),
                    }?;
                }
            }
            Expression::FnLiteral(node) => {
                write!(f, "{}Fn (", pad)?;

                // Write self receiver
                if node.header.has_self_receiver {
                    write!(f, "self, ")?;
                }

                // Write parameters
                for p in node.header.parameters.iter() {
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
                    node.header.return_type.as_ref().unwrap_or(&"_".into())
                )?;
                Self::block_fmt(f, &node.body, depth + 1)?;
            }
            Expression::ArrayLiteral(node) => {
                writeln!(f, "{}Array Literal:", pad)?;
                for (i, c) in node.elements.iter().enumerate() {
                    writeln!(f, "{}- Element {}:", pad, i)?;
                    c.nested_fmt(f, depth + 1)?;
                }
            }
            Expression::SimpleLiteral(literal) => {
                writeln!(f, "{}Literal: {:?}", pad, literal)?;
            }
            Expression::Identifier(identifier) => {
                writeln!(f, "{}Identifier: {:?}", pad, identifier)?;
            }
            Expression::If(node) => {
                writeln!(f, "{}If:", pad)?;
                for (i, c) in node.conditionals.iter().enumerate() {
                    writeln!(f, "{}- Condition {}:", pad, i)?;
                    c.condition.nested_fmt(f, depth + 1)?;
                    writeln!(f, "{}- Body {}:", pad, i)?;
                    Self::block_fmt(f, &c.block, depth + 1)?;
                }
                if let Some(e) = &node.else_block {
                    writeln!(f, "{}- Else:", pad)?;
                    Self::block_fmt(f, e, depth + 1)?;
                }
            }
            Expression::Loop(node) => {
                writeln!(f, "{}Loop:", pad)?;
                if let Some(loop_condition) = &node.condition {
                    writeln!(f, "{}- Condition:", pad)?;
                    loop_condition.nested_fmt(f, depth + 1)?;
                }

                writeln!(f, "{}- Body:", pad)?;
                Self::block_fmt(f, &node.body, depth + 1)?;
            }
        }

        Ok(())
    }

    fn block_fmt(f: &mut Formatter<'_>, block: &BlockNode, depth: usize) -> fmt::Result {
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
