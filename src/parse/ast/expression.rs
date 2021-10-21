use std::fmt::{self, Display, Formatter};

use super::nodes::{BinaryOperator, BlockNode, FnNode, Identifier, IfNode, Literal, UnaryOperator};

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
    Group(Box<Expression>),
    Block(Box<BlockNode>),
    Fn(Box<FnNode>),
    If(Box<IfNode>),
    Literal(Literal),
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
            Expression::Group(node) => {
                writeln!(f, "{}Group:", pad)?;
                node.nested_fmt(f, depth + 1)?;
            }
            Expression::Block(block) => {
                Self::block_fmt(f, block, depth + 1)?;
            }
            Expression::Fn(node) => {
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
            Expression::Literal(literal) => {
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
