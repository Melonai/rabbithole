use std::fmt::{self, Display, Formatter};

use super::value::{
    BinaryOperator, Block, ConditionalBlock, FnHeader, Identifier, Literal, UnaryOperator,
};

#[derive(Debug)]
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
    Block(Box<Block>),
    Fn {
        header: FnHeader,
        body: Box<Block>,
    },
    If {
        conditionals: Vec<ConditionalBlock>,
        else_block: Option<Box<Block>>,
    },
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
            Expression::Fn { header, body } => {
                write!(f, "{}Fn (", pad)?;

                // Write self receiver
                if header.has_self_receiver {
                    write!(f, "self, ")?;
                }

                // Write parameters
                for p in header.parameters.iter() {
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
                    header.return_type.as_ref().unwrap_or(&"_".into())
                )?;
                Self::block_fmt(f, body, depth + 1)?;
            }
            Expression::Literal(literal) => {
                writeln!(f, "{}Literal: {:?}", pad, literal)?;
            }
            Expression::Identifier(identifier) => {
                writeln!(f, "{}Identifier: {:?}", pad, identifier)?;
            }
            Expression::If { conditionals, else_block } => {
                writeln!(f, "{}If:", pad)?;
                for (i, c) in conditionals.iter().enumerate() {
                    writeln!(f, "{}- Condition {}:", pad, i)?;
                    c.condition.nested_fmt(f, depth + 1)?;
                    writeln!(f, "{}- Body {}:", pad, i)?;
                    Self::block_fmt(f, &c.block, depth + 1)?;
                }
                if let Some(e) = else_block {
                    writeln!(f, "{}- Else:", pad)?;
                    Self::block_fmt(f, e, depth + 1)?;
                }
            },
        }

        Ok(())
    }

    fn block_fmt(f: &mut Formatter<'_>, block: &Block, depth: usize) -> fmt::Result {
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
