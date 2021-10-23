use std::fmt::Display;

use super::expression::Expression;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Break(Option<Expression>),
    Continue,
    Return(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.nested_fmt(f, 0)
    }
}

impl Statement {
    pub(crate) fn nested_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
    ) -> std::fmt::Result {
        let pad = "  ".repeat(depth);
        match self {
            Statement::Expression(expression) => expression.nested_fmt(f, depth)?,
            Statement::Print(expression) => {
                writeln!(f, "{}Print:", pad)?;
                expression.nested_fmt(f, depth + 1)?;
            }
            Statement::Return(expression) => {
                writeln!(f, "{}Return:", pad)?;
                expression.nested_fmt(f, depth + 1)?;
            }
            Statement::Break(expression) => {
                if let Some(returned_on_break) = expression {
                    writeln!(f, "{}Break:", pad)?;
                    returned_on_break.nested_fmt(f, depth + 1)?;
                } else {
                    writeln!(f, "{}Break", pad)?;
                }
            }
            Statement::Continue => {
                writeln!(f, "{}Continue", pad)?;
            }
        }

        Ok(())
    }
}
