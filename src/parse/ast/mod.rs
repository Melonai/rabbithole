use std::fmt::Display;

use self::statement::Statement;

pub mod expression;
pub mod statement;
pub mod nodes;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in self.statements.iter() {
            writeln!(f, "{}", statement)?;
        }
        Ok(())
    }
}
