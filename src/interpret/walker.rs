use std::{cell::RefCell, rc::Rc};

use crate::parse::ast::{
    expression::Expression,
    nodes::{BinaryOperator as BinOp, Literal, UnaryOperator as UnOp},
    statement::Statement,
    Program,
};
use anyhow::{anyhow, Result};

use super::{scope::Scope, value::Value};

// No state for now.
pub struct Walker {
    scope: Scope,
}

impl Walker {
    pub fn new() -> Self {
        Walker {
            scope: Scope::new(),
        }
    }

    pub fn walk(&mut self, program: &Program) {
        self.scope.nest();
        for statement in program.statements.iter() {
            self.walk_statement(statement).expect("Runtime error.");
        }
    }

    fn walk_statement(&mut self, statement: &Statement) -> Result<Option<Value>> {
        let result = match statement {
            Statement::Expression(node) => {
                self.walk_expression(node)?;
                None
            }
            Statement::Print(node) => {
                let result = self.walk_expression(node)?;
                println!("{:?}", result);
                None
            }
            Statement::Return(node) => Some(self.walk_expression(node)?),
        };

        Ok(result)
    }

    pub fn walk_expression(&mut self, node: &Expression) -> Result<Value> {
        match node {
            Expression::Binary { left, op, right } => {
                let new_value = match op {
                    BinOp::Plus => self
                        .walk_expression(left)?
                        .add(self.walk_expression(right)?),
                    BinOp::Minus => self
                        .walk_expression(left)?
                        .sub(self.walk_expression(right)?),
                    BinOp::Star => self
                        .walk_expression(left)?
                        .mul(self.walk_expression(right)?),
                    BinOp::Slash => self
                        .walk_expression(left)?
                        .div(self.walk_expression(right)?),
                    // No difference between assignments for now.
                    BinOp::Assign | BinOp::ConstAssign => {
                        let identifier = match left.as_ref() {
                            Expression::Identifier(i) => i,
                            _ => todo!("Lvalues can only be identifiers."),
                        };

                        let value = self.walk_expression(right)?;
                        self.scope.set_var(identifier, value.clone());
                        Ok(value)
                    }
                    // Boolean logic comes later.
                    BinOp::Eq | BinOp::Neq | BinOp::Gt | BinOp::Gte | BinOp::Lt | BinOp::Lte => {
                        todo!()
                    }
                    // No structure access yet.
                    BinOp::Dot => todo!(),
                }?;

                Ok(new_value)
            }
            Expression::Unary { op, right } => {
                let value = self.walk_expression(right)?;

                let new_value = match op {
                    UnOp::Plus => value,
                    UnOp::Minus => todo!(),
                    UnOp::Not => todo!("Implement boolean arithmetic."),
                };

                Ok(new_value)
            }
            Expression::Group(node) => self.walk_expression(node),
            Expression::Literal(token) => {
                let value = match token {
                    Literal::Int(int) => Value::Int(*int as i64),
                    Literal::Float(float) => Value::Float(*float as f64),
                    Literal::Str(string) => Value::Str(string.clone()),
                };

                Ok(value)
            }
            Expression::Block(block) => {
                self.scope.nest();

                for statement in block.statements.iter() {
                    self.walk_statement(statement)?;
                }

                let result = if let Some(tail_expression) = &block.tail_expression {
                    Ok(self.walk_expression(tail_expression)?)
                } else {
                    Ok(Value::Void)
                };

                self.scope.unnest();

                result
            }
            Expression::Fn(fn_node) => {
                let node = fn_node.as_ref().clone();
                Ok(Value::Fn(RefCell::new(Rc::new(node))))
            }
            Expression::If(_) => todo!(),
            Expression::Identifier(ident) => {
                if let Some(value) = self.scope.get_var(ident) {
                    Ok(value)
                } else {
                    Err(anyhow!("Unknown identifier: {}.", ident))
                }
            }
        }
    }
}
