use std::{cell::RefCell, rc::Rc};

use crate::parse::ast::{
    expression::Expression,
    nodes::{BinaryOperator as BinOp, BlockNode, Literal, UnaryOperator as UnOp},
    statement::Statement,
    Program,
};
use anyhow::{anyhow, Result};

use super::{scope::Scope, value::Value};

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
                println!("{}", result);
                None
            }
            Statement::Return(node) => Some(self.walk_expression(node)?),
        };

        Ok(result)
    }

    pub fn walk_expression(&mut self, node: &Expression) -> Result<Value> {
        match node {
            Expression::Binary { left, op, right } => {
                // Assignment
                // No difference between assignments for now.
                if let BinOp::ConstAssign | BinOp::Assign = op {
                    let identifier = match left.as_ref() {
                        Expression::Identifier(i) => i,
                        _ => todo!("Lvalues can only be identifiers."),
                    };

                    let value = self.walk_expression(right)?;
                    self.scope.set_var(identifier, value.clone());
                    return Ok(value);
                }

                let left = self.walk_expression(left)?;
                let right = self.walk_expression(right)?;

                // Other operators
                let new_value = match op {
                    BinOp::Plus => left.add(right),
                    BinOp::Minus => left.sub(right),
                    BinOp::Star => left.mul(right),
                    BinOp::Slash => left.div(right),
                    BinOp::Eq => left.eq(right),
                    BinOp::Neq => left.neq(right),
                    BinOp::Gt => left.gt(right),
                    BinOp::Gte => left.gte(right),
                    BinOp::Lt => right.gt(left),
                    BinOp::Lte => right.gte(left),
                    // No structure access yet.
                    BinOp::Dot => todo!(),
                    _ => unreachable!(),
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
                    Literal::Bool(bool) => Value::Bool(*bool),
                };

                Ok(value)
            }
            Expression::Block(block) => self.walk_block(block.as_ref()),
            Expression::Fn(fn_node) => {
                let node = fn_node.as_ref().clone();
                Ok(Value::Fn(RefCell::new(Rc::new(node))))
            }
            Expression::If(if_node) => {
                for conditional in &if_node.conditionals {
                    if let Value::Bool(bool) = self.walk_expression(&conditional.condition)? {
                        if bool {
                            return self.walk_block(&conditional.block);
                        }
                    } else {
                        return Err(anyhow!(
                            "If and Elif expressions can only take boolean values as conditions."
                        ));
                    }
                }

                if let Some(else_conditional) = &if_node.else_block {
                    self.walk_block(else_conditional)
                } else {
                    Ok(Value::Void)
                }
            }
            Expression::Identifier(ident) => {
                if let Some(value) = self.scope.get_var(ident) {
                    Ok(value)
                } else {
                    Err(anyhow!("Unknown identifier: {}.", ident))
                }
            }
        }
    }

    fn walk_block(&mut self, block: &BlockNode) -> Result<Value> {
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
}
