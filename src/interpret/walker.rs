use std::{cell::RefCell, rc::Rc};

use crate::{
    interpret::value::FnValue,
    parse::ast::{
        expression::Expression,
        nodes::{
            BinaryOperator as BinOp, BlockNode, SimpleLiteral, StrPart, UnaryOperator as UnOp,
        },
        statement::Statement,
        Program,
    },
};
use thiserror::Error;

use super::{
    scope::{ScopeChain, ScopeError},
    value::{OperationError, Value},
};

pub struct Walker {
    scope: ScopeChain,
}

impl Walker {
    pub fn new() -> Self {
        Walker {
            scope: ScopeChain::new(),
        }
    }

    pub fn new_with_scope(scope: ScopeChain) -> Self {
        Walker { scope }
    }

    pub fn walk(&mut self, program: &Program) {
        self.scope.nest();
        for statement in program.statements.iter() {
            self.walk_statement(statement).expect("Runtime error.");
        }
    }

    fn walk_statement(&mut self, statement: &Statement) -> Result<Option<Value>, WalkerError> {
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
            // FIXME: Returns are always expected to have a return even though `return;` is valid.
            Statement::Return(node) => {
                // If there's a function running above us it will catch this error.
                return Err(WalkerError::Return(self.walk_expression(node)?));
            }
            Statement::Break(node) => {
                let returned = if let Some(expression) = node {
                    Some(self.walk_expression(expression)?)
                } else {
                    None
                };
                // If there's a loop above us it will catch this error.
                return Err(WalkerError::LoopBreak(returned));
            }
            // Same here.
            Statement::Continue => return Err(WalkerError::LoopContinue),
        };

        Ok(result)
    }

    pub fn walk_expression(&mut self, node: &Expression) -> Result<Value, WalkerError> {
        match node {
            Expression::Binary { left, op, right } => {
                // Assignment
                match op {
                    BinOp::ConstAssign => return self.assing_to_lvalue(left, right, true),
                    BinOp::Assign => return self.assing_to_lvalue(left, right, false),
                    _ => {}
                }

                // Short-circuting operators
                if let BinOp::And | BinOp::Or = op {
                    let left_value = self.walk_expression(left)?;
                    let is_left_true = match left_value {
                        Value::Bool(bool) => bool,
                        _ => return Err(WalkerError::WrongAndOrType),
                    };

                    if let BinOp::And = op {
                        if !is_left_true {
                            return Ok(Value::Bool(false));
                        }
                    } else if is_left_true {
                        return Ok(Value::Bool(true));
                    }

                    return self.walk_expression(right);
                }

                let left = self.walk_expression(left)?;
                let right = self.walk_expression(right)?;

                // Other operators
                match op {
                    BinOp::Plus => left.add(right),
                    BinOp::Minus => left.sub(right),
                    BinOp::Star => left.mul(right),
                    BinOp::Slash => left.div(right),
                    BinOp::Dot => todo!("Structures not implemented yet."),
                    BinOp::Eq => left.eq(right),
                    BinOp::Neq => left.neq(right),
                    BinOp::Gt => left.gt(right),
                    BinOp::Gte => left.gte(right),
                    BinOp::Lt => right.gt(left),
                    BinOp::Lte => right.gte(left),

                    _ => unreachable!(),
                }
                .map_err(WalkerError::OperationError)
            }
            Expression::Unary { op, right } => {
                let value = self.walk_expression(right)?;

                match op {
                    UnOp::Minus => value.neg(),
                    UnOp::Not => value.not(),
                }
                .map_err(WalkerError::OperationError)
            }
            Expression::Call(node) => {
                let called = self.walk_expression(&node.called)?;

                let mut argument_values = Vec::new();
                for argument_node in node.arguments.iter() {
                    argument_values.push(self.walk_expression(argument_node)?);
                }

                called.call(argument_values)
            }
            Expression::ArrayAccess(node) => {
                let array = self.walk_expression(&node.array)?;
                let index = self.walk_expression(&node.index)?;
                array.subscript(index).map_err(WalkerError::OperationError)
            }
            Expression::MemberAccess(_) => todo!("Structures not implemented yet."),
            Expression::Group(node) => self.walk_expression(node),
            Expression::ArrayLiteral(node) => {
                let mut elements = Vec::new();
                for expression in &node.elements {
                    elements.push(self.walk_expression(expression)?);
                }
                Ok(Value::Array(Rc::new(RefCell::new(elements))))
            }
            Expression::SimpleLiteral(token) => {
                let value = match token {
                    SimpleLiteral::Int(int) => Value::Int(*int as i64),
                    SimpleLiteral::Float(float) => Value::Float(*float as f64),
                    SimpleLiteral::Bool(bool) => Value::Bool(*bool),
                };

                Ok(value)
            }
            Expression::Block(block) => self.walk_block(block.as_ref()),
            Expression::StrLiteral(node) => {
                let mut buffer = String::new();

                for part in &node.parts {
                    match part {
                        StrPart::Literal(literal) => buffer.push_str(literal),
                        StrPart::Embed(embed) => {
                            buffer.push_str(&self.walk_expression(embed)?.to_string())
                        }
                    };
                }

                Ok(Value::Str(buffer))
            }
            Expression::FnLiteral(node) => Ok(Value::Fn(Rc::new(RefCell::new(FnValue {
                node: node.as_ref().clone(),
                scope: self.scope.clone(),
            })))),
            Expression::If(if_node) => {
                for conditional in &if_node.conditionals {
                    if let Value::Bool(bool) = self.walk_expression(&conditional.condition)? {
                        if bool {
                            return self.walk_block(&conditional.block);
                        }
                    } else {
                        return Err(WalkerError::WrongIfConditionType);
                    }
                }

                if let Some(else_conditional) = &if_node.else_block {
                    self.walk_block(else_conditional)
                } else {
                    Ok(Value::Void)
                }
            }
            Expression::Loop(loop_node) => {
                if let Some(condition) = &loop_node.condition {
                    loop {
                        if let Value::Bool(should_repeat) = self.walk_expression(condition)? {
                            if should_repeat {
                                match self.walk_block(&loop_node.body) {
                                    Err(WalkerError::LoopBreak(loop_value)) => {
                                        return Ok(loop_value.unwrap_or(Value::Void));
                                    }
                                    // Do nothing for continue and continue looping of course, you dummy.
                                    Err(WalkerError::LoopContinue) => {}
                                    // This is probably an actual error.
                                    Err(x) => return Err(x),
                                    _ => {}
                                }
                            } else {
                                return Ok(Value::Void);
                            }
                        } else {
                            return Err(WalkerError::WrongLoopConditionType);
                        }
                    }
                } else {
                    // Same as above.
                    loop {
                        match self.walk_block(&loop_node.body) {
                            Err(WalkerError::LoopBreak(loop_value)) => {
                                break Ok(loop_value.unwrap_or(Value::Void));
                            }
                            Err(WalkerError::LoopContinue) => {}
                            Err(x) => return Err(x),
                            _ => {}
                        }
                    }
                }
            }
            Expression::Identifier(ident) => {
                self.scope.get_var(ident).map_err(WalkerError::ScopeError)
            }
        }
    }

    fn walk_block(&mut self, block: &BlockNode) -> Result<Value, WalkerError> {
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

    pub fn assing_to_lvalue(
        &mut self,
        lvalue: &Expression,
        rvalue: &Expression,
        is_constant: bool,
    ) -> Result<Value, WalkerError> {
        // Maybe other expressions could also be l-values, but these are fine for now.
        match lvalue {
            Expression::MemberAccess(_) => todo!("Structures not implemented yet."),
            Expression::ArrayAccess(node) => {
                let mut array = self.walk_expression(&node.array)?;
                let index = self.walk_expression(&node.index)?;
                let value = self.walk_expression(rvalue)?;

                array
                    .subscript_assign(index, value)
                    .map_err(WalkerError::OperationError)
            }
            Expression::Identifier(ident) => {
                let value = self.walk_expression(rvalue)?;
                self.scope
                    .set_var(ident, value.clone(), is_constant)
                    .map_err(WalkerError::ScopeError)?;
                return Ok(value);
            }
            _ => Err(WalkerError::NonLValueAssignment),
        }
    }
}

// TODO: Add source maps to the errors.
#[derive(Error, Debug)]
pub enum WalkerError {
    #[error("Loop expressions can only take boolean values as conditions.")]
    WrongLoopConditionType,
    #[error("If and Elif expressions can only take boolean values as conditions.")]
    WrongIfConditionType,
    #[error("&& and || expressions can only take boolean values as their operands.")]
    WrongAndOrType,
    #[error("Can only assign to identifiers and member or array access results.")]
    NonLValueAssignment,
    #[error(transparent)]
    ScopeError(ScopeError),
    #[error(transparent)]
    OperationError(OperationError),
    // These are used for loop control flow and are only errors
    // if continue and break statements are used outside of loops.
    #[error("Continue statements are only valid inside loops.")]
    LoopContinue,
    #[error("Break statements are only valid inside loops.")]
    LoopBreak(Option<Value>),
    // Same as with the loop control errors, but for functions.
    #[error("Return statements are only valid inside functions.")]
    Return(Value),
}
