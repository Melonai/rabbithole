use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    error::{ErrorLocation, RHError, RHErrorKind},
    interpret::{
        operator::ValueOperator,
        value::{FnValue, ValueKind},
    },
    lex::token::Location,
    parse::ast::{
        expression::{Expression, ExpressionKind},
        nodes::{
            BinaryOperatorKind as BinOp, BlockExpression, SimpleLiteralKind, StrPartKind,
            UnaryOperatorKind as UnOp,
        },
        statement::{Statement, StatementKind},
        Program,
    },
    types::{bag::TypeBag, TypeKind},
};
use thiserror::Error;

use super::{
    operator::OperationError,
    scope::{ScopeChain, ScopeError},
    value::Value,
};

pub struct Walker {
    types: TypeBag,
    scope: ScopeChain,
}

impl Walker {
    // Should preferably be called only once.
    pub fn root() -> Self {
        Walker {
            scope: ScopeChain::new(),
            types: TypeBag::new(),
        }
    }

    pub fn new(scope: ScopeChain, types: TypeBag) -> Self {
        Walker { scope, types }
    }

    pub fn walk(&mut self, program: &Program) -> Result<(), RHError> {
        self.scope.nest();
        for statement in program.statements.iter() {
            self.walk_statement(statement)?;
        }
        Ok(())
    }

    fn walk_statement(&mut self, statement: &Statement) -> Result<Option<Value>, RHError> {
        let result = match &statement.kind {
            StatementKind::Expression(node) => {
                self.walk_expression(node)?;
                None
            }
            StatementKind::Print(node) => {
                let result = self.walk_expression(node)?;
                println!("{}", result);
                None
            }
            // FIXME: Returns are always expected to have a return even though `return;` is valid.
            StatementKind::Return(node) => {
                // If there's a function running above us it will catch this error.
                return Err(walker_error(
                    statement.at,
                    WalkerError::Return(self.walk_expression(node)?),
                ));
            }
            StatementKind::Break(node) => {
                let returned = if let Some(expression) = node {
                    Some(self.walk_expression(expression)?)
                } else {
                    None
                };
                // If there's a loop above us it will catch this error.
                return Err(walker_error(statement.at, WalkerError::LoopBreak(returned)));
            }
            // Same here.
            StatementKind::Continue => {
                return Err(walker_error(statement.at, WalkerError::LoopContinue));
            }
        };

        Ok(result)
    }

    pub fn walk_expression(&mut self, node: &Expression) -> Result<Value, RHError> {
        match &node.kind {
            ExpressionKind::Binary { left, op, right } => {
                // Assignment
                match op.kind {
                    BinOp::ConstAssign => return self.assing_to_lvalue(left, right, true),
                    BinOp::Assign => return self.assing_to_lvalue(left, right, false),
                    _ => {}
                }

                // Short-circuting operators
                if let BinOp::And | BinOp::Or = op.kind {
                    let left_value = self.walk_expression(left)?;
                    let is_left_true = match left_value.kind {
                        ValueKind::Bool(bool) => bool,
                        _ => return Err(walker_error(left.at, WalkerError::WrongAndOrType)),
                    };

                    if let BinOp::And = op.kind {
                        if !is_left_true {
                            return Ok(Value::bool(false, &self.types));
                        }
                    } else if is_left_true {
                        return Ok(Value::bool(true, &self.types));
                    }

                    return self.walk_expression(right);
                }

                let left = self.walk_expression(left)?;
                let right = self.walk_expression(right)?;

                let exe = ValueOperator::new(&self.types);

                // Other operators
                match op.kind {
                    BinOp::Plus => exe.add(left, right),
                    BinOp::Minus => exe.sub(left, right),
                    BinOp::Star => exe.mul(left, right),
                    BinOp::Slash => exe.div(left, right),
                    BinOp::Dot => todo!("Structures not implemented yet."),
                    BinOp::Eq => exe.eq(left, right),
                    BinOp::Neq => exe.neq(left, right),
                    BinOp::Gt => exe.gt(left, right),
                    BinOp::Gte => exe.gte(left, right),
                    BinOp::Lt => exe.gt(right, left),
                    BinOp::Lte => exe.gte(right, left),
                    _ => unreachable!(),
                }
                .map_err(|err| walker_error(op.at, WalkerError::OperationError(err)))
            }
            ExpressionKind::Unary { op, right } => {
                let value = self.walk_expression(right)?;

                let exe = ValueOperator::new(&self.types);

                match op.kind {
                    UnOp::Minus => exe.neg(value),
                    UnOp::Not => exe.not(value),
                }
                .map_err(|err| walker_error(op.at, WalkerError::OperationError(err)))
            }
            ExpressionKind::Call(node) => {
                let called = self.walk_expression(&node.called)?;

                let mut argument_values = Vec::new();
                for argument_node in node.arguments.iter() {
                    argument_values.push(self.walk_expression(argument_node)?);
                }

                let exe = ValueOperator::new(&self.types);
                exe.call(&called, argument_values)
                    .map_err(|err| walker_error(node.at, err))
            }
            ExpressionKind::ArrayAccess(node) => {
                let array = self.walk_expression(&node.array)?;
                let index = self.walk_expression(&node.index)?;

                let exe = ValueOperator::new(&self.types);
                exe.subscript(array, index)
                    .map_err(|err| walker_error(node.at, WalkerError::OperationError(err)))
            }
            ExpressionKind::MemberAccess(_) => todo!("Structures not implemented yet."),
            ExpressionKind::Group(node) => self.walk_expression(node),
            ExpressionKind::ArrayLiteral(node) => {
                let mut elements = Vec::new();
                for expression in &node.elements {
                    elements.push(self.walk_expression(expression)?);
                }
                Ok(Value {
                    kind: ValueKind::Array(Rc::new(RefCell::new(elements))),
                    // FIXME: Use actual type.
                    typ: self.types.create_type(TypeKind::Array(self.types.void())),
                })
            }
            ExpressionKind::SimpleLiteral(node) => {
                let value = match node.kind {
                    SimpleLiteralKind::Int(int) => Value::int(int as i64, &self.types),
                    SimpleLiteralKind::Float(float) => Value::float(float as f64, &self.types),
                    SimpleLiteralKind::Bool(bool) => Value::bool(bool, &self.types),
                };

                Ok(value)
            }
            ExpressionKind::Block(block) => self.walk_block(block.as_ref()),
            ExpressionKind::StrLiteral(node) => {
                let mut buffer = String::new();

                for part in &node.parts {
                    match &part.kind {
                        StrPartKind::Literal(literal) => buffer.push_str(literal),
                        StrPartKind::Embed(embed) => {
                            buffer.push_str(&self.walk_expression(embed)?.to_string())
                        }
                    };
                }

                Ok(Value::str(buffer, &self.types))
            }
            ExpressionKind::FnLiteral(node) => Ok(Value {
                kind: ValueKind::Fn(Rc::new(RefCell::new(FnValue {
                    node: node.as_ref().clone(),
                    scope: self.scope.clone(),
                }))),
                // FIXME: Use actual type here.
                typ: self.types.create_type(TypeKind::Fn {
                    parameters: HashMap::new(),
                    returns: self.types.void(),
                }),
            }),
            ExpressionKind::If(if_node) => {
                for conditional in &if_node.conditionals {
                    if let ValueKind::Bool(bool) =
                        self.walk_expression(&conditional.condition)?.kind
                    {
                        if bool {
                            return self.walk_block(&conditional.block);
                        }
                    } else {
                        return Err(walker_error(
                            conditional.at,
                            WalkerError::WrongIfConditionType,
                        ));
                    }
                }

                if let Some(else_conditional) = &if_node.else_block {
                    self.walk_block(else_conditional)
                } else {
                    Ok(Value::void(&self.types))
                }
            }
            ExpressionKind::Loop(loop_node) => {
                loop {
                    if let Some(condition) = &loop_node.condition {
                        let condition_value = self.walk_expression(condition)?;
                        if let ValueKind::Bool(should_repeat) = condition_value.kind {
                            if !should_repeat {
                                return Ok(Value::void(&self.types));
                            }
                        } else {
                            return Err(walker_error(
                                condition.at,
                                WalkerError::WrongLoopConditionType,
                            ));
                        }
                    }

                    match self.walk_block(&loop_node.body) {
                        Err(RHError {
                            kind: RHErrorKind::Run(err),
                            at: ErrorLocation::Specific(at),
                        }) => match err {
                            WalkerError::LoopBreak(loop_value) => {
                                return Ok(loop_value.unwrap_or(Value::void(&self.types)));
                            }
                            // Do nothing for continue and continue looping of course, you dummy.
                            WalkerError::LoopContinue => {}
                            // This is probably an actual error.
                            _ => return Err(walker_error(at, err)),
                        },
                        Err(_) => panic!("Walker returned non-walker error."),
                        _ => {}
                    }
                }
            }
            ExpressionKind::Identifier(ident) => self
                .scope
                .get_var(ident, &self.types)
                .map_err(|err| walker_error(node.at, WalkerError::ScopeError(err))),
        }
    }

    fn walk_block(&mut self, block: &BlockExpression) -> Result<Value, RHError> {
        self.scope.nest();

        for statement in block.statements.iter() {
            self.walk_statement(statement)?;
        }

        let result = if let Some(tail_expression) = &block.tail_expression {
            Ok(self.walk_expression(tail_expression)?)
        } else {
            Ok(Value::void(&self.types))
        };

        self.scope.unnest();

        result
    }

    pub fn assing_to_lvalue(
        &mut self,
        lvalue: &Expression,
        rvalue: &Expression,
        is_constant: bool,
    ) -> Result<Value, RHError> {
        // Maybe other expressions could also be l-values, but these are fine for now.
        match &lvalue.kind {
            ExpressionKind::MemberAccess(_) => todo!("Structures not implemented yet."),
            ExpressionKind::ArrayAccess(node) => {
                let mut array = self.walk_expression(&node.array)?;
                let index = self.walk_expression(&node.index)?;
                let value = self.walk_expression(rvalue)?;

                let exe = ValueOperator::new(&self.types);

                exe.subscript_assign(&mut array, index, value)
                    .map_err(|err| walker_error(node.at, WalkerError::OperationError(err)))
            }
            ExpressionKind::Identifier(ident) => {
                let value = self.walk_expression(rvalue)?;
                self.scope
                    .set_var(ident, value.clone(), is_constant)
                    .map_err(|err| walker_error(lvalue.at, WalkerError::ScopeError(err)))?;
                return Ok(value);
            }
            _ => Err(walker_error(lvalue.at, WalkerError::NonLValueAssignment)),
        }
    }
}

// This assumes walker errors are always at a specific error location.
fn walker_error(at: Location, walker_error: WalkerError) -> RHError {
    RHError {
        at: ErrorLocation::Specific(at),
        kind: RHErrorKind::Run(walker_error),
    }
}

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
