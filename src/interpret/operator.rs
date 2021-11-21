use thiserror::Error;

use crate::{parse::ast::expression::Expression, types::bag::TypeBag};

use super::{
    value::{Value, ValueKind},
    walker::{Walker, WalkerError},
};

pub struct ValueOperator<'types> {
    types: &'types TypeBag,
}

impl<'t> ValueOperator<'t> {
    pub fn new(types: &'t TypeBag) -> Self {
        ValueOperator { types }
    }

    pub fn add(&self, lhs: Value, rhs: Value) -> Result<Value, OperationError> {
        match lhs.kind {
            ValueKind::Str(ref l) => match rhs.kind {
                ValueKind::Str(r) => Ok(Value::str(format!("{}{}", l, r), self.types)),
                ValueKind::Float(r) => Ok(Value::str(format!("{}{}", l, r), self.types)),
                ValueKind::Int(r) => Ok(Value::str(format!("{}{}", l, r), self.types)),
                _ => Err(OperationError::AddTypes(lhs, rhs)),
            },
            ValueKind::Float(l) => match rhs.kind {
                ValueKind::Str(r) => Ok(Value::str(l.to_string() + &r, self.types)),
                ValueKind::Float(r) => Ok(Value::float(l + r, self.types)),
                ValueKind::Int(r) => Ok(Value::float(l + r as f64, self.types)),
                _ => Err(OperationError::AddTypes(lhs, rhs)),
            },
            ValueKind::Int(l) => match rhs.kind {
                ValueKind::Str(r) => Ok(Value::str(l.to_string() + &r, self.types)),
                ValueKind::Float(r) => Ok(Value::float(l as f64 + r, self.types)),
                ValueKind::Int(r) => Ok(Value::int(l + r, self.types)),
                _ => Err(OperationError::AddTypes(lhs, rhs)),
            },
            _ => Err(OperationError::AddTypes(lhs, rhs)),
        }
    }

    pub fn sub(&self, lhs: Value, rhs: Value) -> Result<Value, OperationError> {
        match lhs.kind {
            ValueKind::Float(l) => match rhs.kind {
                ValueKind::Float(r) => Ok(Value::float(l - r, self.types)),
                ValueKind::Int(r) => Ok(Value::float(l - r as f64, self.types)),
                _ => Err(OperationError::SubTypes(lhs, rhs)),
            },
            ValueKind::Int(l) => match rhs.kind {
                ValueKind::Float(r) => Ok(Value::float(l as f64 - r, self.types)),
                ValueKind::Int(r) => Ok(Value::int(l - r, self.types)),
                _ => Err(OperationError::SubTypes(lhs, rhs)),
            },
            _ => Err(OperationError::SubTypes(lhs, rhs)),
        }
    }

    pub fn mul(&self, lhs: Value, rhs: Value) -> Result<Value, OperationError> {
        match lhs.kind {
            ValueKind::Float(l) => match rhs.kind {
                ValueKind::Float(r) => Ok(Value::float(l * r, self.types)),
                ValueKind::Int(r) => Ok(Value::float(l * r as f64, self.types)),
                _ => Err(OperationError::MulTypes(lhs, rhs)),
            },
            ValueKind::Int(l) => match rhs.kind {
                ValueKind::Float(r) => Ok(Value::float(l as f64 * r, self.types)),
                ValueKind::Int(r) => Ok(Value::int(l * r, self.types)),
                _ => Err(OperationError::MulTypes(lhs, rhs)),
            },
            _ => Err(OperationError::MulTypes(lhs, rhs)),
        }
    }

    pub fn div(&self, lhs: Value, rhs: Value) -> Result<Value, OperationError> {
        match lhs.kind {
            ValueKind::Float(l) => match rhs.kind {
                ValueKind::Float(r) => Ok(Value::float(l / r, self.types)),
                ValueKind::Int(r) => Ok(Value::float(l / r as f64, self.types)),
                _ => Err(OperationError::DivTypes(lhs, rhs)),
            },
            ValueKind::Int(l) => match rhs.kind {
                ValueKind::Float(r) => Ok(Value::float(l as f64 / r, self.types)),
                ValueKind::Int(r) => Ok(Value::float(l as f64 / r as f64, self.types)),
                _ => Err(OperationError::DivTypes(lhs, rhs)),
            },
            _ => Err(OperationError::DivTypes(lhs, rhs)),
        }
    }

    pub fn eq(&self, lhs: Value, rhs: Value) -> Result<Value, OperationError> {
        match lhs.kind {
            ValueKind::Str(l) => match rhs.kind {
                ValueKind::Str(r) => Ok(Value::bool(l == r, self.types)),
                _ => Ok(Value::bool(false, self.types)),
            },
            ValueKind::Float(l) => match rhs.kind {
                ValueKind::Float(r) => Ok(Value::bool(l == r, self.types)),
                _ => Ok(Value::bool(false, self.types)),
            },
            ValueKind::Int(l) => match rhs.kind {
                ValueKind::Int(r) => Ok(Value::bool(l == r, self.types)),
                _ => Ok(Value::bool(false, self.types)),
            },
            ValueKind::Bool(l) => match rhs.kind {
                ValueKind::Bool(r) => Ok(Value::bool(l == r, self.types)),
                _ => Ok(Value::bool(false, self.types)),
            },
            _ => Ok(Value::bool(false, self.types)),
        }
    }

    pub fn neq(&self, lhs: Value, rhs: Value) -> Result<Value, OperationError> {
        if let Ok(Value {
            kind: ValueKind::Bool(value),
            ..
        }) = self.eq(lhs, rhs)
        {
            Ok(Value::bool(!value, self.types))
        } else {
            unreachable!()
        }
    }

    pub fn gt(&self, lhs: Value, rhs: Value) -> Result<Value, OperationError> {
        match lhs.kind {
            ValueKind::Float(r) => match rhs.kind {
                ValueKind::Float(l) => Ok(Value::bool(r > l, self.types)),
                ValueKind::Int(l) => Ok(Value::bool(r > l as f64, self.types)),
                _ => Err(OperationError::CompareTypes(lhs, rhs)),
            },
            ValueKind::Int(r) => match rhs.kind {
                ValueKind::Float(l) => Ok(Value::bool(r as f64 > l, self.types)),
                ValueKind::Int(l) => Ok(Value::bool(r > l, self.types)),
                _ => Err(OperationError::CompareTypes(lhs, rhs)),
            },
            _ => Err(OperationError::CompareTypes(lhs, rhs)),
        }
    }

    pub fn gte(&self, lhs: Value, rhs: Value) -> Result<Value, OperationError> {
        match lhs.kind {
            ValueKind::Float(r) => match rhs.kind {
                ValueKind::Float(l) => Ok(Value::bool(r >= l, self.types)),
                ValueKind::Int(l) => Ok(Value::bool(r >= l as f64, self.types)),
                _ => Err(OperationError::CompareTypes(lhs, rhs)),
            },
            ValueKind::Int(r) => match rhs.kind {
                ValueKind::Float(l) => Ok(Value::bool(r as f64 >= l, self.types)),
                ValueKind::Int(l) => Ok(Value::bool(r >= l, self.types)),
                _ => Err(OperationError::CompareTypes(lhs, rhs)),
            },
            _ => Err(OperationError::CompareTypes(lhs, rhs)),
        }
    }

    pub fn neg(&self, val: Value) -> Result<Value, OperationError> {
        match val.kind {
            ValueKind::Float(float) => Ok(Value::float(-float, self.types)),
            ValueKind::Int(int) => Ok(Value::int(-int, self.types)),
            _ => Err(OperationError::NegType(val)),
        }
    }

    pub fn not(&self, val: Value) -> Result<Value, OperationError> {
        match val.kind {
            ValueKind::Bool(bool) => Ok(Value::bool(bool, self.types)),
            _ => Err(OperationError::NotType(val)),
        }
    }

    pub fn subscript(&self, val: Value, index: Value) -> Result<Value, OperationError> {
        let index = match index.kind {
            ValueKind::Int(i) => i,
            _ => return Err(OperationError::ArrayIndexType(index)),
        };

        match val.kind {
            ValueKind::Array(a) => {
                let array = a.borrow();
                if index < 0 || index as usize >= array.len() {
                    Err(OperationError::ArrayIndexOutOfRange {
                        index,
                        length: array.len(),
                    })
                } else {
                    Ok(array[index as usize].clone())
                }
            }
            // Maybe allow string subscripts?
            _ => Err(OperationError::ArrayType(val)),
        }
    }

    pub fn subscript_assign(
        &self,
        val: &mut Value,
        index: Value,
        value: Value,
    ) -> Result<Value, OperationError> {
        let index = match index.kind {
            ValueKind::Int(i) => i,
            _ => return Err(OperationError::ArrayIndexType(index)),
        };

        match &val.kind {
            ValueKind::Array(a) => {
                let mut array = a.borrow_mut();
                if index < 0 || index as usize >= array.len() {
                    Err(OperationError::ArrayIndexOutOfRange {
                        index,
                        length: array.len(),
                    })
                } else {
                    array[index as usize] = value;
                    Ok(array[index as usize].clone())
                }
            }
            _ => Err(OperationError::ArrayType(val.clone())),
        }
    }

    pub fn call(&self, val: &Value, arguments: Vec<Value>) -> Result<Value, WalkerError> {
        let called = match &val.kind {
            ValueKind::Fn(i) => i,
            _ => {
                return Err(WalkerError::OperationError(OperationError::CallableType(
                    val.clone(),
                )))
            }
        }
        .borrow();

        // FIXME: Currently closures are able to re-assign values from the upper scopes.
        // This is good behaviour, until a closure re-assigns a value that was declared after
        // the closure even existed.
        // Minimal reproducible example:
        // ```rh
        // closure = fn { y = 10; };
        // y = 1;
        // closure();
        // print y;
        // ```
        // Expected: 1
        // Actual: 10
        let mut scope = called.scope.clone();
        scope.nest();

        let parameters = &called.node.header.parameters;

        if parameters.len() != arguments.len() {
            return Err(WalkerError::OperationError(
                OperationError::WrongArgumentCount(parameters.len(), arguments.len()),
            ));
        }

        for (argument, parameter) in arguments.into_iter().zip(parameters.iter()) {
            scope.set_var_shadowed(&parameter.identifier, argument);
        }

        // Yes, we create a new walker for every function call,
        // it's *way* easier that way.
        let mut walker = Walker::new(scope, self.types.clone());
        let result = walker.walk_expression(&Expression::Block(Box::new(called.node.body.clone())));

        if let Err(WalkerError::Return(returned)) = result {
            Ok(returned)
        } else {
            result
        }
    }
}

#[derive(Error, Debug)]
pub enum OperationError {
    #[error("Can't add value '{0}' of type '{}' to value '{1}' of type '{}'.", .0.type_name(), .1.type_name())]
    AddTypes(Value, Value),
    #[error("Can't subtract value '{1}' of type '{}' from value '{0}' of type '{}'.", .1.type_name(), .0.type_name())]
    SubTypes(Value, Value),
    #[error("Can't multiply value '{0}' of type '{}' with value '{1}' of type '{}'.", .0.type_name(), .1.type_name())]
    MulTypes(Value, Value),
    #[error("Can't divide value '{0}' of type '{}' by value '{1}' of type '{}'.", .0.type_name(), .1.type_name())]
    DivTypes(Value, Value),
    #[error("Can't compare value '{0}' of type '{}' with value '{1}' of type '{}'.", .0.type_name(), .1.type_name())]
    CompareTypes(Value, Value),
    #[error("Can't negate value '{0}' of type '{}'.", .0.type_name())]
    NegType(Value),
    #[error("Can't flip value '{0}' of type '{}'.", .0.type_name())]
    NotType(Value),
    #[error("Can't use value '{0}' of type '{}' as a subsript index.", .0.type_name())]
    ArrayIndexType(Value),
    #[error("Can't subscript value '{0}' of type '{}'.", .0.type_name())]
    ArrayType(Value),
    #[error("Array index '{index}' out of range for array of length '{length}'.")]
    ArrayIndexOutOfRange { index: i64, length: usize },
    #[error("Can't call value '{0}' of type '{}'.", .0.type_name())]
    CallableType(Value),
    #[error("Function expects {0} arguments, but {1} were given.")]
    WrongArgumentCount(usize, usize),
}
