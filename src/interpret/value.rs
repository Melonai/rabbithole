use crate::parse::ast::{expression::Expression, nodes::FnNode};
use std::{cell::RefCell, fmt::Display, rc::Rc};
use thiserror::Error;

use super::{
    scope::ScopeChain,
    walker::{Walker, WalkerError},
};

type ReferenceOnCopy<T> = Rc<RefCell<T>>;

#[derive(Clone, Debug)]
pub enum Value {
    Str(String),
    Float(f64),
    Int(i64),
    Bool(bool),
    Array(ReferenceOnCopy<Vec<Value>>),
    Fn(ReferenceOnCopy<FnValue>),
    Void,
}

impl Value {
    pub fn add(self, rhs: Value) -> Result<Value, OperationError> {
        match self {
            Value::Str(ref l) => match rhs {
                Value::Str(r) => Ok(Value::Str(format!("{}{}", l, r))),
                Value::Float(r) => Ok(Value::Str(format!("{}{}", l, r))),
                Value::Int(r) => Ok(Value::Str(format!("{}{}", l, r))),
                r => Err(OperationError::AddTypes(self, r)),
            },
            Value::Float(l) => match rhs {
                Value::Str(r) => Ok(Value::Str(l.to_string() + &r)),
                Value::Float(r) => Ok(Value::Float(l + r)),
                Value::Int(r) => Ok(Value::Float(l + r as f64)),
                r => Err(OperationError::AddTypes(self, r)),
            },
            Value::Int(l) => match rhs {
                Value::Str(r) => Ok(Value::Str(l.to_string() + &r)),
                Value::Float(r) => Ok(Value::Float(l as f64 + r)),
                Value::Int(r) => Ok(Value::Int(l + r)),
                r => Err(OperationError::AddTypes(self, r)),
            },
            _ => Err(OperationError::AddTypes(self, rhs)),
        }
    }

    pub fn sub(self, rhs: Value) -> Result<Value, OperationError> {
        match self {
            Value::Float(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l - r)),
                Value::Int(r) => Ok(Value::Float(l - r as f64)),
                r => Err(OperationError::SubTypes(self, r)),
            },
            Value::Int(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l as f64 - r)),
                Value::Int(r) => Ok(Value::Int(l - r)),
                r => Err(OperationError::SubTypes(self, r)),
            },
            _ => Err(OperationError::SubTypes(self, rhs)),
        }
    }

    pub fn mul(self, rhs: Value) -> Result<Value, OperationError> {
        match self {
            Value::Float(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l * r)),
                Value::Int(r) => Ok(Value::Float(l * r as f64)),
                r => Err(OperationError::MulTypes(self, r)),
            },
            Value::Int(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l as f64 * r)),
                Value::Int(r) => Ok(Value::Int(l * r)),
                r => Err(OperationError::MulTypes(self, r)),
            },
            _ => Err(OperationError::MulTypes(self, rhs)),
        }
    }

    pub fn div(self, rhs: Value) -> Result<Value, OperationError> {
        match self {
            Value::Float(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l / r)),
                Value::Int(r) => Ok(Value::Float(l / r as f64)),
                r => Err(OperationError::DivTypes(self, r)),
            },
            Value::Int(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l as f64 / r)),
                Value::Int(r) => Ok(Value::Float(l as f64 / r as f64)),
                r => Err(OperationError::DivTypes(self, r)),
            },
            _ => Err(OperationError::DivTypes(self, rhs)),
        }
    }

    pub fn eq(self, rhs: Value) -> Result<Value, OperationError> {
        match self {
            Value::Str(l) => match rhs {
                Value::Str(r) => Ok(Value::Bool(l == r)),
                _ => Ok(Value::Bool(false)),
            },
            Value::Float(l) => match rhs {
                Value::Float(r) => Ok(Value::Bool(l == r)),
                _ => Ok(Value::Bool(false)),
            },
            Value::Int(l) => match rhs {
                Value::Int(r) => Ok(Value::Bool(l == r)),
                _ => Ok(Value::Bool(false)),
            },
            Value::Bool(l) => match rhs {
                Value::Bool(r) => Ok(Value::Bool(l == r)),
                _ => Ok(Value::Bool(false)),
            },
            _ => Ok(Value::Bool(false)),
        }
    }

    pub fn neq(self, rhs: Value) -> Result<Value, OperationError> {
        if let Ok(Value::Bool(value)) = self.eq(rhs) {
            Ok(Value::Bool(!value))
        } else {
            unreachable!()
        }
    }

    pub fn gt(self, rhs: Value) -> Result<Value, OperationError> {
        match self {
            Value::Float(r) => match rhs {
                Value::Float(l) => Ok(Value::Bool(r > l)),
                Value::Int(l) => Ok(Value::Bool(r > l as f64)),
                r => Err(OperationError::CompareTypes(self, r)),
            },
            Value::Int(r) => match rhs {
                Value::Float(l) => Ok(Value::Bool(r as f64 > l)),
                Value::Int(l) => Ok(Value::Bool(r > l)),
                r => Err(OperationError::CompareTypes(self, r)),
            },
            _ => Err(OperationError::CompareTypes(self, rhs)),
        }
    }

    pub fn gte(self, rhs: Value) -> Result<Value, OperationError> {
        match self {
            Value::Float(r) => match rhs {
                Value::Float(l) => Ok(Value::Bool(r >= l)),
                Value::Int(l) => Ok(Value::Bool(r >= l as f64)),
                r => Err(OperationError::CompareTypes(self, r)),
            },
            Value::Int(r) => match rhs {
                Value::Float(l) => Ok(Value::Bool(r as f64 >= l)),
                Value::Int(l) => Ok(Value::Bool(r >= l)),
                r => Err(OperationError::CompareTypes(self, r)),
            },
            _ => Err(OperationError::CompareTypes(self, rhs)),
        }
    }

    pub fn neg(self) -> Result<Value, OperationError> {
        match self {
            Value::Float(float) => Ok(Value::Float(-float)),
            Value::Int(int) => Ok(Value::Int(-int)),
            _ => Err(OperationError::NegType(self)),
        }
    }

    pub fn not(self) -> Result<Value, OperationError> {
        match self {
            Value::Bool(bool) => Ok(Value::Bool(bool)),
            _ => Err(OperationError::NotType(self)),
        }
    }

    pub fn subscript(self, index: Value) -> Result<Value, OperationError> {
        let index = match index {
            Value::Int(i) => i,
            i => return Err(OperationError::ArrayIndexType(i)),
        };

        match self {
            Value::Array(a) => {
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
            x => Err(OperationError::ArrayType(x.clone())),
        }
    }

    pub fn subscript_assign(
        &mut self,
        index: Value,
        value: Value,
    ) -> Result<Value, OperationError> {
        let index = match index {
            Value::Int(i) => i,
            i => return Err(OperationError::ArrayIndexType(i)),
        };

        match self {
            Value::Array(a) => {
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
            x => Err(OperationError::ArrayType(x.clone())),
        }
    }

    pub fn call(&self, arguments: Vec<Value>) -> Result<Value, WalkerError> {
        let called = match self {
            Value::Fn(i) => i,
            i => {
                return Err(WalkerError::OperationError(OperationError::CallableType(
                    i.clone(),
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
        let mut walker = Walker::new_with_scope(scope);
        let result =
            walker.walk_expression(&Expression::Block(Box::new(called.node.body.clone())))?;

        Ok(result)
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Str(_) => "Str",
            Value::Float(_) => "Float",
            Value::Int(_) => "Int",
            Value::Bool(_) => "Bool",
            Value::Array(_) => "Array",
            Value::Fn(_) => "Fn",
            Value::Void => "Void",
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Str(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Array(a) => {
                write!(
                    f,
                    "[{}]",
                    a.borrow()
                        .iter()
                        .map(|v| format!("{}", v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Value::Fn(v) => write!(f, "<fn {:?}>", v.as_ptr()),
            Value::Void => write!(f, "<void>"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FnValue {
    pub node: FnNode,
    pub scope: ScopeChain,
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
