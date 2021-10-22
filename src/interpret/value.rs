use crate::parse::ast::nodes::FnNode;
use anyhow::{anyhow, Result};
use std::{cell::RefCell, rc::Rc};

type Ref<T> = RefCell<Rc<T>>;

#[derive(Clone, Debug)]
pub enum Value {
    Str(String),
    Float(f64),
    Int(i64),
    Bool(bool),
    Fn(Ref<FnNode>),
    Void,
}

impl Value {
    pub fn add(self, rhs: Value) -> Result<Value> {
        match self {
            Value::Str(l) => match rhs {
                Value::Str(r) => Ok(Value::Str(l + &r)),
                Value::Float(r) => Ok(Value::Str(l + &r.to_string())),
                Value::Int(r) => Ok(Value::Str(l + &r.to_string())),
                _ => Err(anyhow!("Right operand can't be added.")),
            },
            Value::Float(l) => match rhs {
                Value::Str(r) => Ok(Value::Str(l.to_string() + &r)),
                Value::Float(r) => Ok(Value::Float(l + r)),
                Value::Int(r) => Ok(Value::Float(l + r as f64)),
                _ => Err(anyhow!("Right operand can't be added.")),
            },
            Value::Int(l) => match rhs {
                Value::Str(r) => Ok(Value::Str(l.to_string() + &r)),
                Value::Float(r) => Ok(Value::Float(l as f64 + r)),
                Value::Int(r) => Ok(Value::Int(l + r)),
                _ => Err(anyhow!("Right operand can't be added.")),
            },
            _ => Err(anyhow!("Left operand can't be added.")),
        }
    }

    pub fn sub(self, rhs: Value) -> Result<Value> {
        match self {
            Value::Float(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l - r)),
                Value::Int(r) => Ok(Value::Float(l - r as f64)),
                _ => Err(anyhow!("Right operand can't be substracted.")),
            },
            Value::Int(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l as f64 - r)),
                Value::Int(r) => Ok(Value::Int(l - r)),
                _ => Err(anyhow!("Right operand can't be substracted.")),
            },
            _ => Err(anyhow!("Left operand can't be substracted from.")),
        }
    }

    pub fn mul(self, rhs: Value) -> Result<Value> {
        match self {
            Value::Float(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l * r)),
                Value::Int(r) => Ok(Value::Float(l * r as f64)),
                _ => Err(anyhow!("Right operand can't be multiplied.")),
            },
            Value::Int(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l as f64 * r)),
                Value::Int(r) => Ok(Value::Int(l * r)),
                _ => Err(anyhow!("Right operand can't be multiplied.")),
            },
            _ => Err(anyhow!("Left operand can't be multiplied.")),
        }
    }

    pub fn div(self, rhs: Value) -> Result<Value> {
        match self {
            Value::Float(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l / r)),
                Value::Int(r) => Ok(Value::Float(l / r as f64)),
                _ => Err(anyhow!("Right operand can't be multiplied.")),
            },
            Value::Int(l) => match rhs {
                Value::Float(r) => Ok(Value::Float(l as f64 / r)),
                Value::Int(r) => Ok(Value::Float(l as f64 / r as f64)),
                _ => Err(anyhow!("Right operand can't be multiplied.")),
            },
            _ => Err(anyhow!("Left operand can't be multiplied.")),
        }
    }

    pub fn eq(self, rhs: Value) -> Result<Value> {
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

    pub fn neq(self, rhs: Value) -> Result<Value> {
        if let Ok(Value::Bool(value)) = self.eq(rhs) {
            Ok(Value::Bool(!value))
        } else {
            unreachable!()
        }
    }

    pub fn gt(self, rhs: Value) -> Result<Value> {
        match self {
            Value::Float(r) => match rhs {
                Value::Float(l) => Ok(Value::Bool(r > l)),
                Value::Int(l) => Ok(Value::Bool(r > l as f64)),
                _ => Err(anyhow!("Right operand can't be compared.")),
            },
            Value::Int(r) => match rhs {
                Value::Float(l) => Ok(Value::Bool(r as f64 > l)),
                Value::Int(l) => Ok(Value::Bool(r > l)),
                _ => Err(anyhow!("Right operand can't be compared.")),
            },
            _ => Err(anyhow!("Left operand can't be compared.")),
        }
    }

    pub fn gte(self, rhs: Value) -> Result<Value> {
        match self {
            Value::Float(r) => match rhs {
                Value::Float(l) => Ok(Value::Bool(r >= l)),
                Value::Int(l) => Ok(Value::Bool(r >= l as f64)),
                _ => Err(anyhow!("Right operand can't be compared.")),
            },
            Value::Int(r) => match rhs {
                Value::Float(l) => Ok(Value::Bool(r as f64 >= l)),
                Value::Int(l) => Ok(Value::Bool(r >= l)),
                _ => Err(anyhow!("Right operand can't be compared.")),
            },
            _ => Err(anyhow!("Left operand can't be compared.")),
        }
    }
}
