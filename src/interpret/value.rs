use crate::parse::ast::nodes::FnNode;
use anyhow::{anyhow, Result};
use std::{cell::RefCell, rc::Rc};

type Ref<T> = RefCell<Rc<T>>;

#[derive(Clone, Debug)]
pub enum Value {
    Str(String),
    Float(f64),
    Int(i64),
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
}

// pub enum Value {
//     Float(f64),
//     Int(i64),
// }

// impl Add for WalkValue {
//     type Output = Self;

//     fn add(self, rhs: Self) -> Self::Output {
//         match self {
//             Self::Float(float) => match rhs {
//                 Self::Float(other_float) => Self::Float(float + other_float),
//                 Self::Int(other_int) => Self::Float(float + other_int as f64),
//             },
//             Self::Int(int) => match rhs {
//                 Self::Float(other_float) => Self::Float(int as f64 + other_float),
//                 Self::Int(other_int) => Self::Int(int + other_int),
//             },
//         }
//     }
// }

// impl Sub for WalkValue {
//     type Output = Self;

//     fn sub(self, rhs: Self) -> Self::Output {
//         match self {
//             Self::Float(float) => match rhs {
//                 Self::Float(other_float) => Self::Float(float - other_float),
//                 Self::Int(other_int) => Self::Float(float - other_int as f64),
//             },
//             Self::Int(int) => match rhs {
//                 Self::Float(other_float) => Self::Float(int as f64 - other_float),
//                 Self::Int(other_int) => Self::Int(int - other_int),
//             },
//         }
//     }
// }

// impl Mul for WalkValue {
//     type Output = Self;

//     fn mul(self, rhs: Self) -> Self::Output {
//         match self {
//             Self::Float(float) => match rhs {
//                 Self::Float(other_float) => Self::Float(float * other_float),
//                 Self::Int(other_int) => Self::Float(float * other_int as f64),
//             },
//             Self::Int(int) => match rhs {
//                 Self::Float(other_float) => Self::Float(int as f64 * other_float),
//                 Self::Int(other_int) => Self::Int(int * other_int),
//             },
//         }
//     }
// }

// impl Div for WalkValue {
//     type Output = Self;

//     fn div(self, rhs: Self) -> Self::Output {
//         match self {
//             Self::Float(float) => match rhs {
//                 Self::Float(other_float) => Self::Float(float / other_float),
//                 Self::Int(other_int) => Self::Float(float / other_int as f64),
//             },
//             Self::Int(int) => match rhs {
//                 Self::Float(other_float) => Self::Float(int as f64 / other_float),
//                 Self::Int(other_int) => Self::Float(int as f64 / other_int as f64),
//             },
//         }
//     }
// }

// impl Neg for WalkValue {
//     type Output = Self;

//     fn neg(self) -> Self::Output {
//         match self {
//             Self::Float(float) => Self::Float(-float),
//             Self::Int(int) => Self::Int(-int),
//         }
//     }
// }

// impl Display for WalkValue {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Float(float) => write!(f, "{}", float),
//             Self::Int(int) => write!(f, "{}", int),
//         }
//     }
// }
