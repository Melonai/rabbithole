use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::{lex::token::TokenVariant::*, parse::ast::Expression};
use anyhow::Result;

// No state for now.
pub struct Walker {}

impl Walker {
    pub fn new() -> Self {
        Walker {}
    }

    pub fn walk(&self, node: &Expression) -> Result<WalkValue> {
        match node {
            Expression::Binary { left, op, right } => {
                let left_value = self.walk(left)?;
                let right_value = self.walk(right)?;

                let new_value = match op.variant {
                    OpPlus => left_value + right_value,
                    OpMinus => left_value - right_value,
                    OpStar => left_value * right_value,
                    OpSlash => left_value / right_value,
                    _ => unreachable!(),
                };

                Ok(new_value)
            }
            Expression::Unary { op, right } => {
                let value = self.walk(right)?;

                let new_value = match op.variant {
                    OpPlus => value,
                    OpMinus => -value,
                    OpNot => todo!("Implement boolean arithmetic."),
                    _ => unreachable!(),
                };

                Ok(new_value)
            }
            Expression::Group(node) => self.walk(node),
            Expression::Literal(token) => {
                let value = match token.variant {
                    Int(int) => WalkValue::Int(int as i64),
                    Float(float) => WalkValue::Float(float as f64),
                    _ => unreachable!(),
                };

                Ok(value)
            }
        }
    }
}

pub enum WalkValue {
    Float(f64),
    Int(i64),
}

impl Add for WalkValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::Float(float) => match rhs {
                Self::Float(other_float) => Self::Float(float + other_float),
                Self::Int(other_int) => Self::Float(float + other_int as f64),
            },
            Self::Int(int) => match rhs {
                Self::Float(other_float) => Self::Float(int as f64 + other_float),
                Self::Int(other_int) => Self::Int(int + other_int),
            },
        }
    }
}

impl Sub for WalkValue {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Self::Float(float) => match rhs {
                Self::Float(other_float) => Self::Float(float - other_float),
                Self::Int(other_int) => Self::Float(float - other_int as f64),
            },
            Self::Int(int) => match rhs {
                Self::Float(other_float) => Self::Float(int as f64 - other_float),
                Self::Int(other_int) => Self::Int(int - other_int),
            },
        }
    }
}

impl Mul for WalkValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Self::Float(float) => match rhs {
                Self::Float(other_float) => Self::Float(float * other_float),
                Self::Int(other_int) => Self::Float(float * other_int as f64),
            },
            Self::Int(int) => match rhs {
                Self::Float(other_float) => Self::Float(int as f64 * other_float),
                Self::Int(other_int) => Self::Int(int * other_int),
            },
        }
    }
}

impl Div for WalkValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Self::Float(float) => match rhs {
                Self::Float(other_float) => Self::Float(float / other_float),
                Self::Int(other_int) => Self::Float(float / other_int as f64),
            },
            Self::Int(int) => match rhs {
                Self::Float(other_float) => Self::Float(int as f64 / other_float),
                Self::Int(other_int) => Self::Float(int as f64 / other_int as f64),
            },
        }
    }
}

impl Neg for WalkValue {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Float(float) => Self::Float(-float),
            Self::Int(int) => Self::Int(-int),
        }
    }
}

impl Display for WalkValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(float) => write!(f, "{}", float),
            Self::Int(int) => write!(f, "{}", int),
        }
    }
}
