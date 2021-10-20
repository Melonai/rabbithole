use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::parse::ast::{
    expression::Expression,
    value::{BinaryOperator, Literal, UnaryOperator},
};
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

                let new_value = match op {
                    BinaryOperator::Plus => left_value + right_value,
                    BinaryOperator::Minus => left_value - right_value,
                    BinaryOperator::Star => left_value * right_value,
                    BinaryOperator::Slash => left_value / right_value,
                    BinaryOperator::Eq => todo!(),
                    BinaryOperator::Neq => todo!(),
                    BinaryOperator::Gt => todo!(),
                    BinaryOperator::Gte => todo!(),
                    BinaryOperator::Lt => todo!(),
                    BinaryOperator::Lte => todo!(),
                    BinaryOperator::Assign => todo!(),
                    BinaryOperator::ConstAssign => todo!(),
                    BinaryOperator::Dot => todo!(),
                };

                Ok(new_value)
            }
            Expression::Unary { op, right } => {
                let value = self.walk(right)?;

                let new_value = match op {
                    UnaryOperator::Plus => value,
                    UnaryOperator::Minus => -value,
                    UnaryOperator::Not => todo!("Implement boolean arithmetic."),
                };

                Ok(new_value)
            }
            Expression::Group(node) => self.walk(node),
            Expression::Literal(token) => {
                let value = match token {
                    Literal::Int(int) => WalkValue::Int(*int as i64),
                    Literal::Float(float) => WalkValue::Float(*float as f64),
                    _ => todo!(),
                };

                Ok(value)
            }
            _ => todo!(),
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
