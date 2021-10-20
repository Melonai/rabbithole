use crate::lex::token::{Token, TokenVariant::*};

use super::{expression::Expression, statement::Statement};

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    Assign,
    ConstAssign,
    Dot,
}

impl BinaryOperator {
    pub fn from_token(token: Token) -> Self {
        match token.variant {
            OpPlus => Self::Plus,
            OpMinus => Self::Minus,
            OpStar => Self::Star,
            OpSlash => Self::Slash,
            OpEq => Self::Eq,
            OpNeq => Self::Neq,
            OpLt => Self::Lt,
            OpGt => Self::Gt,
            OpLte => Self::Lte,
            OpGte => Self::Gte,
            Assign => Self::Assign,
            ConstAssign => Self::ConstAssign,
            Dot => Self::Dot,
            _ => panic!("Can't create binary operator from '{:?}'.", token.variant)
        }
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

impl UnaryOperator {
    pub fn from_token(token: Token) -> Self {
        match token.variant {
            OpPlus => Self::Plus,
            OpMinus => Self::Minus,
            OpNot => Self::Not,
            _ => panic!("Can't create unary operator from '{:?}'.", token.variant)
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    Int(u32),
    Float(f32),
    Str(String),
}

impl Literal {
    pub fn from_token(token: Token) -> Self {
        match token.variant {
            Int(int) => Self::Int(int),
            Float(float) => Self::Float(float),
            Str(string) => Self::Str(string),
            _ => panic!("Can't create literal from '{:?}'.", token.variant)
        }
    }
}

pub type Identifier = String;

// If the contraint is None the type will have to be inferred
// during analysis.
#[derive(Debug)]
pub struct TypedIdentifier {
    pub identifier: Identifier,
    pub type_constraint: Option<Identifier>,
}

#[derive(Debug)]
pub struct FnHeader {
    pub has_self_receiver: bool,
    pub parameters: Vec<TypedIdentifier>,
    pub return_type: Option<Identifier>,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub tail_expression: Option<Expression>,
}

#[derive(Debug)]
pub struct ConditionalBlock {
    pub condition: Expression,
    pub block: Block,
}
