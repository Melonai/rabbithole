//! Nodes are parts of an expression, but aren't expressions themselves.
//! Example: A BinaryExpression is made out of two expressions and an operator node,
//! because an operator can't be an expression by itself.
//!
//! Nodes can also contain nested expressions.
//! This module also contains the actual expression structs for this reason,
//! although this might be changed later?

use crate::lex::token::{Location, Token, TokenKind::*};

use super::{expression::Expression, statement::Statement};

#[derive(Debug, Clone)]
pub struct BinaryOperatorNode {
    pub at: Location,
    pub kind: BinaryOperatorKind,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperatorKind {
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
    And,
    Or,
    Assign,
    ConstAssign,
    Dot,
}

impl BinaryOperatorNode {
    pub fn from_token(token: Token) -> Self {
        use BinaryOperatorKind as Kind;

        let kind = match token.kind {
            OpPlus => Kind::Plus,
            OpMinus => Kind::Minus,
            OpStar => Kind::Star,
            OpSlash => Kind::Slash,
            OpEq => Kind::Eq,
            OpNeq => Kind::Neq,
            OpLt => Kind::Lt,
            OpGt => Kind::Gt,
            OpLte => Kind::Lte,
            OpGte => Kind::Gte,
            OpAnd => Kind::And,
            OpOr => Kind::Or,
            Assign => Kind::Assign,
            ConstAssign => Kind::ConstAssign,
            Dot => Kind::Dot,
            _ => panic!("Can't create binary operator from '{:?}'.", token.kind),
        };

        Self {
            at: token.location,
            kind,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOperatorNode {
    pub at: Location,
    pub kind: UnaryOperatorKind,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperatorKind {
    Minus,
    Not,
}

impl UnaryOperatorNode {
    pub fn from_token(token: Token) -> Self {
        use UnaryOperatorKind as Kind;

        let kind = match token.kind {
            OpMinus => Kind::Minus,
            OpNot => Kind::Not,
            _ => panic!("Can't create unary operator from '{:?}'.", token.kind),
        };

        Self {
            at: token.location,
            kind,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SimpleLiteralNode {
    pub at: Location,
    pub kind: SimpleLiteralKind,
}

#[derive(Debug, Clone)]
pub enum SimpleLiteralKind {
    Int(u32),
    Float(f32),
    Bool(bool),
}

impl SimpleLiteralNode {
    pub fn from_token(token: Token) -> Self {
        use SimpleLiteralKind as Kind;

        let kind = match token.kind {
            Int(int) => Kind::Int(int),
            Float(float) => Kind::Float(float),
            KeywordTrue => Kind::Bool(true),
            KeywordFalse => Kind::Bool(false),
            _ => panic!("Can't create literal from '{:?}'.", token.kind),
        };

        Self {
            at: token.location,
            kind,
        }
    }
}

pub type Identifier = String;

// If the constraint is None the type will have to be inferred
// during analysis.
#[derive(Debug, Clone)]
pub struct TypedIdentifierNode {
    // TODO: Add locations to other parts, not just the start of the identifier.
    // i.e. make an IdentifierNode or something like that.
    pub at: Location,
    pub identifier: Identifier,
    pub type_constraint: Option<Identifier>,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    // TODO: Ditto.
    pub at: Location,
    pub called: Expression,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ArrayAccessExpression {
    pub at: Location,
    pub array: Expression,
    pub index: Expression,
}

#[derive(Debug, Clone)]
pub struct MemberAccessExpression {
    // TODO: Ditto.
    pub at: Location,
    pub object: Expression,
    pub member_name: Identifier,
}

#[derive(Debug, Clone)]
pub struct StrExpression {
    pub at: Location,
    pub parts: Vec<StrPartNode>,
}

#[derive(Debug, Clone)]
pub struct StrPartNode {
    pub at: Location,
    pub kind: StrPartKind,
}

#[derive(Debug, Clone)]
pub enum StrPartKind {
    Literal(String),
    Embed(Expression),
}

#[derive(Debug, Clone)]
pub struct FnExpression {
    pub at: Location,
    pub header: FnHeaderNode,
    pub body: BlockExpression,
}

#[derive(Debug, Clone)]
pub struct FnHeaderNode {
    // TODO: ditto...
    pub at: Location,
    pub has_self_receiver: bool,
    pub parameters: Vec<TypedIdentifierNode>,
    pub return_type: Option<Identifier>,
}

#[derive(Debug, Clone)]
pub struct ArrayExpression {
    pub at: Location,
    pub elements: Vec<Expression>,
}
#[derive(Debug, Clone)]
pub struct IfExpression {
    pub at: Location,
    pub conditionals: Vec<ConditionalBlockNode>,
    pub else_block: Option<BlockExpression>,
}

#[derive(Debug, Clone)]
pub struct LoopExpression {
    pub at: Location,
    pub condition: Option<Expression>,
    pub body: BlockExpression,
}

#[derive(Debug, Clone)]
pub struct BlockExpression {
    pub at: Location,
    pub statements: Vec<Statement>,
    pub tail_expression: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct ConditionalBlockNode {
    pub at: Location,
    pub condition: Expression,
    pub block: BlockExpression,
}
