use crate::{
    parse::ast::nodes::{FnExpression, Identifier},
    types::{bag::TypeBag, Type},
};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use super::scope::ScopeChain;

type ReferenceOnCopy<T> = Rc<RefCell<T>>;

#[derive(Clone, Debug)]
pub struct Value {
    pub kind: ValueKind,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub enum ValueKind {
    Str(String),
    Float(f64),
    Int(i64),
    Bool(bool),
    Array(ReferenceOnCopy<Vec<Value>>),
    Fn(ReferenceOnCopy<FnValue>),
    Data(HashMap<Identifier, Value>),
    Void,
}

impl Value {
    pub fn str(val: String, types: &TypeBag) -> Self {
        Self {
            kind: ValueKind::Str(val),
            typ: types.str(),
        }
    }

    pub fn int(val: i64, types: &TypeBag) -> Self {
        Self {
            kind: ValueKind::Int(val),
            typ: types.int(),
        }
    }

    pub fn float(val: f64, types: &TypeBag) -> Self {
        Self {
            kind: ValueKind::Float(val),
            typ: types.float(),
        }
    }

    pub fn bool(val: bool, types: &TypeBag) -> Self {
        Self {
            kind: ValueKind::Bool(val),
            typ: types.bool(),
        }
    }

    pub fn void(types: &TypeBag) -> Self {
        Self {
            kind: ValueKind::Void,
            typ: types.void(),
        }
    }

    pub fn type_name(&self) -> &'static str {
        // TODO: Base this off of the type.
        match self.kind {
            ValueKind::Str(_) => "Str",
            ValueKind::Float(_) => "Float",
            ValueKind::Int(_) => "Int",
            ValueKind::Bool(_) => "Bool",
            ValueKind::Array(_) => "Array",
            ValueKind::Fn(_) => "Fn",
            ValueKind::Void => "Void",
            _ => todo!(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ValueKind::Str(v) => write!(f, "{}", v),
            ValueKind::Float(v) => write!(f, "{}", v),
            ValueKind::Int(v) => write!(f, "{}", v),
            ValueKind::Bool(v) => write!(f, "{}", v),
            ValueKind::Array(a) => {
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
            ValueKind::Fn(v) => write!(f, "<fn {:?}>", v.as_ptr()),
            ValueKind::Void => write!(f, "<void>"),
            _ => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FnValue {
    pub node: FnExpression,
    pub scope: ScopeChain,
}

pub type ValueMap = HashMap<Identifier, Value>;
