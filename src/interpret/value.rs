use crate::{
    parse::ast::nodes::{FnNode, Identifier},
    types::{bag::TypeBag, Type},
};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use super::scope::ScopeChain;

type ReferenceOnCopy<T> = Rc<RefCell<T>>;

#[derive(Clone, Debug)]
pub struct Value {
    pub variant: ValueVariant,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub enum ValueVariant {
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
            variant: ValueVariant::Str(val),
            typ: types.str(),
        }
    }

    pub fn int(val: i64, types: &TypeBag) -> Self {
        Self {
            variant: ValueVariant::Int(val),
            typ: types.int(),
        }
    }

    pub fn float(val: f64, types: &TypeBag) -> Self {
        Self {
            variant: ValueVariant::Float(val),
            typ: types.float(),
        }
    }

    pub fn bool(val: bool, types: &TypeBag) -> Self {
        Self {
            variant: ValueVariant::Bool(val),
            typ: types.bool(),
        }
    }

    pub fn void(types: &TypeBag) -> Self {
        Self {
            variant: ValueVariant::Void,
            typ: types.void(),
        }
    }

    pub fn type_name(&self) -> &'static str {
        // TODO: Base this off of the type.
        match self.variant {
            ValueVariant::Str(_) => "Str",
            ValueVariant::Float(_) => "Float",
            ValueVariant::Int(_) => "Int",
            ValueVariant::Bool(_) => "Bool",
            ValueVariant::Array(_) => "Array",
            ValueVariant::Fn(_) => "Fn",
            ValueVariant::Void => "Void",
            _ => todo!(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.variant {
            ValueVariant::Str(v) => write!(f, "{}", v),
            ValueVariant::Float(v) => write!(f, "{}", v),
            ValueVariant::Int(v) => write!(f, "{}", v),
            ValueVariant::Bool(v) => write!(f, "{}", v),
            ValueVariant::Array(a) => {
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
            ValueVariant::Fn(v) => write!(f, "<fn {:?}>", v.as_ptr()),
            ValueVariant::Void => write!(f, "<void>"),
            _ => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FnValue {
    pub node: FnNode,
    pub scope: ScopeChain,
}

pub type ValueMap = HashMap<Identifier, Value>;
