pub mod bag;

use std::cell::RefCell;
use std::collections::HashMap;

use crate::interpret::value::ValueMap;
use crate::parse::ast::nodes::Identifier;

#[derive(Debug, Clone)]
pub struct Type {
    variant: Box<TypeVariant>,
    global_associated_values: &'static RefValueMap,
}

#[derive(Debug, Clone)]
pub enum TypeVariant {
    // Concrete types
    Str,
    Int,
    Float,
    Bool,
    Void,
    Fn {
        parameters: TypeMap,
        returns: Type,
    },
    Array(Type),
    Tuple(Vec<Type>),
    Data {
        data: TypeMap,
        associated_values: RefValueMap,
    },
    // TODO: Implement abstract types.
    // These should also definitely have `associated_values`,
    // but as they're not implemented yet...
    Face(TypeMap),
    Var(TypeMap),
}

type TypeMap = HashMap<Identifier, Type>;

type RefValueMap = RefCell<ValueMap>;