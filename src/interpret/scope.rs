use super::value::Value;
use crate::parse::ast::nodes::Identifier;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use thiserror::Error;

type InnerScope = HashMap<Identifier, AssignedValue>;

#[derive(Debug, Clone)]
pub struct Scope(Rc<RefCell<InnerScope>>);

impl Scope {
    fn new() -> Self {
        Scope(Rc::new(RefCell::new(HashMap::new())))
    }

    fn get(&self, ident: &str) -> Option<AssignedValue> {
        self.0.borrow().get(ident).cloned()
    }

    fn insert(&self, ident: Identifier, value: AssignedValue) {
        self.0.borrow_mut().insert(ident, value);
    }
}

#[derive(Debug, Clone)]
pub struct ScopeChain {
    scopes: Vec<Scope>,
}

impl ScopeChain {
    pub fn new() -> Self {
        ScopeChain { scopes: Vec::new() }
    }

    pub fn nest(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn unnest(&mut self) {
        self.scopes.pop();
    }

    pub fn set_var(
        &mut self,
        ident: &str,
        value: Value,
        is_constant: bool,
    ) -> Result<(), ScopeError> {
        let wrapped_value = if is_constant {
            AssignedValue::Constant(value)
        } else {
            AssignedValue::Mutable(value)
        };

        let num_scopes = self.scopes.len();
        for scope in self.scopes.iter_mut().take(num_scopes - 1) {
            if let Some(already_assigned_value) = scope.get(ident) {
                if !already_assigned_value.is_constant() {
                    scope.insert(ident.to_string(), wrapped_value);
                    return Ok(());
                } else {
                    return Err(ScopeError::CantReassignConstant(ident.to_string()));
                }
            }
        }

        let inner_scope = self
            .scopes
            .last_mut()
            .expect("Tried accessing scope after last frame is gone.");

        if let Some(already_assigned_value) = inner_scope.get(ident) {
            if !already_assigned_value.is_constant() {
                inner_scope.insert(ident.to_string(), wrapped_value);
                Ok(())
            } else {
                Err(ScopeError::CantReassignConstant(ident.to_string()))
            }
        } else {
            inner_scope.insert(ident.to_string(), wrapped_value);
            Ok(())
        }
    }

    pub fn get_var(&self, ident: &str) -> Result<Value, ScopeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(ident) {
                return Ok(value.get_value());
            }
        }
        Err(ScopeError::UnknownIdentifier(ident.to_string()))
    }

    // In contrast to set_var sets the var in the most inner scope so that it's always found first.
    // This is used when setting parameters.
    pub fn set_var_shadowed(&mut self, ident: &str, value: Value) {
        let inner_scope = self
            .scopes
            .last_mut()
            .expect("Tried accessing scope after last frame is gone.");

        // Can shadowed values be constant? No idea!
        inner_scope.insert(ident.to_string(), AssignedValue::Mutable(value));
    }
}

#[derive(Debug, Clone)]
enum AssignedValue {
    Constant(Value),
    Mutable(Value),
}

impl AssignedValue {
    fn get_value(&self) -> Value {
        match self {
            Self::Mutable(value) => value.clone(),
            Self::Constant(value) => match value {
                Value::Array(reference) => {
                    let underlying_value = reference.borrow().clone();
                    Value::Array(Rc::new(RefCell::new(underlying_value)))
                }
                Value::Fn(reference) => {
                    let underlying_value = reference.borrow().clone();
                    Value::Fn(Rc::new(RefCell::new(underlying_value)))
                }
                x => x.clone(),
            },
        }
    }

    fn is_constant(&self) -> bool {
        matches!(self, AssignedValue::Constant(_))
    }
}

#[derive(Error, Debug)]
pub enum ScopeError {
    #[error("Unknown identifier: '{0}'.")]
    UnknownIdentifier(Identifier),
    #[error("Unable to re-assign the constant '{0}'.")]
    CantReassignConstant(Identifier),
}
