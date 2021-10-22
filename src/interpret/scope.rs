use super::value::Value;
use crate::parse::ast::nodes::Identifier;
use std::collections::HashMap;

pub struct Scope {
    scopes: Vec<HashMap<Identifier, Value>>,
}

impl Scope {
    pub fn new() -> Self {
        Scope { scopes: Vec::new() }
    }

    pub fn nest(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn unnest(&mut self) {
        self.scopes.pop();
    }

    pub fn set_var(&mut self, ident: &str, value: Value) {
        for scope in self.scopes.iter_mut() {
            if scope.contains_key(ident) {
                scope.insert(ident.to_string(), value);
                return;
            }
        }

        let inner_scope = self
            .scopes
            .last_mut()
            .expect("Tried accessing scope after last frame is gone.");
        inner_scope.insert(ident.to_string(), value);
    }

    pub fn get_var(&self, ident: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(ident) {
                return Some(value.clone());
            }
        }
        None
    }
}
