use std::collections::HashMap;
use crate::interpreter::RuntimeError;
use crate::ast::LiteralObject;

use std::collections::hash_map::Entry;
use crate::token::Token;

#[derive(Debug, Clone, Default)]
pub struct Environment{
    map: HashMap<String, Option<LiteralObject>>,
    enclosing: Option<Box<Environment>>
}


impl Environment {
    pub fn new(enclosing_env: Option<Environment>) -> Self {
        let enclosing_env = enclosing_env.map(|env| Box::new(env));
        Self { map: HashMap::new(), enclosing: enclosing_env}
    }

    pub fn get(&self, token: &Token) -> Result<Option<LiteralObject>, RuntimeError> {
        let key = token.lexeme().clone();
        if self.map.contains_key(&key) {
            let v = self.map.get(&key).unwrap().clone();
            Ok(v)
        }
        else {
            if let Some(enclosing) = &self.enclosing {
                return enclosing.get(token)
            }
            Err(RuntimeError::undefined_variable(key.clone()))
        }

    }

    pub fn set(&mut self, key: &Token, value: Option<LiteralObject>) -> Result<(), RuntimeError> {
        self.map.insert(key.lexeme().clone(), value);
        Ok(())
    }

    pub fn assign(&mut self, key: &Token, value: LiteralObject) -> Result<(), RuntimeError> {
        match self.map.entry(key.lexeme().clone()) {
            Entry::Occupied(mut entry) => {
                entry.insert(Some(value));
                Ok(())
            },
            Entry::Vacant(_) => {
                if let Some(enclosing) = &mut self.enclosing{
                        return enclosing.assign(key, value)
                }
                Err(RuntimeError::undefined_variable(key.lexeme().clone()))
            }
        }
    }
}
