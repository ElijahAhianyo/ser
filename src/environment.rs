use crate::ast::LiteralObject;
use crate::interpreter::RuntimeError;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::token::Token;
use std::collections::hash_map::Entry;
use std::rc::Rc;

#[derive(Debug, Clone, Default)]
pub struct Environment {
    map: HashMap<String, Option<LiteralObject>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing_env: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            map: HashMap::new(),
            enclosing: enclosing_env,
        }
    }

    pub fn get(&self, token: &Token) -> Result<Option<LiteralObject>, RuntimeError> {
        let key = token.lexeme().clone();
        if self.map.contains_key(&key) {
            let v = self.map.get(&key).unwrap().clone();
            Ok(v)
        } else {
            if let Some(enclosing) = &self.enclosing {
                return enclosing.borrow().get(token);
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
            }
            Entry::Vacant(_) => {
                if let Some(enclosing) = &mut self.enclosing {
                    return enclosing.borrow_mut().assign(key, value);
                }
                Err(RuntimeError::undefined_variable(key.lexeme().clone()))
            }
        }
    }
}
