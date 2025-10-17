use crate::ast::LiteralObject;
use crate::interpreter::RuntimeError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

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

    pub fn get(&self, key: EnvKey) -> Result<Option<LiteralObject>, RuntimeError> {
        let key_str = match key {
            EnvKey::Token(token) => token.lexeme().clone(),
            EnvKey::String(name) => name.to_string(),
        };

        if self.map.contains_key(&key_str) {
            let v = self.map.get(&key_str).unwrap().clone();
            Ok(v)
        } else {
            if let Some(enclosing) = &self.enclosing {
                return enclosing.borrow().get(key);
            }
            Err(RuntimeError::undefined_variable(key_str.clone()))
        }
    }

    pub fn get_at(
        &self,
        root_env: Rc<RefCell<Environment>>,
        distance: usize,
        name: EnvKey,
    ) -> Result<Option<LiteralObject>, RuntimeError> {
        if let Some(ancestor) = self.ancestor(root_env, distance) {
            return ancestor.borrow().get(name);
        }
        // This branch should be unreachable if the resolver is correct.
        panic!(
            "Environment::get_at: unreachable code reached; resolver should guarantee variable exists at the given distance"
        );
    }

    pub fn set(&mut self, key: EnvKey, value: Option<LiteralObject>) -> Result<(), RuntimeError> {
        let key = match key {
            EnvKey::Token(token) => token.lexeme().clone(),
            EnvKey::String(name) => name.to_string(),
        };
        self.map.insert(key, value);
        Ok(())
    }

    pub fn assign(&mut self, key: EnvKey, value: LiteralObject) -> Result<(), RuntimeError> {
        let key = match key {
            EnvKey::Token(token) => token.lexeme().clone(),
            EnvKey::String(name) => name.to_string(),
        };

        match self.map.entry(key.clone()) {
            Entry::Occupied(mut entry) => {
                entry.insert(Some(value));
                Ok(())
            }
            Entry::Vacant(_) => {
                if let Some(enclosing) = &mut self.enclosing {
                    return enclosing
                        .borrow_mut()
                        .assign(EnvKey::String(key.as_str()), value);
                }
                Err(RuntimeError::undefined_variable(key))
            }
        }
    }

    pub fn assign_at(
        &mut self,
        root_env: Rc<RefCell<Environment>>,
        distance: usize,
        key: EnvKey,
        value: LiteralObject,
    ) -> Result<(), RuntimeError> {
        if let Some(ancestor) = self.ancestor(root_env, distance) {
            return ancestor.borrow_mut().assign(key, value);
        }
        Ok(())
    }

    pub fn ancestor(
        &self,
        env: Rc<RefCell<Environment>>,
        distance: usize,
    ) -> Option<Rc<RefCell<Environment>>> {
        let mut environment = Some(env);
        for _ in 0..distance {
            if let Some(env) = environment {
                environment = env.borrow().enclosing.clone();
            } else {
                return None;
            }
        }
        environment
    }

    pub fn enclosing(&self) -> Option<Rc<RefCell<Environment>>> {
        self.enclosing.clone()
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Environment<Map<{:?}> Enclosing<{:?}>>",
            self.map, self.enclosing
        )
    }
}

pub enum EnvKey<'a> {
    Token(&'a Token),
    String(&'a str),
}
