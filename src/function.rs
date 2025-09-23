use std::cell::RefCell;
use std::rc::Rc;
use std::fmt::Display;
use crate::ast::{LiteralObject, Stmt};
use crate::callable::Callable;
use crate::environment::{EnvKey, Environment};
use crate::interpreter::{Interpreter, RuntimeError};
use crate::token::Token;
use anyhow::Result;

#[derive(Debug, Clone, Display)]
pub(crate) struct BuiltinFunction{
    name: String,
    arity: usize,
    f: Rc<dyn Fn(Interpreter, Vec<LiteralObject>) -> Result<LiteralObject, RuntimeError>>
}


impl BuiltinFunction {
    pub fn new<F>(name: String, arity: usize, f: F) -> Self
    where F: Fn(Interpreter, Vec<LiteralObject>) -> Result<LiteralObject, RuntimeError>{
        Self {name, arity, f: Rc::new(f)}
    }
}

impl Callable for BuiltinFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LiteralObject>) -> Result<LiteralObject, RuntimeError> {
        self.f(interpreter, args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}



#[derive(Debug, Clone, Display)]
pub(crate) struct UserDefinedFunction {
    name: Option<String>, // option to support anonymous functions as well as named.
    params: Vec<Token>,
    body: Vec<Stmt>,
    parent_env: Rc<RefCell<Environment>> // env where the function was created / defined.
}

impl UserDefinedFunction {
    pub fn new(name: Option<String>,params: Vec<Token>, body: Vec<Stmt>, parent_env: Rc<RefCell<Environment>>) -> Self {
        Self {name, params, body, parent_env}
    }
}

impl Callable for UserDefinedFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LiteralObject>) -> Result<LiteralObject, RuntimeError> {
        // we need to create a contained environment visible to only the function
        // at runtime(or when called) and this should include the scope of its parent.
        let env = Rc::new(RefCell::new(Environment::new(Some(self.parent_env.clone()))));

        for (i, param) in self.params.iter().enumerate() {
            let val = args.get(i).cloned().unwrap_or(LiteralObject::Nil);
            env.borrow_mut().set(EnvKey::Token(param), Some(val))?;
        }

        interpreter.execute_block(self.body.clone(), env).into()?;
        Ok(LiteralObject::Nil)

    }

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn name(&self) -> Option<String> {
        self.name.clone()
    }
}


#[derive(Debug, Clone. Copy)]
pub enum FnKind {
    Method,
    Named
}