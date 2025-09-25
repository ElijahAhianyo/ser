use crate::ast::{LiteralObject, Stmt};
use crate::callable::Callable;
use crate::environment::{EnvKey, Environment};
use crate::interpreter::{EvalResult, ExecFlow, Interpreter, RuntimeError};
use crate::token::Token;
use anyhow::Result;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub(crate) struct BuiltinFunction {
    name: String,
    arity: usize,
    f: Rc<dyn Fn(&mut Interpreter, Vec<LiteralObject>) -> EvalResult>,
}

impl BuiltinFunction {
    pub fn new<F>(name: String, arity: usize, f: F) -> Self
    where
        F: Fn(&mut Interpreter, Vec<LiteralObject>) -> EvalResult + 'static,
    {
        Self {
            name,
            arity,
            f: Rc::new(f),
        }
    }
}

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}>", self.name)
    }
}

impl Callable for BuiltinFunction {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<LiteralObject>,
    ) -> Result<LiteralObject, RuntimeError> {
        (self.f)(interpreter, args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}

#[derive(Debug, Clone)]
pub(crate) struct UserDefinedFunction {
    name: Option<String>, // option to support anonymous functions as well as named.
    params: Vec<Token>,
    body: Vec<Stmt>,
    parent_env: Rc<RefCell<Environment>>, // env where the function was created / defined.
}

impl UserDefinedFunction {
    pub fn new(
        name: Option<String>,
        params: Vec<Token>,
        body: Vec<Stmt>,
        parent_env: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            name,
            params,
            body,
            parent_env,
        }
    }
}

impl Display for UserDefinedFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}>", self.name)
    }
}

impl Callable for UserDefinedFunction {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<LiteralObject>,
    ) -> Result<LiteralObject, RuntimeError> {
        // we need to create a contained environment visible to only the function
        // at runtime(or when called) and this should include the scope of its parent.
        let env = Rc::new(RefCell::new(Environment::new(Some(
            self.parent_env.clone(),
        ))));

        for (i, param) in self.params.iter().enumerate() {
            let val = args.get(i).cloned().unwrap_or(LiteralObject::Nil);
            env.borrow_mut().set(EnvKey::Token(param), Some(val))?;
        }

        let exec_block = interpreter.execute_block(self.body.clone(), env)?;
        match exec_block {
            ExecFlow::Return(ret) => Ok(ret),
            // ignore all other flows for now.
            _ => Ok(LiteralObject::Nil),
        }
    }

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn name(&self) -> Option<String> {
        self.name.clone()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FnKind {
    Method,
    Named,
}
