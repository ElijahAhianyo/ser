use crate::ast::{LiteralObject, Stmt};
use crate::callable::Callable;
use crate::class::Instance;
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
pub(crate) struct UserFnMeta {
    name: Option<String>, // option to support anonymous functions as well as named.
    params: Vec<Token>,
    body: Vec<Stmt>,
}

impl UserFnMeta {
    pub fn new(name: Option<String>, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        Self { name, params, body }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct UserDefinedFunction {
    meta: UserFnMeta,
    is_init: bool, // whether this function is an initializer (constructor) for a class
    parent_env: Rc<RefCell<Environment>>, // env(closure) where the function was created / defined.
}

impl UserDefinedFunction {
    pub fn new(meta: UserFnMeta, parent_env: Rc<RefCell<Environment>>, is_init: bool) -> Self {
        Self {
            meta,
            parent_env,
            is_init,
        }
    }

    pub fn bind(&mut self, instance: Rc<RefCell<Instance>>) -> Result<LiteralObject, RuntimeError> {
        // when binding a method to an instance, we need to create a new environment
        // that has the instance as "this" and the parent environment as the function's parent
        // environment.
        let mut environ = Environment::new(Some(self.parent_env.clone()));
        environ.set(
            EnvKey::String("this"),
            Some(LiteralObject::Instance(instance.clone())),
        )?;
        let literal = LiteralObject::Callable(Rc::new(UserDefinedFunction {
            meta: self.meta.clone(),
            parent_env: Rc::new(RefCell::new(environ)),
            is_init: self.is_init,
        }));
        Ok(literal)
    }
}

impl Display for UserDefinedFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<{:?}>",
            self.meta.name.as_ref().unwrap_or(&"anonymous".to_string())
        )
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

        for (i, param) in self.meta.params.iter().enumerate() {
            let val = args.get(i).cloned().unwrap_or(LiteralObject::Nil);
            env.borrow_mut().set(EnvKey::Token(param), Some(val))?;
        }

        let exec_block = interpreter.execute_block(self.meta.body.clone(), env)?;
        match exec_block {
            ExecFlow::Return(ret) => Ok(ret),
            // ignore all other flows for now.
            _ if self.is_init => {
                // if this is an initializer, we need to return the `this`/current instance which lives in the parent env.
                if let Some(LiteralObject::Instance(instance)) = self.parent_env.borrow().get_at(
                    self.parent_env.clone(),
                    0,
                    EnvKey::String("this"),
                )? {
                    return Ok(LiteralObject::Instance(instance));
                }
                Ok(LiteralObject::Nil)
            }
            _ => Ok(LiteralObject::Nil),
        }
    }

    fn arity(&self) -> usize {
        self.meta.params.len()
    }

    fn name(&self) -> Option<&String> {
        self.meta.name.as_ref()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FnKind {
    Method,
    Function,
    Initializer,
    None,
}
