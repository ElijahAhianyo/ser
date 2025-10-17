use crate::ast::{Expr, Stmt};
use crate::ast::{ExprNode, LiteralObject};
use crate::builtins::clock;
use crate::callable::Callable;
use crate::class::Class;
use crate::environment::{EnvKey, Environment};
use crate::function::{UserDefinedFunction, UserFnMeta};
use crate::token::{Token, TokenKind};
use anyhow::Result;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

const ERROR_PREFIX: &'static str = "Runtime Error: ";

#[derive(Debug, Clone)]
pub(crate) enum ExecFlow {
    Normal,
    Return(LiteralObject),
    Break,
    Continue,
}
pub(crate) type ExecResult = std::result::Result<ExecFlow, RuntimeError>;
pub(crate) type EvalResult = std::result::Result<LiteralObject, RuntimeError>;

#[derive(Debug, Clone, Error)]
pub enum RuntimeError {
    #[error("{ERROR_PREFIX} cannot perform operation {operation}")]
    InvalidOperation { token: Token, operation: String },

    #[error("{ERROR_PREFIX} undefined variable {name}")]
    UndefinedVariable { name: String },

    #[error("{ERROR_PREFIX} {message}")]
    Custom { message: String },

    #[error("{ERROR_PREFIX} could not find callee {name} on line {line}")]
    InvalidCallee { name: String, line: usize },

    #[error("{ERROR_PREFIX} {name} takes in {param_len} params, got {args_len} on line {line}")]
    InvalidArity {
        name: String,
        param_len: usize,
        args_len: usize,
        line: usize,
    },
}

impl RuntimeError {
    pub fn invalid_operation(token: Token, operation: String) -> Self {
        Self::InvalidOperation { operation, token }
    }

    pub fn undefined_variable(name: String) -> Self {
        Self::UndefinedVariable { name }
    }

    pub fn custom<M: Into<String>>(message: M) -> Self {
        Self::Custom {
            message: message.into(),
        }
    }

    pub fn invalid_callee(name: String, line: usize) -> Self {
        Self::InvalidCallee { name, line }
    }

    pub fn invalid_arity(name: String, param_len: usize, args_len: usize, line: usize) -> Self {
        Self::InvalidArity {
            name,
            param_len,
            args_len,
            line,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    locals: HashMap<String, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));
        let env = Rc::new(RefCell::new(Environment::new(Some(globals.clone()))));

        let mut this = Self {
            env,
            globals,
            locals: HashMap::new(),
        };

        this.load_globals().expect("could not set global variables");
        this
    }

    fn load_globals(&mut self) -> std::result::Result<&mut Self, RuntimeError> {
        let clk = LiteralObject::Callable(Rc::new(clock()));
        self.globals
            .borrow_mut()
            .set(EnvKey::String("clock"), Some(clk))?;
        Ok(self)
    }
    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> std::result::Result<(), RuntimeError> {
        for stmt in &stmts {
            self.execute_statement(stmt)?; // ignore any returns at top-level
        }

        Ok(())
    }

    pub fn evaluate(&mut self, node: ExprNode) -> EvalResult {
        let expr_id = node.id();
        match node.expr() {
            Expr::Literal(val) => Ok(val.clone()),
            Expr::Grouping(expr_node) => Ok(self.evaluate(*expr_node.clone())?),
            Expr::Unary(op, expr_node) => {
                let right = self.evaluate(*expr_node.clone())?;

                match op.token_type() {
                    TokenKind::MINUS => match &right {
                        LiteralObject::Number(r) => Ok(LiteralObject::Number(r * -1f64)),
                        _ => Err(
                            RuntimeError::invalid_operation(op.clone(), format!("-{right}")).into(),
                        ),
                    },
                    TokenKind::BANG => Ok(LiteralObject::Bool(!self.is_truthy(&right))),

                    _ => Ok(LiteralObject::Nil),
                }
            }
            Expr::Binary(left, op, right) => {
                let left = self.evaluate(*left.clone())?;
                let right = self.evaluate(*right.clone())?;

                match op.token_type() {
                    TokenKind::MINUS => match (&left, &right) {
                        (LiteralObject::Number(l), LiteralObject::Number(r)) => {
                            Ok(LiteralObject::Number(l - r))
                        }
                        _ => Err(RuntimeError::invalid_operation(
                            op.clone(),
                            format!("{left:?} + {right:?}"),
                        )
                        .into()),
                    },
                    TokenKind::SLASH => match (&left, &right) {
                        (LiteralObject::Number(l), LiteralObject::Number(r)) => {
                            Ok(LiteralObject::Number(l / r))
                        }
                        _ => Err(RuntimeError::invalid_operation(
                            op.clone(),
                            format!("{left:?} / {right:?}"),
                        )
                        .into()),
                    },
                    TokenKind::STAR => match (&left, &right) {
                        (LiteralObject::Number(l), LiteralObject::Number(r)) => {
                            Ok(LiteralObject::Number(l * r))
                        }
                        _ => Err(RuntimeError::invalid_operation(
                            op.clone(),
                            format!("{left:?} * {right:?}"),
                        )
                        .into()),
                    },
                    TokenKind::PLUS => match (&left, &right) {
                        (LiteralObject::Number(l), LiteralObject::Number(r)) => {
                            Ok(LiteralObject::Number(l + r))
                        }
                        (LiteralObject::Str(l), LiteralObject::Str(r)) => {
                            Ok(LiteralObject::Str(l.clone() + &*r))
                        }
                        _ => panic!("+ operand not supported between {left:?} and {right:?}"),
                    },
                    TokenKind::GREATER => match (&left, &right) {
                        (LiteralObject::Number(l), LiteralObject::Number(r)) => {
                            Ok(LiteralObject::Bool(l > r))
                        }
                        _ => Err(RuntimeError::invalid_operation(
                            op.clone(),
                            format!("{left:?} > {right:?}"),
                        )
                        .into()),
                    },
                    TokenKind::GREATEREQUAL => match (&left, &right) {
                        (LiteralObject::Number(l), LiteralObject::Number(r)) => {
                            Ok(LiteralObject::Bool(l >= r))
                        }
                        _ => Err(RuntimeError::invalid_operation(
                            op.clone(),
                            format!("{left:?} >= {right:?}"),
                        )
                        .into()),
                    },
                    TokenKind::LESS => match (&left, &right) {
                        (LiteralObject::Number(l), LiteralObject::Number(r)) => {
                            Ok(LiteralObject::Bool(l < r))
                        }
                        _ => Err(RuntimeError::invalid_operation(
                            op.clone(),
                            format!("{left:?} < {right:?}"),
                        )
                        .into()),
                    },
                    TokenKind::LESSEQUAL => match (&left, &right) {
                        (LiteralObject::Number(l), LiteralObject::Number(r)) => {
                            Ok(LiteralObject::Bool(l <= r))
                        }
                        _ => Err(RuntimeError::invalid_operation(
                            op.clone(),
                            format!("{left:?} <= {right:?}"),
                        )
                        .into()),
                    },
                    TokenKind::BANGEQUAL => Ok(LiteralObject::Bool(!self.is_equal(left, right))),
                    TokenKind::EQUALEQUAL => Ok(LiteralObject::Bool(self.is_equal(left, right))),
                    _ => Ok(LiteralObject::Nil),
                }
            }

            Expr::Var(v) => match self.lookup_variable(v, expr_id)? {
                Some(val) => Ok(val),
                None => Err(RuntimeError::custom(format!(
                    "variable '{}' is uninitialized",
                    v.lexeme()
                ))
                .into()),
            },

            Expr::Assignment(lhs, rhs) => {
                let value = self.evaluate(*rhs.clone())?;
                if let Some(distance) = self.locals.get(rhs.id()) {
                    self.env.borrow_mut().assign_at(
                        self.env.clone(),
                        *distance,
                        EnvKey::Token(&lhs),
                        value.clone(),
                    )?
                }
                // fallback on globals
                else {
                    self.env
                        .borrow_mut()
                        .assign(EnvKey::Token(&lhs), value.clone())?
                }

                Ok(value)
            }

            Expr::Logical(lhs, op, rhs) => {
                let lhs = self.evaluate(*lhs.clone())?;

                if *op.token_type() == TokenKind::OR {
                    if self.is_truthy(&lhs.clone()) {
                        return Ok(lhs);
                    }
                } else if !self.is_truthy(&lhs.clone()) {
                    return Ok(lhs);
                };

                self.evaluate(*rhs.clone())
            }

            Expr::Call {
                callee,
                paren,
                args,
            } => {
                let callee = self.evaluate(*callee.clone())?;

                let callee = match callee {
                    LiteralObject::Callable(callable) => callable,
                    _ => {
                        return Err(RuntimeError::invalid_callee(
                            callee.to_string(),
                            paren.line(),
                        ));
                    }
                };

                if callee.arity() != args.len() {
                    let name = callee
                        .name()
                        .cloned()
                        .unwrap_or_else(|| "anonymous".to_string());
                    return Err(RuntimeError::invalid_arity(
                        name,
                        callee.arity(),
                        args.len(),
                        paren.line(),
                    ));
                }

                let mut arg_list: Vec<LiteralObject> = Vec::new();
                for arg in args {
                    arg_list.push(self.evaluate(arg.clone())?);
                }

                callee.call(self, arg_list)
            }

            Expr::Get { obj, name } => {
                let obj = self.evaluate(*(*obj).clone())?;

                match &obj {
                    LiteralObject::Instance(instance) => instance
                        .borrow_mut()
                        .get(name.lexeme().to_string())
                        .ok_or_else(|| {
                            RuntimeError::invalid_operation(
                                name.clone(),
                                format!(
                                    "get on {}. no prop named {:?}",
                                    obj.clone().to_string(),
                                    name.lexeme()
                                ),
                            )
                        }),
                    _ => Err(RuntimeError::invalid_operation(
                        name.clone(),
                        format!(
                            "get on {}. Only instances can have properties",
                            obj.to_string()
                        ),
                    )),
                }
            }

            Expr::Set { obj, name, value } => {
                let obj = self.evaluate(*(*obj).clone())?;
                match obj {
                    LiteralObject::Instance(instance) => {
                        let value = self.evaluate(*value.clone())?;
                        instance
                            .borrow_mut()
                            .set(name.lexeme().clone(), value.clone());
                        Ok(value)
                    }
                    _ => Err(RuntimeError::invalid_operation(
                        name.clone(),
                        format!("set on {}", obj.to_string()),
                    )),
                }
            }

            Expr::This(token) => {
                let this = self.lookup_variable(token, expr_id)?;
                match this {
                    Some(obj) => Ok(obj.clone()),
                    None => Err(RuntimeError::undefined_variable(token.lexeme().to_string())),
                }
            }

            Expr::Super(kw, method) => {
                // we need to find the superclass and the instance of "this" in the environment
                // at the distance recorded for "super" and "this" respectively.
                // then we need to find the method on the superclass and bind it to the instance of "this"
                // and return the bound method as a LiteralObject::Callable
                // if any of these steps fail, we return an appropriate runtime error.

                // TODO: refactor this
                let distance = self.locals.get(&method.lexeme().clone()).unwrap();
                let super_class = self.env.borrow().get_at(
                    self.env.clone(),
                    *distance,
                    EnvKey::String("super"),
                )?;
                let object = self.env.borrow().get_at(
                    self.env.clone(),
                    *distance - 1,
                    EnvKey::String("this"),
                )?;
                if let Some(super_class) = super_class {
                    if let LiteralObject::Callable(callable) = super_class {
                        let class = callable
                            .as_any()
                            .downcast_ref::<Class>()
                            .ok_or(RuntimeError::custom("super must be a class"))?;
                        if let Some(method) = class.get_method(&method.lexeme()) {
                            if let Some(object) = object {
                                if let LiteralObject::Instance(instance) = object {
                                    return method.clone().bind(instance);
                                } else {
                                    return Err(RuntimeError::custom(
                                        "could not find instance for 'this'",
                                    ));
                                }
                            } else {
                                return Err(RuntimeError::custom(
                                    "could not find instance for 'this'",
                                ));
                            }
                        } else {
                            return Err(RuntimeError::custom(format!(
                                "could not find method {} on superclass {}",
                                method.lexeme(),
                                class.name()
                            )));
                        }
                    }
                }
                Err(RuntimeError::custom("super must be a class"))
            }
        }
    }

    pub(crate) fn execute_statement(&mut self, stmt: &Stmt) -> ExecResult {
        match stmt {
            Stmt::ExprStmt(s) => {
                self.evaluate(*s.clone())?;
                Ok(ExecFlow::Normal)
            }
            Stmt::PrintStmt(s) => {
                let val = self.evaluate(*s.clone())?;
                println!("{}", val.to_string());
                Ok(ExecFlow::Normal)
            }

            Stmt::VarDeclaration(token, expr) => {
                let mut value: Option<LiteralObject> = None;
                if let Some(expr) = expr {
                    value = Some(self.evaluate(*expr.clone())?);
                }

                self.env.borrow_mut().set(EnvKey::Token(&token), value)?;
                Ok(ExecFlow::Normal)
            }

            Stmt::Block(block) => {
                let exec_blk = self.execute_block(
                    block.clone(),
                    Rc::new(RefCell::new(Environment::new(Some(self.env.clone())))),
                )?;
                Ok(exec_blk)
            }

            Stmt::If(cond, then_block, else_block) => {
                let cond_eval = self.evaluate(*cond.clone())?;
                if self.is_truthy(&cond_eval) {
                    return Ok(self.execute_statement(&*then_block)?);
                } else if let Some(else_block) = else_block {
                    return Ok(self.execute_statement(&*else_block)?);
                }
                Ok(ExecFlow::Normal)
            }

            Stmt::While(cond, body) => {
                loop {
                    let cond_val = self.evaluate(*(*cond).clone())?;
                    if !self.is_truthy(&cond_val) {
                        break;
                    }

                    let exec_stmt = self.execute_statement(&*body)?;
                    if let ExecFlow::Return(_) = &exec_stmt {
                        return Ok(exec_stmt);
                    }
                }
                Ok(ExecFlow::Normal)
            }

            Stmt::Fn(name, params, body) => {
                let meta = UserFnMeta::new(
                    Some(name.lexeme().to_string()),
                    params.clone(),
                    body.clone(),
                );
                let func = UserDefinedFunction::new(meta, self.env.clone(), false);
                // whenever we define a fn(or come across a fn declaration stmt), we should save this in the environment so it can be looked up
                let func_obj = LiteralObject::Callable(Rc::new(func));
                self.env
                    .borrow_mut()
                    .set(EnvKey::Token(name), Some(func_obj))?;
                Ok(ExecFlow::Normal)
            }

            Stmt::Class(name, super_class, methods) => {
                let super_class = if let Some(sc) = super_class {
                    if let Ok(LiteralObject::Callable(callable)) = self.evaluate(*sc.clone()) {
                        let c = callable
                            .as_any()
                            .downcast_ref::<Class>()
                            .cloned()
                            .ok_or_else(|| {
                                RuntimeError::custom("superclass must be a class name")
                            })?;
                        Some(c)
                    } else {
                        return Err(RuntimeError::custom("superclass must be a class name"));
                    }
                } else {
                    None
                };

                self.env.borrow_mut().set(EnvKey::Token(name), None)?;

                if let Some(super_class) = &super_class {
                    let enclosing_env = self.env.clone();
                    let super_env = Rc::new(RefCell::new(Environment::new(Some(enclosing_env))));
                    super_env.borrow_mut().set(
                        EnvKey::String("super"),
                        Some(LiteralObject::Callable(Rc::new(super_class.clone()))),
                    )?;
                    self.env = super_env;
                }

                let mut method_map: HashMap<String, UserDefinedFunction> = HashMap::new();
                for method in methods {
                    match method {
                        Stmt::Fn(name, params, body) => {
                            let func = {
                                let meta = UserFnMeta::new(
                                    Some(name.lexeme().to_string()),
                                    params.clone(),
                                    body.clone(),
                                );
                                UserDefinedFunction::new(
                                    meta,
                                    self.env.clone(),
                                    name.lexeme() == "init",
                                )
                            };
                            method_map.insert(name.lexeme().to_string(), func);
                        }
                        _ => {}
                    }
                }

                let class = LiteralObject::Callable(Rc::new(Class::new(
                    name.lexeme().to_string(),
                    method_map,
                    super_class.clone(),
                )));

                // restore the previous environment if we had a superclass
                if super_class.is_some() {
                    let enclosing = self.env.borrow().enclosing().clone().unwrap();
                    self.env = enclosing;
                }
                self.env.borrow_mut().assign(EnvKey::Token(name), class)?;
                Ok(ExecFlow::Normal)
            }

            Stmt::Return(_, value) => {
                let value = if let Some(expr_node) = value {
                    self.evaluate(*expr_node.clone())?
                } else {
                    LiteralObject::Nil
                };
                Ok(ExecFlow::Return(value))
            }
        }
    }

    pub(crate) fn execute_block(
        &mut self,
        statements: Vec<Stmt>,
        environment: Rc<RefCell<Environment>>,
    ) -> ExecResult {
        let previous_env = std::mem::take(&mut self.env);
        let _ = std::mem::replace(&mut self.env, environment.clone());

        for stmt in &statements {
            let exec_stmt = self.execute_statement(&*stmt)?;
            match &exec_stmt {
                ExecFlow::Return(_) => {
                    let _ = std::mem::replace(&mut self.env, previous_env);
                    return Ok(exec_stmt);
                }
                _ => {}
            }
        }
        let _ = std::mem::replace(&mut self.env, previous_env);
        Ok(ExecFlow::Normal)
    }

    pub(crate) fn record_resolution(&mut self, expr_id: String, depth: usize) {
        // Record the lexical distance (scope depth) for this expression id.
        // If an entry already existed, we simply overwrite it.
        self.locals.insert(expr_id, depth);
    }

    fn lookup_variable(
        &self,
        name: &Token,
        expr_id: &String,
    ) -> Result<Option<LiteralObject>, RuntimeError> {
        if let Some(distance) = self.locals.get(expr_id) {
            self.env
                .borrow()
                .get_at(self.env.clone(), *distance, EnvKey::Token(name))
        } else {
            // search the current environment which has globals as its ancestor(check how this is created in the constructor)
            self.env.borrow().get(EnvKey::Token(name))
        }
    }

    pub fn is_truthy(&self, object: &LiteralObject) -> bool {
        match object {
            LiteralObject::Nil => false,
            LiteralObject::Bool(val) => *val,
            _ => true,
        }
    }

    pub fn is_equal(&self, left: LiteralObject, right: LiteralObject) -> bool {
        match (&left, &right) {
            (LiteralObject::Nil, LiteralObject::Nil) => true,
            (LiteralObject::Nil, _) => false,
            (LiteralObject::Str(l), LiteralObject::Str(r)) => l == r,
            _ => false,
        }
    }
}
