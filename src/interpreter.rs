use crate::ast::LiteralObject;
use crate::ast::{Expr, Stmt};
use crate::environment::Environment;
use crate::token::{Token, TokenKind};
use anyhow::Result;
use std::cell::RefCell;
use std::rc::Rc;
use thiserror::Error;

const ERROR_PREFIX: &'static str = "Runtime Error: ";
#[derive(Debug, Clone, Error)]
pub enum RuntimeError {
    #[error("{ERROR_PREFIX} cannot perform operation {operation}")]
    InvalidOperation { token: Token, operation: String },

    #[error("{ERROR_PREFIX} undefined variable {name}")]
    UndefinedVariable { name: String },

    #[error("{ERROR_PREFIX} {message}")]
    Custom { message: String },
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
}

#[derive(Debug, Clone, Default)]
pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new(None))),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<()> {
        for stmt in &stmts {
            self.execute_statement(stmt)?
        }

        Ok(())
    }

    pub fn evaluate(&mut self, node: Expr) -> Result<LiteralObject> {
        match node {
            Expr::Literal(val) => Ok(val.clone()),
            Expr::Grouping(expr) => Ok(self.evaluate(*expr.clone())?),
            Expr::Unary(op, expr) => {
                let right = self.evaluate(*expr)?;

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
                let left = self.evaluate(*left)?;
                let right = self.evaluate(*right)?;

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

            Expr::Var(v) => match self.env.borrow().get(&v)? {
                Some(val) => Ok(val),
                None => Err(RuntimeError::custom(format!(
                    "variable '{}' is uninitialized",
                    v.lexeme()
                ))
                .into()),
            },

            Expr::Assignment(lhs, rhs) => {
                let value = self.evaluate(*rhs)?;
                self.env.borrow_mut().assign(&lhs, value.clone()).unwrap(); // TODO: we cant use unwrap here
                Ok(value)
            }

            Expr::Logical(lhs, op, rhs) => {
                let lhs = self.evaluate(*lhs)?;

                if *op.token_type() == TokenKind::OR {
                    if self.is_truthy(&lhs.clone()) {
                        return Ok(lhs);
                    }
                } else if !self.is_truthy(&lhs.clone()) {
                    return Ok(lhs);
                };

                self.evaluate(*rhs)
            }
        }
    }

    fn execute_statement(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::ExprStmt(s) => {
                self.evaluate(*s.clone())?;
            }
            Stmt::PrintStmt(s) => {
                let val = self.evaluate(*s.clone())?;
                println!("{}", val.to_string());
            }

            Stmt::VarDeclaration(token, expr) => {
                let mut value: Option<LiteralObject> = None;
                if let Some(expr) = expr {
                    value = Some(self.evaluate(*expr.clone())?);
                }

                self.env.borrow_mut().set(&token, value)?;
            }

            Stmt::Block(block) => self.execute_block(
                block.clone(),
                Rc::new(RefCell::new(Environment::new(Some(self.env.clone())))),
            )?,

            Stmt::If(cond, then_block, else_block) => {
                let cond_eval = self.evaluate(*cond.clone())?;
                if self.is_truthy(&cond_eval) {
                    self.execute_statement(&*then_block)?
                } else if let Some(else_block) = else_block {
                    self.execute_statement(&*else_block)?;
                }
            }

            Stmt::While(cond, body) => loop {
                let cond_val = self.evaluate(*(*cond).clone())?;
                if !self.is_truthy(&cond_val) {
                    break;
                }

                self.execute_statement(&*body)?
            },
        }
        Ok(())
    }

    fn execute_block(
        &mut self,
        statements: Vec<Box<Stmt>>,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<()> {
        let previous_env = std::mem::take(&mut self.env);
        let _ = std::mem::replace(&mut self.env, environment);

        for stmt in &statements {
            self.execute_statement(&*stmt)?
        }

        let _ = std::mem::replace(&mut self.env, previous_env);
        Ok(())
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
