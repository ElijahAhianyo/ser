use crate::ast::{Expr, ExprNode, Stmt};
use crate::interpreter::Interpreter;
use crate::token::Token;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ResolverError {
    #[error("Variable '{0}' is not initialized before use.")]
    VariableNotInitialized(String),
    #[error("Variable '{0}' is already declared in this scope.")]
    VariableAlreadyDeclared(String),
    #[error("Cannot return from top-level code at line {0}.")]
    ReturnFromTopLevel(usize),
}

#[derive(Debug)]
pub struct Resolver<'a> {
    scopes: Vec<HashMap<String, bool>>,
    interpreter: &'a mut Interpreter,
    fn_depth: usize,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            scopes: Vec::new(),
            interpreter,
            fn_depth: 0,
        }
    }

    pub fn resolve(&mut self, statements: &[Stmt]) -> Result<(), ResolverError> {
        for stmt in statements {
            self.resolve_statement(stmt)?
        }
        Ok(())
    }

    fn resolve_statement(&mut self, statement: &Stmt) -> Result<(), ResolverError> {
        match statement {
            Stmt::VarDeclaration(identifier, expr) => {
                self.declare(identifier)?;
                if let Some(expr_node) = expr {
                    self.resolve_expr(&*expr_node)?
                }
                self.define(identifier);
            }
            Stmt::Block(stmts) => {
                self.begin_scope();
                self.resolve(stmts)?;
                self.end_scope();
            }

            Stmt::Fn(ident, params, body) => {
                self.declare(ident)?;
                self.define(ident);

                // Enter function context
                let enclosing_depth = self.fn_depth;
                self.fn_depth += 1;

                self.begin_scope();
                for p in params {
                    self.declare(p)?;
                    self.define(p);
                }
                self.resolve(body)?;
                self.end_scope();

                // Restore previous function context
                self.fn_depth = enclosing_depth;
            }

            Stmt::ExprStmt(expr) => self.resolve_expr(expr)?,

            Stmt::If(cond, then, else_) => {
                self.resolve_expr(&(cond))?;
                self.resolve_statement(&*then)?;
                if let Some(else_) = else_ {
                    self.resolve_statement(&*else_)?
                }
            }
            Stmt::While(cond, body) => {
                self.resolve_expr(&*cond)?;
                self.resolve_statement(&*body)?;
            }
            Stmt::PrintStmt(expr) => {
                self.resolve_expr(&*expr)?;
            }
            Stmt::Return(keyword, value) => {
                if self.fn_depth == 0 {
                    return Err(ResolverError::ReturnFromTopLevel(keyword.line()));
                }
                if let Some(value) = value {
                    self.resolve_expr(&*value)?;
                }
            }
        }

        Ok(())
    }

    fn resolve_expr(&mut self, expr_node: &ExprNode) -> Result<(), ResolverError> {
        let expr = expr_node.expr();
        match expr {
            Expr::Var(token) => {
                // If the variable is declared but not defined, we're trying
                // to access an uninitialized variable. which is illegal
                if !self.scopes.is_empty() {
                    if let Some(scope) = self.scopes.last_mut() {
                        if let Some(defined) = scope.get(token.lexeme()) {
                            if !defined {
                                return Err(ResolverError::VariableNotInitialized(
                                    token.lexeme().clone(),
                                ));
                            }
                        }
                    }
                }

                self.resolve_local(token.lexeme(), expr_node)?
            }

            Expr::Assignment(identifier, expr) => {
                self.resolve_expr(&*expr)?;
                self.resolve_local(identifier.lexeme(), expr_node)?;
            }

            Expr::Literal(literal) => {}
            Expr::Unary(op, expr) => {
                self.resolve_expr(&*expr)?;
            }
            Expr::Binary(lhs, op, rhs) => {
                self.resolve_expr(&*lhs)?;
                self.resolve_expr(&*rhs)?;
            }
            Expr::Grouping(expr) => {
                self.resolve_expr(&*expr)?;
            }
            Expr::Logical(lhs, op, rhs) => {
                self.resolve_expr(&*lhs)?;
                self.resolve_expr(&*rhs)?;
            }
            Expr::Call {
                callee,
                paren,
                args,
            } => {
                self.resolve_expr(&*callee)?;
                for arg in args {
                    self.resolve_expr(arg)?
                }
            }
        }
        Ok(())
    }

    fn resolve_local(&mut self, name: &str, expr: &ExprNode) -> Result<(), ResolverError> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(exists) = scope.get(name) {
                self.interpreter.record_resolution(expr.id().clone(), i)
            }
        }
        Ok(())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn declare(&mut self, token: &Token) -> Result<(), ResolverError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(token.lexeme()) {
                return Err(ResolverError::VariableAlreadyDeclared(
                    token.lexeme().clone(),
                ));
            }
            scope.insert(token.lexeme().clone(), false);
        }
        Ok(())
    }

    fn define(&mut self, token: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(token.lexeme().clone(), true);
        }
    }
}
