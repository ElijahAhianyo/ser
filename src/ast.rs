use crate::token::Token;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::callable::Callable;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(LiteralObject),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Assignment(Token, Box<Expr>),
    Var(Token),
    Logical(Box<Expr>, Token, Box<Expr>),
    Call {
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr >
    },

}

#[derive(Debug, Clone)]
pub enum VarDecl {
    Name(Token),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt(Box<Expr>),
    PrintStmt(Box<Expr>),
    VarDeclaration(Token, Option<Box<Expr>>),
    Block(Vec<Stmt>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
    // name, params, body
    Fn(Token, Vec<Token>, Vec<Stmt>)
}

#[derive(Debug, Clone)]
pub(crate) enum LiteralObject {
    Number(f64),
    Str(String),
    Bool(bool),
    Nil,
    Callable(Rc<dyn Callable>) // Rc here so the callable is thread-safe. cheap clones.
}

impl Display for LiteralObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            LiteralObject::Number(num) => {
                let x = num.to_string();
                if x.ends_with(".0") {
                    x.strip_suffix(".0").unwrap().to_string()
                } else {
                    x
                }
            }
            LiteralObject::Nil => "nil".to_string(),
            LiteralObject::Str(val) => val.clone(),
            LiteralObject::Bool(b) => b.to_string(),
            LiteralObject::Callable(c) => c.to_string()
        };

        write!(f, "{}", display)
    }
}
