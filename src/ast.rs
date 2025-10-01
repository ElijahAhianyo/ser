use crate::callable::Callable;
use crate::token::Token;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use uuid::Timestamp;
use uuid::{NoContext, Uuid};

#[derive(Debug, Clone)]
pub struct ExprNode {
    expr: Expr,
    id: String,
}

fn generate_uuid7() -> String {
    let ts = Timestamp::now(NoContext);
    let uuid = Uuid::new_v7(ts);
    uuid.to_string()
}

impl ExprNode {
    pub fn new(expr: Expr) -> Self {
        let id = generate_uuid7();
        Self { expr, id }
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }

    pub fn id(&self) -> &String {
        &self.id
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(LiteralObject),
    Unary(Token, Box<ExprNode>),
    Binary(Box<ExprNode>, Token, Box<ExprNode>),
    Grouping(Box<ExprNode>),
    Assignment(Token, Box<ExprNode>),
    Var(Token),
    Logical(Box<ExprNode>, Token, Box<ExprNode>),
    Call {
        callee: Box<ExprNode>,
        paren: Token,
        args: Vec<ExprNode>,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt(Box<ExprNode>),
    PrintStmt(Box<ExprNode>),
    VarDeclaration(Token, Option<Box<ExprNode>>),
    Block(Vec<Stmt>),
    If(Box<ExprNode>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<ExprNode>, Box<Stmt>),
    // name, params, body
    Fn(Token, Vec<Token>, Vec<Stmt>),
    //keyword, value
    Return(Token, Option<Box<ExprNode>>),
}

#[derive(Clone)]
pub(crate) enum LiteralObject {
    Number(f64),
    Str(String),
    Bool(bool),
    Nil,
    Callable(Rc<dyn Callable>), // Rc here so the callable is thread-safe. cheap clones.
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
            LiteralObject::Callable(c) => c.to_string(),
        };

        write!(f, "{}", display)
    }
}

impl std::fmt::Debug for LiteralObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralObject::Number(num) => f.debug_tuple("Number").field(num).finish(),
            LiteralObject::Str(val) => f.debug_tuple("Str").field(val).finish(),
            LiteralObject::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
            LiteralObject::Nil => f.debug_tuple("Nil").finish(),
            LiteralObject::Callable(c) => f
                .debug_tuple("Callable")
                .field(&format!("<{}>", c.to_string()))
                .finish(),
        }
    }
}
