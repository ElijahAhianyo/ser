use crate::callable::Callable;
use crate::class::{Class, Instance};
use crate::token::Token;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static EXPR_NODE_ID_COUNTER: AtomicUsize = AtomicUsize::new(1);

#[derive(Debug, Clone)]
pub struct ExprNode {
    expr: Expr,
    id: String,
}

impl ExprNode {
    pub fn new(expr: Expr) -> Self {
        let id = EXPR_NODE_ID_COUNTER
            .fetch_add(1, Ordering::Relaxed)
            .to_string();
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
    Get {
        obj: Box<ExprNode>,
        name: Token,
    },
    Set {
        obj: Box<ExprNode>,
        name: Token,
        value: Box<ExprNode>,
    },
    This(Token),
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
    // name, methods
    Class(Token, Vec<Stmt>),
}

#[derive(Clone)]
pub(crate) enum LiteralObject {
    Number(f64),
    Str(String),
    Bool(bool),
    Nil,
    Callable(Rc<dyn Callable>), // Rc here so the callable is thread-safe. cheap clones.
    Instance(Rc<RefCell<Instance>>),
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
            LiteralObject::Instance(i) => i.borrow().to_string(),
        };

        write!(f, "{}", display)
    }
}

impl std::fmt::Debug for LiteralObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralObject::Number(num) => f.debug_tuple("Number").field(num).finish(),
            LiteralObject::Str(val) => f.debug_tuple("Str").field(val).finish(),
            LiteralObject::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
            LiteralObject::Nil => f.debug_tuple("Nil").finish(),
            LiteralObject::Callable(c) => f
                .debug_tuple("Callable")
                .field(&format!("<{}>", c.to_string()))
                .finish(),
            LiteralObject::Instance(instance) => f
                .debug_tuple("Instance")
                .field(&format!("<{}>", instance.borrow().to_string()))
                .finish(),
        }
    }
}
