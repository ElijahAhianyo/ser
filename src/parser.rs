use crate::ast::Expr::Literal;
use crate::ast::{Expr, Stmt};
use crate::ast::{ExprNode, LiteralObject};
use crate::function::FnKind;
use crate::token::TokenKind::{IDENTIFIER, LEFTBRACE, RIGHTBRACE};
use crate::token::{Token, TokenKind};
use thiserror::Error;

const ERROR_PREFIX: &'static str = "Syntax Error: ";
const FN_ARGS_MAX: u32 = 255;

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("{ERROR_PREFIX} Expected {symbol} at {line}.")]
    MissingTrailingSymbol { symbol: String, line: usize },
    #[error("{ERROR_PREFIX} Invalid assignment target `{expr}` on line {line} ")]
    InvalidAssignment { expr: String, line: usize },
    #[error("{ERROR_PREFIX} Unsupported primary {token} on line {line} ")]
    UnsupportedPrimary { token: String, line: usize },
    #[error("{ERROR_PREFIX} Cannot have more than {FN_ARGS_MAX} args")]
    MaxFunctionArgsLimitExceeded { arg_name: String },
    #[error("{ERROR_PREFIX} {message}")]
    Custom { message: String },
}

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn peek_next(&self) -> Option<&Token> {
        if self.current + 1 >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[self.current + 1])
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type().clone() == TokenKind::EOF
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
            return self.tokens[self.current - 1].clone();
        }
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn check(&self, token_type: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        *self.peek().token_type() == token_type
    }

    // If current token matches any in `types`, advance and return true (consumes exactly once)
    fn matches(&mut self, types: &[TokenKind]) -> bool {
        for t in types {
            if *self.peek().token_type() == *t {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, token_type: TokenKind, symbol: &str) -> Result<Token> {
        if self.check(token_type) {
            return Ok(self.advance());
        }
        let token = self.peek();
        Err(ParseError::MissingTrailingSymbol {
            symbol: symbol.to_string(),
            line: token.line(),
        })
    }

    /// This makes the semicolon optional if followed by a right brace.
    /// If a semicolon is present, we consume it and move forward. If not,
    /// we check if a brace follows(indicating end of block),
    /// if not we return an error if `require` is true.
    fn consume_optional_semicolon(&mut self, require: bool) -> Result<()> {
        if self.check(TokenKind::SEMICOLON) {
            self.advance();
            Ok(())
        } else if !require || self.check(TokenKind::RIGHTBRACE) || self.is_at_end() {
            Ok(())
        } else {
            let token = self.peek();
            Err(ParseError::MissingTrailingSymbol {
                symbol: ";".to_string(),
                line: token.line(),
            })
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.is_at_end() {
            let stmt = self.declaration();
            if let Ok(stmt) = stmt {
                statements.push(stmt)
            } else {
                self.synchronize()
            }
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt> {
        if self.matches(&[TokenKind::VAR]) {
            return self.var_declaration();
        }

        if self.matches(&[TokenKind::FN]) {
            return self.function(FnKind::Function);
        }

        if self.matches(&[TokenKind::CLASS]) {
            return self.class();
        }
        self.statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        // `VAR` has already been matched by the caller (or you can do `self.consume(TokenType::VAR, "var")?`)
        let name = self.consume(TokenKind::IDENTIFIER, "variable name")?;
        let mut initializer: Option<ExprNode> = None;
        if self.matches(&[TokenKind::EQUAL]) {
            initializer = Some(self.expression()?);
        }
        // make semicolon optional if followed by right brace (for block var decls)
        self.consume_optional_semicolon(true)?;

        Ok(Stmt::VarDeclaration(name, initializer.map(Box::new)))
    }

    fn function(&mut self, kind: FnKind) -> Result<Stmt> {
        // fn keyword has already been consumed.
        let token_name = self.consume(
            TokenKind::IDENTIFIER,
            format!("Expect {kind:?} name").as_str(),
        )?;
        self.consume(
            TokenKind::LEFTPAREN,
            format!("Expect '(' after {kind:?} name").as_str(),
        )?;

        let mut params: Vec<Token> = Vec::new();

        if !self.check(TokenKind::RIGHTPAREN) {
            loop {
                if params.len() >= FN_ARGS_MAX as usize {
                    return Err(ParseError::MaxFunctionArgsLimitExceeded {
                        arg_name: format!("{:?}", self.peek()),
                    });
                }

                params.push(self.consume(TokenKind::IDENTIFIER, "Expect parameter name")?);

                if !self.matches(&[TokenKind::COMMA]) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RIGHTPAREN, "Expect ')' after parameter")?;

        // consume function block
        self.consume(
            LEFTBRACE,
            format!("Expect '{}' before {kind:?} body", "{").as_str(),
        )?;

        let block = self.block()?;
        let body = match block {
            Stmt::Block(blk) => blk,
            _ => {
                return Err(ParseError::Custom {
                    message: "Expected a block here".to_string(),
                });
            }
        };
        Ok(Stmt::Fn(token_name, params, body))
    }

    fn class(&mut self) -> Result<Stmt> {
        let name = self.consume(TokenKind::IDENTIFIER, "Expect class name")?;

        let superclass = if self.matches(&[TokenKind::LESS]) {
            let id = self.consume(IDENTIFIER, "Expect superclass name after '<'")?;
            Some(ExprNode::new(Expr::Var(id)))
        } else {
            None
        };

        self.consume(LEFTBRACE, "Expect '{' before class body")?;

        let mut methods: Vec<Stmt> = Vec::new();
        while !self.check(TokenKind::RIGHTBRACE) && !self.is_at_end() {
            methods.push(self.function(FnKind::Method)?);
        }

        self.consume(RIGHTBRACE, "Expect '}' after class body")?;
        Ok(Stmt::Class(name, superclass.map(Box::new), methods))
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.matches(&[TokenKind::PRINT]) {
            return self.print_statement();
        }

        if self.matches(&[TokenKind::LEFTBRACE]) {
            return self.block();
        }

        if self.matches(&[TokenKind::IF]) {
            return self.if_statement();
        }

        if self.matches(&[TokenKind::WHILE]) {
            return self.while_statement();
        }

        if self.matches(&[TokenKind::FOR]) {
            return self.for_statement();
        }

        if self.matches(&[TokenKind::RETURN]) {
            return self.return_statement();
        }

        self.expr_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        // assumed `PRINT` already matched; if not, use matches above
        let expr = self.expression()?;
        // make semicolon optional if followed by right brace (for block var decls)
        self.consume_optional_semicolon(true)?;
        Ok(Stmt::PrintStmt(Box::new(expr)))
    }

    fn expr_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        // make semicolon optional if followed by right brace (for block var decls)
        self.consume_optional_semicolon(true)?;
        Ok(Stmt::ExprStmt(Box::new(expr)))
    }

    fn block(&mut self) -> Result<Stmt> {
        // assume LEFTBRACE already consumed by caller

        let mut statements: Vec<Stmt> = Vec::new();
        while !self.check(TokenKind::RIGHTBRACE) && !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    eprintln!("Error in block: {:?}", e);
                    self.synchronize();
                }
            }
        }
        // consume right brace
        self.consume(TokenKind::RIGHTBRACE, "}")?;
        Ok(Stmt::Block(statements))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenKind::LEFTPAREN, "(")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RIGHTPAREN, ")")?;
        // consume then branch left brace
        let then_branch = self.statement()?;
        let mut else_branch: Option<Stmt> = None;

        if self.matches(&[TokenKind::ELSE]) {
            else_branch = Some(self.statement()?);
        }

        Ok(Stmt::If(
            Box::new(condition),
            Box::new(then_branch),
            else_branch.map(Box::new),
        ))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenKind::LEFTPAREN, "(")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RIGHTPAREN, ")")?;

        let body = self.statement()?;
        Ok(Stmt::While(Box::new(condition), Box::new(body)))
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenKind::LEFTPAREN, "( after for")?;

        let initializer = if self.matches(&[TokenKind::SEMICOLON]) {
            None
        } else if self.matches(&[TokenKind::VAR]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expr_statement()?)
        };

        let mut condition: Option<ExprNode> = None;
        if !self.check(TokenKind::SEMICOLON) {
            condition = Some(self.expression()?);
        }
        self.consume(TokenKind::SEMICOLON, "; after loop condition")?;

        let mut increment: Option<ExprNode> = None;
        if !self.check(TokenKind::RIGHTPAREN) {
            increment = Some(self.expression()?);
        }
        self.consume(TokenKind::RIGHTPAREN, ") after for clauses.")?;
        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(Vec::from([body, Stmt::ExprStmt(Box::new(increment))]));
        }

        let condition =
            Box::new(condition.unwrap_or(ExprNode::new(Literal(LiteralObject::Bool(true)))));
        body = Stmt::While(condition, Box::new(body));

        if let Some(initializer) = initializer {
            body = Stmt::Block(Vec::from([initializer, body]));
        }

        Ok(body)
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let keyword = self.previous().clone();
        let value = if !self.check(TokenKind::SEMICOLON) {
            let expr = self.expression()?;
            Some(expr)
        } else {
            None
        };

        // make semicolon optional if followed by right brace (for block var decls)
        self.consume_optional_semicolon(true)?;

        Ok(Stmt::Return(keyword.clone(), value.map(Box::new)))
    }

    fn expression(&mut self) -> Result<ExprNode> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<ExprNode> {
        let expr = self.or()?;

        if self.matches(&[TokenKind::EQUAL]) {
            let assignment = self.assignment()?;
            return match expr.expr() {
                Expr::Var(v) => Ok(ExprNode::new(Expr::Assignment(
                    v.clone(),
                    Box::new(assignment),
                ))),
                Expr::Get { obj, name } => Ok(ExprNode::new(Expr::Set {
                    obj: obj.clone(),
                    name: name.clone(),
                    value: Box::new(assignment),
                })),
                _ => {
                    let equals = self.previous();
                    Err(ParseError::InvalidAssignment {
                        expr: equals.lexeme().clone(),
                        line: equals.line(),
                    })
                }
            };
        }
        Ok(expr)
    }

    fn or(&mut self) -> Result<ExprNode> {
        let mut expr = self.and()?;

        while self.matches(&[TokenKind::OR]) {
            let op = self.previous().clone();
            let rhs = self.and()?;
            expr = ExprNode::new(Expr::Logical(Box::new(expr), op, Box::new(rhs)));
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<ExprNode> {
        let mut expr = self.equality()?;

        while self.matches(&[TokenKind::AND]) {
            let op = self.previous().clone();
            let rhs = self.equality()?;
            expr = ExprNode::new(Expr::Logical(Box::new(expr), op.clone(), Box::new(rhs)))
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<ExprNode> {
        let mut left = self.comparison()?;

        while self.matches(&[TokenKind::EQUALEQUAL, TokenKind::BANGEQUAL]) {
            let op = self.previous().clone();
            let right = self.comparison()?;
            left = ExprNode::new(Expr::Binary(Box::new(left), op, Box::new(right)));
        }
        Ok(left)
    }

    fn comparison(&mut self) -> Result<ExprNode> {
        let mut lhs = self.term()?;
        while self.matches(&[
            TokenKind::GREATEREQUAL,
            TokenKind::GREATER,
            TokenKind::LESS,
            TokenKind::LESSEQUAL,
        ]) {
            let op = self.previous().clone();
            let rhs = self.term()?;
            lhs = ExprNode::new(Expr::Binary(Box::new(lhs), op, Box::new(rhs)));
        }
        Ok(lhs)
    }

    fn term(&mut self) -> Result<ExprNode> {
        let mut lhs = self.factor()?;
        while self.matches(&[TokenKind::PLUS, TokenKind::MINUS]) {
            let op = self.previous().clone();
            let rhs = self.factor()?;
            lhs = ExprNode::new(Expr::Binary(Box::new(lhs), op, Box::new(rhs)));
        }
        Ok(lhs)
    }

    fn factor(&mut self) -> Result<ExprNode> {
        let mut lhs = self.unary()?;

        while self.matches(&[TokenKind::SLASH, TokenKind::STAR]) {
            let op = self.previous().clone();
            let rhs = self.unary()?;
            lhs = ExprNode::new(Expr::Binary(Box::new(lhs), op, Box::new(rhs)));
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> Result<ExprNode> {
        if self.matches(&[TokenKind::BANG, TokenKind::MINUS]) {
            let op = self.previous().clone();
            let rhs = self.unary()?;
            return Ok(ExprNode::new(Expr::Unary(op, Box::new(rhs))));
        }
        self.call()
    }

    fn call(&mut self) -> Result<ExprNode> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(&[TokenKind::LEFTPAREN]) {
                expr = self.finish_call(&expr)?;
            } else if self.matches(&[TokenKind::DOT]) {
                // this allows us to capture examples like `a.b.c`
                // which results in
                // `Get {obj: Box::new(Get {obj: Box::new(Var(a)), name: b}), name: c}`
                let name = self.consume(TokenKind::IDENTIFIER, "Expect property name after '.'")?;
                expr = ExprNode::new(Expr::Get {
                    obj: Box::new(expr),
                    name: name.clone(),
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: &ExprNode) -> Result<ExprNode> {
        let mut args: Vec<ExprNode> = Vec::new();
        if !self.check(TokenKind::RIGHTPAREN) {
            // we are expecting args.
            loop {
                if args.len() >= 255 {
                    return Err(ParseError::MaxFunctionArgsLimitExceeded {
                        arg_name: format!("{:?}", self.peek()),
                    });
                }
                args.push(self.expression()?);

                if !self.matches(&[TokenKind::COMMA]) {
                    break;
                }
            }
        }

        let parenthesis = self.consume(TokenKind::RIGHTPAREN, ") after arguments")?;
        let call = Expr::Call {
            callee: Box::new(callee.clone()),
            paren: parenthesis,
            args,
        };
        Ok(ExprNode::new(call))
    }

    fn primary(&mut self) -> Result<ExprNode> {
        if self.matches(&[TokenKind::FALSE]) {
            return Ok(ExprNode::new(Literal(LiteralObject::Bool(false))));
        }
        if self.matches(&[TokenKind::TRUE]) {
            return Ok(ExprNode::new(Literal(LiteralObject::Bool(true))));
        }
        if self.matches(&[TokenKind::NIL]) {
            return Ok(ExprNode::new(Literal(LiteralObject::Nil)));
        }
        if self.matches(&[TokenKind::NUMBER, TokenKind::STRING]) {
            let val = self
                .previous()
                .literal()
                .clone()
                .expect("num/string literal missing");
            return Ok(ExprNode::new(Literal(val)));
        }
        if self.matches(&[TokenKind::LEFTPAREN]) {
            let expr = self.expression()?;
            let _ = self.consume(TokenKind::RIGHTPAREN, ")");

            let grouping = ExprNode::new(Expr::Grouping(Box::new(expr)));
            return Ok(grouping);
        }

        if self.matches(&[TokenKind::IDENTIFIER]) {
            let var_tok = self.previous().clone();
            return Ok(ExprNode::new(Expr::Var(var_tok)));
        }

        if self.matches(&[TokenKind::THIS]) {
            return Ok(ExprNode::new(Expr::This(self.previous().clone())));
        }

        if self.matches(&[TokenKind::SUPER]) {
            let kw = self.previous().clone();
            self.consume(TokenKind::DOT, "Expect '.' after 'super'")?;
            let method = self.consume(TokenKind::IDENTIFIER, "Expect superclass method name")?;
            return Ok(ExprNode::new(Expr::Super(kw.clone(), method.clone())));
        }

        // If nothing matched:
        let tok = self.peek().clone();
        Err(ParseError::UnsupportedPrimary {
            token: tok.lexeme().clone(),
            line: tok.line(),
        })
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if *self.previous().token_type() == TokenKind::SEMICOLON {
                return;
            }

            if self.check(TokenKind::CLASS)
                || self.check(TokenKind::FN)
                || self.check(TokenKind::VAR)
                || self.check(TokenKind::FOR)
                || self.check(TokenKind::IF)
                || self.check(TokenKind::WHILE)
                || self.check(TokenKind::PRINT)
                || self.check(TokenKind::RETURN)
            {
                return;
            }

            self.advance();
        }
    }
}
