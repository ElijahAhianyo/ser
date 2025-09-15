use thiserror::Error;
use crate::ast::LiteralObject;
use crate::ast::{Expr, Stmt};
use crate::ast::Expr::Literal;
use crate::token::{Token, TokenKind};

const ERROR_PREFIX: &'static str = "Syntax Error: ";


#[derive(Debug, Clone, Error)]
pub enum ParseError{
    #[error("{ERROR_PREFIX} Expected {symbol} at {line}.")]
    MissingTrailingSymbol{
        symbol: String,
        line: usize
    },
    #[error("{ERROR_PREFIX} Invalid assignment target `{expr}` on line {line} ")]
    InvalidAssignment{
        expr: String,
        line: usize
    },
    #[error("{ERROR_PREFIX} Unsupported primary {token} on line {line} ")]
    UnsupportedPrimary{
        token: String,
        line: usize
    }
}

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize
}


impl Parser{
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {tokens, current: 0}
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn peek_next(&self) -> &Token {
        &self.tokens[self.current + 1]
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type().clone() == TokenKind::EOF
    }

    fn advance(& mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
            return self.tokens[self.current - 1].clone()
        }
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn check(&self, token_type: TokenKind) -> bool {
        if self.is_at_end(){
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
            return Ok(self.advance())
        }
        let token = self.peek();
        Err(ParseError::MissingTrailingSymbol {symbol: symbol.to_string(), line: token.line()})
    }


    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.is_at_end() {
            let stmt = self.declaration();
            if let Ok(stmt) = stmt {
                statements.push(stmt)
            }
            else {
                self.synchronize()
            }

        }
        Ok(statements)
    }



    fn declaration(&mut self) -> Result<Stmt> {
        if self.matches(&[TokenKind::VAR]) {
            return self.var_declaration()
        }
        self.statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        // `VAR` has already been matched by the caller (or you can do `self.consume(TokenType::VAR, "var")?`)
        let name = self.consume(TokenKind::IDENTIFIER, "variable name")?;
        let mut initializer: Option<Expr> = None;
        if self.matches(&[TokenKind::EQUAL]) {
            initializer = Some(self.expression()?);
        }
        self.consume(TokenKind::SEMICOLON, ";")?;
        Ok(Stmt::VarDeclaration(name, initializer.map(Box::new)))

    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.matches(&[TokenKind::PRINT]) {
            return self.print_statement()
        }

        if self.matches(&[TokenKind::LEFTBRACE]) {
            return self.block()
        }
        self.expr_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        // assumed `PRINT` already matched; if not, use matches above
        let expr = self.expression()?;
        self.consume(TokenKind::SEMICOLON, ";")?;
        Ok(Stmt::PrintStmt(Box::new(expr)))
    }

    fn expr_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenKind::SEMICOLON, ";")?;
        Ok(Stmt::ExprStmt(Box::new(expr)))
    }

    fn block(&mut self) -> Result<Stmt> {
        // assume LEFTBRACE already consumed by caller

        let mut statements: Vec<Box<Stmt>> = Vec::new();
        while !self.check(TokenKind::RIGHTBRACE) && !self.is_at_end() {
            statements.push(Box::new(self.declaration()?));
        }
        // consume right brace
        self.consume(TokenKind::RIGHTBRACE, "}")?;
        Ok(Stmt::Block(statements))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.equality()?;

        if self.matches(&[TokenKind::EQUAL]) {
            let assignment = self.assignment()?;
            return match expr {
                Expr::Var(v) => {
                    Ok(Expr::Assignment(v.clone(), Box::new(assignment)))
                }
                _ => {
                    let equals = self.previous();
                    Err(ParseError::InvalidAssignment { expr: equals.lexeme().clone(), line: equals.line() })
                }
            }
        }
        Ok(expr)

    }

    fn equality(&mut self) -> Result<Expr> {
        let mut left = self.comparison()?;

        while self.matches(&[TokenKind::EQUALEQUAL , TokenKind::BANGEQUAL]) {
            let op = self.previous().clone();
            let right = self.comparison()?;
            left =  Expr::Binary(Box::new(left),op ,Box::new(right));
        }
        Ok(left)
    }


    fn comparison(&mut self) -> Result<Expr> {
        let mut lhs = self.term()?;
        while self.matches(&[TokenKind::GREATEREQUAL, TokenKind::GREATER, TokenKind::LESS, TokenKind::LESSEQUAL]) {
            let op = self.previous().clone();
            let rhs = self.term()?;
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    fn term(&mut self) -> Result<Expr> {
      let mut lhs = self.factor()?;
        while self.matches(&[TokenKind::PLUS , TokenKind::MINUS]) {
            let op = self.previous().clone();
            let rhs = self.factor()?;
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut lhs = self.unary()?;

        while self.matches( &[TokenKind::SLASH , TokenKind::STAR]) {
            let op = self.previous().clone();
            let rhs = self.unary()?;
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs))
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> Result<Expr> {
        if self.matches(&[TokenKind::BANG , TokenKind::MINUS]) {
            let op = self.previous().clone();
            let rhs = self.unary()?;
            return Ok(Expr::Unary(op, Box::new(rhs)));
        }
        self.primary()
    }


    fn primary(&mut self) -> Result<Expr> {
        if self.matches(&[TokenKind::FALSE]) {
            return Ok(Literal(LiteralObject::Bool(false)));
        }
        if self.matches(&[TokenKind::TRUE]) {
            return Ok(Literal(LiteralObject::Bool(true)));
        }
        if self.matches(&[TokenKind::NIL]) {
            return Ok(Literal(LiteralObject::Nil));
        }
        if self.matches(&[TokenKind::NUMBER, TokenKind::STRING]) {
            let val = self.previous().literal().clone().expect("num/string literal missing");
            return Ok(Literal(val));
        }
        if self.matches(&[TokenKind::LEFTPAREN]) {
            let expr = self.expression()?;
            let _ = self.consume(TokenKind::RIGHTPAREN, ")");
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        if self.matches(&[TokenKind::IDENTIFIER]) {
            let var_tok = self.previous().clone();
            return Ok(Expr::Var(var_tok));
        }

        // If nothing matched:
        let tok = self.peek().clone();
        Err(ParseError::UnsupportedPrimary { token: tok.lexeme().clone(), line: tok.line() })
    }



    fn synchronize(&mut self) {
        while !self.is_at_end() {
            if self.matches(&[TokenKind::SEMICOLON , TokenKind::CLASS , TokenKind::FN , TokenKind::VAR , TokenKind::FOR , TokenKind::IF , TokenKind::WHILE , TokenKind::PRINT , TokenKind::RETURN]) {
                return
            }
            self.advance();
        }
        return
    }

}