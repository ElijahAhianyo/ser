use std::collections::HashMap;
use std::fmt::Display;
use std::str::FromStr;
use std::sync::OnceLock;
use crate::ast::LiteralObject;
use crate::token::{Token, TokenKind};


#[derive(Debug, Clone)]
pub(crate) struct Lexer {
    source: Vec<char>,
    tokens: Vec<Token>,
    line: usize,
    start: usize,
    current: usize,

}

impl Lexer {
    pub fn new(source: String) -> Self{
        Self {
            source: source.chars().collect::<Vec<_>>(),
            tokens: Vec::new(),
            line: 1,
            current: 0,
            start: 0
        }
    }

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.scan_token()
        }
        // add EOF to mark end of file
        self.add_token(TokenKind::EOF, None);
    }

    pub fn scan_token(&mut self) {
        self.start = self.current;
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenKind::LEFTPAREN, None),
            ')' => self.add_token(TokenKind::RIGHTPAREN, None),
            '{' => self.add_token(TokenKind::LEFTBRACE, None),
            '}' => self.add_token(TokenKind::RIGHTBRACE, None),
            ',' => self.add_token(TokenKind::COMMA, None),
            '.' => self.add_token(TokenKind::DOT, None),
            '-' => self.add_token(TokenKind::MINUS, None),
            '+' => self.add_token(TokenKind::PLUS, None),
            ';' => self.add_token(TokenKind::SEMICOLON, None),
            '*' => self.add_token(TokenKind::STAR, None),
            '!' => {
                let t = if self.match_next('=') {
                    TokenKind::BANGEQUAL
                } else {
                    TokenKind::BANG
                };
                self.add_token(t, None)
            }
            '=' => {
                let t = if self.match_next('=') {
                    TokenKind::EQUALEQUAL
                } else {
                    TokenKind::EQUAL
                };
                self.add_token(t, None)
            }
            '<' => {
                let t = if self.match_next('=') {
                    TokenKind::LESSEQUAL
                } else {
                    TokenKind::LESS
                };
                self.add_token(t, None)
            }
            '>' => {
                let t = if self.match_next('=') {
                    TokenKind::GREATEREQUAL
                } else {
                    TokenKind::GREATER
                };
                self.add_token(t, None)
            }
            '/' => {
                if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                    // detect c-style multi-line comment( /* ... */)
                else if self.match_next('*'){
                        while !self.match_next('/') && !self.match_next('/')  && !self.is_at_end() {
                            self.advance();
                        }
                }
                else {
                        self.add_token(TokenKind::SLASH, None);
                    }
            }
            ' ' | '\t' | '\r' => {

            }
            '\n' => {
                self.line += 1;
            }
            '"' => {
                self.string()
            }

            _ => {
                if self.is_digit(c.into()){
                    self.number();
                }
                else if self.is_alphanumeric(c.into())  {
                    self.identifier();
                }
                else {
                    panic!("unsupported token");
                }

            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn add_token(&mut self, token_type: TokenKind, literal: Option<LiteralObject>) {
        let text = self.source[self.start..self.current].iter().collect::<String>();
        self.tokens.push(Token::new(token_type, text, literal,self.line))
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() || (self.source[self.current] != expected){
            false
        }
        else{
            // only consume the next if its a match.
            self.current += 1;
            true
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end(){
            return '\0';
        }
        self.source[self.current]
    }

    fn peek_next(&self) -> char {
        if self.current + 1 > self.source.len() {
            return '\0';
        }
        self.source[self.current + 1]
    }

    fn string(&mut self) {
        while self.peek() != '\"' && !self.is_at_end() {
            if self.peek() == '\n'{
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            panic!("unterminated String");
        }

        // the closing ".
        self.advance();
        let value = self.source[self.start + 1.. self.current - 1].iter().collect::<String>();
        self.add_token(TokenKind::STRING, Some(LiteralObject::Str(value)))
    }

    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn number(&mut self) {
        while self.is_digit(self.peek().into()){
            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()){
            self.advance();
        }

        while self.is_digit(self.peek().into()){
            self.advance();
        }
        let content = self.source[self.start..self.current].iter().collect::<String>();
        self.add_token(TokenKind::NUMBER, Some(LiteralObject::Number(f64::from_str(content.as_str()).unwrap())))
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn identifier(&mut self) {
        while self.is_alphanumeric(self.peek().into()){
            self.advance();
        }

        // check if identifier is a keyword.
        let text: String = self.source[self.start..self.current].iter().collect();
        let ty = Self::keywords().get(text.as_str());

        if let Some(ty) = ty {
            self.add_token(*ty, None)
        }
        else{
            self.add_token(TokenKind::IDENTIFIER, None)
        }

    }

    fn keywords() -> &'static HashMap<&'static str, TokenKind> {
        static KW: OnceLock<HashMap<&'static str, TokenKind>> = OnceLock::new();

        KW.get_or_init(|| {
            let mut map = HashMap::new();
            map.insert("and", TokenKind::AND);
            map.insert("class", TokenKind::CLASS);
            map.insert("else", TokenKind::ELSE);
            map.insert("false", TokenKind::FALSE);
            map.insert("for", TokenKind::FOR);
            map.insert("fn", TokenKind::FN);
            map.insert("if", TokenKind::IF);
            map.insert("nil", TokenKind::NIL);
            map.insert("or", TokenKind::OR);
            map.insert("print", TokenKind::PRINT);
            map.insert("return", TokenKind::RETURN);
            map.insert("super", TokenKind::SUPER);
            map.insert("this", TokenKind::THIS);
            map.insert("var", TokenKind::VAR);
            map.insert("while", TokenKind::WHILE);
            map.insert("true", TokenKind::TRUE);
            map
        })

    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

}

