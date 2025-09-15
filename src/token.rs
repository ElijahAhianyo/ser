use crate::ast::LiteralObject;

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) enum TokenKind {
    // single-char tokens
    LEFTPAREN,
    RIGHTPAREN,
    LEFTBRACE,
    RIGHTBRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANGEQUAL,
    EQUAL,
    EQUALEQUAL,
    GREATER,
    GREATEREQUAL,
    LESS,
    LESSEQUAL,

    // Literals

    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords

    AND,
    CLASS,
    ELSE,
    FALSE,
    FN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    EOF
}

#[derive(Debug, Clone)]
pub(crate) struct Token{
    token_type: TokenKind,
    lexeme: String,
    literal: Option<LiteralObject>,
    line: usize

}

impl Token{
    pub fn new(token_type: TokenKind, lexeme: String, literal: Option<LiteralObject>, line: usize) -> Self {
        Self{ token_type, lexeme, literal, line}
    }

    pub fn token_type(&self) -> &TokenKind {
        &self.token_type
    }

    pub fn lexeme(&self) -> &String {
        &self.lexeme
    }

    pub fn literal(&self) -> &Option<LiteralObject> {
        &self.literal
    }

    pub fn line(&self) -> usize {
        self.line
    }

}