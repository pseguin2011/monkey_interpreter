pub type TokenType<'a> = &'a str;

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";

pub const IDENT: &str = "IDENT";
pub const INT: &str = "INT";

pub const EQ: &str = "==";
pub const NOT_EQ: &str = "!=";
pub const ASSIGN: &str = "=";
pub const ASTERISK: &str = "*";
pub const SLASH: &str = "/";
pub const PLUS: &str = "+";
pub const MINUS: &str = "-";

pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";

pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
pub const BANG: &str = "!";
pub const LT: &str = "<";
pub const GT: &str = ">";

pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";
pub const TRUE: &str = "TRUE";
pub const FALSE: &str = "FALSE";
pub const IF: &str = "IF";
pub const ELSE: &str = "ELSE";
pub const RETURN: &str = "RETURN";

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub token_type: TokenType<'a>,
    pub literal: String,
}

impl <'a> Token <'a> {
    pub fn new<S: Into<String>>(token_type: TokenType<'a>, literal: S) -> Token {
        Token {
            token_type,
            literal: literal.into(),
        }
    }
}