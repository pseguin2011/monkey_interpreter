use crate::parser::ast::{Expression, Identifier, LetStatement, Node, Program, Statements};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token;

struct Parser {
    lexer: Lexer,
    current_token: Option<Token<'static>>,
    peek_token: Option<Token<'static>>,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            errors: Vec::new(),
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        self.current_token = std::mem::take(&mut self.peek_token);
        self.peek_token = Some(self.lexer.next_token());
    }

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    pub fn peek_error(&mut self, t: token::TokenType) {
        if let Some(peek_token) = &self.peek_token {
            let msg = format!(
                    "expected next token to be {}, got {} instead",
                    t,
                    peek_token.token_type
            );
            self.errors.push(msg);
        }
    }

    pub fn parse_program(&mut self) -> Option<Program<'static>> {
        let mut program = Self::new_program_ast_node();

        while let Some(token) = &self.current_token {
            if let Token {token_type: token::EOF,..} = token {
                break;
            }
            let statement = 
                match token {
                    Token {token_type: token::LET,..}       =>  self.parse_let_statement(),
                    Token {token_type: token::RETURN,..}    =>  self.parse_return_statement(),
                    Token {token_type: token::IF,..}        =>  self.parse_if_statement(),
                    _ => None,
                };
            if let Some(st) = statement {
                program.statements.push(st);
            }
            self.next_token();
        }
        Some(program)
    }

    fn parse_let_statement(&mut self) -> Option<Statements<'static>> {
        let statement;
        let mut let_token = self.current_token.take();

        if !self.expected_peek(token::IDENT) {
            return None;
        }
        if let (Some(let_token), Some(identifier_token)) = (let_token.take(), self.current_token.take()) {
            statement =
                LetStatement {
                    token: let_token,
                    name: Identifier {
                        value: identifier_token.literal.clone(),
                        token: identifier_token,
                    },
                    value: None
                };
        } else {
            return None;
        }
        if !self.expected_peek(token::ASSIGN) {
            return None;
        }

        while !self.current_token_is(token::SEMICOLON) {
            self.next_token();
        }
        Some(Statements::LetStatement(statement))
    }

    fn expected_peek(&mut self, token_type: token::TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn peek_token_is(&self, token_type: token::TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            token.token_type == token_type
        } else {
            false
        }
    }


    fn current_token_is(&self, token_type: token::TokenType) -> bool {
        if let Some(token) = &self.current_token {
            token.token_type == token_type
        } else {
            false
        }
    }

    fn parse_if_statement(&mut self) -> Option<Statements<'static>> {
        None
    }

    fn parse_return_statement(&mut self) -> Option<Statements<'static>> {
        None
    }

    fn new_program_ast_node() -> Program<'static> {
        Program {
            statements: Vec::new(),
        }
    }
}

#[test]
fn test_let_statements() {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);
    assert!(prog.is_some(), "parse_program() returned None");
    if let Some(program) = prog {
        assert_eq!(program.statements.len(), 3, "program.statements does not contain 3 statements, got: {}", program.statements.len());

        let tests = ["x", "y", "foobar"];

        for (i, tt) in tests.iter().enumerate() {
            match &program.statements[i] {
                Statements::LetStatement(stmt) => {
                    assert_eq!(&stmt.token_literal(), "let");
                    assert_eq!(&stmt.name.value, tt);
                    assert_eq!(&stmt.name.token_literal(), tt);
                }
            }
        }
    }

}

fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();
    if errors.is_empty() {
        return;
    }

    println!("Parser has {} errors", errors.len());
    for msg in errors.iter() {
        println!("parser error: {}", msg);
    }
    panic!();
}

