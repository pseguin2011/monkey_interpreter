use crate::parser::ast::{Expressions, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Node, Program, ReturnStatement, Statements};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token;

use std::collections::HashMap;

// Constants to show order of expression priority
const LOWEST: u8 = 1;
const EQUALS: u8 = 2;         // ==
const LESSGREATER: u8 = 3;    // < or >
const SUM: u8 = 4;            // + 
const PRODUCT: u8 = 5;        // *
const PREFIX: u8 = 6;         // -X or !X
const CALL: u8 = 7;           // myFunction(X)

type PrefixParsingFn<'a> = dyn Fn(&Parser) -> Expressions<'a>;
type InfixParsingFn<'a> = dyn Fn(&Parser, Expressions<'a>) -> Expressions<'a>;

struct Parser {
    lexer: Lexer,
    current_token: Option<Token<'static>>,
    peek_token: Option<Token<'static>>,
    errors: Vec<String>,
    prefix_parsing_fns: HashMap<token::TokenType<'static>, &'static PrefixParsingFn<'static>>,
    infix_parsing_fns:  HashMap<token::TokenType<'static>, &'static InfixParsingFn<'static>>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            errors: Vec::new(),
            prefix_parsing_fns: HashMap::new(),
            infix_parsing_fns: HashMap::new(),
        };
        parser.register_prefix(token::IDENT, &Parser::parse_identifier);
        parser.register_prefix(token::INT, &Parser::parse_integer_literal);
        
        parser.next_token();
        parser.next_token();
        parser
    }

    /// Registers prefix handling functions for the provided token types
    /// 
    /// # Arguments
    /// `token_type` - The Token type being registered
    /// `func` - The function associated with the parsing the token in an prefix format
    pub fn register_prefix(&mut self, token_type: token::TokenType<'static>, func: &'static PrefixParsingFn<'static>) {
        if let Some(p) = self.prefix_parsing_fns.get_mut(token_type) {
            *p = func;
        } else {
            self.prefix_parsing_fns.insert(token_type, func);
        }
    }

    /// Registers infix handling functions for the provided token types
    /// 
    /// # Arguments
    /// `token_type` - The Token type being registered
    /// `func` - The function associated with the parsing the token in an infix format
    pub fn register_infix(&mut self, token_type: token::TokenType<'static>, func: &'static InfixParsingFn<'static>) {
        if let Some(p) = self.infix_parsing_fns.get_mut(token_type) {
            *p = func;
        } else {
            self.infix_parsing_fns.insert(token_type, func);
        }
    }

    fn new_program_ast_node() -> Program<'static> {
        Program {
            statements: Vec::new(),
        }
    }
    
    /// Advances the tokens from the lexer
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

    /// Iterates through the tokens and dispatches each token from the lexer to their appropriate
    /// parsing methods.
    /// 
    /// # Returns
    /// The fully parsed program ready for execution.
    pub fn parse_program(&mut self) -> Option<Program<'static>> {
        let mut program = Self::new_program_ast_node();

        while let Some(token) = &self.current_token {
            if let Token {token_type: token::EOF,..} = token {
                break;
            }
            let statement = 
                match token {
                    Token {token_type: token::LET,..}
                        =>  self.parse_let_statement(),
                    Token {token_type: token::RETURN,..}
                        =>  self.parse_return_statement(),
                    _
                        => self.parse_expression_statement(),
                };
            if let Some(st) = statement {
                program.statements.push(st);
            }
            self.next_token();
        }
        Some(program)
    }

    /// Verifies the peek token to be as expected and advances the tokens
    /// 
    /// # Returns
    /// If the token (before being advanced) was the provided type
    fn expected_peek(&mut self, token_type: token::TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    /// Checks if peek token is the token provided
    fn peek_token_is(&self, token_type: token::TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            token.token_type == token_type
        } else {
            false
        }
    }

    /// Checks if the current token is the token provided
    fn current_token_is(&self, token_type: token::TokenType) -> bool {
        if let Some(token) = &self.current_token {
            token.token_type == token_type
        } else {
            false
        }
    }
}

/// Expression Parsing
impl Parser {
    /// Creates identifier from current token
    /// 
    /// # Returns
    /// The identifier expression
    pub fn parse_identifier(&self) -> Expressions<'static> {
        if let Some(token) = &self.current_token {
            Expressions::Identifier(
                Identifier {
                    token: token.clone(),
                    value: token.literal.clone(),
                }
            )
        } else {
            Expressions::InvalidExpression
        }
    }

    pub fn parse_integer_literal(&self) -> Expressions<'static> {
        if let Some(token) = &self.current_token {
            if let Ok(value) = token.literal.parse::<u64>() {
                return Expressions::IntegerLiteral(
                    IntegerLiteral {
                        token: token.clone(),
                        value,
                    }
                );
            }
        }
        Expressions::InvalidExpression
    }
}


/// Statement Parsing
impl Parser {
    /// Parses the let statements from the lexer's combination of identifier tokens,
    /// until a semicolon is reached.
    /// 
    /// # Returns
    /// A let statement
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

    /// Parses the return statements from the lexer's combination of identifier tokens or value tokens,
    /// until a semicolon is reached.
    /// 
    /// # Returns
    /// A return statement
    fn parse_return_statement(&mut self) -> Option<Statements<'static>> {
        let statement;
        if let Some(return_token) = self.current_token.take() {
            statement = ReturnStatement {
                token: return_token,
                return_value: None,
            };
        } else {
            return None;
        }
        self.next_token();
        while !self.current_token_is(token::SEMICOLON) {
            self.next_token();
        }
        Some(Statements::ReturnStatement(statement))
    }

    /// Parses the expression statements from the lexer's combination tokens,
    /// until a semicolon is reached.
    /// 
    /// # Returns
    /// An Expression statement
    fn parse_expression_statement(&mut self) -> Option<Statements<'static>>{
        let statement;
        if let Some(token) = self.current_token.clone() {
            let token_type = token.token_type.clone();
            statement = ExpressionStatement {
                token,
                expression: self.parse_expression(LOWEST, &token_type),
            };
        } else {
            return None;
        }

        if self.peek_token_is(token::SEMICOLON) {
            self.next_token();
        }
        Some(Statements::ExpressionStatement(statement))
    }

    /// Finds the token parsing function stored in the hashmap and dispatches it.
    fn parse_expression<'a>(&mut self, precedence: u8, token_type: token::TokenType) -> Option<Expressions<'static>> {
        if let Some(prefix) =  self.prefix_parsing_fns.get(token_type) {
            Some(prefix(&self))
        } else {
            None
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
                },
                _ => panic!("Statement is not a let statement"),
            }
        }
    }

}

#[test]
fn test_return_statements() {
    let input = "
        return 5;
        return 10;
        return 838383;
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);
    assert!(prog.is_some(), "parse_program() returned None");
    if let Some(program) = prog {
        assert_eq!(program.statements.len(), 3, "program.statements does not contain 3 statements, got: {}", program.statements.len());

        for statement in &program.statements {
            match statement {
                Statements::ReturnStatement(stmt) => {
                    assert_eq!(&stmt.token_literal(), "return");
                },
                _ => panic!("Statement is not a return statement"),
            }
        }
    }

}

#[test]
fn test_identifier_expression() {
    let input = "
        foobar;
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);
    assert!(prog.is_some(), "parse_program() returned None");
    if let Some(program) = prog {
        assert_eq!(program.statements.len(), 1, "program.statements does not contain 1 statements, got: {}", program.statements.len());

        for statement in &program.statements {
            match statement {
                Statements::ExpressionStatement(
                    ExpressionStatement {
                        token: t,
                        expression: Some(Expressions::Identifier(i))
                    }
                ) => {
                    assert_eq!(&t.literal, "foobar");
                    assert_eq!(&i.value, "foobar");
                },
                _ => {
                    println!("Statement is not an expression statement {:?}", statement);
                    panic!()
                },
            }
        }
    }

}

#[test]
fn test_integer_literal_expression() {
    let input = "
        5;
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);
    assert!(prog.is_some(), "parse_program() returned None");
    if let Some(program) = prog {
        assert_eq!(program.statements.len(), 1, "program.statements does not contain 1 statements, got: {}", program.statements.len());

        for statement in &program.statements {
            match statement {
                Statements::ExpressionStatement(
                    ExpressionStatement {
                        token: t,
                        expression: Some(Expressions::IntegerLiteral(i))
                    }
                ) => {
                    assert_eq!(&t.literal, "5");
                    assert_eq!(i.value, 5);
                },
                _ => {
                    println!("not an integer literal expression, got: {:?}", statement);
                    panic!()
                },
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

