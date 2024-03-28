use crate::lexer::Lexer;
use crate::parser::ast::{
    Boolean, ExpressionStatement, Expressions, Identifier, IntegerLiteral, LetStatement, Node,
    Program, ReturnStatement, Statements,
};
use crate::token::Token;
use crate::token::{self, TokenType};
use std::any::{Any, TypeId};
use std::rc::Rc;

use std::collections::HashMap;

use super::ast::{InfixExpression, PrefixExpression};

// Constants to show order of expression priority
const LOWEST: u8 = 1;
const EQUALS: u8 = 2; // ==
const LESSGREATER: u8 = 3; // < or >
const SUM: u8 = 4; // +
const PRODUCT: u8 = 5; // *
const PREFIX: u8 = 6; // -X or !X
const CALL: u8 = 7; // myFunction(X)

const PRECEDENCES: [(&str, u8); 8] = [
    (token::EQ, EQUALS),
    (token::NOT_EQ, EQUALS),
    (token::LT, LESSGREATER),
    (token::GT, LESSGREATER),
    (token::PLUS, SUM),
    (token::MINUS, SUM),
    (token::SLASH, PRODUCT),
    (token::ASTERISK, PRODUCT),
];

type PrefixParsingFn<'a> = dyn Fn(&mut Parser) -> Expressions<'a>;
type InfixParsingFn<'a> = dyn Fn(&mut Parser, Expressions<'a>) -> Expressions<'a>;

struct Parser {
    lexer: Lexer,
    current_token: Option<Token<'static>>,
    peek_token: Option<Token<'static>>,
    errors: Vec<String>,
    prefix_parsing_fns: HashMap<token::TokenType<'static>, Rc<&'static PrefixParsingFn<'static>>>,
    infix_parsing_fns: HashMap<token::TokenType<'static>, Rc<&'static InfixParsingFn<'static>>>,
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
        parser.register_prefix(token::BANG, &Parser::parse_prefix_expression);
        parser.register_prefix(token::MINUS, &Parser::parse_prefix_expression);
        parser.register_prefix(token::TRUE, &Parser::parse_boolean);
        parser.register_prefix(token::FALSE, &Parser::parse_boolean);

        parser.register_infix(token::PLUS, &Parser::parse_infix_expression);
        parser.register_infix(token::MINUS, &Parser::parse_infix_expression);
        parser.register_infix(token::SLASH, &Parser::parse_infix_expression);
        parser.register_infix(token::ASTERISK, &Parser::parse_infix_expression);
        parser.register_infix(token::EQ, &Parser::parse_infix_expression);
        parser.register_infix(token::NOT_EQ, &Parser::parse_infix_expression);
        parser.register_infix(token::LT, &Parser::parse_infix_expression);
        parser.register_infix(token::GT, &Parser::parse_infix_expression);

        parser.next_token();
        parser.next_token();
        parser
    }

    /// Registers prefix handling functions for the provided token types
    ///
    /// # Arguments
    /// `token_type` - The Token type being registered
    /// `func` - The function associated with the parsing the token in an prefix format
    pub fn register_prefix(
        &mut self,
        token_type: token::TokenType<'static>,
        func: &'static PrefixParsingFn<'static>,
    ) {
        if let Some(p) = self.prefix_parsing_fns.get_mut(token_type) {
            *p = Rc::new(func);
        } else {
            self.prefix_parsing_fns.insert(token_type, Rc::new(func));
        }
    }

    /// Registers infix handling functions for the provided token types
    ///
    /// # Arguments
    /// `token_type` - The Token type being registered
    /// `func` - The function associated with the parsing the token in an infix format
    pub fn register_infix(
        &mut self,
        token_type: token::TokenType<'static>,
        func: &'static InfixParsingFn<'static>,
    ) {
        if let Some(p) = self.infix_parsing_fns.get_mut(token_type) {
            *p = Rc::new(func);
        } else {
            self.infix_parsing_fns.insert(token_type, Rc::new(func));
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
                t, peek_token.token_type
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
            if let Token {
                token_type: token::EOF,
                ..
            } = token
            {
                break;
            }
            let statement = match token {
                Token {
                    token_type: token::LET,
                    ..
                } => self.parse_let_statement(),
                Token {
                    token_type: token::RETURN,
                    ..
                } => self.parse_return_statement(),
                _ => self.parse_expression_statement(),
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
    pub fn parse_identifier(&mut self) -> Expressions<'static> {
        if let Some(token) = &self.current_token {
            Expressions::Identifier(Identifier {
                token: token.clone(),
                value: token.literal.clone(),
            })
        } else {
            Expressions::InvalidExpression
        }
    }

    pub fn parse_integer_literal(&mut self) -> Expressions<'static> {
        if let Some(token) = &self.current_token {
            if let Ok(value) = token.literal.parse::<u64>() {
                return Expressions::IntegerLiteral(IntegerLiteral {
                    token: token.clone(),
                    value,
                });
            }
        }
        Expressions::InvalidExpression
    }

    pub fn parse_boolean(&mut self) -> Expressions<'static> {
        if let Some(token) = &self.current_token {
            return Expressions::Boolean(Boolean {
                token: token.clone(),
                value: self.current_token_is(token::TRUE),
            });
        }
        Expressions::InvalidExpression
    }

    pub fn parse_prefix_expression(&mut self) -> Expressions<'static> {
        let operator;
        let token_clone;
        if let Some(token) = &self.current_token {
            token_clone = token.clone();
            operator = token.literal.clone();
        } else {
            return Expressions::InvalidExpression;
        }
        self.next_token();

        if let Some(right) = self
            .current_token
            .clone()
            .and_then(|token| self.parse_expression(PREFIX, token.token_type))
        {
            return Expressions::PrefixExpression(PrefixExpression {
                token: token_clone,
                operator,
                right: Rc::new(right),
            });
        }
        Expressions::InvalidExpression
    }

    fn parse_infix_expression(&mut self, left: Expressions<'static>) -> Expressions<'static> {
        let token;
        let literal;
        if let Some(t) = &self.current_token {
            token = t.clone();
            literal = token.literal.clone();
        } else {
            return Expressions::InvalidExpression;
        }

        let precedence = self.cur_precedence();
        self.next_token();
        if let Some(right) = self
            .current_token
            .clone()
            .and_then(|token| self.parse_expression(precedence, token.token_type))
        {
            return Expressions::InfixExpression(InfixExpression {
                token,
                left: Rc::new(left),
                right: Rc::new(right),
                operator: literal,
            });
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
        if let (Some(let_token), Some(identifier_token)) =
            (let_token.take(), self.current_token.take())
        {
            statement = LetStatement {
                token: let_token,
                name: Identifier {
                    value: identifier_token.literal.clone(),
                    token: identifier_token,
                },
                value: None,
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
    fn parse_expression_statement(&mut self) -> Option<Statements<'static>> {
        let statement;
        if let Some(token) = self.current_token.clone() {
            let token_type = token.token_type;
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
    fn parse_expression<'a>(
        &mut self,
        precedence: u8,
        token_type: token::TokenType,
    ) -> Option<Expressions<'static>> {
        let mut left_exp;
        if let Some(prefix) = self.prefix_parsing_fns.get_mut(token_type) {
            left_exp = prefix.clone()(self);
        } else {
            self.no_prefix_parse_fn_error(token_type);
            return None;
        }

        while !self.peek_token_is(token::SEMICOLON) && precedence < self.peek_precedence() {
            let infix_fn;
            if let Some(infix) = self
                .peek_token
                .clone()
                .and_then(|peek_token| self.infix_parsing_fns.get_mut(peek_token.token_type))
            {
                infix_fn = infix.clone();
            } else {
                return Some(left_exp);
            }
            self.next_token();
            left_exp = infix_fn(self, left_exp);
        }

        Some(left_exp)
    }

    fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        let msg = format!("no prefix parse function for {} found", t);
        self.errors.push(msg);
    }
}

impl Parser {
    fn peek_precedence(&self) -> u8 {
        if let Some((_, v)) = self.peek_token.as_ref().and_then(|peek_token| {
            PRECEDENCES
                .iter()
                .find(|(k, _)| k == &peek_token.token_type)
        }) {
            return *v;
        }
        LOWEST
    }
    fn cur_precedence(&self) -> u8 {
        if let Some((_, v)) = self
            .current_token
            .as_ref()
            .and_then(|token| PRECEDENCES.iter().find(|(k, _)| k == &token.token_type))
        {
            return *v;
        }
        LOWEST
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
        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements, got: {}",
            program.statements.len()
        );

        let tests = ["x", "y", "foobar"];

        for (i, tt) in tests.iter().enumerate() {
            match &program.statements[i] {
                Statements::LetStatement(stmt) => {
                    assert_eq!(&stmt.token_literal(), "let");
                    assert_eq!(&stmt.name.value, tt);
                    assert_eq!(&stmt.name.token_literal(), tt);
                }
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
        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements, got: {}",
            program.statements.len()
        );

        for statement in &program.statements {
            match statement {
                Statements::ReturnStatement(stmt) => {
                    assert_eq!(&stmt.token_literal(), "return");
                }
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
        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements, got: {}",
            program.statements.len()
        );

        for statement in &program.statements {
            match statement {
                Statements::ExpressionStatement(ExpressionStatement {
                    token: t,
                    expression: Some(Expressions::Identifier(i)),
                }) => {
                    assert_eq!(&t.literal, "foobar");
                    assert_eq!(&i.value, "foobar");
                }
                _ => {
                    println!("Statement is not an expression statement {:?}", statement);
                    panic!()
                }
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
        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements, got: {}",
            program.statements.len()
        );

        for statement in &program.statements {
            match statement {
                Statements::ExpressionStatement(ExpressionStatement {
                    token: t,
                    expression: Some(Expressions::IntegerLiteral(i)),
                }) => {
                    assert_eq!(&t.literal, "5");
                    assert_eq!(i.value, 5);
                }
                _ => {
                    println!("not an integer literal expression, got: {:?}", statement);
                    panic!()
                }
            }
        }
    }
}

#[test]
fn test_parsin_prefix_expressions() {
    let prefix_test_1 = [("!5;", "!", 5), ("-15;", "-", 15)];
    let prefix_test_2 = [("!true;", "!", true), ("!false;", "!", false)];
}

fn parsing_prefix_expressions_helper(tests: &[(&str, &str, impl Any + Sized)]) {
    for input in tests {
        let l = Lexer::new(input.0);
        let mut p = Parser::new(l);
        let prog = p.parse_program();
        check_parser_errors(&p);

        assert!(prog.is_some(), "parse_program() returned None");
        if let Some(program) = prog {
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statements, got: {} {:?}",
                program.statements.len(),
                program.statements
            );

            match &program.statements[0] {
                Statements::ExpressionStatement(ExpressionStatement {
                    token: t,
                    expression: Some(Expressions::PrefixExpression(exp)),
                }) => {
                    assert_eq!(&t.literal, input.1);
                    assert_eq!(
                        exp.operator, input.1,
                        "exp.Operator is not {:?}, got {:?}",
                        input.1, exp.operator
                    );
                    if !test_literal_expression(&exp.right, &input.2) {
                        return;
                    }
                }
                statement => {
                    println!("not a PrefixExpression, got: {:?}", statement);
                    panic!()
                }
            }
        }
    }
}

#[test]
fn test_parsing_infix_expression() {
    let infix_test_1: [(&str, u64, &str, u64); 8] = [
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
    ];
    let infix_test_2 = [
        ("true == true", true, "==", true),
        ("true != false", true, "!=", false),
        ("false == false", false, "==", false),
    ];
    test_parsing_infix_helper(&infix_test_1);
    test_parsing_infix_helper(&infix_test_2);
}

fn test_parsing_infix_helper(tests: &[(&str, impl Any + Sized, &str, impl Any + Sized)]) {
    for (input, left_value, operator, right_value) in tests {
        let l = Lexer::new(*input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();
        check_parser_errors(&p);

        assert!(prog.is_some(), "parse_program() returned None");
        if let Some(program) = prog {
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statements, got: {} {:?}",
                program.statements.len(),
                program.statements
            );

            let stmt;
            if let Statements::ExpressionStatement(ExpressionStatement {
                expression: Some(e),
                ..
            }) = &program.statements[0]
            {
                stmt = e;
            } else {
                println!(
                    "program.statements[0] is not a ExpressionStatement, got: {:?}",
                    program.statements[0]
                );
                panic!()
            }
            if let Expressions::InfixExpression(exp) = stmt {
                assert!(test_literal_expression(&exp.left, left_value));
                assert_eq!(
                    exp.operator, *operator,
                    "exp.operator is not {}, got={}",
                    operator, exp.operator
                );
                assert!(test_literal_expression(&exp.right, right_value));
            }
        }
    }
}

#[test]
fn test_operator_procedure_parsing() {
    let tests = [
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
    ];
    for (input, expected) in tests {
        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        let actual = program.unwrap().to_string();
        assert_eq!(
            expected,
            actual.as_str(),
            "expected={}, got={}",
            expected,
            actual
        );
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

fn test_integer_literal(il: &Expressions, value: u64) -> bool {
    if let Expressions::IntegerLiteral(integ) = il {
        if integ.value != value {
            eprintln!("integ.value is not {}. got={}", value, integ.value);
            return false;
        }
        if integ.token_literal() != value.to_string() {
            eprintln!(
                "integ.token_literal not {}. got={}",
                value,
                integ.token_literal()
            );
            return false;
        }
        return true;
    }
    eprintln!("il not ast.IntegerLiteral. got={:?}", il);
    false
}

fn test_boolean_literal<'a>(exp: &Expressions<'a>, value: bool) -> bool {
    let bo;
    match exp {
        Expressions::Boolean(b) => bo = b,
        _ => {
            eprintln!("exp is not ast.Boolean got={:?}", exp);
            return false;
        }
    }
    if bo.value != value {
        eprintln!("bo.value is not {}, got={}", value, bo.value);
        return false;
    }
    if bo.token_literal() != value.to_string() {
        eprintln!(
            "bo.token_literal is not {}, got={}",
            value,
            bo.token_literal()
        );
        return false;
    }
    true
}

fn test_identifier<'a>(exp: &Expressions<'a>, value: String) -> bool {
    if let Expressions::Identifier(ident) = exp {
        if ident.value != value {
            eprintln!("ident.Value not {}. got {}", value, ident.value);
            return false;
        }
        if ident.token_literal() != value {
            eprintln!(
                "ident.TokenLiteral not {}. got {}",
                value,
                ident.token_literal()
            );
            return false;
        }
    } else {
        eprintln!("exp not Identifier. got {:?}", exp);
        return false;
    }
    true
}

fn test_literal_expression<'a, T: Sized + Any>(exp: &Expressions<'a>, expected: &T) -> bool {
    let value = expected as &dyn Any;
    // return test_integer_literal(&exp, value.downcast_mut::<u64>().unwrap().clone());

    if value.type_id() == TypeId::of::<i32>() {
        return test_integer_literal(&exp, value.downcast_ref::<i32>().unwrap().clone() as u64);
    }
    if value.type_id() == TypeId::of::<u64>() {
        return test_integer_literal(&exp, value.downcast_ref::<u64>().unwrap().clone());
    }

    if value.type_id() == TypeId::of::<String>() {
        return test_identifier(exp, value.downcast_ref::<String>().unwrap().to_string());
    }

    if value.type_id() == TypeId::of::<bool>() {
        return test_boolean_literal(exp, value.downcast_ref::<bool>().unwrap().clone());
    }
    eprintln!("type of exp not handled, got={:?}", value);
    return false;
}

fn test_infix_expression<'a, T: Sized + Any>(
    exp: Expressions<'a>,
    left: &T,
    operator: String,
    right: &T,
) -> bool {
    let op_exp;
    if let Expressions::InfixExpression(exp) = exp {
        op_exp = exp;
    } else {
        eprintln!("exp is not ast.operatorExpression");
        return false;
    }

    if !test_literal_expression(&op_exp.left, left) {
        return false;
    }
    if op_exp.operator != operator {
        eprintln!("exp.operator is not {}, got {}", operator, op_exp.operator);
        return false;
    }
    if !test_literal_expression(&op_exp.right, right) {
        return false;
    }
    true
}
