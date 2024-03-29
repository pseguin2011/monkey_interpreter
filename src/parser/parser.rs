use crate::lexer::Lexer;
use crate::parser::ast::{
    Boolean, ExpressionStatement, Expressions, Identifier, IntegerLiteral, LetStatement, Program,
    ReturnStatement, Statements,
};
use crate::token::Token;
use crate::token::{self, TokenType};
use std::rc::Rc;

use std::collections::HashMap;

use super::ast::{
    BlockStatement, FunctionLiteral, IfExpression, InfixExpression, PrefixExpression,
};

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

pub struct Parser {
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
        parser.register_prefix(token::LPAREN, &Parser::parse_grouped_expression);
        parser.register_prefix(token::IF, &Parser::parse_if_expression);
        parser.register_prefix(token::FUNCTION, &Parser::parse_function_literal);

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
            if token.token_type == token::EOF {
                break;
            }

            let stmt = self.parse_statement();
            if let Some(st) = stmt {
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

    fn parse_grouped_expression(&mut self) -> Expressions<'static> {
        self.next_token();
        let token;
        if let Some(tok) = &self.current_token {
            token = tok;
        } else {
            return Expressions::InvalidExpression;
        }
        if let Some(exp) = self.parse_expression(LOWEST, token.token_type) {
            if !self.expected_peek(token::RPAREN) {
                return Expressions::InvalidExpression;
            }
            return exp;
        }
        Expressions::InvalidExpression
    }

    fn parse_if_expression(&mut self) -> Expressions<'static> {
        let token;
        let condition;
        let consequence;
        let mut alternative = None;

        if !self.expected_peek(token::LPAREN) {
            return Expressions::InvalidExpression;
        }

        self.next_token();

        if let Some(tok) = &self.current_token {
            token = tok.clone();
        } else {
            return Expressions::InvalidExpression;
        }

        if let Some(cond) = self.parse_expression(LOWEST, token.token_type) {
            condition = cond;
        } else {
            return Expressions::InvalidExpression;
        }

        if !self.expected_peek(token::RPAREN) {
            return Expressions::InvalidExpression;
        }

        if !self.expected_peek(token::LBRACE) {
            return Expressions::InvalidExpression;
        }

        if let Some(cons) = self.parse_block_statement() {
            consequence = cons;
        } else {
            return Expressions::InvalidExpression;
        }

        if self.peek_token_is(token::ELSE) {
            self.next_token();
            if !self.expected_peek(token::LBRACE) {
                return Expressions::InvalidExpression;
            }
            alternative = self.parse_block_statement();
        }

        Expressions::IfExpression(IfExpression {
            token,
            condition: Rc::new(condition),
            consequence: Rc::new(consequence),
            alternative,
        })
    }

    fn parse_function_literal(&mut self) -> Expressions<'static> {
        let token = match &self.current_token {
            Some(token) => token.clone(),
            None => return Expressions::InvalidExpression,
        };

        if !self.expected_peek(token::LPAREN) {
            return Expressions::InvalidExpression;
        }

        let parameters = match self.parse_function_parameters() {
            Some(params) => params,
            None => return Expressions::InvalidExpression,
        };

        if !self.expected_peek(token::LBRACE) {
            return Expressions::InvalidExpression;
        }

        let body = match self.parse_block_statement() {
            Some(parsed_body) => parsed_body,
            None => return Expressions::InvalidExpression,
        };

        Expressions::FunctionLiteral(FunctionLiteral {
            body,
            parameters,
            token,
        })
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement<'static>> {
        self.next_token();
        let token;
        if let Some(tok) = &self.current_token {
            token = tok.clone();
        } else {
            return None;
        }

        let mut statements = Vec::new();
        while !self.current_token_is(token::RBRACE) && !self.current_token_is(token::EOF) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }
        Some(BlockStatement { statements, token })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier<'static>>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(token::RPAREN) {
            self.next_token();
            return Some(identifiers);
        }
        self.next_token();
        match &self.current_token {
            Some(token) => identifiers.push(Identifier {
                value: token.literal.clone(),
                token: token.clone(),
            }),
            None => return None,
        };

        while self.peek_token_is(token::COMMA) {
            self.next_token();
            self.next_token();
            match &self.current_token {
                Some(token) => identifiers.push(Identifier {
                    value: token.literal.clone(),
                    token: token.clone(),
                }),
                None => return None,
            };
        }

        if !self.expected_peek(token::RPAREN) {
            return None;
        }
        Some(identifiers)
    }

    fn parse_statement(&mut self) -> Option<Statements<'static>> {
        match self
            .current_token
            .as_ref()
            .and_then(|tok| Some(tok.token_type))
        {
            Some(token::LET) => self.parse_let_statement(),
            Some(token::RETURN) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
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
