use std::{fmt::Display, rc::Rc};

use crate::token::Token;

#[derive(Debug)]
pub enum Statements<'a> {
    LetStatement(LetStatement<'a>),
    ReturnStatement(ReturnStatement<'a>),
    ExpressionStatement(ExpressionStatement<'a>),
}

impl<'a> Display for Statements<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match self {
            Self::LetStatement(l) => l.to_string(),
            Self::ReturnStatement(r) => r.to_string(),
            Self::ExpressionStatement(e) => e.to_string(),
        })
    }
}

#[derive(Debug)]
pub enum Expressions<'a> {
    Identifier(Identifier<'a>),
    Boolean(Boolean<'a>),
    IntegerLiteral(IntegerLiteral<'a>),
    PrefixExpression(PrefixExpression<'a>),
    InfixExpression(InfixExpression<'a>),
    IfExpression(IfExpression<'a>),
    FunctionLiteral(FunctionLiteral<'a>),
    CallExpression(CallExpression<'a>),
    InvalidExpression,
}

impl<'a> Display for Expressions<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match self {
            Expressions::Identifier(i) => i.to_string(),
            Expressions::Boolean(b) => b.to_string(),
            Expressions::IntegerLiteral(i) => i.to_string(),
            Expressions::PrefixExpression(p) => p.to_string(),
            Expressions::InfixExpression(i) => i.to_string(),
            Expressions::IfExpression(i) => i.to_string(),
            Expressions::FunctionLiteral(f) => f.to_string(),
            Expressions::CallExpression(c) => c.to_string(),
            Expressions::InvalidExpression => "".into(),
        })
    }
}

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

#[derive(Debug)]
pub struct Program<'a> {
    pub statements: Vec<Statements<'a>>,
}

impl<'a> Node for Program<'a> {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            match &self.statements[0] {
                Statements::LetStatement(s) => s.token_literal(),
                Statements::ReturnStatement(s) => s.token_literal(),
                Statements::ExpressionStatement(s) => s.token_literal(),
            }
        } else {
            "".into()
        }
    }
}

impl<'a> Display for Program<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            &self
                .statements
                .iter()
                .map(ToString::to_string)
                .collect::<String>(),
        )
    }
}

#[derive(Debug, Clone)]
pub struct Identifier<'a> {
    pub token: Token<'a>,
    pub value: String,
}

impl<'a> Expression for Identifier<'a> {
    fn expression_node(&self) {}
}

impl<'a> Node for Identifier<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for Identifier<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value)
    }
}

#[derive(Debug)]
pub struct IntegerLiteral<'a> {
    pub token: Token<'a>,
    pub value: i64,
}

impl<'a> Expression for IntegerLiteral<'a> {
    fn expression_node(&self) {}
}

impl<'a> Node for IntegerLiteral<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for IntegerLiteral<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value.to_string())
    }
}

#[derive(Debug)]
pub struct FunctionLiteral<'a> {
    pub token: Token<'a>,
    pub parameters: Vec<Identifier<'a>>,
    pub body: BlockStatement<'a>,
}

impl<'a> Expression for FunctionLiteral<'a> {
    fn expression_node(&self) {}
}

impl<'a> Node for FunctionLiteral<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for FunctionLiteral<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} ({}) {}",
            self.token_literal(),
            self.parameters
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            self.body
        ))
    }
}

#[derive(Debug)]
pub struct CallExpression<'a> {
    pub token: Token<'a>,              // The '(' token
    pub function: Rc<Expressions<'a>>, // Either an Identifier or FunctionLiteral
    pub arguments: Vec<Expressions<'a>>,
}

impl<'a> Expression for CallExpression<'a> {
    fn expression_node(&self) {}
}

impl<'a> Node for CallExpression<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for CallExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
        ))
    }
}

#[derive(Debug)]
pub struct PrefixExpression<'a> {
    pub token: Token<'a>,
    pub operator: String,
    pub right: Rc<Expressions<'a>>,
}

impl<'a> Expression for PrefixExpression<'a> {
    fn expression_node(&self) {}
}

impl<'a> Node for PrefixExpression<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for PrefixExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("({}{})", self.operator, self.right))
    }
}

#[derive(Debug)]
pub struct InfixExpression<'a> {
    pub token: Token<'a>,
    pub left: Rc<Expressions<'a>>,
    pub operator: String,
    pub right: Rc<Expressions<'a>>,
}

impl<'a> Expression for InfixExpression<'a> {
    fn expression_node(&self) {}
}

impl<'a> Node for InfixExpression<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for InfixExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "({} {} {})",
            self.left, self.operator, self.right
        ))
    }
}

#[derive(Debug)]
pub struct IfExpression<'a> {
    pub token: Token<'a>,
    pub condition: Rc<Expressions<'a>>,
    pub consequence: Rc<BlockStatement<'a>>,
    pub alternative: Option<BlockStatement<'a>>,
}

impl<'a> Expression for IfExpression<'a> {
    fn expression_node(&self) {}
}

impl<'a> Node for IfExpression<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for IfExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = format!(
            "if{} {}",
            self.condition.to_string(),
            self.consequence.to_string()
        );

        if let Some(alt) = self.alternative.as_ref() {
            str.push_str(&format!("else {}", alt.to_string()));
        }
        f.write_str(&str)
    }
}

#[derive(Debug)]
pub struct BlockStatement<'a> {
    pub token: Token<'a>,
    pub statements: Vec<Statements<'a>>,
}

impl<'a> Expression for BlockStatement<'a> {
    fn expression_node(&self) {}
}
impl<'a> Node for BlockStatement<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for BlockStatement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            &self
                .statements
                .iter()
                .map(ToString::to_string)
                .collect::<String>(),
        )
    }
}

#[derive(Debug)]
pub struct LetStatement<'a> {
    pub token: Token<'a>,
    pub name: Identifier<'a>,
    pub value: Option<Expressions<'a>>,
}

impl<'a> Statement for LetStatement<'a> {
    fn statement_node(&self) {}
}
impl<'a> Node for LetStatement<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for LetStatement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(v) => f.write_fmt(format_args!(
                "{} {} = {};",
                self.token_literal(),
                self.name,
                v
            )),
            None => f.write_fmt(format_args!("{} {};", self.token_literal(), self.name)),
        }
    }
}

#[derive(Debug)]
pub struct ReturnStatement<'a> {
    pub token: Token<'a>,
    pub return_value: Option<Expressions<'a>>,
}

impl<'a> Statement for ReturnStatement<'a> {
    fn statement_node(&self) {}
}

impl<'a> Node for ReturnStatement<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for ReturnStatement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.return_value {
            Some(v) => f.write_fmt(format_args!("{} {};", self.token_literal(), v)),
            None => f.write_fmt(format_args!("{};", self.token_literal())),
        }
    }
}
#[derive(Debug)]
pub struct ExpressionStatement<'a> {
    pub token: Token<'a>,
    pub expression: Option<Expressions<'a>>,
}

impl<'a> Statement for ExpressionStatement<'a> {
    fn statement_node(&self) {}
}

impl<'a> Node for ExpressionStatement<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Display for ExpressionStatement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expression {
            Some(e) => f.write_str(&e.to_string()),
            None => f.write_str(""),
        }
    }
}

#[derive(Debug)]
pub struct Boolean<'a> {
    pub token: Token<'a>,
    pub value: bool,
}

impl<'a> Node for Boolean<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Expression for Boolean<'a> {
    fn expression_node(&self) {}
}

impl<'a> Display for Boolean<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.token.literal)
    }
}
