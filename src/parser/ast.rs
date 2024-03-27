use crate::token::Token;

#[derive(Debug)]
pub enum Statements<'a> {
    LetStatement(LetStatement<'a>),
    ReturnStatement(ReturnStatement<'a>),
    ExpressionStatement(ExpressionStatement<'a>),
}

#[derive(Debug)]
pub enum Expressions<'a> {
    Identifier(Identifier<'a>),
    IntegerLiteral(IntegerLiteral<'a>),
    PrefixExpression(PrefixExpression<'a>),
    InvalidExpression,
}

impl<'a> ToString for Expressions<'a> {
    fn to_string(&self) -> String {
        match self {
            Expressions::Identifier(i) => i.to_string(),
            Expressions::IntegerLiteral(i) => i.to_string(),
            Expressions::PrefixExpression(p) => p.to_string(),
            Expressions::InvalidExpression => "".into(),
        }
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

#[derive(Debug)]
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

impl<'a> ToString for Identifier<'a> {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug)]
pub struct IntegerLiteral<'a> {
    pub token: Token<'a>,
    pub value: u64,
}

impl<'a> Expression for IntegerLiteral<'a> {
    fn expression_node(&self) {}
}

impl<'a> Node for IntegerLiteral<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> ToString for IntegerLiteral<'a> {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug)]
pub struct PrefixExpression<'a> {
    pub token: Token<'a>,
    pub operator: String,
    pub right: Box<Expressions<'a>>,
}

impl<'a> Expression for PrefixExpression<'a> {
    fn expression_node(&self) {}
}

impl<'a> Node for PrefixExpression<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> ToString for PrefixExpression<'a> {
    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
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
