use crate::token::Token;

pub enum Statements<'a> {
    LetStatement(LetStatement<'a>),
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

type E = Box<dyn Expression>;

pub struct Program<'a> {
    pub statements: Vec<Statements<'a>>,
}

impl<'a> Node for Program<'a> {
    fn token_literal(&self) -> String {

        if !self.statements.is_empty() {
            match &self.statements[0] {
                Statements::LetStatement(s) => s.token_literal(),
            }
        } else {
            "".into()
        }
    }
}

pub struct Identifier<'a> {
    pub token: Token<'a>,
    pub value: String,
}

impl <'a> Expression for Identifier <'a>{
    fn expression_node(&self) {}
}

impl <'a> Node for Identifier <'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

pub struct LetStatement <'a> {
    pub token: Token<'a>,
    pub name:  Identifier<'a>,
    pub value: Option<E>,
}

impl <'a> Statement for LetStatement <'a> {
    fn statement_node(&self) {

    }
}
impl <'a> Node for LetStatement <'a> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}