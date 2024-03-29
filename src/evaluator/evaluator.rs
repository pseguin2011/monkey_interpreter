use crate::{
    object::{self, Objects},
    parser::ast::{ExpressionStatement, Expressions, Program, Statements},
};

#[derive(Debug)]
pub enum EvaluatorType<'a> {
    Program(Program<'a>),
    Expressions(Expressions<'a>),
    Statements(Statements<'a>),
}

pub fn eval(node: EvaluatorType<'static>) -> Option<Objects> {
    match node {
        EvaluatorType::Program(p) => return eval_statement(p.statements),
        EvaluatorType::Statements(Statements::ExpressionStatement(ExpressionStatement {
            expression: Some(e),
            ..
        })) => return eval(EvaluatorType::Expressions(e)),
        EvaluatorType::Expressions(Expressions::IntegerLiteral(i)) => {
            return Some(Objects::Integer(object::Integer { value: i.value }));
        }
        // TODO we need to handle the expressions later
        EvaluatorType::Expressions(_) => return None,
        EvaluatorType::Statements(_) => return None,
        _ => None,
    }
}

fn eval_statement(stmts: Vec<Statements<'static>>) -> Option<Objects> {
    let mut result = None;
    for statement in stmts {
        result = eval(EvaluatorType::Statements(statement));
    }
    result
}
