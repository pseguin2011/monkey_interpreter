use std::rc::Rc;

use crate::{
    object::{self, Boolean, Integer, Null, Object, ObjectType, Objects},
    parser::ast::{self, ExpressionStatement, Expressions, Program, Statements},
};

const TRUE: Boolean = Boolean { value: true };
const FALSE: Boolean = Boolean { value: false };
const NULL: Null = Null {};

#[derive(Debug)]
pub enum EvaluatorType<'a> {
    Program(Program<'a>),
    Expressions(Rc<Expressions<'a>>),
    Statements(Statements<'a>),
}

pub fn eval(node: EvaluatorType<'static>) -> Option<Objects> {
    match node {
        EvaluatorType::Program(p) => return eval_statement(p.statements),
        EvaluatorType::Statements(Statements::ExpressionStatement(ExpressionStatement {
            expression: Some(e),
            ..
        })) => return eval_expression(&e),
        EvaluatorType::Expressions(e) => return eval_expression(&e),
        // TODO we need to handle the statements later
        EvaluatorType::Statements(_) => return None,
        _ => None,
    }
}

fn eval_expression(expression: &Expressions<'static>) -> Option<Objects> {
    match expression {
        Expressions::IntegerLiteral(i) => {
            return Some(Objects::Integer(object::Integer { value: i.value }));
        }
        Expressions::Boolean(ast::Boolean { value: true, .. }) => {
            return Some(Objects::Boolean(TRUE))
        }
        Expressions::Boolean(ast::Boolean { value: false, .. }) => {
            return Some(Objects::Boolean(FALSE))
        }
        Expressions::PrefixExpression(pref) => {
            if let Some(right) = eval(EvaluatorType::Expressions(pref.right.clone())) {
                return Some(eval_prefix_expression(&pref.operator, right));
            }
            None
        }
        Expressions::InfixExpression(infix) => {
            if let Some((left, right)) =
                eval_expression(&infix.left).zip(eval_expression(&infix.right))
            {
                return Some(eval_infix_expression(&infix.operator, left, right));
            }
            None
        }
        // TODO we need to handle the expressions later
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

fn eval_prefix_expression(operator: &str, right: Objects) -> Objects {
    match operator {
        "!" => Objects::Boolean(eval_bang_operator(right)),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Objects::Null(NULL),
    }
}

fn eval_infix_expression(operator: &str, left: Objects, right: Objects) -> Objects {
    match (operator, &left, &right) {
        (_, Objects::Integer(_), Objects::Integer(_)) => {
            eval_integer_infix_expression(operator, left, right)
        }
        ("<", Objects::Boolean(left), Objects::Boolean(right)) => Objects::Boolean(Boolean {
            value: left.value < right.value,
        }),
        (">", Objects::Boolean(left), Objects::Boolean(right)) => Objects::Boolean(Boolean {
            value: left.value > right.value,
        }),
        ("==", Objects::Boolean(left), Objects::Boolean(right)) => Objects::Boolean(Boolean {
            value: left.value == right.value,
        }),
        ("!=", Objects::Boolean(left), Objects::Boolean(right)) => Objects::Boolean(Boolean {
            value: left.value != right.value,
        }),
        _ => Objects::Null(NULL),
    }
}

fn eval_bang_operator(object: Objects) -> Boolean {
    match object {
        Objects::Boolean(Boolean { value: true }) => FALSE,
        Objects::Boolean(Boolean { value: false }) => TRUE,
        Objects::Null(_) => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(object: Objects) -> Objects {
    if let Objects::Integer(mut object) = object {
        object.value = -object.value;
        Objects::Integer(object)
    } else {
        Objects::Null(NULL)
    }
}

fn eval_integer_infix_expression(operator: &str, left: Objects, right: Objects) -> Objects {
    match (operator, left, right) {
        ("+", Objects::Integer(left), Objects::Integer(right)) => Objects::Integer(Integer {
            value: left.value + right.value,
        }),
        ("-", Objects::Integer(left), Objects::Integer(right)) => Objects::Integer(Integer {
            value: left.value - right.value,
        }),
        ("*", Objects::Integer(left), Objects::Integer(right)) => Objects::Integer(Integer {
            value: left.value * right.value,
        }),
        ("/", Objects::Integer(left), Objects::Integer(right)) => Objects::Integer(Integer {
            value: left.value / right.value,
        }),
        ("<", Objects::Integer(left), Objects::Integer(right)) => Objects::Boolean(Boolean {
            value: left.value < right.value,
        }),
        (">", Objects::Integer(left), Objects::Integer(right)) => Objects::Boolean(Boolean {
            value: left.value > right.value,
        }),
        ("==", Objects::Integer(left), Objects::Integer(right)) => Objects::Boolean(Boolean {
            value: left.value == right.value,
        }),
        ("!=", Objects::Integer(left), Objects::Integer(right)) => Objects::Boolean(Boolean {
            value: left.value != right.value,
        }),
        _ => Objects::Null(NULL),
    }
}
