use std::rc::Rc;

use crate::{
    object::{self, Boolean, Integer, Null, Object, Objects, ReturnValue},
    parser::ast::{
        self, BlockStatement, ExpressionStatement, Expressions, IfExpression, Program, Statements,
    },
};

const TRUE: Boolean = Boolean { value: true };
const FALSE: Boolean = Boolean { value: false };
const NULL: Null = Null {};

#[derive(Debug)]
pub enum EvaluatorType<'a> {
    Program(Program<'a>),
    Expressions(Rc<Expressions<'a>>),
    Statements(Rc<Statements<'a>>),
    BlockStatement(Rc<BlockStatement<'a>>),
}

pub fn eval(node: EvaluatorType<'static>) -> Option<Objects> {
    match node {
        EvaluatorType::Program(p) => eval_program(&p),
        EvaluatorType::Expressions(e) => return eval_expression(&e),
        // TODO we need to handle the statements later
        EvaluatorType::Statements(s) => eval_statement(&s),
        EvaluatorType::BlockStatement(b) => return eval_block_statements(&b),
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
                if is_error(&right) {
                    return Some(right);
                }
                return Some(eval_prefix_expression(&pref.operator, right));
            }
            None
        }
        Expressions::InfixExpression(infix) => {
            if let Some((left, right)) =
                eval_expression(&infix.left).zip(eval_expression(&infix.right))
            {
                if is_error(&left) {
                    return Some(left);
                }
                if is_error(&right) {
                    return Some(right);
                }
                return Some(eval_infix_expression(&infix.operator, left, right));
            }
            None
        }
        Expressions::IfExpression(if_exp) => return Some(eval_if_expression(if_exp)),
        // TODO we need to handle the expressions later
        _ => None,
    }
}

fn eval_statement(statement: &Statements<'static>) -> Option<Objects> {
    match statement {
        Statements::ExpressionStatement(ExpressionStatement {
            expression: Some(e),
            ..
        }) => return eval_expression(&e),
        Statements::ReturnStatement(r) => {
            if let Some(val) = r
                .return_value
                .as_ref()
                .and_then(|val| eval_expression(&val))
            {
                if is_error(&val) {
                    return Some(val);
                }
                return Some(Objects::ReturnValue(Rc::new(ReturnValue { value: val })));
            }
            None
        }
        _ => None,
    }
}

fn eval_program(program: &Program<'static>) -> Option<Objects> {
    let mut result = None;
    for statement in &program.statements {
        result = eval_statement(&statement);
        match result {
            Some(Objects::ReturnValue(return_value)) => {
                return Some(return_value.value.clone());
            }
            result @ Some(Objects::Error(_)) => return result,
            _ => {}
        }
    }
    result
}

fn eval_block_statements(block: &BlockStatement<'static>) -> Option<Objects> {
    let mut result = None;
    for statement in &block.statements {
        result = eval_statement(&statement);
        if let Some(Objects::ReturnValue(_) | Objects::Error(_)) = result {
            return result;
        }
    }
    result
}

fn eval_prefix_expression(operator: &str, right: Objects) -> Objects {
    match operator {
        "!" => Objects::Boolean(eval_bang_operator(right)),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Objects::Error(object::Error {
            message: format!("unknown operator: {}{}", operator, right.object_type()),
        }),
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
        (_, a, b) if a.object_type() != b.object_type() => Objects::Error(object::Error {
            message: format!(
                "type mismatch: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            ),
        }),
        _ => Objects::Error(object::Error {
            message: format!(
                "unknown operator: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            ),
        }),
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
        Objects::Error(object::Error {
            message: format!("unknown operator: -{}", object.object_type()),
        })
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
        (_, left, right) => Objects::Error(object::Error {
            message: format!(
                "unknown operator: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            ),
        }),
    }
}

fn eval_if_expression(ie: &IfExpression<'static>) -> Objects {
    if let Some(condition) = eval(EvaluatorType::Expressions(ie.condition.clone())) {
        if is_error(&condition) {
            return condition;
        }
        if is_truthy(condition) {
            return eval(EvaluatorType::BlockStatement(ie.consequence.clone()))
                .unwrap_or(Objects::Null(NULL));
        }
        if let Some(alt) = ie
            .alternative
            .clone()
            .and_then(|alt| eval(EvaluatorType::BlockStatement(Rc::new(alt.clone()))))
        {
            return alt;
        }
    }
    Objects::Null(NULL)
}

fn is_truthy(obj: Objects) -> bool {
    match obj {
        Objects::Null(_) => false,
        Objects::Boolean(Boolean { value: true }) => true,
        Objects::Boolean(Boolean { value: false }) => false,
        _ => true,
    }
}

fn is_error(obj: &Objects) -> bool {
    match obj {
        Objects::Error(_) => true,
        _ => false,
    }
}
