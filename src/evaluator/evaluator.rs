use std::{rc::Rc, sync::Mutex};

use crate::{
    object::{self, Boolean, Environment, Function, Integer, Null, Object, Objects, ReturnValue},
    parser::ast::{
        self, BlockStatement, ExpressionStatement, Expressions, Identifier, IfExpression, Program,
        Statements,
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

pub fn eval(node: EvaluatorType<'static>, environment: Rc<Mutex<Environment>>) -> Option<Objects> {
    match node {
        EvaluatorType::Program(p) => eval_program(&p, environment),
        EvaluatorType::Expressions(e) => return eval_expression(&e, environment),
        // TODO we need to handle the statements later
        EvaluatorType::Statements(s) => eval_statement(&s, environment),
        EvaluatorType::BlockStatement(b) => return eval_block_statements(&b, environment),
    }
}

fn eval_expression(
    expression: &Expressions<'static>,
    environment: Rc<Mutex<Environment>>,
) -> Option<Objects> {
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
            if let Some(right) = eval(EvaluatorType::Expressions(pref.right.clone()), environment) {
                if is_error(&right) {
                    return Some(right);
                }
                return Some(eval_prefix_expression(&pref.operator, right));
            }
            None
        }
        Expressions::InfixExpression(infix) => {
            if let Some((left, right)) = eval_expression(&infix.left, environment.clone())
                .zip(eval_expression(&infix.right, environment.clone()))
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
        Expressions::IfExpression(if_exp) => {
            return Some(eval_if_expression(if_exp, environment.clone()))
        }
        Expressions::Identifier(ident) => return Some(eval_identifier(ident, environment.clone())),
        Expressions::FunctionLiteral(func) => {
            let parameters = func.parameters.clone();
            let body = func.body.clone();
            return Some(Objects::Function(object::Function {
                parameters,
                body,
                env: environment.clone(),
            }));
        }
        Expressions::CallExpression(call) => {
            match eval_expression(&call.function, environment.clone()) {
                Some(Objects::Error(e)) => return Some(Objects::Error(e)),
                Some(Objects::Function(fun)) => {
                    let args = eval_expressions(&call.arguments, environment.clone());
                    if args.len() == 1 && is_error(&args[0]) {
                        return Some(args[0].clone());
                    }
                    return apply_function(Objects::Function(fun), args);
                }
                _ => None,
            }
        }
        // TODO we need to handle the expressions later
        _ => None,
    }
}

fn eval_statement(
    statement: &Statements<'static>,
    environment: Rc<Mutex<Environment>>,
) -> Option<Objects> {
    match statement {
        Statements::ExpressionStatement(ExpressionStatement {
            expression: Some(e),
            ..
        }) => return eval_expression(&e, environment),
        Statements::ReturnStatement(r) => {
            if let Some(val) = r
                .return_value
                .as_ref()
                .and_then(|val| eval_expression(&val, environment.clone()))
            {
                if is_error(&val) {
                    return Some(val);
                }
                return Some(Objects::ReturnValue(Rc::new(ReturnValue { value: val })));
            }
            None
        }
        Statements::LetStatement(l) => {
            if let Some(val) = l
                .value
                .as_ref()
                .and_then(|let_exp| eval_expression(&let_exp, environment.clone()))
            {
                if is_error(&val) {
                    return Some(val);
                }
                environment.lock().unwrap().set(l.name.value.clone(), val);
            }
            return None;
        }
        _ => None,
    }
}

fn eval_program(
    program: &Program<'static>,
    environment: Rc<Mutex<Environment>>,
) -> Option<Objects> {
    let mut result = None;
    for statement in &program.statements {
        result = eval_statement(&statement, environment.clone());
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

fn eval_block_statements(
    block: &BlockStatement<'static>,
    environment: Rc<Mutex<Environment>>,
) -> Option<Objects> {
    let mut result = None;
    for statement in &block.statements {
        result = eval_statement(&statement, environment.clone());
        if let Some(Objects::ReturnValue(_) | Objects::Error(_)) = result {
            return result;
        }
    }
    result
}

fn eval_expressions(
    exps: &[Expressions<'static>],
    environment: Rc<Mutex<Environment>>,
) -> Vec<Objects> {
    let mut result = Vec::new();
    for e in exps {
        match eval_expression(e, environment.clone()) {
            Some(Objects::Error(e)) => return vec![Objects::Error(e)],
            Some(evaluated) => result.push(evaluated),
            _ => {}
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

fn eval_if_expression(ie: &IfExpression<'static>, environment: Rc<Mutex<Environment>>) -> Objects {
    if let Some(condition) = eval(
        EvaluatorType::Expressions(ie.condition.clone()),
        environment.clone(),
    ) {
        if is_error(&condition) {
            return condition;
        }
        if is_truthy(condition) {
            return eval(
                EvaluatorType::BlockStatement(ie.consequence.clone()),
                environment.clone(),
            )
            .unwrap_or(Objects::Null(NULL));
        }
        if let Some(alt) = ie.alternative.clone().and_then(|alt| {
            eval(
                EvaluatorType::BlockStatement(Rc::new(alt.clone())),
                environment.clone(),
            )
        }) {
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

fn eval_identifier(ident: &Identifier<'static>, environment: Rc<Mutex<Environment>>) -> Objects {
    match environment.lock().unwrap().get(&ident.value) {
        Some(val) => val.clone(),
        None => Objects::Error(object::Error {
            message: format!("identifier not found: {}", ident.value),
        }),
    }
}

fn apply_function(func: Objects, args: Vec<Objects>) -> Option<Objects> {
    if let Objects::Function(object) = func {
        let extended_env = extend_function_env(&object, &args);
        if let Some(evaluated) =
            eval_block_statements(&object.body, Rc::new(Mutex::new(extended_env)))
        {
            return Some(unwrap_return_value(evaluated));
        }
        None
    } else {
        Some(Objects::Error(object::Error {
            message: format!("not a function: {}", func.object_type()),
        }))
    }
}

fn extend_function_env(func: &Function, args: &[Objects]) -> Environment {
    let mut env = Environment::new_enclosed(func.env.clone());
    for (param_idx, param) in func.parameters.iter().enumerate() {
        env.set(param.value.clone(), args[param_idx].clone());
    }
    env
}

fn unwrap_return_value(obj: Objects) -> Objects {
    if let Objects::ReturnValue(r) = obj {
        return r.value.clone();
    }
    obj
}
