use crate::{lexer::Lexer, object::Objects, parser::Parser};

use super::evaluator;

#[test]
fn test_eval_integer_expression() {
    let tests = [
        ("5", 5),
        ("10", 10),
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
    ];
    for (input, expected) in tests {
        if let Some(evaluated) = test_eval(input) {
            test_integer_object(evaluated, expected);
        } else {
            eprintln!("The evaluation did not succeed");
            panic!();
        }
    }
}

fn test_eval(input: &str) -> Option<Objects> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = match parser.parse_program() {
        Some(p) => p,
        None => return None,
    };
    return evaluator::eval(super::EvaluatorType::Program(program));
}

fn test_integer_object(obj: Objects, expected: i64) -> bool {
    let result;
    if let Objects::Integer(i) = obj {
        result = i;
    } else {
        eprintln!("object is not Integer. got={:?}", obj);
        return false;
    }

    if result.value != expected {
        eprintln!(
            "object has wrong value, got={}, want={}",
            result.value, expected
        );
        return false;
    }
    true
}

#[test]
fn test_eval_boolean_expression() {
    let tests = [("true", true), ("false", false)];

    for (input, expected) in tests {
        if let Some(evaluated) = test_eval(input) {
            test_boolean_object(evaluated, expected);
        } else {
            eprintln!("The evaluation did not succeed");
            panic!();
        }
    }
}

fn test_boolean_object(obj: Objects, expected: bool) -> bool {
    let result;
    if let Objects::Boolean(o) = obj {
        result = o;
    } else {
        eprintln!("object is not boolean, got={:?}", obj);
        return false;
    }

    if result.value != expected {
        eprintln!(
            "object has wrong value, got={}, want={}",
            result.value, expected
        );
        return false;
    }
    true
}
#[test]
fn test_bang_operator() {
    let tests = [
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];
    for (input, expected) in tests {
        if let Some(evaluated) = test_eval(input) {
            test_boolean_object(evaluated, expected);
        } else {
            eprintln!("The evaluation did not succeed");
            panic!();
        }
    }
}
