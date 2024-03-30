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
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];
    let mut evaluation_failed = false;
    for (input, expected) in tests {
        if let Some(evaluated) = test_eval(input) {
            if !test_integer_object(evaluated, expected) {
                eprintln!("could not evaluate {}", input);
                evaluation_failed = true;
            }
        } else {
            eprintln!("The evaluation did not succeed");
            evaluation_failed = true;
        }
    }
    if evaluation_failed {
        panic!()
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
        eprint!("object is not Integer. got={:?} ", obj);
        return false;
    }

    if result.value != expected {
        eprint!(
            "object has wrong value, got={}, want={}",
            result.value, expected
        );
        return false;
    }
    true
}

#[test]
fn test_eval_boolean_expression() {
    let tests = [
        ("true", true),
        ("false", false),
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];
    let mut evaluation_failed = false;
    for (input, expected) in tests {
        if let Some(evaluated) = test_eval(input) {
            if !test_boolean_object(evaluated, expected) {
                eprintln!("could not evaluate {}", input);
                evaluation_failed = true;
            }
        } else {
            eprintln!("The evaluation did not succeed");
            evaluation_failed = true;
        }
    }
    if evaluation_failed {
        panic!()
    }
}

fn test_boolean_object(obj: Objects, expected: bool) -> bool {
    let result;
    if let Objects::Boolean(o) = obj {
        result = o;
    } else {
        eprint!("object is not boolean, got={:?} ", obj);
        return false;
    }

    if result.value != expected {
        eprint!(
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
    let mut evaluation_failed = false;
    for (input, expected) in tests {
        if let Some(evaluated) = test_eval(input) {
            if !test_boolean_object(evaluated, expected) {
                eprintln!("could not evaluate {}", input);
                evaluation_failed = true;
            }
        } else {
            eprintln!("The evaluation did not succeed");
            evaluation_failed = true;
        }
    }
    if evaluation_failed {
        panic!()
    }
}

#[test]
fn test_if_else_expressions() {
    let tests = [
        ("if (true) { 10 }", 10),
        ("if (1) { 10 }", 10),
        ("if (1 < 2) { 10 }", 10),
        ("if (1 > 2) { 10 } else { 20 }", 20),
        ("if (1 < 2) { 10 } else { 20 }", 10),
    ];
    // tests that have an expected null output
    let null_tests = [("if (1 > 2) { 10 }"), ("if (false) { 10 }")];

    let mut evaluation_failed = false;

    for (input, expected) in tests {
        if let Some(evaluated) = test_eval(input) {
            if !test_integer_object(evaluated, expected) {
                eprintln!("could not evaluate {}", input);
                evaluation_failed = true;
            }
        } else {
            eprintln!("The evaluation did not succeed");
            evaluation_failed = true;
        }
    }

    for input in null_tests {
        if let Some(evaluated) = test_eval(input) {
            if !test_null_object(evaluated) {
                eprintln!("could not evaluate {}", input);
                evaluation_failed = true;
            }
        } else {
            eprintln!("The evaluation did not succeed");
            evaluation_failed = true;
        }
    }
    if evaluation_failed {
        panic!()
    }
}

fn test_null_object(obj: Objects) -> bool {
    if let Objects::Null(_) = obj {
        return true;
    }
    eprintln!("object is not NULL. got={:?}", obj);
    false
}

#[test]
fn test_return_statements() {
    let tests = [
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        (
            "\nif (10 > 1) {\n\tif (10 > 1) {\n\t\treturn 10;\n\t}\n\treturn 1;\n}",
            10,
        ),
    ];
    let mut evaluation_failed = false;
    for (input, expected) in tests {
        if let Some(evaluated) = test_eval(input) {
            if !test_integer_object(evaluated, expected) {
                eprintln!(" could not evaluate {} ", input);
                evaluation_failed = true;
            }
        } else {
            eprintln!("The evaluation did not succeed");
            evaluation_failed = true;
        }
    }
    if evaluation_failed {
        panic!()
    }
}
