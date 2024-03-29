use crate::{lexer::Lexer, object::Objects, parser::Parser};

use super::evaluator;

#[test]
fn test_eval_integer_expression() {
    let tests = [("5", 5), ("10", 10)];
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

fn test_integer_object(obj: Objects, expected: u64) -> bool {
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
