use std::{io::Write, rc::Rc, sync::Mutex};

use object::{Environment, Object};

use crate::{lexer::Lexer, parser::Parser};

pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;

const PROMPT: &str = "Let's Start Transpiling, start typing commands.";
fn main() {
    let scanner = std::io::stdin();
    let mut stdout = std::io::stdout();
    stdout.flush().unwrap();

    stdout.write_fmt(format_args!("{}", PROMPT)).unwrap();

    let environment = Rc::new(Mutex::new(Environment::new()));
    loop {
        stdout.write_all(b"\n>>").unwrap();
        stdout.flush().unwrap();
        let mut input = String::new();
        match scanner.read_line(&mut input) {
            Ok(_scan_size) => {}
            Err(e) => {
                eprintln!("Error {}", e);
                return;
            }
        }
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = match parser.parse_program() {
            Some(program) => program,
            None => continue,
        };
        if parser.errors().len() != 0 {
            print_parser_errors(parser.errors());
            continue;
        }
        if let Some(evaluated) = evaluator::eval(
            evaluator::EvaluatorType::Program(program),
            environment.clone(),
        ) {
            stdout
                .write_fmt(format_args!("{}", evaluated.inspect()))
                .unwrap();
        }
    }
}

fn print_parser_errors(errors: &[String]) {
    eprintln!("We ran into some parser errors: ");
    for e in errors {
        eprintln!("\t{}", e);
    }
}
