use std::io::Write;

use crate::{lexer::Lexer, parser::Parser};

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
    // println!("{}", PROMPT);
    loop {
        stdout.write_all(b"\n>>").unwrap();
        stdout.flush().unwrap();
        // println!(">>");
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
        stdout
            .write_fmt(format_args!("{}", program.to_string()))
            .unwrap();
    }
}

fn print_parser_errors(errors: &[String]) {
    eprintln!("We ran into some parser errors: ");
    for e in errors {
        eprintln!("\t{}", e);
    }
}
