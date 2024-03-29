use crate::lexer::Lexer;
use crate::parser::ast::{ExpressionStatement, Expressions, IfExpression, Node, Statements};
use crate::parser::Parser;
use std::any::{Any, TypeId};

#[test]
fn test_let_statements() {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);
    assert!(prog.is_some(), "parse_program() returned None");
    if let Some(program) = prog {
        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements, got: {}",
            program.statements.len()
        );

        let tests = ["x", "y", "foobar"];

        for (i, tt) in tests.iter().enumerate() {
            match &program.statements[i] {
                Statements::LetStatement(stmt) => {
                    assert_eq!(&stmt.token_literal(), "let");
                    assert_eq!(&stmt.name.value, tt);
                    assert_eq!(&stmt.name.token_literal(), tt);
                }
                _ => panic!("Statement is not a let statement"),
            }
        }
    }
}

#[test]
fn test_return_statements() {
    let input = "
        return 5;
        return 10;
        return 838383;
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);
    assert!(prog.is_some(), "parse_program() returned None");
    if let Some(program) = prog {
        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements, got: {}",
            program.statements.len()
        );

        for statement in &program.statements {
            match statement {
                Statements::ReturnStatement(stmt) => {
                    assert_eq!(&stmt.token_literal(), "return");
                }
                _ => panic!("Statement is not a return statement"),
            }
        }
    }
}

#[test]
fn test_identifier_expression() {
    let input = "
        foobar;
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);
    assert!(prog.is_some(), "parse_program() returned None");
    if let Some(program) = prog {
        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements, got: {}",
            program.statements.len()
        );

        for statement in &program.statements {
            match statement {
                Statements::ExpressionStatement(ExpressionStatement {
                    token: t,
                    expression: Some(Expressions::Identifier(i)),
                }) => {
                    assert_eq!(&t.literal, "foobar");
                    assert_eq!(&i.value, "foobar");
                }
                _ => {
                    println!("Statement is not an expression statement {:?}", statement);
                    panic!()
                }
            }
        }
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "
        5;
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);
    assert!(prog.is_some(), "parse_program() returned None");
    if let Some(program) = prog {
        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements, got: {}",
            program.statements.len()
        );

        for statement in &program.statements {
            match statement {
                Statements::ExpressionStatement(ExpressionStatement {
                    token: t,
                    expression: Some(Expressions::IntegerLiteral(i)),
                }) => {
                    assert_eq!(&t.literal, "5");
                    assert_eq!(i.value, 5);
                }
                _ => {
                    println!("not an integer literal expression, got: {:?}", statement);
                    panic!()
                }
            }
        }
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    let prefix_test_1 = [("!5;", "!", 5), ("-15;", "-", 15)];
    let prefix_test_2 = [("!true;", "!", true), ("!false;", "!", false)];
    parsing_prefix_expressions_helper(&prefix_test_1);
    parsing_prefix_expressions_helper(&prefix_test_2);
}

fn parsing_prefix_expressions_helper(tests: &[(&str, &str, impl Any + Sized)]) {
    for input in tests {
        let l = Lexer::new(input.0);
        let mut p = Parser::new(l);
        let prog = p.parse_program();
        check_parser_errors(&p);

        assert!(prog.is_some(), "parse_program() returned None");
        if let Some(program) = prog {
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statements, got: {} {:?}",
                program.statements.len(),
                program.statements
            );

            match &program.statements[0] {
                Statements::ExpressionStatement(ExpressionStatement {
                    token: t,
                    expression: Some(Expressions::PrefixExpression(exp)),
                }) => {
                    assert_eq!(&t.literal, input.1);
                    assert_eq!(
                        exp.operator, input.1,
                        "exp.Operator is not {:?}, got {:?}",
                        input.1, exp.operator
                    );
                    if !test_literal_expression(&exp.right, &input.2) {
                        return;
                    }
                }
                statement => {
                    println!("not a PrefixExpression, got: {:?}", statement);
                    panic!()
                }
            }
        }
    }
}

#[test]
fn test_parsing_infix_expression() {
    let infix_test_1: [(&str, u64, &str, u64); 8] = [
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
    ];
    let infix_test_2 = [
        ("true == true", true, "==", true),
        ("true != false", true, "!=", false),
        ("false == false", false, "==", false),
    ];
    test_parsing_infix_helper(&infix_test_1);
    test_parsing_infix_helper(&infix_test_2);
}

fn test_parsing_infix_helper(tests: &[(&str, impl Any + Sized, &str, impl Any + Sized)]) {
    for (input, left_value, operator, right_value) in tests {
        let l = Lexer::new(*input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();
        check_parser_errors(&p);

        assert!(prog.is_some(), "parse_program() returned None");
        if let Some(program) = prog {
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statements, got: {} {:?}",
                program.statements.len(),
                program.statements
            );

            let stmt;
            if let Statements::ExpressionStatement(ExpressionStatement {
                expression: Some(e),
                ..
            }) = &program.statements[0]
            {
                stmt = e;
            } else {
                println!(
                    "program.statements[0] is not a ExpressionStatement, got: {:?}",
                    program.statements[0]
                );
                panic!()
            }
            if let Expressions::InfixExpression(exp) = stmt {
                assert!(test_literal_expression(&exp.left, left_value));
                assert_eq!(
                    exp.operator, *operator,
                    "exp.operator is not {}, got={}",
                    operator, exp.operator
                );
                assert!(test_literal_expression(&exp.right, right_value));
            }
        }
    }
}

#[test]
fn test_operator_procedure_parsing() {
    let tests = [
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
    ];
    for (input, expected) in tests {
        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        let actual = program.unwrap().to_string();
        assert_eq!(
            expected,
            actual.as_str(),
            "expected={}, got={}",
            expected,
            actual
        );
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);
    if let Some(prog) = program {
        if prog.statements.len() != 1 {
            eprintln!(
                "program.body does not contain 1 statement. got={}",
                prog.statements.len()
            );
            panic!();
        }

        if let Statements::ExpressionStatement(ExpressionStatement {
            expression:
                Some(Expressions::IfExpression(
                    if_expression @ IfExpression {
                        consequence,
                        alternative,
                        ..
                    },
                )),
            ..
        }) = &prog.statements[0]
        {
            if !test_infix_expression(&if_expression.condition, &"x", "<".to_string(), &"y") {
                return;
            }
            if if_expression.consequence.statements.len() != 1 {
                eprintln!(
                    "consequence is not 1 statement. got={}",
                    if_expression.consequence.statements.len()
                );
            }
            if let Statements::ExpressionStatement(ExpressionStatement {
                expression: Some(expression),
                ..
            }) = &consequence.statements[0]
            {
                if !test_identifier(&expression, "x".to_string()) {
                    return;
                }
            }
            if alternative.is_some() {
                eprintln!("alternative.statements was not nil. got={:?}", alternative);
            }
        } else {
            eprintln!(
                "program.statements[0] is not an ExpressionStatement and not an IfExpression. got={:?}",
                prog.statements[0]
            );
            panic!();
        }
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);
    if let Some(prog) = program {
        if prog.statements.len() != 1 {
            eprintln!(
                "program.body does not contain 1 statement. got={}",
                prog.statements.len()
            );
            panic!();
        }

        if let Statements::ExpressionStatement(ExpressionStatement {
            expression:
                Some(Expressions::IfExpression(
                    if_expression @ IfExpression {
                        consequence,
                        alternative,
                        ..
                    },
                )),
            ..
        }) = &prog.statements[0]
        {
            if !test_infix_expression(&if_expression.condition, &"x", "<".to_string(), &"y") {
                return;
            }
            if if_expression.consequence.statements.len() != 1 {
                eprintln!(
                    "consequence is not 1 statement. got={}",
                    if_expression.consequence.statements.len()
                );
            }
            if let Statements::ExpressionStatement(ExpressionStatement {
                expression: Some(expression),
                ..
            }) = &consequence.statements[0]
            {
                if !test_identifier(&expression, "x".to_string()) {
                    return;
                }
            }

            if alternative.is_some() && alternative.as_ref().unwrap().statements.len() != 1 {
                eprintln!(
                    "alternative does not exist of has more than 1 statement. got={:?}",
                    alternative
                );
            }

            if let Some(Statements::ExpressionStatement(ExpressionStatement {
                expression: Some(expression),
                ..
            })) = alternative.as_ref().and_then(|a| Some(&a.statements[0]))
            {
                if !test_identifier(&expression, "y".to_string()) {
                    return;
                }
            }
        } else {
            eprintln!(
                "program.statements[0] is not an ExpressionStatement and not an IfExpression. got={:?}",
                prog.statements[0]
            );
            panic!();
        }
    }
}
#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let prog = parser.parse_program();
    check_parser_errors(&parser);
    let program;
    if let Some(prog) = prog {
        if prog.statements.len() != 1 {
            eprintln!(
                "program.body does not contain 1 statement. got={}",
                prog.statements.len()
            );
            panic!();
        }
        program = prog;
    } else {
        panic!("Program does not exist");
    }
    let function;
    match &program.statements[0] {
        Statements::ExpressionStatement(ExpressionStatement {
            expression: Some(Expressions::FunctionLiteral(fn_literal)),
            ..
        }) => {
            function = fn_literal;
        }
        _ => {
            eprintln!(
                "stmt.expression is not ast.FunctionLiteral, got={:?}",
                program.statements[0]
            );
            panic!();
        }
    };
    if function.parameters.len() != 2 {
        eprintln!(
            "function literal parameters wrong. want 2, got={}",
            function.parameters.len()
        );
        panic!();
    }

    test_literal_expression(
        &Expressions::Identifier(function.parameters[0].clone()),
        &"x",
    );
    test_literal_expression(
        &Expressions::Identifier(function.parameters[1].clone()),
        &"y",
    );

    if function.body.statements.len() != 1 {
        eprintln!(
            "function body statements does not 1 statement. got={}",
            function.body.statements.len()
        )
    }
    match &function.body.statements[0] {
        Statements::ExpressionStatement(ExpressionStatement {
            expression: Some(body_stmt),
            ..
        }) => {
            test_infix_expression(&body_stmt, &"x", "+".to_string(), &"y");
        }
        _ => {
            eprintln!(
                "function body stmt is not ast.ExpressionStatement, got={:?}",
                function.body.statements[0]
            )
        }
    };
}

#[test]
fn test_function_parameter_parsing() {
    let tests: [(&str, &[&str]); 3] = [
        ("fn() {};", &[]),
        ("fn(x) {};", &["x"]),
        ("fn(x, y, z) {};", &["x", "y", "Z"]),
    ];

    for (input, expected_params) in tests {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let prog = parser.parse_program();
        check_parser_errors(&parser);
        let program = match prog {
            Some(prog) => {
                if prog.statements.len() != 1 {
                    eprintln!(
                        "program.body does not contain 1 statement. got={}",
                        prog.statements.len()
                    );
                    panic!();
                }
                prog
            }
            _ => {
                panic!("Program does not exist");
            }
        };

        match &program.statements[0] {
            Statements::ExpressionStatement(ExpressionStatement {
                expression: Some(Expressions::FunctionLiteral(func)),
                ..
            }) => {
                if func.parameters.len() != expected_params.len() {
                    eprintln!(
                        "length parameters wrong. expected {}, got={}",
                        expected_params.len(),
                        func.parameters.len()
                    );
                    panic!();
                }
                for (i, ident) in expected_params.iter().enumerate() {
                    test_literal_expression(
                        &Expressions::Identifier(func.parameters[i].clone()),
                        ident,
                    );
                }
            }
            _ => {
                eprintln!("program.statements[0] was not a function literal");
                return;
            }
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 * 5);";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let prog = parser.parse_program();
    check_parser_errors(&parser);
    let program = match prog {
        Some(prog) => {
            if prog.statements.len() != 1 {
                eprintln!(
                    "program.body does not contain 1 statement. got={}",
                    prog.statements.len()
                );
                panic!();
            }
            prog
        }
        _ => {
            panic!("Program does not exist");
        }
    };
    match &program.statements[0] {
        Statements::ExpressionStatement(ExpressionStatement {
            expression: Some(Expressions::CallExpression(call)),
            ..
        }) => {
            if !test_identifier(&call.function, "add".to_string()) {
                eprintln!("wrong call expression, expected=add, got={}", call.function);
                return;
            }

            if call.arguments.len() != 3 {
                eprintln!(
                    "wrong number of arguments, expected=3, got={}",
                    call.arguments.len()
                );
            }
            test_literal_expression(&call.arguments[0], &1);
            test_infix_expression(&call.arguments[1], &2, "*".to_string(), &3);
            test_infix_expression(&call.arguments[2], &4, "+".to_string(), &5);
        }
        _ => {
            eprintln!("program.statements[0] was not an ast.CallExpression");
            return;
        }
    }
}

fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();
    if errors.is_empty() {
        return;
    }

    println!("Parser has {} errors", errors.len());
    for msg in errors.iter() {
        println!("parser error: {}", msg);
    }
    panic!();
}

fn test_integer_literal(il: &Expressions, value: u64) -> bool {
    if let Expressions::IntegerLiteral(integ) = il {
        if integ.value != value {
            eprintln!("integ.value is not {}. got={}", value, integ.value);
            return false;
        }
        if integ.token_literal() != value.to_string() {
            eprintln!(
                "integ.token_literal not {}. got={}",
                value,
                integ.token_literal()
            );
            return false;
        }
        return true;
    }
    eprintln!("il not ast.IntegerLiteral. got={:?}", il);
    false
}

fn test_boolean_literal<'a>(exp: &Expressions<'a>, value: bool) -> bool {
    let bo;
    match exp {
        Expressions::Boolean(b) => bo = b,
        _ => {
            eprintln!("exp is not ast.Boolean got={:?}", exp);
            return false;
        }
    }
    if bo.value != value {
        eprintln!("bo.value is not {}, got={}", value, bo.value);
        return false;
    }
    if bo.token_literal() != value.to_string() {
        eprintln!(
            "bo.token_literal is not {}, got={}",
            value,
            bo.token_literal()
        );
        return false;
    }
    true
}

fn test_identifier<'a>(exp: &Expressions<'a>, value: String) -> bool {
    if let Expressions::Identifier(ident) = exp {
        if ident.value != value {
            eprintln!("ident.Value not {}. got {}", value, ident.value);
            return false;
        }
        if ident.token_literal() != value {
            eprintln!(
                "ident.TokenLiteral not {}. got {}",
                value,
                ident.token_literal()
            );
            return false;
        }
    } else {
        eprintln!("exp not Identifier. got {:?}", exp);
        return false;
    }
    true
}

fn test_literal_expression<'a, T: Sized + Any>(exp: &Expressions<'a>, expected: &T) -> bool {
    let value = expected as &dyn Any;
    // return test_integer_literal(&exp, value.downcast_mut::<u64>().unwrap().clone());

    if value.type_id() == TypeId::of::<i32>() {
        return test_integer_literal(&exp, value.downcast_ref::<i32>().unwrap().clone() as u64);
    }
    if value.type_id() == TypeId::of::<u64>() {
        return test_integer_literal(&exp, value.downcast_ref::<u64>().unwrap().clone());
    }

    if value.type_id() == TypeId::of::<String>() {
        return test_identifier(exp, value.downcast_ref::<String>().unwrap().to_string());
    }

    if value.type_id() == TypeId::of::<bool>() {
        return test_boolean_literal(exp, value.downcast_ref::<bool>().unwrap().clone());
    }
    eprintln!("type of exp not handled, got={:?}", value);
    return false;
}

fn test_infix_expression<'a, T: Sized + Any>(
    exp: &Expressions<'a>,
    left: &T,
    operator: String,
    right: &T,
) -> bool {
    let op_exp;
    if let Expressions::InfixExpression(exp) = exp {
        op_exp = exp;
    } else {
        eprintln!("exp is not ast.operatorExpression");
        return false;
    }

    if !test_literal_expression(&op_exp.left, left) {
        return false;
    }
    if op_exp.operator != operator {
        eprintln!("exp.operator is not {}, got {}", operator, op_exp.operator);
        return false;
    }
    if !test_literal_expression(&op_exp.right, right) {
        return false;
    }
    true
}
