use crate::token;

use super::Lexer;

#[test]
fn test_next_token() {
    let input = "=+(){},;";
    let tests: [(&str, &str); 9] = [
        (token::ASSIGN, "="),
        (token::PLUS, "+"),
        (token::LPAREN, "("),
        (token::RPAREN, ")"),
        (token::LBRACE, "{"),
        (token::RBRACE, "}"),
        (token::COMMA, ","),
        (token::SEMICOLON, ";"),
        (token::EOF, "\\"),
    ];

    let mut l = Lexer::new(input);

    for (i, (expected_token, expected_literal)) in tests.iter().enumerate() {
        let tok = l.next_token();
        assert_eq!(
            &tok.token_type, expected_token,
            "tests[{}] - tokentype wrong. expected={}, got={}",
            i, expected_token, tok.token_type,
        );
        assert_eq!(
            &tok.literal, expected_literal,
            "tests[{}] - literal wrong. expected={}, got={}",
            i, expected_literal, tok.literal,
        );
    }
}

#[test]
fn test_next_token_2() {
    let input = "
        let five=5;
        let ten=10;
        
        let add = fn(x, y) {
            x + y;
        };
        
        let result = add(five, ten);";
    let tests: [(&str, &str); 37] = [
        (token::LET, "let"),
        (token::IDENT, "five"),
        (token::ASSIGN, "="),
        (token::INT, "5"),
        (token::SEMICOLON, ";"),
        (token::LET, "let"),
        (token::IDENT, "ten"),
        (token::ASSIGN, "="),
        (token::INT, "10"),
        (token::SEMICOLON, ";"),
        (token::LET, "let"),
        (token::IDENT, "add"),
        (token::ASSIGN, "="),
        (token::FUNCTION, "fn"),
        (token::LPAREN, "("),
        (token::IDENT, "x"),
        (token::COMMA, ","),
        (token::IDENT, "y"),
        (token::RPAREN, ")"),
        (token::LBRACE, "{"),
        (token::IDENT, "x"),
        (token::PLUS, "+"),
        (token::IDENT, "y"),
        (token::SEMICOLON, ";"),
        (token::RBRACE, "}"),
        (token::SEMICOLON, ";"),
        (token::LET, "let"),
        (token::IDENT, "result"),
        (token::ASSIGN, "="),
        (token::IDENT, "add"),
        (token::LPAREN, "("),
        (token::IDENT, "five"),
        (token::COMMA, ","),
        (token::IDENT, "ten"),
        (token::RPAREN, ")"),
        (token::SEMICOLON, ";"),
        (token::EOF, "\\"),
    ];

    let mut l = Lexer::new(input);

    for (i, (expected_token, expected_literal)) in tests.iter().enumerate() {
        let tok = l.next_token();
        assert_eq!(
            &tok.token_type, expected_token,
            "tests[{}] - tokentype wrong. expected={}, got={} with value {}",
            i, expected_token, tok.token_type, tok.literal,
        );
        assert_eq!(
            &tok.literal, expected_literal,
            "tests[{}] - literal wrong. expected={}, got={}",
            i, expected_literal, tok.literal,
        );
    }
}

#[test]
fn test_next_token_3() {
    let input = "
        !-/*5;
        5 < 10 > 5;";
    let tests: [(&str, &str); 13] = [
        (token::BANG, "!"),
        (token::MINUS, "-"),
        (token::SLASH, "/"),
        (token::ASTERISK, "*"),
        (token::INT, "5"),
        (token::SEMICOLON, ";"),
        (token::INT, "5"),
        (token::LT, "<"),
        (token::INT, "10"),
        (token::GT, ">"),
        (token::INT, "5"),
        (token::SEMICOLON, ";"),
        (token::EOF, "\\"),
    ];

    let mut l = Lexer::new(input);

    for (i, (expected_token, expected_literal)) in tests.iter().enumerate() {
        let tok = l.next_token();
        assert_eq!(
            &tok.token_type, expected_token,
            "tests[{}] - tokentype wrong. expected={}, got={} with value {}",
            i, expected_token, tok.token_type, tok.literal,
        );
        assert_eq!(
            &tok.literal, expected_literal,
            "tests[{}] - literal wrong. expected={}, got={}",
            i, expected_literal, tok.literal,
        );
    }
}

#[test]
fn test_next_token_lt_gt_if_else_return() {
    let input = "if (5 < 10) {
        return true;
        } else {
        return false;
        }";
    let tests: [(&str, &str); 18] = [
        (token::IF, "if"),
        (token::LPAREN, "("),
        (token::INT, "5"),
        (token::LT, "<"),
        (token::INT, "10"),
        (token::RPAREN, ")"),
        (token::LBRACE, "{"),
        (token::RETURN, "return"),
        (token::TRUE, "true"),
        (token::SEMICOLON, ";"),
        (token::RBRACE, "}"),
        (token::ELSE, "else"),
        (token::LBRACE, "{"),
        (token::RETURN, "return"),
        (token::FALSE, "false"),
        (token::SEMICOLON, ";"),
        (token::RBRACE, "}"),
        (token::EOF, "\\"),
    ];

    let mut l = Lexer::new(input);

    for (i, (expected_token, expected_literal)) in tests.iter().enumerate() {
        let tok = l.next_token();
        assert_eq!(
            &tok.token_type, expected_token,
            "tests[{}] - tokentype wrong. expected={}, got={}",
            i, expected_token, tok.token_type,
        );
        assert_eq!(
            &tok.literal, expected_literal,
            "tests[{}] - literal wrong. expected={}, got={}",
            i, expected_literal, tok.literal,
        );
    }
}

#[test]
fn test_next_token_eq_ne() {
    let input = "
        10 == 10;
        10 != 9;";
    let tests: [(&str, &str); 9] = [
        (token::INT, "10"),
        (token::EQ, "=="),
        (token::INT, "10"),
        (token::SEMICOLON, ";"),
        (token::INT, "10"),
        (token::NOT_EQ, "!="),
        (token::INT, "9"),
        (token::SEMICOLON, ";"),
        (token::EOF, "\\"),
    ];

    let mut l = Lexer::new(input);

    for (i, (expected_token, expected_literal)) in tests.iter().enumerate() {
        let tok = l.next_token();
        assert_eq!(
            &tok.token_type, expected_token,
            "tests[{}] - tokentype wrong. expected={}, got={}",
            i, expected_token, tok.token_type,
        );
        assert_eq!(
            &tok.literal, expected_literal,
            "tests[{}] - literal wrong. expected={}, got={}",
            i, expected_literal, tok.literal,
        );
    }
}
