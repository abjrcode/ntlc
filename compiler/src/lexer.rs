use std::error::Error;
use std::fmt::Debug;
use std::vec::Vec;

/*
 *    t ::= // terms
 *        true  // constant true
 *        false // constant false
 *        if t then t else t // conditional
 *        0 // constant zero
 *        succ t // successor
 *        pred t // predecessor
 *        iszero t // zero test
 *        */

static KEYWORD_TRUE: &str = "true";
static KEYWORD_FALSE: &str = "false";
static KEYWORD_IF: &str = "if";
static KEYWORD_THEN: &str = "then";
static KEYWORD_ELSE: &str = "else";
static KEYWORD_IS_ZERO: &str = "iszero";
static KEYWORD_ZERO: &str = "0";
static KEYWORD_SUCC: &str = "succ";
static KEYWORD_PRED: &str = "pred";

#[allow(clippy::upper_case_acronyms)]
#[derive(PartialEq)]
pub enum Token {
    True,
    False,
    If,
    Then,
    Else,
    Zero,
    Succ,
    Pred,
    IsZero,
    LeftParenthesis,
    RightParenthesis,
    EOF,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::True => write!(f, "TRUE"),
            Token::False => write!(f, "FALSE"),
            Token::If => write!(f, "IF"),
            Token::Then => write!(f, "THEN"),
            Token::Else => write!(f, "ELSE"),
            Token::Zero => write!(f, "0"),
            Token::Succ => write!(f, "SUCC"),
            Token::Pred => write!(f, "PRED"),
            Token::IsZero => write!(f, "IS_ZERO"),
            Token::LeftParenthesis => write!(f, "("),
            Token::RightParenthesis => write!(f, ")"),
            Token::EOF => write!(f, "<EOF>"),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::True => write!(f, "TRUE"),
            Token::False => write!(f, "FALSE"),
            Token::If => write!(f, "IF"),
            Token::Then => write!(f, "THEN"),
            Token::Else => write!(f, "ELSE"),
            Token::Zero => write!(f, "0"),
            Token::Succ => write!(f, "SUCC"),
            Token::Pred => write!(f, "PRED"),
            Token::IsZero => write!(f, "IS_ZERO"),
            Token::LeftParenthesis => write!(f, "("),
            Token::RightParenthesis => write!(f, ")"),
            Token::EOF => write!(f, "<EOF>"),
        }
    }
}

#[derive(PartialEq)]
pub enum LexingError {
    UnrecognizedToken {
        found: String,
        column: usize,
        line: usize,
    },
    UnexpectedCharacter {
        found: char,
        expected: Vec<String>,
        column: usize,
        line: usize,
    },
    UnexpectedEOF {
        expected: Vec<String>,
        column: usize,
        line: usize,
    },
}

fn format_expectation_error(expected: &Vec<String>) -> String {
    let mut result: Vec<String> = Vec::new();

    for e in expected {
        match e {
            s if s == " " => result.push("<SPACE>".to_string()),
            s if s == "\t" => result.push("<TAB>".to_string()),
            s if s == "\0" => result.push("<EOF>".to_string()),
            s => result.push(format!("'{}'", s)),
        }
    }

    result.join(", ")
}

impl std::fmt::Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexingError::UnrecognizedToken {
                found,
                column,
                line,
            } => {
                write!(
                    f,
                    "Unrecognized token '{}' at line {}, column {}",
                    found,
                    line + 1,
                    column
                )
            }
            LexingError::UnexpectedCharacter {
                found,
                expected,
                column,
                line,
            } => {
                write!(
                    f,
                    "Found '{}' at line {}, column {} when perhaphs you meant '{}'?",
                    found,
                    line,
                    column + 1,
                    format_expectation_error(expected)
                )
            }
            LexingError::UnexpectedEOF {
                expected,
                column,
                line,
            } => {
                write!(
                    f,
                    "Unexpected end of file at line {}, column {} when you perhaps meant '{}'?",
                    line,
                    column + 1,
                    format_expectation_error(expected)
                )
            }
        }
    }
}

impl std::fmt::Debug for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexingError::UnrecognizedToken {
                found,
                column,
                line,
            } => {
                write!(f, "Unknown token '{}' at ({}, {})", found, line, column)
            }
            LexingError::UnexpectedCharacter {
                found,
                expected,
                column,
                line,
            } => {
                write!(
                    f,
                    "Unexpected token '{}' at ({}, {}). Expectation context {}",
                    found,
                    line,
                    column,
                    format_expectation_error(expected)
                )
            }
            LexingError::UnexpectedEOF {
                expected,
                column,
                line,
            } => {
                write!(
                    f,
                    "Unexpected EOF looking for lexeme '{}' at ({}, {})",
                    format_expectation_error(expected),
                    line,
                    column
                )
            }
        }
    }
}

impl Error for LexingError {}

pub fn scan(input: &str) -> Result<Vec<Token>, LexingError> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut chars = input
        .chars()
        .zip(input.chars().skip(1).chain(['\0']))
        .peekable();

    let line = 1usize;
    let mut index = 0usize;

    while let Some(&(c, next)) = chars.peek() {
        match c {
            't' => match next {
                'r' => {
                    index += consume(&mut chars, KEYWORD_TRUE, line, index)?;
                    tokens.push(Token::True);
                    match_one_of(
                        chars.peek().map(|&(x, _)| x),
                        &[' ', '\t', ')'],
                        true,
                        line,
                        index,
                    )?;
                }
                'h' => {
                    index += consume(&mut chars, KEYWORD_THEN, line, index)?;
                    tokens.push(Token::Then);
                    match_one_of(
                        chars.peek().map(|&(x, _)| x),
                        &[' ', '\t'],
                        false,
                        line,
                        index,
                    )?;
                }
                '\0' => {
                    return Err(LexingError::UnexpectedEOF {
                        expected: vec!["true".to_string(), "then".to_string()],
                        column: index,
                        line,
                    })
                }
                c => {
                    return Err(LexingError::UnexpectedCharacter {
                        found: c,
                        expected: vec!["true".to_string(), "then".to_string()],
                        column: index,
                        line,
                    })
                }
            },
            'f' => {
                index += consume(&mut chars, KEYWORD_FALSE, line, index)?;
                tokens.push(Token::False);
                match_one_of(
                    chars.peek().map(|&(x, _)| x),
                    &[' ', '\t', ')'],
                    true,
                    line,
                    index,
                )?;
            }
            'i' => match next {
                'f' => {
                    index += consume(&mut chars, KEYWORD_IF, line, index)?;
                    tokens.push(Token::If);
                    match_one_of(
                        chars.peek().map(|&(x, _)| x),
                        &[' ', '\t'],
                        false,
                        line,
                        index,
                    )?;
                }
                's' => {
                    index += consume(&mut chars, KEYWORD_IS_ZERO, line, index)?;
                    tokens.push(Token::IsZero);
                    match_one_of(
                        chars.peek().map(|&(x, _)| x),
                        &[' ', '\t', '('],
                        false,
                        line,
                        index,
                    )?;
                }
                '\0' => {
                    return Err(LexingError::UnexpectedEOF {
                        expected: vec!["if".to_string(), "iszero".to_string()],
                        column: index,
                        line,
                    })
                }
                c => {
                    return Err(LexingError::UnexpectedCharacter {
                        found: c,
                        expected: vec!["if".to_string(), "iszero".to_string()],
                        column: index,
                        line,
                    })
                }
            },
            'e' => {
                index += consume(&mut chars, KEYWORD_ELSE, line, index)?;
                tokens.push(Token::Else);
                match_one_of(
                    chars.peek().map(|&(x, _)| x),
                    &[' ', '\t'],
                    false,
                    line,
                    index,
                )?;
            }
            '0' => {
                index += consume(&mut chars, KEYWORD_ZERO, line, index)?;
                tokens.push(Token::Zero);
                match_one_of(
                    chars.peek().map(|&(x, _)| x),
                    &[' ', '\t', ')'],
                    true,
                    line,
                    index,
                )?;
            }
            's' => {
                index += consume(&mut chars, KEYWORD_SUCC, line, index)?;
                tokens.push(Token::Succ);
                match_one_of(
                    chars.peek().map(|&(x, _)| x),
                    &[' ', '\t', '('],
                    false,
                    line,
                    index,
                )?;
            }
            'p' => {
                index += consume(&mut chars, KEYWORD_PRED, line, index)?;
                tokens.push(Token::Pred);
                match_one_of(
                    chars.peek().map(|&(x, _)| x),
                    &[' ', '\t', '('],
                    false,
                    line,
                    index,
                )?;
            }
            '(' => {
                chars.next();
                index += 1;
                tokens.push(Token::LeftParenthesis);
            }
            ')' => {
                chars.next();
                index += 1;
                tokens.push(Token::RightParenthesis);
            }
            '\t' | ' ' => {
                chars.next();
                index += 1;
            }
            ut => {
                return Err(LexingError::UnrecognizedToken {
                    found: ut.to_string(),
                    column: index,
                    line,
                })
            }
        }
    }

    tokens.push(Token::EOF);

    Ok(tokens)
}

fn consume<T>(
    input: &mut T,
    literal: &'static str,
    line: usize,
    index: usize,
) -> Result<usize, LexingError>
where
    T: std::iter::Iterator<Item = (char, char)>,
{
    let mut i = index;

    for expected in literal.chars() {
        match input.next() {
            Some((c, _)) => {
                if c != expected {
                    return Err(LexingError::UnexpectedCharacter {
                        found: c,
                        expected: vec![literal.to_string()],
                        column: i,
                        line,
                    });
                } else {
                    i += 1;
                }
            }
            None => {
                return Err(LexingError::UnexpectedEOF {
                    expected: vec![literal.to_string()],
                    column: i,
                    line,
                });
            }
        }
    }

    Ok(literal.len())
}

fn match_one_of(
    input: Option<char>,
    literals: &[char],
    allow_eof: bool,
    line: usize,
    index: usize,
) -> Result<usize, LexingError> {
    match input {
        Some(next_next) => {
            let eof_expectation = if allow_eof {
                vec!["<EOF>".to_string()]
            } else {
                vec![]
            };

            if !literals.contains(&next_next) {
                return Err(LexingError::UnexpectedCharacter {
                    found: next_next,
                    expected: literals
                        .iter()
                        .map(|c| format!("{}", c))
                        .chain(eof_expectation)
                        .collect(),
                    column: index,
                    line,
                });
            }

            Ok(1)
        }
        None => {
            if !allow_eof {
                return Err(LexingError::UnexpectedEOF {
                    expected: literals.iter().map(|c| format!("{}", c)).collect(),
                    column: index,
                    line,
                });
            }

            Ok(0)
        }
    }
}

#[cfg(test)]
mod lexer_tests_happy_path {
    use super::*;

    #[test]
    fn test_scan_empty_string() {
        let input = "";

        let tokens = scan(input).unwrap();

        assert_eq!(tokens[0], Token::EOF);
    }

    #[test]
    fn test_scan_basic() {
        let input = "true false if then else 0 succ pred iszero ()";

        let tokens = scan(input).unwrap();

        assert_eq!(tokens[0], Token::True);
        assert_eq!(tokens[1], Token::False);
        assert_eq!(tokens[2], Token::If);
        assert_eq!(tokens[3], Token::Then);
        assert_eq!(tokens[4], Token::Else);
        assert_eq!(tokens[5], Token::Zero);
        assert_eq!(tokens[6], Token::Succ);
        assert_eq!(tokens[7], Token::Pred);
        assert_eq!(tokens[8], Token::IsZero);
        assert_eq!(tokens[9], Token::LeftParenthesis);
        assert_eq!(tokens[10], Token::RightParenthesis);
    }

    #[test]
    fn test_scan_mixture_of_whitespaces() {
        let input = "if\ttrue\t\t then         0 else        succ\t0";

        let tokens = scan(input).unwrap();

        assert_eq!(tokens[0], Token::If);
        assert_eq!(tokens[1], Token::True);
        assert_eq!(tokens[2], Token::Then);
        assert_eq!(tokens[3], Token::Zero);
        assert_eq!(tokens[4], Token::Else);
        assert_eq!(tokens[5], Token::Succ);
        assert_eq!(tokens[6], Token::Zero);
        assert_eq!(tokens[7], Token::EOF);
    }

    #[test]
    fn test_scan_parantheses() {
        let input = "if (true) then 0 else (succ (pred(0))";

        let tokens = scan(input).unwrap();

        assert_eq!(tokens[0], Token::If);
        assert_eq!(tokens[1], Token::LeftParenthesis);
        assert_eq!(tokens[2], Token::True);
        assert_eq!(tokens[3], Token::RightParenthesis);
        assert_eq!(tokens[4], Token::Then);
        assert_eq!(tokens[5], Token::Zero);
        assert_eq!(tokens[6], Token::Else);
        assert_eq!(tokens[7], Token::LeftParenthesis);
        assert_eq!(tokens[8], Token::Succ);
        assert_eq!(tokens[9], Token::LeftParenthesis);
        assert_eq!(tokens[10], Token::Pred);
        assert_eq!(tokens[11], Token::LeftParenthesis);
        assert_eq!(tokens[12], Token::Zero);
        assert_eq!(tokens[13], Token::RightParenthesis);
        assert_eq!(tokens[14], Token::RightParenthesis);
        assert_eq!(tokens[15], Token::EOF);
    }

    #[test]
    fn test_scan_valid_tokens_bad_syntax() {
        let input = "true)false)if then 0";

        let tokens = scan(input).unwrap();

        assert_eq!(tokens[0], Token::True);
        assert_eq!(tokens[1], Token::RightParenthesis);
        assert_eq!(tokens[2], Token::False);
        assert_eq!(tokens[3], Token::RightParenthesis);
        assert_eq!(tokens[4], Token::If);
        assert_eq!(tokens[5], Token::Then);
        assert_eq!(tokens[6], Token::Zero);
        assert_eq!(tokens[7], Token::EOF);
    }
}

#[cfg(test)]
mod lexer_tests_sad_path {
    use super::*;

    #[test]
    fn test_scan_unrecognized_token_error() {
        let input = "if    true then 0   else succ    0 1";

        let tokens = scan(input);

        assert_eq!(
            tokens.err(),
            Some(LexingError::UnrecognizedToken {
                found: String::from("1"),
                column: 35,
                line: 1,
            })
        );
    }

    #[test]
    fn test_scan_unexpected_token_error() {
        let input = "succ( suc ( 0 ) )";

        let tokens = scan(input);

        assert_eq!(
            tokens.err(),
            Some(LexingError::UnexpectedCharacter {
                found: ' ',
                expected: vec!["succ".to_string()],
                column: 9,
                line: 1,
            })
        );
    }

    #[test]
    fn test_scan_unexpected_eof_error() {
        let input = "tr";

        let tokens = scan(input);

        assert_eq!(
            tokens.err(),
            Some(LexingError::UnexpectedEOF {
                expected: vec!["true".to_string()],
                column: 2,
                line: 1,
            })
        );
    }

    #[test]
    fn test_scan_literals_no_spaces_unexpected_token_error() {
        let input = "truefalse";

        let tokens = scan(input);

        assert_eq!(
            tokens.err(),
            Some(LexingError::UnexpectedCharacter {
                found: 'f',
                expected: vec![
                    " ".to_string(),
                    "\t".to_string(),
                    ")".to_string(),
                    "<EOF>".to_string()
                ],
                column: 4,
                line: 1,
            })
        );
    }
}
