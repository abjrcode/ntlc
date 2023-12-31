use std::iter::Peekable;

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
use crate::lexer::Token;

#[derive(PartialEq)]
pub enum SyntaxError {
    UnexpectedToken { expected: Vec<Token>, found: Token },
    UnexpectedEndOfInput { expected: Vec<Token> },
}

impl std::fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SyntaxError::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "Unexpected '{:?}'. expected one of: {:?}",
                    found, expected
                )
            }
            SyntaxError::UnexpectedEndOfInput { expected } => {
                write!(f, "Unexpected <EOF>. expected one of: {:?}", expected)
            }
        }
    }
}

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SyntaxError::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "Unexpected '{:?}'. expected one of {:?}",
                    found, expected
                )
            }
            SyntaxError::UnexpectedEndOfInput { expected } => {
                write!(
                    f,
                    "Unexpected end of stream. expected one of {:?}",
                    expected
                )
            }
        }
    }
}

impl std::error::Error for SyntaxError {}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
/**
 * This is our Abstract Syntax Tree (AST)
 * It is a recursive data structure that represents the program
 * Ignore those `Box<>` for now, they are just there to make the
 * Rust borrow checker happy.
 *
 * This should be self explanatory as it literally
 * follows the structure of our grammar.
*/
pub enum Term {
    Conditional {
        condition: Box<Term>,
        consequence: Box<Term>,
        alternative: Box<Term>,
    },
    Successor(Box<Term>),
    Predecessor(Box<Term>),
    IsZero(Box<Term>),
    True,
    False,
    Zero,
    Empty,
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Term::Conditional {
                condition,
                consequence,
                alternative,
            } => write!(
                f,
                "COND[{}] -> CONSEQUENCE[{}] ;; ALT[{}]",
                condition, consequence, alternative
            ),
            Term::Successor(expr) => write!(f, "Successor[{}]", expr),
            Term::Predecessor(expr) => write!(f, "Predcessor[{}]", expr),
            Term::IsZero(expr) => write!(f, "ZeroAssertion[{}]", expr),
            Term::True => write!(f, "T"),
            Term::False => write!(f, "F"),
            Term::Zero => write!(f, "ZERO"),
            Term::Empty => write!(f, "<EMPTY>"),
        }
    }
}

fn match_left_parantheses<T>(tokens: &mut Peekable<T>) -> Result<(), SyntaxError>
where
    T: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token::LeftParenthesis) => Ok(()),
        Some(token) => Err(SyntaxError::UnexpectedToken {
            expected: vec![Token::LeftParenthesis],
            found: token,
        }),
        None => Err(SyntaxError::UnexpectedEndOfInput {
            expected: vec![Token::LeftParenthesis],
        }),
    }
}

fn match_right_parenthesis<T>(tokens: &mut Peekable<T>) -> Result<(), SyntaxError>
where
    T: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token::RightParenthesis) => Ok(()),
        Some(token) => Err(SyntaxError::UnexpectedToken {
            expected: vec![Token::RightParenthesis],
            found: token,
        }),
        None => Err(SyntaxError::UnexpectedEndOfInput {
            expected: vec![Token::RightParenthesis],
        }),
    }
}

fn match_then<T>(tokens: &mut Peekable<T>) -> Result<(), SyntaxError>
where
    T: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token::Then) => Ok(()),
        Some(token) => Err(SyntaxError::UnexpectedToken {
            expected: vec![Token::Then],
            found: token,
        }),
        None => Err(SyntaxError::UnexpectedEndOfInput {
            expected: vec![Token::Then],
        }),
    }
}

fn match_else<T>(tokens: &mut Peekable<T>) -> Result<(), SyntaxError>
where
    T: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token::Else) => Ok(()),
        Some(token) => Err(SyntaxError::UnexpectedToken {
            expected: vec![Token::Else],
            found: token,
        }),
        None => Err(SyntaxError::UnexpectedEndOfInput {
            expected: vec![Token::Else],
        }),
    }
}

/**
 * This is the recursive function that does most of the work
 * It essentially follows the structure of our grammar
 */
fn parse_term<T>(tokens: &mut Peekable<T>) -> Result<Term, SyntaxError>
where
    T: Iterator<Item = Token>,
{
    // Get the next token
    match tokens.next() {
        // If it is `true` then our AST contains a single node `Term::True`
        Some(Token::True) => Ok(Term::True),
        // If it is `false` then our AST contains a single node `Term::False`
        Some(Token::False) => Ok(Term::False),
        Some(Token::Zero) => Ok(Term::Zero),
        /*
         * If it is `iszero` then we need to check if there is a left parenthesis
         * as our grammar allows both `iszero 0` and `iszero(TERM)`
         *
         * The same applies for other builtin functions
         */
        Some(Token::IsZero) => {
            let match_right = match tokens.peek() {
                Some(&Token::LeftParenthesis) => {
                    match_left_parantheses(tokens)?;
                    true
                }
                _ => false,
            };

            /*
               This is the heart of how recursive descent works
               We call `parse_term()` again to parse the inner expression
            */
            let inner_expr = parse_term(tokens)?;

            // If we matched a left parenthesis, we need to match a right parenthesis
            if match_right {
                match_right_parenthesis(tokens)?;
            }

            Ok(Term::IsZero(Box::new(inner_expr)))
        }
        Some(Token::Succ) => {
            let match_right = match tokens.peek() {
                Some(&Token::LeftParenthesis) => {
                    match_left_parantheses(tokens)?;
                    true
                }
                _ => false,
            };

            let inner_expr = parse_term(tokens)?;

            if match_right {
                match_right_parenthesis(tokens)?;
            }

            Ok(Term::Successor(Box::new(inner_expr)))
        }
        Some(Token::Pred) => {
            let match_right = match tokens.peek() {
                Some(&Token::LeftParenthesis) => {
                    match_left_parantheses(tokens)?;
                    true
                }
                _ => false,
            };

            let inner_expr = parse_term(tokens)?;

            if match_right {
                match_right_parenthesis(tokens)?;
            }

            Ok(Term::Predecessor(Box::new(inner_expr)))
        }
        /*
           If terms might seem more complex but they follow the same
           pattern of invoking `parse_term()` recursively

        */
        Some(Token::If) => {
            let match_right = match tokens.peek() {
                Some(&Token::LeftParenthesis) => {
                    match_left_parantheses(tokens)?;
                    true
                }
                _ => false,
            };

            let condition = parse_term(tokens)?;

            if match_right {
                match_right_parenthesis(tokens)?;
            }

            match_then(tokens)?;

            let match_right = match tokens.peek() {
                Some(&Token::LeftParenthesis) => {
                    match_left_parantheses(tokens)?;
                    true
                }
                _ => false,
            };

            let consequence = parse_term(tokens)?;

            if match_right {
                match_right_parenthesis(tokens)?;
            }

            match_else(tokens)?;

            let match_right = match tokens.peek() {
                Some(&Token::LeftParenthesis) => {
                    match_left_parantheses(tokens)?;
                    true
                }
                _ => false,
            };

            let alternative = parse_term(tokens)?;

            if match_right {
                match_right_parenthesis(tokens)?;
            }

            Ok(Term::Conditional {
                condition: Box::new(condition),
                consequence: Box::new(consequence),
                alternative: Box::new(alternative),
            })
        }
        Some(Token::EOF) => Ok(Term::Empty),
        Some(token) => Err(SyntaxError::UnexpectedToken {
            expected: vec![
                Token::If,
                Token::True,
                Token::False,
                Token::Zero,
                Token::Pred,
                Token::Succ,
                Token::IsZero,
            ],
            found: token,
        }),
        None => Ok(Term::Empty),
    }
}

/**
 * Entry point of our parser
 * This is the implementation of the Recursive Descent algorithm
 *
 * It takes a vector of tokens and returns a Term (AST) or a SyntaxError
 * if the program is not valid.
 *
 * It delegates most of the work to `parse_term()` which is a recursive function
 *
 * After `parse_term()` returns, we check if there are any tokens left in the stream
 * At this stage, there must be none left otherwise the program is not valid.
 * For example, succ(0) 0 is not valid because there is a 0 at the end of the program
 */
pub fn parse(tokens: Vec<Token>) -> Result<Term, SyntaxError> {
    let mut tokens = tokens.into_iter().peekable();

    let ast = parse_term(&mut tokens)?;

    match tokens.next() {
        Some(Token::EOF) => (),
        Some(token) => {
            return Err(SyntaxError::UnexpectedToken {
                expected: vec![Token::EOF],
                found: token,
            })
        }
        None => (),
    }

    Ok(ast)
}

#[cfg(test)]
mod tests_parser_happy_path {
    use super::*;
    use crate::lexer::scan;

    #[test]
    fn test_parse_empty_program() {
        let input = "";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        assert_eq!(ast, Term::Empty);
    }

    #[test]
    fn test_parse_literal_true() {
        let input = "true";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        assert_eq!(ast, Term::True);
    }

    #[test]
    fn test_parse_literal_false() {
        let input = "false";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        assert_eq!(ast, Term::False);
    }

    #[test]
    fn test_parse_literal_zero() {
        let input = "0";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        assert_eq!(ast, Term::Zero);
    }

    #[test]
    fn test_parse_conditional() {
        let input = "if true then false else true";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        assert_eq!(
            ast,
            Term::Conditional {
                condition: Box::new(Term::True),
                consequence: Box::new(Term::False),
                alternative: Box::new(Term::True),
            }
        );
    }

    #[test]
    fn test_parse_successor() {
        let input = "succ 0";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        assert_eq!(ast, Term::Successor(Box::new(Term::Zero),));
    }

    #[test]
    fn test_parse_predecessor() {
        let input = "pred 0";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        assert_eq!(ast, Term::Predecessor(Box::new(Term::Zero),));
    }

    #[test]
    fn test_parse_iszero() {
        let input = "iszero 0";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        assert_eq!(ast, Term::IsZero(Box::new(Term::Zero),));
    }

    #[test]
    fn test_parse_nested() {
        let input = "if true then succ   (0) else pred( 0 )";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        assert_eq!(
            ast,
            Term::Conditional {
                condition: Box::new(Term::True),
                consequence: Box::new(Term::Successor(Box::new(Term::Zero))),
                alternative: Box::new(Term::Predecessor(Box::new(Term::Zero)))
            }
        );
    }
}

#[cfg(test)]
mod tests_sad_path {
    use super::*;
    use crate::lexer::scan;

    #[test]
    fn test_parse_unexpected_true() {
        let input = "true true";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens);

        assert_eq!(
            ast,
            Err(SyntaxError::UnexpectedToken {
                expected: vec![Token::EOF],
                found: Token::True,
            })
        );
    }

    #[test]
    fn test_parse_unexpected_false() {
        let input = "true false";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens);

        assert_eq!(
            ast,
            Err(SyntaxError::UnexpectedToken {
                expected: vec![Token::EOF],
                found: Token::False,
            })
        );
    }

    #[test]
    fn test_parse_unbalanced_parenthesis() {
        let input = "iszero(true";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens);

        assert_eq!(
            ast,
            Err(SyntaxError::UnexpectedToken {
                expected: vec![Token::RightParenthesis],
                found: Token::EOF,
            })
        );
    }

    #[test]
    fn test_unbalanced_if() {
        let input = "if true then false iszero 0";

        let tokens = scan(input).unwrap();

        let ast = super::parse(tokens);

        assert_eq!(
            ast,
            Err(SyntaxError::UnexpectedToken {
                expected: vec![Token::Else],
                found: Token::IsZero,
            })
        );
    }

    #[test]
    fn test_invalid_syntax_starting_with_else() {
        let input = "else 0 if true";

        let tokens = scan(input).unwrap();

        let ast = super::parse(tokens);

        assert_eq!(
            ast,
            Err(SyntaxError::UnexpectedToken {
                expected: vec![
                    Token::If,
                    Token::True,
                    Token::False,
                    Token::Zero,
                    Token::Pred,
                    Token::Succ,
                    Token::IsZero
                ],
                found: Token::Else,
            })
        );
    }

    #[test]
    fn test_parse_unexpected_token() {
        let input = "if true then false else true 0";

        let tokens = scan(input).unwrap();

        let ast = super::parse(tokens);

        assert_eq!(
            ast,
            Err(SyntaxError::UnexpectedToken {
                expected: vec![Token::EOF],
                found: Token::Zero,
            })
        );
    }
}
