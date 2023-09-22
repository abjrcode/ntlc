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

use std::fmt::Display;

use crate::parser::Term;

#[derive(Debug, Clone)]
/**
 * This is like a Typed AST.
 *
 * There are two real possibilities for an NTLC program.
 * The whole program either evaluates to a boolean or an integer.
 * Check the grammar above to see why this is the case.
 *
 * `Void` is just a convenience to represent an empty program.
 */
pub enum TypedTerm {
    Boolean(Term),
    Integer(Term),
    Void,
}

/**
 * We implement equality manually for `TypedTerm` because we want to
 * ignore the inner term when comparing.
 *
 * If two expressions are of the same type, then they are equal.
 * We don't care about the inner term.
 */
impl PartialEq for TypedTerm {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (TypedTerm::Boolean(_), TypedTerm::Boolean(_))
                | (TypedTerm::Integer(_), TypedTerm::Integer(_))
                | (TypedTerm::Void, TypedTerm::Void)
        )
    }
}

impl std::fmt::Display for TypedTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypedTerm::Boolean(_) => write!(f, "BOOLEAN"),
            TypedTerm::Integer(_) => write!(f, "INTEGER"),
            TypedTerm::Void => write!(f, "<VOID - EMPTY PROGRAM>"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeMismatch {
        expected: Vec<TypedTerm>,
        found: TypedTerm,
    },
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeError::TypeMismatch { expected, found } => {
                write!(f, "Type error! Expected one of: ")?;
                for (i, t) in expected.iter().enumerate() {
                    if i == expected.len() - 1 {
                        write!(f, "{}", t)?;
                    } else {
                        write!(f, "{}, ", t)?;
                    }
                }
                write!(f, " but found: {}", found)
            }
        }
    }
}

/**
 * This is the entry point of the type checker.
 * Although I called it `infer` it doesn't really do any inference.
 * The reason being that type inference is actually not needed
 *
 * For our grammar since we know the type of everything.
 * There is no inference needed.
 *
 * It just checks if the program is "well typed".
 * "Well typed": means you are not doing things like
 * iszero true or if 0 then true else false.
 *
 * It uses the same algorithm as recursive descent
 * traversing the AST and checking the types of the nodes.
 */
pub fn infer(ast: &Term) -> Result<TypedTerm, TypeError> {
    match ast {
        // The terminals which are also the base cases of the recursion
        // are the easiest to type check.
        Term::True => Ok(TypedTerm::Boolean(Term::True)),
        Term::False => Ok(TypedTerm::Boolean(Term::False)),
        Term::Zero => Ok(TypedTerm::Integer(Term::Zero)),
        /*
           We need to make sure that the inner term of the successor
           is an integer.
        */
        Term::Successor(t) => {
            let t = infer(t)?;
            match t {
                TypedTerm::Integer(inner_term) => Ok(TypedTerm::Integer(inner_term)),
                _ => Err(TypeError::TypeMismatch {
                    expected: vec![TypedTerm::Integer(Term::Empty)],
                    found: t,
                }),
            }
        }
        Term::Predecessor(t) => {
            let t = infer(t)?;
            match t {
                TypedTerm::Integer(inner_term) => Ok(TypedTerm::Integer(inner_term)),
                _ => Err(TypeError::TypeMismatch {
                    expected: vec![TypedTerm::Integer(Term::Empty)],
                    found: t,
                }),
            }
        }
        Term::IsZero(t) => {
            let t = infer(t)?;
            match t {
                TypedTerm::Integer(inner_term) => Ok(TypedTerm::Boolean(inner_term)),
                _ => Err(TypeError::TypeMismatch {
                    expected: vec![TypedTerm::Integer(Term::Empty)],
                    found: t,
                }),
            }
        }
        Term::Conditional {
            condition,
            consequence,
            alternative,
        } => {
            let condition_type = infer(condition)?;
            let conseq_type = infer(consequence)?;
            let alt_type = infer(alternative)?;

            /*
             * For the conditional we need to make sure that the condition
             * is a boolean and that the consequence and alternative are of the same type.
             *
             * This is to avoid things like:
             * if true then 0 else false
             *
             * While there is nothing wrong with the above program
             * We don't have a way to represent it in our grammar
             * or in our type system.
             *
             * The resulting type would be what is typically known
             * as a union type.
             *
             * So we simply disallow it in our case
             */
            match condition_type {
                TypedTerm::Boolean(_) => {
                    if conseq_type == alt_type {
                        Ok(conseq_type)
                    } else {
                        let expected_type = match conseq_type {
                            TypedTerm::Boolean(_) => TypedTerm::Boolean(Term::Empty),
                            TypedTerm::Integer(_) => TypedTerm::Integer(Term::Empty),
                            TypedTerm::Void => TypedTerm::Void,
                        };
                        Err(TypeError::TypeMismatch {
                            expected: vec![expected_type],
                            found: alt_type,
                        })
                    }
                }
                _ => Err(TypeError::TypeMismatch {
                    expected: vec![TypedTerm::Boolean(Term::Empty)],
                    found: condition_type,
                }),
            }
        }
        Term::Empty => Ok(TypedTerm::Void),
    }
}

#[cfg(test)]
mod test_type_checker_happy_path {
    use super::*;

    use crate::lexer::scan;
    use crate::parser::parse;

    #[test]
    fn test_empty_program() {
        let input = "";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Void);
    }

    #[test]
    fn test_true() {
        let input = "true";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Boolean(Term::True));
    }

    #[test]
    fn test_false() {
        let input = "false";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Boolean(Term::False));
    }

    #[test]
    fn test_zero() {
        let input = "0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer(Term::Zero));
    }

    #[test]
    fn test_successor() {
        let input = "succ 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer(Term::Zero));
    }

    #[test]
    fn test_predecessor() {
        let input = "pred 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer(Term::Zero));
    }

    #[test]
    fn test_is_zero() {
        let input = "iszero 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Boolean(Term::Zero));
    }

    #[test]
    fn test_conditional() {
        let input = "if true then 0 else 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer(Term::Zero));
    }

    #[test]
    fn test_boolean_conditional() {
        let input = "if true then false else true";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Boolean(Term::True));
    }

    #[test]
    fn test_nested_conditional() {
        let input = "if true then if true then 0 else 0 else 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer(Term::Zero));
    }

    #[test]
    fn test_condition_with_function_calls() {
        let input = "if iszero 0 then succ 0 else pred 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer(Term::Zero));
    }
}

#[cfg(test)]
mod test_type_checker_unhappy_path {
    use super::*;

    use crate::lexer::scan;
    use crate::parser::parse;

    #[test]
    fn test_successor() {
        let input = "succ true";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Integer(Term::Empty)],
                found: TypedTerm::Boolean(Term::True)
            })
        );
    }

    #[test]
    fn test_predecessor() {
        let input = "pred true";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Integer(Term::Empty)],
                found: TypedTerm::Boolean(Term::True)
            })
        );
    }

    #[test]
    fn test_is_zero() {
        let input = "iszero true";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Integer(Term::Empty)],
                found: TypedTerm::Boolean(Term::True)
            })
        );
    }

    #[test]
    fn test_conditional() {
        let input = "if true then true else 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Boolean(Term::Empty)],
                found: TypedTerm::Integer(Term::Zero)
            })
        );
    }

    #[test]
    fn test_nested_conditional() {
        let input = "if true then if true then true else 0 else 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(&ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Boolean(Term::Empty)],
                found: TypedTerm::Integer(Term::Zero)
            })
        );
    }
}
