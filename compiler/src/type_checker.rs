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

#[derive(Debug, PartialEq, Clone)]
pub enum TypedTerm {
    Boolean,
    Integer,
    Void,
}

impl std::fmt::Display for TypedTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypedTerm::Boolean => write!(f, "BOOLEAN"),
            TypedTerm::Integer => write!(f, "INTEGER"),
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

pub fn infer(ast: Term) -> Result<TypedTerm, TypeError> {
    match ast {
        Term::True => Ok(TypedTerm::Boolean),
        Term::False => Ok(TypedTerm::Boolean),
        Term::Zero => Ok(TypedTerm::Integer),
        Term::Successor(t) => {
            let t = infer(*t)?;
            match t {
                TypedTerm::Integer => Ok(TypedTerm::Integer),
                _ => Err(TypeError::TypeMismatch {
                    expected: vec![TypedTerm::Integer],
                    found: t,
                }),
            }
        }
        Term::Predecessor(t) => {
            let t = infer(*t)?;
            match t {
                TypedTerm::Integer => Ok(TypedTerm::Integer),
                _ => Err(TypeError::TypeMismatch {
                    expected: vec![TypedTerm::Integer],
                    found: t,
                }),
            }
        }
        Term::IsZero(t) => {
            let t = infer(*t)?;
            match t {
                TypedTerm::Integer => Ok(TypedTerm::Boolean),
                _ => Err(TypeError::TypeMismatch {
                    expected: vec![TypedTerm::Integer],
                    found: t,
                }),
            }
        }
        Term::Conditional {
            condition,
            consequence,
            alternative,
        } => {
            let condition_type = infer(*condition)?;
            let conseq_type = infer(*consequence)?;
            let alt_type = infer(*alternative)?;

            match condition_type {
                TypedTerm::Boolean => {
                    if conseq_type == alt_type {
                        Ok(conseq_type)
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: vec![conseq_type],
                            found: alt_type,
                        })
                    }
                }
                _ => Err(TypeError::TypeMismatch {
                    expected: vec![TypedTerm::Boolean],
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
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Void);
    }

    #[test]
    fn test_true() {
        let input = "true";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Boolean);
    }

    #[test]
    fn test_false() {
        let input = "false";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Boolean);
    }

    #[test]
    fn test_zero() {
        let input = "0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer);
    }

    #[test]
    fn test_successor() {
        let input = "succ 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer);
    }

    #[test]
    fn test_predecessor() {
        let input = "pred 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer);
    }

    #[test]
    fn test_is_zero() {
        let input = "iszero 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Boolean);
    }

    #[test]
    fn test_conditional() {
        let input = "if true then 0 else 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer);
    }

    #[test]
    fn test_nested_conditional() {
        let input = "if true then if true then 0 else 0 else 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer);
    }

    #[test]
    fn test_condition_with_function_calls() {
        let input = "if iszero 0 then succ 0 else pred 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast).unwrap();

        assert_eq!(typed_ast, TypedTerm::Integer);
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
        let typed_ast = infer(ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Integer],
                found: TypedTerm::Boolean
            })
        );
    }

    #[test]
    fn test_predecessor() {
        let input = "pred true";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Integer],
                found: TypedTerm::Boolean
            })
        );
    }

    #[test]
    fn test_is_zero() {
        let input = "iszero true";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Integer],
                found: TypedTerm::Boolean
            })
        );
    }

    #[test]
    fn test_conditional() {
        let input = "if true then true else 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Boolean],
                found: TypedTerm::Integer
            })
        );
    }

    #[test]
    fn test_nested_conditional() {
        let input = "if true then if true then true else 0 else 0";
        let tokens = scan(input).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = infer(ast);

        assert_eq!(
            typed_ast,
            Err(TypeError::TypeMismatch {
                expected: vec![TypedTerm::Boolean],
                found: TypedTerm::Integer
            })
        );
    }
}
