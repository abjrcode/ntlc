mod lexer;
mod parser;

mod type_checker;

use crate::parser::parse;
use lexer::scan;

fn main() {
    let input = "if    true then (0)   else (succ    (0))";

    let tokens = scan(input).unwrap();

    tokens.iter().for_each(|t| print!("{} ", t));

    println!();

    let ast = parse(tokens).unwrap();

    println!("{}", ast);

    let typed_ast = type_checker::infer(ast).unwrap();

    println!("{}", typed_ast);
}
