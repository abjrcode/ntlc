mod lexer;
mod parser;

use crate::parser::parse;
use lexer::scan;

fn main() {
    let input = "if    true then (0)   else (succ    (true))";

    let tokens = scan(input).unwrap();

    tokens.iter().for_each(|t| print!("{} ", t));

    println!();

    let ast = parse(tokens).unwrap();

    println!("{}", ast);
}
