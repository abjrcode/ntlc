mod lexer;
mod parser;

mod type_checker;

use std::fs::File;

use crate::parser::parse;
use lexer::scan;

fn main() {
    let input = File::open("examples/a.ntlc").unwrap();

    let mut program = std::io::read_to_string(input).unwrap();

    program = program.trim().to_string();

    let tokens = scan(&program).unwrap();

    tokens.iter().for_each(|t| print!("{} ", t));

    println!();

    let ast = parse(tokens).unwrap();

    println!("{}", ast);

    let typed_ast = type_checker::infer(ast).unwrap();

    println!("{}", typed_ast);
}
