mod lexer;

use lexer::scan;

fn main() {
    let input = "if    true then 0   else (succ    0)";

    let tokens = scan(input).unwrap();

    tokens.iter().for_each(|t| print!("{:?} ", t));
}
