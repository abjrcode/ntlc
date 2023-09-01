mod lexer;
mod parser;

mod type_checker;

mod codegen;

use std::{fs::File, path};

use crate::parser::parse;
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};
use lexer::scan;

fn main() {
    let input = File::open("examples/good.ntlc").unwrap();

    let mut program = std::io::read_to_string(input).unwrap();

    program = program.trim().to_string();

    let tokens = scan(&program).unwrap();

    println!(
        "{} LEXER {}",
        String::from("-").repeat(40),
        String::from("-").repeat(40)
    );
    println!("{:?}", tokens);
    println!(
        "{} END LEXER {}",
        String::from("-").repeat(40),
        String::from("-").repeat(40)
    );

    println!();
    println!();

    let ast = parse(tokens).unwrap();

    println!(
        "{} PARSER {}\n",
        String::from("-").repeat(40),
        String::from("-").repeat(40)
    );
    println!("{}", ast);
    println!(
        "\n{} END PARSER {}",
        String::from("-").repeat(40),
        String::from("-").repeat(40)
    );

    println!();
    println!();

    let typed_ast = type_checker::infer(&ast).unwrap();

    println!(
        "{} TYPE CHECKER {}",
        String::from("*").repeat(40),
        String::from("*").repeat(40)
    );
    println!("{}", typed_ast);
    println!(
        "{} END TYPE CHECKER {}",
        String::from("*").repeat(40),
        String::from("*").repeat(40)
    );

    println!();
    println!();

    let context = Context::create();
    let module = codegen::generate(&context, ast).unwrap();

    let module_ir = module.to_string();

    println!(
        "{} LLVM IR {}",
        String::from("=").repeat(40),
        String::from("=").repeat(40)
    );
    println!("{}", module_ir);
    println!(
        "{} END LLVM IR {}",
        String::from("=").repeat(40),
        String::from("=").repeat(40)
    );

    println!();
    println!();

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let main =
        unsafe { execution_engine.get_function("main").unwrap() as JitFunction<codegen::MainFunc> };

    unsafe {
        let return_value = main.call();
        println!("EVALUATION RESULT: {}", return_value);
        println!();
    }

    module.write_bitcode_to_path(path::Path::new("bin/good.bc"));

    std::process::Command::new("clang")
        .arg("bin/good.bc")
        .arg("-o")
        .arg("bin/good")
        .spawn()
        .and_then(|x| x.wait_with_output())
        .unwrap();
}
