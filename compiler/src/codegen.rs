use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};

use crate::parser::Term;

/**
 * `MainFunc` represents how our NTLC program
 * looks like after it's compiled to LLVM IR.
 *
 * It is a function that takes no arguments
 * and returns an integer.
 *
 * You might think, hey where the the boolean
 * return value?
 *
 * Well, on the OS level there is no such thing
 * Everything is represented as numbers.
 *
 * So we will map boolean values like true and false to 0 and 1 respectively.
 *
 * That does make it hard to distinguish between booleans and integers
 * such as when the program returns an integer 1 but since
 * our language is simple we don't need to worry about that.
 *
 * I mean after all this is just a shortcut not to link with libc
 * or provide a way to print to the console.
 */
pub type MainFunc = unsafe extern "C" fn() -> i32;

#[derive(Debug)]
pub enum CodegenError {
    LlvmNative(String),
}

/**
 * This module contains the internals of the code generator.
 *
 * I chose to wrap things in an isolated module because the code generator
 * needs to keep track of some state as part of translation process.
 *
 * This way consumers of the "codegen" module don't need to worry about
 */
mod internals {
    use inkwell::values::IntValue;

    use crate::parser::Term;

    use super::*;

    /**
     * `context`, `module` and `builder` are the state that the code generator
     * needs to keep track of.
     *
     * These are constructs provided by the LLVM library.
     */
    pub struct CodeGenerator<'ctx> {
        pub context: &'ctx Context,
        pub module: Module<'ctx>,
        pub builder: Builder<'ctx>,
    }

    /**
     * Here we are implementing the actual
     * functionality of the code generator
     *
     * These functions appear in an impl block
     * which makes them methods of the CodeGenerator struct
     * defined above.
     */
    impl<'ctx> CodeGenerator<'ctx> {
        /**
         * This function adds the builtin `izzero` function to the module.
         */
        fn add_builtin_izzero(&self) -> Result<(), CodegenError> {
            // This is a representation of the integer type in LLVM IR
            let i32_type = self.context.i32_type();

            // This is a representation of the boolean type in LLVM IR
            let bool_type = self.context.bool_type();

            // This is the type of the `izzero` function
            // It takes an integer and returns a boolean
            let iz_zero_fn_type = bool_type.fn_type(&[i32_type.into()], false);

            // Now we add the function declaration to the module
            // We give it the name "izzero"
            let iz_zero_fn =
                self.module
                    .add_function("izzero", iz_zero_fn_type, Some(Linkage::Internal));

            // Now we add a basic block to the function
            // A basic block is a sequence of instructions
            // that execute sequentially
            // This is where we implement our function's body
            let basic_block = self.context.append_basic_block(iz_zero_fn, "entry");

            self.builder.position_at_end(basic_block);

            // Now we get the value of the first parameter of the function
            // which is the integer we want to check if it's zero
            let x = iz_zero_fn
                .get_nth_param(0)
                .ok_or(CodegenError::LlvmNative(
                    "Failed to get nth param".to_string(),
                ))?
                .into_int_value();

            // Now we create a constant integer with the value 0
            let zero = i32_type.const_int(0, false);

            // Now we compare the value of the parameter with the constant zero
            let iz_zero =
                self.builder
                    .build_int_compare(inkwell::IntPredicate::EQ, x, zero, "iz_zero_cmp");

            // Now we return the result of the comparison
            self.builder.build_return(Some(&iz_zero));

            Ok(())
        }

        /**
         * This function adds the builtin `pred` function to the module.
         *
         * It follows a similar construction to the `izzero` function.
         */
        fn add_builtin_pred(&self) -> Result<(), CodegenError> {
            let i32_type = self.context.i32_type();

            let pred_fn_type = i32_type.fn_type(&[i32_type.into()], false);

            let pred_fn = self
                .module
                .add_function("pred", pred_fn_type, Some(Linkage::Internal));

            let basic_block = self.context.append_basic_block(pred_fn, "entry");

            self.builder.position_at_end(basic_block);

            let x = pred_fn
                .get_nth_param(0)
                .ok_or(CodegenError::LlvmNative(
                    "Failed to get nth param".to_string(),
                ))?
                .into_int_value();

            let constant_one = i32_type.const_int(1, false);

            let pred_value = self.builder.build_int_sub(x, constant_one, "minus_one");

            self.builder.build_return(Some(&pred_value));

            Ok(())
        }

        /**
         * This function adds the builtin `succ` function to the module.
         *
         * It follows a similar construction to the `izzero` function.
         */
        fn add_builtin_succ(&self) -> Result<(), CodegenError> {
            let i32_type = self.context.i32_type();

            let succ_fn_type = i32_type.fn_type(&[i32_type.into()], false);

            let succ_fn = self
                .module
                .add_function("succ", succ_fn_type, Some(Linkage::Internal));

            let basic_block = self.context.append_basic_block(succ_fn, "entry");

            self.builder.position_at_end(basic_block);

            let x = succ_fn
                .get_nth_param(0)
                .ok_or(CodegenError::LlvmNative(
                    "Failed to get nth param".to_string(),
                ))?
                .into_int_value();

            let constant_one = i32_type.const_int(1, false);

            let succ_value = self.builder.build_int_add(x, constant_one, "plus_one");

            self.builder.build_return(Some(&succ_value));

            Ok(())
        }

        /**
         * new is similar to constructors in Object Oriented Languages
         * It initializes a new LLVM module and injects our builtin
         * functions and constants into it.
         *
         * LLVM modules are the smallest unit of compilation.
         * You can think of them as a single source file in C or Rust.
         */
        pub fn new(ctx: &'ctx Context, module_name: &str) -> Result<Self, CodegenError> {
            let module = ctx.create_module(module_name);

            let code_generator = CodeGenerator {
                context: ctx,
                module,
                builder: ctx.create_builder(),
            };

            code_generator.add_builtin_izzero()?;
            code_generator.add_builtin_pred()?;
            code_generator.add_builtin_succ()?;

            Ok(code_generator)
        }

        /**
         * This is the main function of the code generator.
         * It takes an AST and compiles it to LLVM IR.
         *
         * It does it the same way as we did we the type checker
         * by recursively traversing the AST and compiling each node
         */
        fn compile_internal(&self, ast: Term) -> IntValue {
            match ast {
                /*
                 * Terminal nodes map to simple constants in LLVM
                 */
                Term::True => {
                    let bool_type = self.context.bool_type();
                    bool_type.const_int(1, false)
                }
                Term::False => {
                    let bool_type = self.context.bool_type();
                    bool_type.const_zero()
                }
                Term::Zero => {
                    let int_type = self.context.i32_type();
                    int_type.const_zero()
                }
                /*
                 * Here we are not building the `succ` function
                 * Remember, we already added it as a builtin function
                 *
                 * This is how we call a function in LLVM IR
                 */
                Term::Successor(inner_term) => {
                    // First we get the function from the module
                    let succ_fn = self
                        .module
                        .get_function("succ")
                        .ok_or(CodegenError::LlvmNative(
                            "Failed to get succ function".to_string(),
                        ))
                        .unwrap();

                    // Then we compile the inner term
                    // which could be any valid NTLC term
                    let succ_value = self.compile_internal(*inner_term);

                    // Then we call the function with the value of the inner term
                    let succ_call =
                        self.builder
                            .build_call(succ_fn, &[succ_value.into()], "succ_call");

                    // Then we get the return value of the function call
                    let succ_call_value = succ_call.try_as_basic_value().left().unwrap();

                    // Then we return the value
                    succ_call_value.into_int_value()
                }
                // Same as above
                Term::Predecessor(inner_term) => {
                    let pred_fn = self
                        .module
                        .get_function("pred")
                        .ok_or(CodegenError::LlvmNative(
                            "Failed to get pred function".to_string(),
                        ))
                        .unwrap();

                    let pred_value = self.compile_internal(*inner_term);

                    let pred_call =
                        self.builder
                            .build_call(pred_fn, &[pred_value.into()], "pred_call");

                    let pred_call_value = pred_call.try_as_basic_value().left().unwrap();

                    pred_call_value.into_int_value()
                }
                // Same as above
                Term::IsZero(inner_term) => {
                    let iz_zero_fn = self
                        .module
                        .get_function("izzero")
                        .ok_or(CodegenError::LlvmNative(
                            "Failed to get izzero function".to_string(),
                        ))
                        .unwrap();

                    let iz_zero_value = self.compile_internal(*inner_term);

                    let iz_zero_call = self.builder.build_call(
                        iz_zero_fn,
                        &[iz_zero_value.into()],
                        "iz_zero_call",
                    );

                    let iz_zero_call_value = iz_zero_call.try_as_basic_value().left().unwrap();

                    iz_zero_call_value.into_int_value()
                }
                /*
                 * Conditionals are a bit more involved
                 * If you have seen goto statements before they might seem familiar
                 *
                 * Essentially we are creating a "label" for each possible
                 * branch of the conditional.
                 *
                 * And then we use jump instructions to execute one or the other
                 * depending on the value of the condition.
                 */
                Term::Conditional {
                    condition,
                    consequence,
                    alternative,
                } => {
                    let condition_value = self.compile_internal(*condition);

                    let bool_type = self.context.bool_type();

                    let true_value = bool_type.const_int(1, false);

                    let condition_cmp = self.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        condition_value,
                        true_value,
                        "condition_cmp",
                    );

                    let then_block = self.context.append_basic_block(
                        self.builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap(),
                        "then",
                    );

                    let else_block = self.context.append_basic_block(
                        self.builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap(),
                        "else",
                    );

                    let merge_block = self.context.append_basic_block(
                        self.builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap(),
                        "merge",
                    );

                    self.builder
                        .build_conditional_branch(condition_cmp, then_block, else_block);

                    self.builder.position_at_end(then_block);

                    let consequence_value = self.compile_internal(*consequence);

                    self.builder.build_unconditional_branch(merge_block);

                    let then_block = self.builder.get_insert_block().unwrap();

                    self.builder.position_at_end(else_block);

                    let alternative_value = self.compile_internal(*alternative);

                    self.builder.build_unconditional_branch(merge_block);

                    let else_block = self.builder.get_insert_block().unwrap();

                    self.builder.position_at_end(merge_block);

                    /*
                       phi is a special construction in LLVM
                       that tells us which branch of the conditional
                       was executed to get us to this point
                    */
                    let phi = self.builder.build_phi(self.context.i32_type(), "phi");

                    phi.add_incoming(&[
                        (&consequence_value, then_block),
                        (&alternative_value, else_block),
                    ]);

                    phi.as_basic_value().into_int_value()
                }
                _ => {
                    let int_type = self.context.i32_type();
                    int_type.const_int(0, false)
                }
            }
        }

        /*
         * This is the public interface of the code generator.
         * It takes an AST and compiles it to LLVM IR.
         *
         * We basically we just create a main function
         * and we put compile our NTLC program to be the body
         * of that function
         *
         * The result of evaluating the NTLC program is the return value
         * of the main function
         */
        pub fn compile(&self, ast: Term) -> Result<(), CodegenError> {
            let i32_type = self.context.i32_type();
            let main_fn_type = i32_type.fn_type(&[], false);

            let main_fn = self
                .module
                .add_function("main", main_fn_type, Some(Linkage::External));

            let basic_block = self.context.append_basic_block(main_fn, "entry");

            self.builder.position_at_end(basic_block);

            let return_value = self.compile_internal(ast);

            self.builder.build_return(Some(&return_value));

            Ok(())
        }
    }
}

/**
 * This is the only function exported from the code generator
 * Check `main.rs` to see how it's used.
 *
 * As you can see, it delegates all of its work to the code generator module
 * defined above.
 */
pub fn generate(context: &Context, ast: Term) -> Result<Module<'_>, CodegenError> {
    let codegen = internals::CodeGenerator::new(context, "main")?;

    codegen.compile(ast)?;

    Ok(codegen.module)
}

#[cfg(test)]
mod code_generator_tests {
    use inkwell::{execution_engine::JitFunction, OptimizationLevel};

    use crate::{lexer::scan, parser::parse};

    use super::*;

    #[test]
    fn test_codegen_true() {
        let input = "true";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_true").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 1);
        }
    }

    #[test]
    fn test_codegen_false() {
        let input = "false";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_false").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 0);
        }
    }

    #[test]
    fn test_codegen_zero() {
        let input = "0";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_zero").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 0);
        }
    }

    #[test]
    fn test_codegen_succ() {
        let input = "succ 0";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_succ").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 1);
        }
    }

    #[test]
    fn test_codegen_pred() {
        let input = "pred 0";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_pred").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), -1);
        }
    }

    #[test]
    fn test_codegen_izzero() {
        let input = "iszero 0";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_izzero").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 1);
        }
    }

    #[test]
    fn test_codegen_if_then_branch() {
        let input = "if true then true else false";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_if").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 1);
        }
    }

    #[test]
    fn test_codegen_if_else_branch() {
        let input = "if false then true else false";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_if").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 0);
        }
    }

    #[test]
    fn test_codegen_nested_successors() {
        let input = "succ(succ(0))";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_nested_succ").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 2);
        }
    }

    #[test]
    fn test_codegen_nested_predecessors() {
        let input = "pred(pred(0))";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_nested_pred").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), -2);
        }
    }

    #[test]
    fn test_nested_if() {
        let input = "if true then if false then false else true else false";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_codegen_nested_if").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 1);
        }
    }

    #[test]
    fn test_complex_nesting() {
        let input = "if iszero(pred(succ(0))) then succ(succ(0)) else pred(pred(0))";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen =
            internals::CodeGenerator::new(&context, "test_codegen_complex_nesting").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), 2);
        }
    }

    #[test]
    fn test_most_complex_example() {
        let input = "if iszero(pred(succ(0))) then if iszero(pred(0)) then succ(succ(0)) else pred(pred(pred(0))) else succ(pred(pred(0)))";

        let tokens = scan(input).unwrap();

        let ast = parse(tokens).unwrap();

        let context = Context::create();

        let codegen =
            internals::CodeGenerator::new(&context, "test_codegen_complex_nesting").unwrap();

        codegen.compile(ast).unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main =
            unsafe { execution_engine.get_function("main").unwrap() as JitFunction<MainFunc> };

        unsafe {
            assert_eq!(main.call(), -3);
        }
    }
}

#[cfg(test)]
mod code_generator_builtins_tests {
    use inkwell::{execution_engine::JitFunction, OptimizationLevel};

    use super::*;

    type IzZeroFunc = unsafe extern "C" fn(i32) -> bool;
    type SuccFunc = unsafe extern "C" fn(i32) -> i32;
    type PredFunc = unsafe extern "C" fn(i32) -> i32;

    #[test]
    fn test_builtin_izzero() {
        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_builtin_izzero").unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let iszero =
            unsafe { execution_engine.get_function("izzero").unwrap() as JitFunction<IzZeroFunc> };

        unsafe {
            assert!(!(iszero.call(1)));
            assert!(iszero.call(0))
        }
    }

    #[test]
    fn test_builtin_succ() {
        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_builtin_succ").unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let succ =
            unsafe { execution_engine.get_function("succ").unwrap() as JitFunction<SuccFunc> };

        unsafe {
            assert_eq!(succ.call(0), 1);
        }
    }

    #[test]
    fn test_builtin_pred() {
        let context = Context::create();

        let codegen = internals::CodeGenerator::new(&context, "test_builtin_pred").unwrap();

        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let pred =
            unsafe { execution_engine.get_function("pred").unwrap() as JitFunction<PredFunc> };

        unsafe {
            assert_eq!(pred.call(0), -1);
        }
    }
}
