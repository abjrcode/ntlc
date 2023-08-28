use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};

use crate::parser::Term;

pub type MainFunc = unsafe extern "C" fn() -> i32;

#[derive(Debug)]
pub enum CodegenError {
    LlvmNative(String),
}

mod internals {
    use inkwell::values::IntValue;

    use crate::parser::Term;

    use super::*;

    pub struct CodeGenerator<'ctx> {
        pub context: &'ctx Context,
        pub module: Module<'ctx>,
        pub builder: Builder<'ctx>,
    }

    impl<'ctx> CodeGenerator<'ctx> {
        fn add_builtin_izzero(&self) -> Result<(), CodegenError> {
            let i32_type = self.context.i32_type();
            let bool_type = self.context.bool_type();

            let iz_zero_fn_type = bool_type.fn_type(&[i32_type.into()], false);

            let iz_zero_fn =
                self.module
                    .add_function("izzero", iz_zero_fn_type, Some(Linkage::Internal));

            let basic_block = self.context.append_basic_block(iz_zero_fn, "entry");

            self.builder.position_at_end(basic_block);

            let x = iz_zero_fn
                .get_nth_param(0)
                .ok_or(CodegenError::LlvmNative(
                    "Failed to get nth param".to_string(),
                ))?
                .into_int_value();

            let zero = i32_type.const_int(0, false);

            let iz_zero =
                self.builder
                    .build_int_compare(inkwell::IntPredicate::EQ, x, zero, "iz_zero_cmp");

            self.builder.build_return(Some(&iz_zero));

            Ok(())
        }

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

        fn compile_internal(&self, ast: Term) -> IntValue {
            match ast {
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
                Term::Successor(inner_term) => {
                    let succ_fn = self
                        .module
                        .get_function("succ")
                        .ok_or(CodegenError::LlvmNative(
                            "Failed to get succ function".to_string(),
                        ))
                        .unwrap();

                    let succ_value = self.compile_internal(*inner_term);

                    let succ_call =
                        self.builder
                            .build_call(succ_fn, &[succ_value.into()], "succ_call");

                    let succ_call_value = succ_call.try_as_basic_value().left().unwrap();

                    succ_call_value.into_int_value()
                }
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
