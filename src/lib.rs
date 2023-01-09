use std::{fs, path::Path};

use crate::parser::{Parser, TARGET_FUNC_NAME};
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};

mod parser;

pub fn run(path: &str) {
    let path = Path::new(path);

    // parse source code
    let input = fs::read_to_string(path).expect("failed to read file");
    let parser = Parser::new(&input);
    let func_ast: Vec<_> = parser.collect();

    // prepare LLVM stuffs
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module(path.file_stem().unwrap().to_str().unwrap());
    let engine = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .unwrap();
    module.set_source_file_name(path.file_name().unwrap().to_str().unwrap());

    // generate code
    for func in &func_ast {
        func.codegen(&context, &module, &builder);
    }

    // output LLVM IR
    module
        .print_to_file(path.with_extension("ll"))
        .expect("failed to dump LLVM IR");
    module.write_bitcode_to_path(&path.with_extension("bc"));
    println!("LLVM IR dumped");

    // evaluate target (if exists)
    unsafe {
        type TargetFunc = unsafe extern "C" fn() -> f64;
        let target: Result<JitFunction<TargetFunc>, _> = engine.get_function(TARGET_FUNC_NAME);
        match target {
            Ok(target) => println!("result = {}", target.call()),
            Err(_) => println!("result = ()"),
        }
    }
}
