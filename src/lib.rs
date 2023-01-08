use std::fs;

use inkwell::context::Context;
use parser::Parser;

mod parser;

pub fn run(file: &str) {
    let input = fs::read_to_string(file).unwrap();
    let p = Parser::new(&input);

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("main");
    module.set_source_file_name(file);

    for f in p {
        f.codegen(&context, &module, &builder);
    }

    let ir = module.print_to_string().to_string();
    println!("{}", ir);
}
