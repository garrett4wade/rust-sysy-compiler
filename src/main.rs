use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::prelude::*;

pub mod asm;
pub mod ast;
lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    sysy
);

fn main() -> Result<(), String> {
    let mut options = args();
    options.next();

    let _mode = options.next().unwrap();

    // Read file content.
    let file = options.next().unwrap();
    let content = read_to_string(file).map_err(|e| e.to_string())?;

    options.next();
    let ofile = options.next().unwrap();
    let mut ofile = File::create(&ofile).map_err(|e| e.to_string())?;

    // Use the lexer and parser created by lalrpop to convert the
    // source code into AST, a data structure defined in src/ast.rs.
    let ast = sysy::CompUnitParser::new()
        .parse(&content)
        .map_err(|e| e.to_string())?;
    // println!("{:?}\n", &ast);

    // Convert the AST data structure into Koopa IR using
    // Koopa IR Rust APIs.
    let program = ast::build_program(&ast).unwrap();

    if _mode == "-koopa" {
        let mut gen = KoopaGenerator::new(vec![]);
        gen.generate_on(&program).map_err(|e| e.to_string())?;
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
        ofile
            .write_all(text_form_ir.as_bytes())
            .map_err(|e| e.to_string())?;
    } else if _mode == "-riscv" {
        let asm = asm::build_riscv(&program);
        ofile.write_all(asm.as_bytes()).map_err(|e| e.to_string())?;
    } else {
        panic!("Invalid mode");
    }
    Ok(())
}
