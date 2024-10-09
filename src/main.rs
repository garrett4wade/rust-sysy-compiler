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

fn main() -> std::io::Result<()> {
    let mut options = args();
    options.next();

    let _mode = options.next().unwrap();

    // Read file content.
    let file = options.next().unwrap();
    let content = read_to_string(file)?;

    options.next();
    let ofile = options.next().unwrap();
    let mut ofile = File::create(&ofile)?;

    // Use the lexer and parser created by lalrpop to convert the
    // source code into AST, a data structure defined in src/ast.rs.
    let ast = sysy::CompUnitParser::new().parse(&content);

    // Parsing fails (due to possibly many reasons, e.g., syntax error, unexpected EOF, etc.).
    if let Err(_) = ast {
        return Ok(());
    }
    // println!("{:?}\n", &ast);

    // Convert the AST data structure into Koopa IR using
    // Koopa IR Rust APIs.
    let program = ast::build_program(&ast.unwrap()).unwrap();

    if _mode == "-koopa" {
        let mut gen = KoopaGenerator::new(vec![]);
        gen.generate_on(&program)?;
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
        ofile.write_all(text_form_ir.as_bytes())?;
    } else if _mode == "-riscv" {
        let asm = asm::build_riscv(&program);
        ofile.write_all(asm.as_bytes())?;
    } else {
        panic!("Invalid mode");
    }
    Ok(())
}
