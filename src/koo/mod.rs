use ctx::KoopaLocalContext;
use koopa::ir::Program;

pub mod array;
pub mod block;
pub mod ctx;
pub mod expr;
pub mod func;
pub mod traits;
pub mod var;
use crate::koo::traits::{KoopaGlobalDeclaration, KoopaGlobalInit};
use func::KoopaFunc;

use array::{KoopaConstArray, KoopaVarArray};
use var::{KoopaConst, KoopaVar};
pub struct KoopaProgram {
    pub defs: Vec<KoopaFuncGlobalVarDecl>,
}

pub enum KoopaFuncGlobalVarDecl {
    Func(KoopaFunc),
    Var(KoopaVar),
    Const(KoopaConst),
    VarArray(KoopaVarArray),
    ConstArray(KoopaConstArray),
}

pub fn build_program(prog: &KoopaProgram) -> Result<Program, String> {
    // Initialize program and the symbol table.
    let mut ctx = ctx::KoopaGlobalContext::new();

    // Declare library functions.
    KoopaFunc::lib_funcs()
        .into_iter()
        .for_each(|f| f.decl(&mut ctx));

    let mut funcs = vec![];
    // Insert all functions and global variables to the symtable.
    for decl in prog.defs.iter() {
        match decl {
            KoopaFuncGlobalVarDecl::Func(func) => {
                func.def(&mut ctx);
                funcs.push(func);
            }
            KoopaFuncGlobalVarDecl::Var(var) => {
                var.global_decl(&mut ctx);
                var.global_init(&mut ctx);
            }
            KoopaFuncGlobalVarDecl::Const(c) => c.global_decl(&mut ctx),
            KoopaFuncGlobalVarDecl::VarArray(arr) => {
                arr.global_decl(&mut ctx);
                arr.global_init(&mut ctx);
            }
            KoopaFuncGlobalVarDecl::ConstArray(arr) => {
                arr.global_decl(&mut ctx);
                arr.global_init(&mut ctx);
            }
        }
    }

    for func in funcs {
        let mut fctx = KoopaLocalContext::new(&mut ctx, &func.name);
        func.implement(&mut fctx);
        fctx.destroy();
    }

    Ok(ctx.program)
}
