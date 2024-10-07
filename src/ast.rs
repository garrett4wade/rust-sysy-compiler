use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{FunctionData, Program, Type};

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}
#[derive(Debug)]
pub struct FuncDef {
    pub type_: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub ret: Ret,
}

#[derive(Debug)]
pub struct Ret {
    pub retv: i32,
}

pub trait KoopaAST {
    fn to_ir(&self) -> Program;
}

impl KoopaAST for CompUnit {
    fn to_ir(&self) -> Program {
        let mut program = Program::new();
        let main_fn = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.func_def.ident),
            vec![],
            Type::get_i32(),
        ));
        let main_fn_data = program.func_mut(main_fn);
        let entry = main_fn_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        main_fn_data.layout_mut().bbs_mut().extend([entry]);

        let ans = main_fn_data
            .dfg_mut()
            .new_value()
            .integer(self.func_def.block.ret.retv);
        let ret = main_fn_data.dfg_mut().new_value().ret(Some(ans));
        main_fn_data
            .layout_mut()
            .bb_mut(entry)
            .insts_mut()
            .extend([ret]);

        program
    }
}
