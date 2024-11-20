use koopa::ir::{FunctionData, Type, Value};

use crate::koo::block::KoopaBlock;
use crate::koo::ctx::{KoopaGlobalContext, KoopaLocalContext};
use crate::symtable::SymEntry;

use super::ctx::KoopaContext;

pub struct KoopaFunc {
    pub name: String,
    pub param_types: Vec<Type>,
    pub param_names: Vec<Option<String>>,
    pub ret_type: Type,
    pub block: Option<KoopaBlock>,
}

impl KoopaFunc {
    // Define a function in the program.
    // This function should be further implemented with KoopaFunc::implement.
    pub fn def(&self, ctx: &mut KoopaGlobalContext) {
        assert!(self.param_types.len() == self.param_names.len());
        ctx.new_func(FunctionData::with_param_names(
            format!("@{}", &self.name),
            self.param_names
                .iter()
                .map(|s| s.as_ref().map(|ss| format!("@{}", ss)))
                .zip(self.param_types.iter().cloned())
                .collect(),
            self.ret_type.clone(),
        ));
    }

    // Implement a function defined before.
    pub fn implement(&self, ctx: &mut KoopaLocalContext) {
        // Function parameters.
        let params = ctx
            .program
            .func_mut(ctx.func)
            .params()
            .into_iter()
            .cloned()
            .zip(self.param_names.iter().cloned())
            .collect::<Vec<(Value, Option<String>)>>();
        for (pv, p) in params.into_iter() {
            let pname = p
                .as_ref()
                .expect("Cannot implement function with anonymous parameters")
                .clone();
            let ty = ctx
                .program
                .func(ctx.func)
                .dfg()
                .value(pv.clone())
                .ty()
                .clone();
            let alloc = ctx.alloc(ty.clone());
            ctx.set_value_name(&alloc, &format!("%{}", p.unwrap()));
            let store = ctx.store(pv.clone(), alloc.clone());
            ctx.symtable
                .insert(pname, SymEntry::Var(alloc, ty.clone()))
                .expect("Inserting func param fails");
            ctx.new_instr(alloc);
            ctx.new_instr(store);
        }

        // A recursive conversion call. A block may have nested blocks.
        self.block.as_ref().unwrap().implement(ctx);

        if !ctx.returned() {
            let ret = ctx.ret(None);
            ctx.new_instr(ret);
        }
    }

    // Declare a function without explicit implementation.
    // Used for library functions.
    pub fn decl(&self, ctx: &mut KoopaGlobalContext) {
        ctx.new_func(FunctionData::new_decl(
            format!("@{}", &self.name),
            self.param_types.clone(),
            self.ret_type.clone(),
        ));
    }

    pub fn lib_funcs() -> Vec<Self> {
        vec![
            KoopaFunc {
                name: "getint".to_string(),
                param_types: vec![],
                param_names: vec![],
                ret_type: Type::get_i32(),
                block: None,
            },
            KoopaFunc {
                name: "getch".to_string(),
                param_types: vec![],
                param_names: vec![],
                ret_type: Type::get_i32(),
                block: None,
            },
            KoopaFunc {
                name: "getarray".to_string(),
                param_types: vec![Type::get_pointer(Type::get_i32())],
                param_names: vec![None],
                ret_type: Type::get_i32(),
                block: None,
            },
            KoopaFunc {
                name: "putint".to_string(),
                param_types: vec![Type::get_i32()],
                param_names: vec![None],
                ret_type: Type::get_unit(),
                block: None,
            },
            KoopaFunc {
                name: "putch".to_string(),
                param_types: vec![Type::get_i32()],
                param_names: vec![None],
                ret_type: Type::get_unit(),
                block: None,
            },
            KoopaFunc {
                name: "putarray".to_string(),
                param_types: vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
                param_names: vec![None, None],
                ret_type: Type::get_unit(),
                block: None,
            },
            KoopaFunc {
                name: "starttime".to_string(),
                param_types: vec![],
                param_names: vec![],
                ret_type: Type::get_unit(),
                block: None,
            },
            KoopaFunc {
                name: "stoptime".to_string(),
                param_types: vec![],
                param_names: vec![],
                ret_type: Type::get_unit(),
                block: None,
            },
        ]
    }
}
