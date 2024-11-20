use koopa::ir::{Type, Value};

use crate::koo::ctx::{KoopaContext, KoopaGlobalContext, KoopaLocalContext};
use crate::koo::traits::{
    KoopaAssignment, KoopaGlobalDeclaration, KoopaGlobalInit, KoopaLocalDeclaration, KoopaLocalInit,
};
use crate::symtable::{SymEntry, SymTable};

#[derive(Clone, Debug)]
pub struct KoopaConst(pub String, pub i32);

impl KoopaConst {
    pub fn decl(&self, symtable: &mut SymTable) {
        // Constants in the SysY language must be determined during compilation.
        // Just replace them with values recorded in the symbol table.
        // No need to add additional IR instructions.
        symtable
            .insert(self.0.clone(), SymEntry::Const(self.1))
            .unwrap();
    }
}

impl KoopaLocalDeclaration for KoopaConst {
    fn local_decl(&self, ctx: &mut KoopaLocalContext) {
        self.decl(ctx.symtable);
    }
}

impl KoopaGlobalDeclaration for KoopaConst {
    fn global_decl(&self, ctx: &mut KoopaGlobalContext) {
        self.decl(&mut ctx.symtable);
    }
}

#[derive(Clone, Debug)]
pub struct KoopaVar(pub String, pub Option<i32>);

impl KoopaVar {
    pub fn empty(name: String) -> Self {
        KoopaVar(name, None)
    }

    pub fn decl(&self, symtable: &mut SymTable) {
        // Allocate variable and set its name, if it has never been declared.
        symtable
            .insert(self.0.clone(), SymEntry::VarType)
            .unwrap();
    }
}

impl KoopaLocalDeclaration for KoopaVar {
    fn local_decl(&self, ctx: &mut KoopaLocalContext) {
        // Allocate variable and set its name, if it has never been declared.
        let v = ctx.alloc(Type::get_i32());
        ctx.set_value_name(&v, &format!("@{}", &self.0));
        ctx.new_instr(v);
        ctx.symtable_mut()
            .insert(self.0.clone(), SymEntry::Var(v, Type::get_i32()))
            .unwrap();
    }
}

impl KoopaGlobalDeclaration for KoopaVar {
    fn global_decl(&self, ctx: &mut KoopaGlobalContext) {
        // Allocate variable and set its name, if it has never been declared.
        let iv = ctx.zero_init(Type::get_i32());
        let v = ctx.global_alloc(iv);
        ctx.set_value_name(&v, &format!("@{}", &self.0));
        ctx.new_instr(v);
        ctx.symtable_mut()
            .insert(self.0.clone(), SymEntry::Var(v, Type::get_i32()))
            .unwrap();
    }
}

impl KoopaAssignment for KoopaVar {
    fn assign(&self, ctx: &mut KoopaLocalContext, new_v: Value) {
        let v = ctx.symtable.get(&self.0).unwrap().get_var();
        let store = ctx.store(new_v, v);
        ctx.new_instr(store);
    }
}

impl KoopaGlobalInit for KoopaVar {
    fn global_init(&self, ctx: &mut KoopaGlobalContext) {
        if self.1.is_none() {
            return;
        }
        let old_v = ctx.symtable.get(&self.0).unwrap().get_var();
        ctx.program.remove_value(old_v);
        let iv = self.1.map(|x| ctx.integer(x)).unwrap();
        let v = ctx.global_alloc(iv);
        ctx.program.set_value_name(v, Some(format!("@{}", &self.0)));
        ctx.symtable
            .replace(self.0.clone(), SymEntry::Var(v, Type::get_i32()))
            .unwrap();
    }
}

impl KoopaLocalInit for KoopaVar {
    fn local_init(&self, ctx: &mut KoopaLocalContext) {
        if self.1.is_none() {
            return;
        }
        let v = ctx.integer(self.1.unwrap());
        self.assign(ctx, v);
    }
}
