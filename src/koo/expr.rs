use crate::koo::ctx::{KoopaContext, KoopaLocalContext};
use crate::symtable::SymEntry;
use koopa::ir::{BinaryOp, Value};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::OpCode;
use crate::koo::array::{KoopaArray, KoopaVarArray};
use crate::koo::traits::{KoopaLocalDeclaration, KoopaLocalInit};
use crate::koo::var::KoopaVar;

fn shortpath_eval(
    ctx: &mut KoopaLocalContext,
    op_code: &OpCode,
    lhs: &Box<KoopaExpr>,
    rhs: &Box<KoopaExpr>,
) -> Value {
    // Prepare some metadata.
    let init_v: i32;
    let comp: BinaryOp;
    let repr: String;
    match op_code {
        OpCode::LogicOr => {
            static LOGIC_OR_CNT: AtomicUsize = AtomicUsize::new(0);
            let cnt = LOGIC_OR_CNT.fetch_add(1, Ordering::Relaxed);
            repr = format!("__logicOr{}", cnt);
            init_v = 1;
            comp = BinaryOp::Eq;
        }
        OpCode::LogicAnd => {
            static LOGIC_AND_CNT: AtomicUsize = AtomicUsize::new(0);
            let cnt = LOGIC_AND_CNT.fetch_add(1, Ordering::Relaxed);
            repr = format!("__logicAnd{}", cnt);
            init_v = 0;
            comp = BinaryOp::NotEq;
        }
        _ => panic!(),
    }

    // The LHS.
    let _tmp_name = format!("{}Res", repr);
    let sym = KoopaVar(_tmp_name.clone(), Some(init_v));
    sym.local_decl(ctx);
    sym.local_init(ctx);

    let l = lhs.unroll(ctx);
    let z = ctx.integer(0);
    let leq0 = ctx.binary(comp, l, z);

    // Create BBs for shortpath eval.
    let shortpath_bb = ctx.new_bb(&format!("%{}", repr));
    let end_bb = ctx.new_bb(&format!("%{}fi", repr));

    let branching = ctx.branch(leq0, shortpath_bb, end_bb);
    ctx.new_instr(leq0);
    ctx.new_instr(branching);

    let v = ctx.symtable.get(&_tmp_name).unwrap().get_var();
    // Add instructions for the shortpath.
    ctx.switch(shortpath_bb);
    let exprv = rhs.unroll(ctx);
    let rneq0 = ctx.binary(BinaryOp::NotEq, exprv, z);
    ctx.new_instr(rneq0);
    let store = ctx.store(rneq0, v);
    ctx.new_instr(store);
    let jp = ctx.jump(end_bb);
    ctx.new_instr(jp);

    // Change the current BB.
    ctx.switch(end_bb);

    // The final result.
    let ret_v = ctx.load(v);
    ctx.new_instr(ret_v)
}

#[derive(Clone, Debug)]
pub enum KoopaExpr {
    Number(i32),
    Unary(OpCode, Box<KoopaExpr>),
    Binary(Box<KoopaExpr>, OpCode, Box<KoopaExpr>),
    Const(i32),
    Var(String),
    ArrayElem(String, Vec<KoopaExpr>),
    ArrayElemParam(String, Vec<KoopaExpr>),
    FuncCall(String, Vec<KoopaExpr>),
}

// Converting an AST to Koopa IR using koopa::ir API.
impl KoopaExpr {
    /*
    Unwrap a single-line command into a series of IR instructions.
    A single-line command may contain many operations, e.g., `return -(!2)`.
    We should decompose it into a series of binary instructions, e.g.,
    %entry:
        %0 = eq 0, 2
        %1 = sub 0, %0
        ret %1
    */
    pub fn unroll(&self, ctx: &mut KoopaLocalContext) -> Value {
        match self {
            KoopaExpr::Number(n) => ctx.integer(n.clone()),
            KoopaExpr::Unary(op, sub_expr) => match op {
                OpCode::Sub | OpCode::Not => {
                    let r = sub_expr.unroll(ctx);
                    let l = ctx.integer(0);
                    let v = ctx.binary(op.into(), l, r);
                    ctx.new_instr(v)
                }
                OpCode::Add => sub_expr.unroll(ctx),
                _ => panic!("Unsupported unary operator: {:?}", op),
            },
            KoopaExpr::Binary(lhs, op, rhs) => match op {
                OpCode::LogicOr | OpCode::LogicAnd => shortpath_eval(ctx, op, lhs, rhs),
                _ => {
                    let l = lhs.unroll(ctx);
                    let r = rhs.unroll(ctx);
                    let v = ctx.binary(op.into(), l, r);
                    ctx.new_instr(v)
                }
            },
            KoopaExpr::Const(n) => ctx.integer(n.clone()),
            KoopaExpr::Var(name) => {
                let var = ctx.symtable().get(name).unwrap().get_var();
                let v = ctx.load(var);
                ctx.new_instr(v)
            }
            KoopaExpr::ArrayElemParam(name, indices) => {
                let array = ctx.symtable().get(name).unwrap();
                match array {
                    SymEntry::Ptr(ptr, _, _dims) => {
                        // An upper level array parameter in the function.
                        let mut ptr = ctx.load(ptr);
                        ctx.new_instr(ptr);
                        if indices.len() > 0 {
                            let i = indices[0].unroll(ctx);
                            ptr = ctx.get_ptr(ptr, i);
                            ctx.new_instr(ptr);
                            for i in indices.iter().skip(1) {
                                let idx = i.unroll(ctx);
                                ptr = ctx.get_elem_ptr(ptr, idx);
                                ctx.new_instr(ptr);
                            }
                            if indices.len() < _dims.len() + 1 {
                                // A sub-array.
                                let i = ctx.integer(0);
                                let v = ctx.get_elem_ptr(ptr, i);
                                ptr = ctx.new_instr(v);
                            } else {
                                // Array element.
                                ptr = ctx.load(ptr);
                                ctx.new_instr(ptr);
                            }
                        }
                        ptr
                    }
                    SymEntry::Array(ptr, dims) | SymEntry::ConstArray(ptr, dims) => {
                        // A local array.
                        let mut ptr = ptr;
                        for i in indices.iter() {
                            let idx = i.unroll(ctx);
                            ptr = ctx.get_elem_ptr(ptr, idx);
                            ctx.new_instr(ptr);
                        }
                        if indices.len() < dims.len() {
                            // A sub-array.
                            let i = ctx.integer(0);
                            let v = ctx.get_elem_ptr(ptr, i);
                            ptr = ctx.new_instr(v);
                        } else {
                            // Array element.
                            ptr = ctx.load(ptr);
                            ctx.new_instr(ptr);
                        }
                        ptr
                    }
                    _ => panic!("Invalid ArrayElemParam: {:?}", array),
                }
            }
            KoopaExpr::ArrayElem(name, indices) => {
                let arr_v = KoopaVarArray::empty(name.clone()).get_coord_var(ctx, indices);
                let load = ctx.load(arr_v.clone());
                ctx.new_instr(arr_v);
                ctx.new_instr(load)
            }
            KoopaExpr::FuncCall(funcname, args) => {
                let callee = ctx.symtable().get(funcname).unwrap().get_func();
                let args = args.iter().map(|e| e.unroll(ctx)).collect::<Vec<Value>>();
                let call = ctx.call(callee, args);
                ctx.new_instr(call)
            }
        }
    }
}
