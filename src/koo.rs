use koopa::ir::builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value, ValueKind};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{
    Block, BlockItem, CompUnit, CompUnitDecl, Expr, OpCode, Symbol, SymbolValue, SysYType,
};
use crate::symtable::{SymEntry, SymTable};

fn is_bb_returned(program: &Program, func: &Function, bb: &BasicBlock) -> bool {
    let insts = program.func(*func).layout().bbs().node(bb).unwrap().insts();
    insts
        .back_key()
        .map(|x| {
            matches!(
                program.func(*func).dfg().value(x.clone()).kind(),
                ValueKind::Return(_)
            )
        })
        .unwrap_or(false)
}

fn shrink_instrs(program: &mut Program, func: &Function, bb: &BasicBlock) {
    let mut shrinked = program
        .func(*func)
        .layout()
        .bbs()
        .node(bb)
        .expect("Basic block does not exist.")
        .insts()
        .keys()
        .take_while(|&&v| {
            !matches!(
                program.func(*func).dfg().value(v.clone()).kind(),
                ValueKind::Return(_) | ValueKind::Jump(_) | ValueKind::Branch(..)
            )
        })
        .copied()
        .collect::<Vec<Value>>();
    if let Some(&ins) = program
        .func(*func)
        .layout()
        .bbs()
        .node(bb)
        .expect("Basic block does not exist.")
        .insts()
        .keys()
        .skip(shrinked.len())
        .next()
    {
        shrinked.push(ins);
    }
    program
        .func_mut(*func)
        .layout_mut()
        .bb_mut(bb.clone())
        .insts_mut()
        .clear();
    program
        .func_mut(*func)
        .layout_mut()
        .bb_mut(bb.clone())
        .insts_mut()
        .extend(shrinked);
}

fn shortpath_eval(
    program: &mut Program,
    func: &Function,
    symtable: &mut SymTable,
    bb: &mut BasicBlock,
    op_code: &OpCode,
    lhs: &Box<Expr>,
    rhs: &Box<Expr>,
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
    let sym = Symbol {
        name: _tmp_name.clone(),
        value: SymbolValue::Var(Some(Box::new(Expr::Number(init_v)))),
    };
    sym.add_to_bb(program, func, symtable, bb);

    let l = lhs.unroll(program, func, symtable, bb);
    let z = program.func_mut(*func).dfg_mut().new_value().integer(0);
    let leq0 = program
        .func_mut(*func)
        .dfg_mut()
        .new_value()
        .binary(comp, l, z);

    // Create BBs for shortpath eval.
    let mut shortpath_bb = program
        .func_mut(*func)
        .dfg_mut()
        .new_bb()
        .basic_block(Some(format!("%{}", repr)));
    let end_bb = program
        .func_mut(*func)
        .dfg_mut()
        .new_bb()
        .basic_block(Some(format!("%{}fi", repr)));

    program
        .func_mut(*func)
        .layout_mut()
        .bbs_mut()
        .extend([shortpath_bb, end_bb]);

    let branching =
        program
            .func_mut(*func)
            .dfg_mut()
            .new_value()
            .branch(leq0, shortpath_bb, end_bb);

    // Add instructions for the LHS.
    // Flush instructions to the current BB.
    program
        .func_mut(*func)
        .layout_mut()
        .bb_mut(bb.clone())
        .insts_mut()
        .extend([leq0, branching]);

    let v = symtable.get(&_tmp_name).unwrap();
    match v {
        SymEntry::Var(v) => {
            // Add instructions for the shortpath.
            let mut rhs_stack = vec![];
            let exprv = rhs.unroll(program, func, symtable, &mut shortpath_bb);
            let rneq0 =
                program
                    .func_mut(*func)
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::NotEq, exprv, z);
            rhs_stack.push(rneq0);
            rhs_stack.push(
                program
                    .func_mut(*func)
                    .dfg_mut()
                    .new_value()
                    .store(rneq0, v),
            );
            rhs_stack.push(program.func_mut(*func).dfg_mut().new_value().jump(end_bb));
            program
                .func_mut(*func)
                .layout_mut()
                .bb_mut(shortpath_bb)
                .insts_mut()
                .extend(rhs_stack);

            // Change the current BB.
            *bb = end_bb;

            // The final result.
            let ret_v = program.func_mut(*func).dfg_mut().new_value().load(v);
            program
                .func_mut(*func)
                .layout_mut()
                .bb_mut(end_bb)
                .insts_mut()
                .extend([ret_v]);
            ret_v
        }
        _ => unreachable!(),
    }
}
// Converting an AST to Koopa IR using koopa::ir API.
impl Expr {
    /*
    Unwrap a single-line command into a series of IR instructions.
    A single-line command may contain many operations, e.g., `return -(!2)`.
    We should decompose it into a series of binary instructions, e.g.,
    %entry:
        %0 = eq 0, 2
        %1 = sub 0, %0
        ret %1
    */
    pub fn unroll(
        &self,
        program: &mut Program,
        func: &Function,
        symtable: &mut SymTable,
        bb: &mut BasicBlock,
    ) -> Value {
        match self {
            Expr::Number(n) => program
                .func_mut(*func)
                .dfg_mut()
                .new_value()
                .integer(n.clone()),
            Expr::Unary(op, sub_expr) => match op {
                OpCode::Sub | OpCode::Not => {
                    let r = sub_expr.unroll(program, func, symtable, bb);
                    let dfg = program.func_mut(*func).dfg_mut();
                    let l = dfg.new_value().integer(0);
                    let v = dfg.new_value().binary(op.into(), l, r);
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(bb.clone())
                        .insts_mut()
                        .extend([v]);
                    v
                }
                OpCode::Add => sub_expr.unroll(program, func, symtable, bb),
                _ => panic!("Unsupported unary operator: {:?}", op),
            },
            Expr::Binary(lhs, op, rhs) => match op {
                OpCode::LogicOr | OpCode::LogicAnd => {
                    shortpath_eval(program, func, symtable, bb, op, lhs, rhs)
                }
                _ => {
                    let l = lhs.unroll(program, func, symtable, bb);
                    let r = rhs.unroll(program, func, symtable, bb);
                    let v = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_value()
                        .binary(op.into(), l, r);
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(bb.clone())
                        .insts_mut()
                        .extend([v]);
                    v
                }
            },
            Expr::Symbol(name) => {
                let symv = symtable.get(name).unwrap();
                let dfg = program.func_mut(*func).dfg_mut();
                match symv {
                    SymEntry::Const(v) => dfg.new_value().integer(v.clone()),
                    SymEntry::Var(v) => {
                        let v = dfg.new_value().load(v.clone());
                        program
                            .func_mut(*func)
                            .layout_mut()
                            .bb_mut(bb.clone())
                            .insts_mut()
                            .extend([v]);
                        v
                    }
                    SymEntry::FuncParam(v) => {
                        let ty = dfg.value(v).ty().clone();
                        let alloc = dfg.new_value().alloc(ty);
                        dfg.set_value_name(alloc.clone(), Some(format!("%{}", name)));
                        let store = dfg.new_value().store(v.clone(), alloc.clone());
                        let load = dfg.new_value().load(alloc.clone());
                        program
                            .func_mut(*func)
                            .layout_mut()
                            .bb_mut(bb.clone())
                            .insts_mut()
                            .extend([alloc, store, load]);
                        // Replace the original symbol with the new one.
                        symtable
                            .replace(name.clone(), SymEntry::Var(alloc))
                            .unwrap();
                        load
                    }
                    SymEntry::Func(..) => {
                        panic!("Function call in expression but expressed as Expr::Symbol.")
                    }
                }
            }
            Expr::FuncCall { funcname, args } => {
                if let SymEntry::Func(callee) = symtable.get(funcname).unwrap() {
                    let args = args
                        .iter()
                        .map(|e| e.unroll(program, func, symtable, bb))
                        .collect::<Vec<Value>>();
                    let v = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_value()
                        .call(callee, args);
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(bb.clone())
                        .insts_mut()
                        .extend([v]);
                    v
                } else {
                    panic!("Function {} not found", funcname);
                }
            }
        }
    }
}

impl Symbol {
    pub fn add_to_bb(
        &self,
        program: &mut Program,
        func: &Function,
        symtable: &mut SymTable,
        bb: &mut BasicBlock,
    ) {
        match &self.value {
            SymbolValue::Const(expr) => {
                // Constants in the SysY language must be determined during compilation.
                // Just replace them with values recorded in the symbol table.
                // No need to add additional IR instructions.
                symtable
                    .insert(self.name.clone(), SymEntry::Const(expr.reduce(&symtable)))
                    .unwrap();
            }
            SymbolValue::Var(init) => {
                // Allocate variable and set its name, if it has never been declared.
                let v: Value;
                v = program
                    .func_mut(*func)
                    .dfg_mut()
                    .new_value()
                    .alloc(Type::get_i32());
                program
                    .func_mut(*func)
                    .dfg_mut()
                    .set_value_name(v, Some(format!("@{}", &self.name)));
                program
                    .func_mut(*func)
                    .layout_mut()
                    .bb_mut(bb.clone())
                    .insts_mut()
                    .extend([v]);
                symtable
                    .insert(self.name.clone(), SymEntry::Var(v))
                    .unwrap();

                // Initialize the variable if given.
                if let Some(init_value) = init {
                    let iv = init_value.unroll(program, func, symtable, bb);
                    let store_v = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_value()
                        .store(iv, v.clone());
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(bb.clone())
                        .insts_mut()
                        .extend([store_v]);
                }
            }
        }
    }
}

impl BlockItem {
    fn add_to_bb(
        &self,
        program: &mut Program,
        symtable: &mut SymTable,
        func: &Function,
        bb: &mut BasicBlock,
    ) -> Result<(), String> {
        let mut instrs = vec![];
        match self {
            BlockItem::Decl(symbols) => {
                for s in symbols.iter() {
                    s.add_to_bb(program, func, symtable, bb);
                }
            }
            BlockItem::Assign(name, expr) => {
                let exprv = expr.unroll(program, func, symtable, bb);
                let v = symtable.get(name).unwrap();
                match v {
                    SymEntry::Var(v) => {
                        instrs.push(
                            program
                                .func_mut(*func)
                                .dfg_mut()
                                .new_value()
                                .store(exprv, v.clone()),
                        );
                    }
                    SymEntry::Func(_) => {
                        panic!("Cannot assign to a function: {}", name)
                    }
                    SymEntry::FuncParam(_) => {
                        panic!("Cannot assign to a function parameter: {}", name)
                    }
                    SymEntry::Const(_) => panic!("Cannot assign to a constant: {}", name),
                }
            }
            BlockItem::Ret(expr) => {
                // Create instructions with recursion.
                let retv = expr.clone().map(|e| e.unroll(program, func, symtable, bb));
                // Return the final value.
                instrs.push(program.func_mut(*func).dfg_mut().new_value().ret(retv));
            }
            BlockItem::Expr(e) => {
                if let Some(e) = e {
                    e.unroll(program, func, symtable, bb);
                }
            }
            BlockItem::If { .. }
            | BlockItem::Block(..)
            | BlockItem::While { .. }
            | BlockItem::Break
            | BlockItem::Continue => {
                panic!("Should not called in BlockItem::add_to_bb")
            }
        }
        // Record all instructions.
        program
            .func_mut(*func)
            .layout_mut()
            .bb_mut(bb.clone())
            .insts_mut()
            .extend(instrs);
        Ok(())
    }
}

impl Block {
    fn add_to_bb(
        &self,
        program: &mut Program,
        symtable: &mut SymTable,
        func: &Function,
        bb: &mut BasicBlock,
        break_dst: Option<&BasicBlock>,
        conti_dst: Option<&BasicBlock>,
    ) -> Result<(), String> {
        for item in self.items.iter() {
            match item {
                BlockItem::If {
                    cond,
                    then_block,
                    else_block,
                } => {
                    static IF_COUNTER: AtomicUsize = AtomicUsize::new(0);
                    let if_cnt = IF_COUNTER.fetch_add(1, Ordering::Relaxed);

                    // Create basic blocks for the if-else statement.
                    let mut then_bb = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some(format!("%if{}", if_cnt)));
                    let mut else_bb = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some(format!("%if{}else", if_cnt)));
                    let end_bb = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some(format!("%fi{}", if_cnt)));
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bbs_mut()
                        .extend([then_bb, else_bb, end_bb]);

                    // Create the branching instruction and complete the current basic block.
                    let mut instr_stack = vec![];
                    let condv = cond.unroll(program, func, symtable, bb);
                    instr_stack.push(
                        program
                            .func_mut(*func)
                            .dfg_mut()
                            .new_value()
                            .branch(condv, then_bb, else_bb),
                    );
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(bb.clone())
                        .insts_mut()
                        .extend(instr_stack);

                    // Handle the "then" block.
                    // Wrap it as a block and call Block::add_to_bb recursively.
                    Block {
                        items: vec![then_block.as_ref().clone()],
                    }
                    .add_to_bb(
                        program,
                        symtable,
                        func,
                        &mut then_bb,
                        break_dst,
                        conti_dst,
                    )?;
                    let jump_v = program.func_mut(*func).dfg_mut().new_value().jump(end_bb);
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(then_bb.clone())
                        .insts_mut()
                        .extend([jump_v]);
                    shrink_instrs(program, func, &then_bb);

                    // Handle the "else" block.
                    // If there's no "else", create an empty block for the jump instruction.
                    if else_block.is_some() {
                        Block {
                            items: vec![else_block.as_ref().unwrap().as_ref().clone()],
                        }
                        .add_to_bb(
                            program,
                            symtable,
                            func,
                            &mut else_bb,
                            break_dst,
                            conti_dst,
                        )?;
                    } else {
                        Block {
                            items: vec![BlockItem::Expr(None)],
                        }
                        .add_to_bb(
                            program,
                            symtable,
                            func,
                            &mut else_bb,
                            break_dst,
                            conti_dst,
                        )?;
                    }
                    let jump_v = program.func_mut(*func).dfg_mut().new_value().jump(end_bb);
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(else_bb.clone())
                        .insts_mut()
                        .extend([jump_v]);
                    shrink_instrs(program, func, &else_bb);

                    // Finally, change the current basic block to "end_bb".
                    *bb = end_bb;
                }
                BlockItem::While { cond, while_block } => {
                    static WHILE_COUNTER: AtomicUsize = AtomicUsize::new(0);
                    let while_cnt = WHILE_COUNTER.fetch_add(1, Ordering::Relaxed);

                    // Create basic blocks for the while statement.
                    let mut cond_bb = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some(format!("%while{}entry", while_cnt)));
                    let entry_bb = cond_bb.clone();
                    let mut body_bb = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some(format!("%while{}body", while_cnt)));
                    let end_bb = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some(format!("%while{}fi", while_cnt)));
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bbs_mut()
                        .extend([cond_bb, body_bb, end_bb]);

                    // Create the jump instruction and complete the current basic block.
                    let jp = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_value()
                        .jump(cond_bb.clone());
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(bb.clone())
                        .insts_mut()
                        .extend([jp]);

                    // Handle the "cond" block.
                    let condv = cond.unroll(program, func, symtable, &mut cond_bb);
                    let branchv = program.func_mut(*func).dfg_mut().new_value().branch(
                        condv,
                        body_bb.clone(),
                        end_bb.clone(),
                    );
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(cond_bb.clone())
                        .insts_mut()
                        .extend([branchv]);

                    // Handle the "body" block.
                    // Wrap it as a block and call Block::add_to_bb recursively.
                    Block {
                        items: vec![while_block.as_ref().clone()],
                    }
                    .add_to_bb(
                        program,
                        symtable,
                        func,
                        &mut body_bb,
                        Some(&end_bb),
                        Some(&entry_bb),
                    )?;
                    let jump_v = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_value()
                        .jump(entry_bb.clone());
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(body_bb.clone())
                        .insts_mut()
                        .extend([jump_v]);
                    shrink_instrs(program, func, &body_bb);

                    // Finally, change the current basic block to "end_bb".
                    *bb = end_bb;
                }
                BlockItem::Break => {
                    if break_dst.is_none() {
                        panic!("Break statement not in a while loop");
                    }
                    let jump_v = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_value()
                        .jump(break_dst.unwrap().clone());
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(bb.clone())
                        .insts_mut()
                        .extend([jump_v]);
                }
                BlockItem::Continue => {
                    if conti_dst.is_none() {
                        panic!("Continue statement not in a while loop");
                    }
                    let jump_v = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_value()
                        .jump(conti_dst.unwrap().clone());
                    program
                        .func_mut(*func)
                        .layout_mut()
                        .bb_mut(bb.clone())
                        .insts_mut()
                        .extend([jump_v]);
                }
                BlockItem::Block(block) => {
                    symtable.fork();
                    block.add_to_bb(program, symtable, func, bb, break_dst, conti_dst)?;
                    symtable.join()?;
                }
                _ => {
                    item.add_to_bb(program, symtable, func, bb)?;
                }
            }
            if is_bb_returned(program, func, &bb) {
                break;
            }
        }
        Ok(())
    }
}

pub fn build_program(comp_unit: &CompUnit) -> Result<Program, String> {
    // Initialize program and symbol table.
    let mut program: Program = Program::new();
    let mut symtable = SymTable::new();

    // Declare library functions.
    let libfunc_names = [
        "getint",
        "getch",
        "getarray",
        "putint",
        "putch",
        "putarray",
        "starttime",
        "stoptime",
    ];
    let libfunc_ptys: [Vec<Type>; 8] = [
        vec![],
        vec![],
        vec![Type::get_pointer(Type::get_i32())],
        vec![Type::get_i32()],
        vec![Type::get_i32()],
        vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
        vec![],
        vec![],
    ];
    let libfunc_rettys: [Type; 8] = [
        Type::get_i32(),
        Type::get_i32(),
        Type::get_i32(),
        Type::get_unit(),
        Type::get_unit(),
        Type::get_unit(),
        Type::get_unit(),
        Type::get_unit(),
    ];
    for ((&name, params_ty), ret_ty) in libfunc_names
        .iter()
        .zip(libfunc_ptys.iter())
        .zip(libfunc_rettys.iter())
    {
        let data = FunctionData::new_decl(format!("@{}", name), params_ty.clone(), ret_ty.clone());
        symtable.insert(name.to_string(), SymEntry::Func(program.new_func(data)))?;
    }

    // Insert all functions to the symtable.
    for decl in comp_unit.defs.iter() {
        match decl {
            CompUnitDecl::FuncDef {
                type_,
                ident,
                params,
                block: _,
            } => {
                let function = program.new_func(FunctionData::with_param_names(
                    format!("@{}", ident),
                    params
                        .iter()
                        .map(|p| {
                            (
                                Some(format!("@{}", p.ident)),
                                match p.type_ {
                                    SysYType::Int => Type::get_i32(),
                                    _ => {
                                        panic!("unsupported type: {:?}", p.type_)
                                    }
                                },
                            )
                        })
                        .collect(),
                    match type_ {
                        SysYType::Int => Type::get_i32(),
                        SysYType::Void => Type::get_unit(),
                    },
                ));
                // Insert the function into the symbol table.
                symtable.insert(ident.clone(), SymEntry::Func(function))?;
            }
            CompUnitDecl::VarDecl(symbols) => {
                for s in symbols {
                    match &s.value {
                        SymbolValue::Const(expr) => {
                            // Constants in the SysY language must be determined during compilation.
                            // Just replace them with values recorded in the symbol table.
                            // No need to add additional IR instructions.
                            symtable
                                .insert(s.name.clone(), SymEntry::Const(expr.reduce(&symtable)))
                                .unwrap();
                        }
                        SymbolValue::Var(init) => {
                            let mut iv: i32 = 0;
                            if let Some(init_value) = init {
                                iv = init_value.reduce(&symtable);
                            }
                            let iv = program.new_value().integer(iv);
                            let v = program.new_value().global_alloc(iv);
                            program.set_value_name(v, Some(format!("@{}", &s.name)));
                            symtable.insert(s.name.clone(), SymEntry::Var(v)).unwrap();
                        }
                    }
                }
            }
        }
    }

    for decl in comp_unit.defs.iter() {
        match decl {
            CompUnitDecl::FuncDef {
                type_: _,
                ident,
                params,
                block,
            } => {
                let function = symtable.get(ident)?;
                if let SymEntry::Func(function) = function {
                    // Fork the symtable to insert function parameters in this scope.
                    symtable.fork();

                    // Function parameters.
                    for (pv, p) in program.func(function).params().iter().zip(params) {
                        symtable.insert(p.ident.clone(), SymEntry::FuncParam(pv.clone()))?;
                    }

                    // Create the entry bb.
                    let mut bb = program
                        .func_mut(function)
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some("%entry".into()));
                    program
                        .func_mut(function)
                        .layout_mut()
                        .bbs_mut()
                        .extend([bb.clone()]);
                    // A recursive conversion call. A block may have nested blocks.
                    block.add_to_bb(&mut program, &mut symtable, &function, &mut bb, None, None)?;

                    if !is_bb_returned(&program, &function, &bb) {
                        let ret = program.func_mut(function).dfg_mut().new_value().ret(None);
                        program
                            .func_mut(function)
                            .layout_mut()
                            .bb_mut(bb)
                            .insts_mut()
                            .extend([ret]);
                    }

                    // Destroy the symtable.
                    symtable.join()?;
                } else {
                    panic!("Function {} not found", ident);
                }
            }
            _ => {}
        }
    }

    Ok(program)
}
