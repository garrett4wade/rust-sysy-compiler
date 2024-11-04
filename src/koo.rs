use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value, ValueKind};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{Block, BlockItem, CompUnit, Expr, FuncType, OpCode, Symbol, SymbolValue};
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
                let dfg = program.func_mut(*func).dfg_mut();
                // Constants in the SysY language must be determined during compilation.
                // Just replace them with values recorded in the symbol table.
                // No need to add additional IR instructions.
                symtable
                    .insert(
                        self.name.clone(),
                        SymEntry::Const(expr.reduce(dfg, &symtable)),
                    )
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
                let v = symtable.get(name).unwrap();
                match v {
                    SymEntry::Var(v) => {
                        let exprv = expr.unroll(program, func, symtable, bb);
                        instrs.push(
                            program
                                .func_mut(*func)
                                .dfg_mut()
                                .new_value()
                                .store(exprv, v.clone()),
                        );
                    }
                    _ => panic!("Cannot assign to a constant: {}", name),
                }
            }
            BlockItem::Ret(expr) => {
                // Create instructions with recursion.
                let retv = expr.unroll(program, func, symtable, bb);
                // Return the final value.
                instrs.push(
                    program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_value()
                        .ret(Some(retv)),
                );
            }
            BlockItem::Expr(_) => {
                // Do nothing.
            }
            BlockItem::If { .. } | BlockItem::Block(..) => {
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
                    .add_to_bb(program, symtable, func, &mut then_bb)?;
                    if !is_bb_returned(program, func, &then_bb) {
                        let jump_v = program.func_mut(*func).dfg_mut().new_value().jump(end_bb);
                        program
                            .func_mut(*func)
                            .layout_mut()
                            .bb_mut(then_bb.clone())
                            .insts_mut()
                            .extend([jump_v]);
                    }

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
                        )?;
                    }
                    if !is_bb_returned(program, func, &else_bb) {
                        let jump_v = program.func_mut(*func).dfg_mut().new_value().jump(end_bb);
                        program
                            .func_mut(*func)
                            .layout_mut()
                            .bb_mut(else_bb.clone())
                            .insts_mut()
                            .extend([jump_v]);
                    }

                    // Finally, change the current basic block to "end_bb".
                    *bb = end_bb;
                }
                BlockItem::Block(block) => {
                    symtable.fork();
                    block.add_to_bb(program, symtable, func, bb)?;
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

    // TODO: Only the main function is supported.
    let func = &comp_unit.func_def;
    let function = program.new_func(FunctionData::with_param_names(
        format!("@{}", func.ident),
        vec![],
        match func.type_ {
            FuncType::Int => Type::get_i32(),
        },
    ));
    let func_data = program.func_mut(function);
    // Create the entry bb.
    let mut entry = func_data
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));
    func_data.layout_mut().bbs_mut().extend([entry.clone()]);
    // A recursive conversion call. A block may have nested blocks.
    func.block
        .add_to_bb(&mut program, &mut symtable, &function, &mut entry)?;

    Ok(program)
}
