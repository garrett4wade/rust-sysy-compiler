use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder};
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, Value, ValueKind};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{Block, BlockItem, CompUnit, FuncType};
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

// Converting an AST to Koopa IR using koopa::ir API.
impl BlockItem {
    fn add_to_bb(
        &self,
        program: &mut Program,
        symtable: &mut SymTable,
        func: &Function,
        bb: &BasicBlock,
    ) -> Result<(), String> {
        let mut instr_stack: Vec<Value> = vec![];
        match self {
            BlockItem::Decl(symbols) => {
                symbols.iter().for_each(|x| {
                    x.decl_symbol(
                        program.func_mut(*func).dfg_mut(),
                        symtable,
                        &mut instr_stack,
                    )
                });
            }
            BlockItem::Assign(name, expr) => {
                let v = symtable.get(name).unwrap();
                match v {
                    SymEntry::Var(v) => {
                        let exprv = expr.unroll(
                            program.func_mut(*func).dfg_mut(),
                            &mut instr_stack,
                            &symtable,
                        );
                        instr_stack.push(
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
                let retv = expr.unroll(
                    program.func_mut(*func).dfg_mut(),
                    &mut instr_stack,
                    &symtable,
                );
                // Return the final value.
                instr_stack.push(
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
            .bb_mut(*bb)
            .insts_mut()
            .extend(instr_stack);
        Ok(())
    }
}

impl Block {
    fn add_to_bb(
        &self,
        program: &mut Program,
        symtable: &mut SymTable,
        func: &Function,
        bb: &BasicBlock,
    ) -> Result<BasicBlock, String> {
        let mut bb = bb.clone();
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
                    let then_bb = program
                        .func_mut(*func)
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some(format!("%if{}", if_cnt)));
                    let else_bb = program
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
                    let condv = cond.unroll(
                        program.func_mut(*func).dfg_mut(),
                        &mut instr_stack,
                        &symtable,
                    );
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
                        .bb_mut(bb)
                        .insts_mut()
                        .extend(instr_stack);

                    // Handle the "then" block.
                    // Wrap it as a block and call Block::add_to_bb recursively.
                    let then_ret_bb = Block {
                        items: vec![then_block.as_ref().clone()],
                    }
                    .add_to_bb(program, symtable, func, &then_bb)?;
                    if !is_bb_returned(program, func, &then_ret_bb) {
                        let jump_v = program.func_mut(*func).dfg_mut().new_value().jump(end_bb);
                        program
                            .func_mut(*func)
                            .layout_mut()
                            .bb_mut(then_ret_bb)
                            .insts_mut()
                            .extend([jump_v]);
                    }

                    // Handle the "else" block.
                    // If there's no "else", create an empty block for the jump instruction.
                    let else_ret_bb;
                    if else_block.is_some() {
                        else_ret_bb = Block {
                            items: vec![else_block.as_ref().unwrap().as_ref().clone()],
                        }
                        .add_to_bb(program, symtable, func, &else_bb)?;
                    } else {
                        else_ret_bb = Block {
                            items: vec![BlockItem::Expr(None)],
                        }
                        .add_to_bb(program, symtable, func, &else_bb)?;
                    }
                    if !is_bb_returned(program, func, &else_ret_bb) {
                        let jump_v = program.func_mut(*func).dfg_mut().new_value().jump(end_bb);
                        program
                            .func_mut(*func)
                            .layout_mut()
                            .bb_mut(else_ret_bb)
                            .insts_mut()
                            .extend([jump_v]);
                    }

                    // Finally, change the current basic block to "end_bb".
                    bb = end_bb;
                }
                BlockItem::Block(block) => {
                    symtable.fork();
                    bb = block.add_to_bb(program, symtable, func, &bb)?;
                    symtable.join()?;
                }
                _ => {
                    item.add_to_bb(program, symtable, func, &bb)?;
                }
            }
            if is_bb_returned(program, func, &bb) {
                break;
            }
        }
        Ok(bb)
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
    let entry = func_data
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));
    func_data.layout_mut().bbs_mut().extend([entry]);
    // A recursive conversion call. A block may have nested blocks.
    func.block
        .add_to_bb(&mut program, &mut symtable, &function, &entry)?;

    Ok(program)
}
