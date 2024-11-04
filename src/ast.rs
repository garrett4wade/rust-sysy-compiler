use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value, ValueKind};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::symtable::{SymEntry, SymTable};
// AST definition
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

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Decl(Vec<Symbol>),
    Assign(String, Box<Expr>),
    Ret(Box<Expr>),
    Block(Block),
    Expr(Option<Box<Expr>>),
    If {
        cond: Box<Expr>,
        then_block: Box<BlockItem>,
        else_block: Option<Box<BlockItem>>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, OpCode, Box<Expr>),
    Unary(OpCode, Box<Expr>),
    Symbol(String),
    Number(i32),
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub value: SymbolValue,
}

#[derive(Debug, Clone)]
pub enum SymbolValue {
    Const(Box<Expr>),
    Var(Option<Box<Expr>>), // init val
}

impl Symbol {
    fn decl_symbol(
        &self,
        program: &mut Program,
        func: &Function,
        symtable: &mut SymTable,
        instr_stack: &mut Vec<Value>,
    ) {
        match &self.value {
            SymbolValue::Const(expr) => {
                // Constants in the SysY language must be determined during compilation.
                // Just replace them with values recorded in the symbol table.
                // No need to add additional IR instructions.
                symtable
                    .insert(
                        self.name.clone(),
                        SymEntry::Const(expr.reduce(program.func_mut(*func).dfg(), &symtable)),
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
                instr_stack.push(v);
                symtable
                    .insert(self.name.clone(), SymEntry::Var(v))
                    .unwrap();

                // Initialize the variable if given.
                if let Some(init_value) = init {
                    let iv = init_value.unroll(
                        program.func_mut(*func).dfg_mut(),
                        instr_stack,
                        &symtable,
                    );
                    instr_stack.push(
                        program
                            .func_mut(*func)
                            .dfg_mut()
                            .new_value()
                            .store(iv, v.clone()),
                    );
                }
            }
        };
    }
}

#[derive(Debug)]
pub enum BType {
    Int,
}

#[derive(Debug, Clone)]
pub enum OpCode {
    // lv 1
    LogicOr,
    // lv 2
    LogicAnd,
    // lv 3
    Eq,
    Ne,
    // lv 4
    Le,
    Ge,
    Lt,
    Gt,
    // lv 5
    Mul,
    Div,
    Mod,
    // lv 6
    Sub,
    Add,
    // lv 7, unary, including Sub and Add
    Not,
}

impl From<&OpCode> for BinaryOp {
    fn from(value: &OpCode) -> Self {
        match value {
            OpCode::Eq => BinaryOp::Eq,
            OpCode::Ne => BinaryOp::NotEq,
            OpCode::Le => BinaryOp::Le,
            OpCode::Ge => BinaryOp::Ge,
            OpCode::Lt => BinaryOp::Lt,
            OpCode::Gt => BinaryOp::Gt,
            OpCode::Add => BinaryOp::Add,
            OpCode::Sub => BinaryOp::Sub,
            OpCode::Mul => BinaryOp::Mul,
            OpCode::Div => BinaryOp::Div,
            OpCode::Mod => BinaryOp::Mod,
            OpCode::Not => BinaryOp::Eq,
            _ => panic!("Invalid operator: {:?}", value),
        }
    }
}

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
    fn unroll(
        &self,
        dfg: &mut DataFlowGraph,
        stack: &mut Vec<Value>,
        symtable: &SymTable,
    ) -> Value {
        match self {
            Expr::Number(n) => dfg.new_value().integer(n.clone()),
            Expr::Unary(op, sub_expr) => match op {
                OpCode::Sub | OpCode::Not => {
                    let l = dfg.new_value().integer(0);
                    let r = sub_expr.unroll(dfg, stack, symtable);
                    let v = dfg.new_value().binary(op.into(), l, r);
                    stack.push(v);
                    v
                }
                OpCode::Add => sub_expr.unroll(dfg, stack, symtable),
                _ => panic!("Unsupported unary operator: {:?}", op),
            },
            Expr::Binary(lhs, op, rhs) => {
                let l = lhs.unroll(dfg, stack, symtable);
                let r = rhs.unroll(dfg, stack, symtable);
                match op {
                    OpCode::LogicOr => {
                        let z = dfg.new_value().integer(0);
                        let lneq0 = dfg.new_value().binary(BinaryOp::NotEq, l, z);
                        let rneq0 = dfg.new_value().binary(BinaryOp::NotEq, r, z);
                        let v = dfg.new_value().binary(BinaryOp::Or, lneq0, rneq0);
                        stack.extend([lneq0, rneq0, v]);
                        v
                    }
                    OpCode::LogicAnd => {
                        let z = dfg.new_value().integer(0);
                        let lneq0 = dfg.new_value().binary(BinaryOp::NotEq, l, z);
                        let rneq0 = dfg.new_value().binary(BinaryOp::NotEq, r, z);
                        let v = dfg.new_value().binary(BinaryOp::And, lneq0, rneq0);
                        stack.extend([lneq0, rneq0, v]);
                        v
                    }
                    _ => {
                        let v = dfg.new_value().binary(op.into(), l, r);
                        stack.push(v);
                        v
                    }
                }
            }
            Expr::Symbol(name) => {
                let symv = symtable.get(name).unwrap();
                match symv {
                    SymEntry::Const(v) => dfg.new_value().integer(v.clone()),
                    SymEntry::Var(v) => {
                        let v = dfg.new_value().load(v.clone());
                        stack.push(v);
                        v
                    }
                }
            }
        }
    }

    pub fn reduce(&self, dfg: &DataFlowGraph, symtable: &SymTable) -> i32 {
        match self {
            Expr::Number(n) => *n,
            Expr::Unary(op, sub_expr) => match op {
                OpCode::Sub => -sub_expr.reduce(dfg, symtable),
                OpCode::Not => {
                    let v = sub_expr.reduce(dfg, symtable);
                    if v == 0 {
                        1
                    } else {
                        0
                    }
                }
                _ => panic!("Unsupported unary operator: {:?}", op),
            },
            Expr::Binary(lhs, op, rhs) => {
                let l = lhs.reduce(dfg, symtable);
                let r = rhs.reduce(dfg, symtable);
                match op {
                    OpCode::Add => l + r,
                    OpCode::Sub => l - r,
                    OpCode::Mul => l * r,
                    OpCode::Div => l / r,
                    OpCode::Mod => l % r,
                    OpCode::Eq => (l == r).into(),
                    OpCode::Ne => (l != r).into(),
                    OpCode::Lt => (l < r).into(),
                    OpCode::Gt => (l > r).into(),
                    OpCode::Le => (l <= r).into(),
                    OpCode::Ge => (l >= r).into(),
                    OpCode::LogicAnd => ((l != 0) && (l != 0)).into(),
                    OpCode::LogicOr => ((l != 0) || (l != 0)).into(),
                    OpCode::Not => panic!("Unsupported binary operator: {:?}", op),
                }
            }
            Expr::Symbol(name) => {
                let symv = symtable.get(name).unwrap();
                match symv {
                    SymEntry::Const(v) => v.clone(),
                    _ => panic!(
                        "Cannot reduce an expression at compilation with a variable: {}",
                        name
                    ),
                }
            }
        }
    }
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
                symbols
                    .iter()
                    .for_each(|x| x.decl_symbol(program, func, symtable, &mut instr_stack));
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
