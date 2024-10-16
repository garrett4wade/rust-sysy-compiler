use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};

use std::cell::RefCell;
use std::collections::HashMap;
use std::iter;
use std::rc::Rc;
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

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Vec<Symbol>),
    Assign(String, Box<Expr>),
    Ret(Box<Expr>),
    Block(Block),
    Expr(Option<Box<Expr>>),
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
pub trait KoopaAST {
    fn add_to_program(
        &self,
        program: &mut Program,
        symtable: &mut SymTable,
        func: Option<&Function>,
        bb: Option<&BasicBlock>,
    ) -> Result<(), String>;
}

impl KoopaAST for BlockItem {
    fn add_to_program(
        &self,
        program: &mut Program,
        symtable: &mut SymTable,
        func: Option<&Function>,
        bb: Option<&BasicBlock>,
    ) -> Result<(), String> {
        let mut instr_stack: Vec<Value> = vec![];
        match self {
            BlockItem::Decl(symbols) => {
                for sym in symbols.iter() {
                    match &sym.value {
                        SymbolValue::Const(expr) => {
                            // Constants in the SysY language must be determined during compilation.
                            // Just replace them with values recorded in the symbol table.
                            // No need to add additional IR instructions.
                            symtable.insert(
                                sym.name.clone(),
                                SymEntry::Const(
                                    expr.reduce(program.func_mut(*func.unwrap()).dfg(), &symtable),
                                ),
                            )?;
                        }
                        SymbolValue::Var(init) => {
                            // Allocate variable and set its name, if it has never been declared.
                            let v: Value;
                            v = program
                                .func_mut(*func.unwrap())
                                .dfg_mut()
                                .new_value()
                                .alloc(Type::get_i32());
                            program
                                .func_mut(*func.unwrap())
                                .dfg_mut()
                                .set_value_name(v, Some(format!("@{}", &sym.name)));
                            instr_stack.push(v);
                            symtable.insert(sym.name.clone(), SymEntry::Var(v))?;

                            // Initialize the variable if given.
                            if let Some(init_value) = init {
                                let iv = init_value.unroll(
                                    program.func_mut(*func.unwrap()).dfg_mut(),
                                    &mut instr_stack,
                                    &symtable,
                                );
                                instr_stack.push(
                                    program
                                        .func_mut(*func.unwrap())
                                        .dfg_mut()
                                        .new_value()
                                        .store(iv, v.clone()),
                                );
                            }
                        }
                    };
                }
            }
            BlockItem::Assign(name, expr) => {
                let v = symtable.get(name).unwrap();
                match v {
                    SymEntry::Var(v) => {
                        let exprv = expr.unroll(
                            program.func_mut(*func.unwrap()).dfg_mut(),
                            &mut instr_stack,
                            &symtable,
                        );
                        instr_stack.push(
                            program
                                .func_mut(*func.unwrap())
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
                    program.func_mut(*func.unwrap()).dfg_mut(),
                    &mut instr_stack,
                    &symtable,
                );
                // Return the final value.
                instr_stack.push(
                    program
                        .func_mut(*func.unwrap())
                        .dfg_mut()
                        .new_value()
                        .ret(Some(retv)),
                );
            }
            BlockItem::Block(block) => {
                symtable.fork();
                block.add_to_program(program, symtable, func, bb)?;
                symtable.join()?;
            }
            BlockItem::Expr(_) => {
                // Do nothing.
            }
        }
        // Record all instructions.
        program
            .func_mut(*func.unwrap())
            .layout_mut()
            .bb_mut(*bb.unwrap())
            .insts_mut()
            .extend(instr_stack);
        Ok(())
    }
}

impl KoopaAST for Block {
    fn add_to_program(
        &self,
        program: &mut Program,
        symtable: &mut SymTable,
        func: Option<&Function>,
        bb: Option<&BasicBlock>,
    ) -> Result<(), String> {
        for item in self.items.iter() {
            item.add_to_program(program, symtable, func, bb)?;
        }
        Ok(())
    }
}
impl KoopaAST for FuncDef {
    fn add_to_program(
        &self,
        program: &mut Program,
        symtable: &mut SymTable,
        _: Option<&Function>,
        _: Option<&BasicBlock>,
    ) -> Result<(), String> {
        // TODO: Only the main function is supported.
        let function = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident),
            vec![],
            match self.type_ {
                FuncType::Int => Type::get_i32(),
            },
        ));
        let func_data = program.func_mut(function);
        let entry = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);
        self.block
            .add_to_program(program, symtable, Some(&function), Some(&entry))?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum SymEntry {
    Const(i32),
    Var(Value),
}
pub struct SymTable {
    parent: Vec<Rc<RefCell<HashMap<String, SymEntry>>>>,
    table: Rc<RefCell<HashMap<String, SymEntry>>>,
}

impl SymTable {
    fn new() -> Self {
        SymTable {
            parent: vec![],
            table: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    fn fork(&mut self) {
        self.parent.push(self.table.clone());
        self.table = Rc::new(RefCell::new(HashMap::new()));
    }

    fn join(&mut self) -> Result<(), String> {
        self.table = self
            .parent
            .pop()
            .ok_or_else(|| "Cannot join the root symbol table".to_string())?;
        Ok(())
    }

    fn get(&self, name: &String) -> Result<SymEntry, String> {
        for tab in iter::once(&self.table).chain(self.parent.iter().rev()) {
            if let Some(v) = tab.borrow().get(name) {
                return Ok(v.clone());
            }
        }
        Err(format!("Undefined symbol: {}", name))
    }

    fn insert(&mut self, k: String, v: SymEntry) -> Result<(), String> {
        if self.table.borrow().contains_key(&k) {
            return Err(format!("Symbol {} cannot be defined twice", k));
        }
        self.table.borrow_mut().insert(k, v);
        Ok(())
    }
}

pub fn build_program(comp_unit: &CompUnit) -> Result<Program, String> {
    let mut program: Program = Program::new();
    let mut symtable = SymTable::new();
    comp_unit
        .func_def
        .add_to_program(&mut program, &mut symtable, None, None)?;
    Ok(program)
}
