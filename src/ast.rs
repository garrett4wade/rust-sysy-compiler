use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};

use std::collections::HashMap;

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
        let func_data = program.func_mut(*func.unwrap());
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
                                SymEntry::Const(expr.reduce(func_data.dfg(), &symtable)),
                            )?;
                        }
                        SymbolValue::Var(init) => {
                            // Allocate variable and set its name, if it has never been declared.
                            let v: Value;
                            if symtable.get(&sym.name).is_err() {
                                v = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                                func_data
                                    .dfg_mut()
                                    .set_value_name(v, Some(format!("@{}", &sym.name)));
                                instr_stack.push(v);
                                symtable.insert(sym.name.clone(), SymEntry::Var(v))?;
                            } else {
                                if let SymEntry::Var(vv) = symtable.get(&sym.name).unwrap() {
                                    v = vv.clone();
                                } else {
                                    panic!("Cannot declare a variable with the same name as a constant: {}", sym.name);
                                }
                            }

                            // Initialize the variable if given.
                            if let Some(init_value) = init {
                                let iv = init_value.unroll(
                                    func_data.dfg_mut(),
                                    &mut instr_stack,
                                    &symtable,
                                );
                                instr_stack
                                    .push(func_data.dfg_mut().new_value().store(iv, v.clone()));
                            }
                        }
                    };
                }
            }
            BlockItem::Assign(name, expr) => {
                let v = symtable.get(name).unwrap();
                match v {
                    SymEntry::Var(v) => {
                        let exprv = expr.unroll(func_data.dfg_mut(), &mut instr_stack, &symtable);
                        instr_stack.push(func_data.dfg_mut().new_value().store(exprv, v.clone()));
                    }
                    _ => panic!("Cannot assign to a constant: {}", name),
                }
            }
            BlockItem::Ret(expr) => {
                // Create instructions with recursion.
                let retv = expr.unroll(func_data.dfg_mut(), &mut instr_stack, &symtable);
                // Return the final value.
                instr_stack.push(func_data.dfg_mut().new_value().ret(Some(retv)));
            }
        }
        // Record all instructions.
        func_data
            .layout_mut()
            .bb_mut(*bb.unwrap())
            .insts_mut()
            .extend(instr_stack);
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
        for item in self.block.items.iter() {
            item.add_to_program(program, symtable, Some(&function), Some(&entry))?;
        }
        Ok(())
    }
}

enum SymEntry {
    Const(i32),
    Var(Value),
}
pub struct SymTable {
    table: HashMap<String, SymEntry>,
}

impl SymTable {
    fn new() -> Self {
        SymTable {
            table: HashMap::new(),
        }
    }

    fn get(&self, name: &String) -> Result<&SymEntry, String> {
        self.table
            .get(name)
            .ok_or_else(|| format!("Undefined symbol: {}", name))
    }

    fn insert(&mut self, k: String, v: SymEntry) -> Result<(), String> {
        match v {
            SymEntry::Const(_) => {
                if self.table.contains_key(&k) {
                    return Err(format!("Const symbol {} cannot be defined twice", k));
                }
                self.table.insert(k, v);
                Ok(())
            }
            SymEntry::Var(_) => {
                self.table.insert(k, v);
                Ok(())
            }
        }
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
