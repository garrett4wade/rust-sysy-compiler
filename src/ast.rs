use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{BinaryOp, Type, Value};

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
    pub fn decl_symbol(
        &self,
        dfg: &mut DataFlowGraph,
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
                        SymEntry::Const(expr.reduce(dfg, &symtable)),
                    )
                    .unwrap();
            }
            SymbolValue::Var(init) => {
                // Allocate variable and set its name, if it has never been declared.
                let v: Value;
                v = dfg.new_value().alloc(Type::get_i32());
                dfg.set_value_name(v, Some(format!("@{}", &self.name)));
                instr_stack.push(v);
                symtable
                    .insert(self.name.clone(), SymEntry::Var(v))
                    .unwrap();

                // Initialize the variable if given.
                if let Some(init_value) = init {
                    let iv = init_value.unroll(dfg, instr_stack, &symtable);
                    instr_stack.push(dfg.new_value().store(iv, v.clone()));
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
    pub fn unroll(
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
