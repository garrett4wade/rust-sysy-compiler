use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::BinaryOp;

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
    While {
        cond: Box<Expr>,
        while_block: Box<BlockItem>,
    },
    Break,
    Continue,
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
