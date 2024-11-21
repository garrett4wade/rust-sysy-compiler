use koopa::ir::{BinaryOp, Type};

use crate::symtable::{SymEntry, SymTable};
// AST definition
#[derive(Debug)]
pub struct CompUnit {
    pub defs: Vec<CompUnitDecl>,
}
#[derive(Debug)]
pub enum CompUnitDecl {
    FuncDef {
        type_: SysYType,
        ident: String,
        params: Vec<FuncParam>,
        block: Block,
    },
    VarDecl(Vec<Symbol>),
}

#[derive(Debug, Clone)]
pub enum FuncParam {
    Var(SysYType, String),
    Arr(SysYType, String, Vec<Box<Expr>>),
}

impl FuncParam {
    pub fn name(&self) -> &str {
        match self {
            FuncParam::Var(_, name) => name,
            FuncParam::Arr(_, name, _) => name,
        }
    }

    pub fn ty(&self, symtable: &mut SymTable) -> Type {
        match self {
            FuncParam::Var(ty, _) => ty.into(),
            FuncParam::Arr(ty, _, dims) => {
                Type::get_pointer(dims.iter().rev().fold(ty.into(), |acc, d| {
                    Type::get_array(acc, d.reduce(symtable).try_into().unwrap())
                }))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SysYType {
    Int,
    Void,
}

// Type conversion.
impl From<&SysYType> for Type {
    fn from(ty: &SysYType) -> Type {
        match ty {
            SysYType::Int => Type::get_i32(),
            SysYType::Void => Type::get_unit(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub enum LVal {
    Ident(String),
    ArrayElem(String, Vec<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Decl(Vec<Symbol>),
    Assign(LVal, Box<Expr>),
    Ret(Option<Box<Expr>>),
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
    Symbol(LVal),
    Number(i32),
    FuncCall {
        funcname: String,
        args: Vec<Box<Expr>>,
    },
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
    ConstArr {
        lens: Vec<Box<Expr>>,
        init: InitListElem,
    },
    Arr {
        lens: Vec<Box<Expr>>,
        init: Option<InitListElem>,
    },
}

#[derive(Debug, Clone)]
pub enum InitListElem {
    Item(Box<Expr>),
    List(Vec<InitListElem>),
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
    pub fn reduce(&self, symtable: &SymTable) -> i32 {
        match self {
            Expr::Number(n) => *n,
            Expr::Unary(op, sub_expr) => match op {
                OpCode::Sub => -sub_expr.reduce(symtable),
                OpCode::Not => {
                    let v = sub_expr.reduce(symtable);
                    if v == 0 {
                        1
                    } else {
                        0
                    }
                }
                _ => panic!("Unsupported unary operator: {:?}", op),
            },
            Expr::Binary(lhs, op, rhs) => {
                let l = lhs.reduce(symtable);
                let r = rhs.reduce(symtable);
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
            Expr::Symbol(lval) => match lval {
                LVal::Ident(name) => {
                    let symv = symtable.get(name).unwrap();
                    match symv {
                        SymEntry::Const(v) => v.clone(),
                        _ => panic!(
                            "Cannot reduce an expression at compilation with a variable: {}",
                            name
                        ),
                    }
                }
                LVal::ArrayElem(..) => {
                    panic!("Evaluating an array element at compliation is not supported.")
                }
            },
            Expr::FuncCall { .. } => panic!("Cannot reduce a function."),
        }
    }
}
