use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};

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
    pub ret: Ret,
}

#[derive(Debug)]
pub struct Ret {
    pub retv: Box<Expr>,
}

#[derive(Debug)]
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

impl OpCode {
    fn to_koopa_op(&self) -> BinaryOp {
        match self {
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
            // special case
            OpCode::Not => BinaryOp::Eq,
            _ => panic!("Invalid operator: {:?}", self),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary(Box<Expr>, OpCode, Box<Expr>),
    Unary(OpCode, Box<Expr>),
    Number(i32),
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
    fn unroll(&self, dfg: &mut DataFlowGraph, stack: &mut Vec<Value>) -> Value {
        match self {
            Expr::Number(n) => dfg.new_value().integer(n.clone()),
            Expr::Unary(op, sub_expr) => match op {
                OpCode::Sub | OpCode::Not => {
                    let l = dfg.new_value().integer(0);
                    let r = sub_expr.unroll(dfg, stack);
                    let v = dfg.new_value().binary(op.to_koopa_op(), l, r);
                    stack.push(v);
                    v
                }
                OpCode::Add => sub_expr.unroll(dfg, stack),
                _ => panic!("Unsupported unary operator: {:?}", op),
            },
            Expr::Binary(lhs, op, rhs) => {
                let l = lhs.unroll(dfg, stack);
                let r = rhs.unroll(dfg, stack);
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
                        let v = dfg.new_value().binary(op.to_koopa_op(), l, r);
                        stack.push(v);
                        v
                    }
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
        func: Option<&Function>,
        bb: Option<&BasicBlock>,
    ) -> Result<(), String>;
}

impl KoopaAST for Ret {
    fn add_to_program(
        &self,
        program: &mut Program,
        func: Option<&Function>,
        bb: Option<&BasicBlock>,
    ) -> Result<(), String> {
        let func_data = program.func_mut(*func.unwrap());
        let mut instr_stack: Vec<Value> = vec![];

        // Create instructions with recursion.
        let retv = self.retv.unroll(func_data.dfg_mut(), &mut instr_stack);
        // Return the final value.
        instr_stack.push(func_data.dfg_mut().new_value().ret(Some(retv)));

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
            .ret
            .add_to_program(program, Some(&function), Some(&entry))?;
        Ok(())
    }
}

pub fn build_program(comp_unit: &CompUnit) -> Result<Program, String> {
    let mut program: Program = Program::new();
    comp_unit
        .func_def
        .add_to_program(&mut program, None, None)?;
    Ok(program)
}
