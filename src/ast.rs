use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
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
    Neg,
    Pos,
    Not,
}
#[derive(Debug)]
pub enum Expr {
    Unary(OpCode, Box<Expr>),
    Number(i32),
}

struct _KoopaBinaryInst {
    op: Option<BinaryOp>,
    lhs: Option<Value>,
    rhs: Option<Value>,
}

fn _unwrap_expr(stack: &mut Vec<_KoopaBinaryInst>, expr: &Expr, func_data: &mut FunctionData) {
    match expr {
        Expr::Number(n) => {
            let num = func_data.dfg_mut().new_value().integer(n.clone());
            stack.push(_KoopaBinaryInst {
                op: None,
                lhs: Some(num),
                rhs: None,
            })
        }
        Expr::Unary(op, sub_expr) => {
            match op {
                OpCode::Neg => {
                    let lhs = func_data.dfg_mut().new_value().integer(0);
                    stack.push(_KoopaBinaryInst {
                        op: Some(BinaryOp::Sub),
                        lhs: Some(lhs),
                        rhs: None,
                    });
                }
                OpCode::Pos => {}
                OpCode::Not => {
                    let lhs = func_data.dfg_mut().new_value().integer(0);
                    stack.push(_KoopaBinaryInst {
                        op: Some(BinaryOp::Eq),
                        lhs: Some(lhs),
                        rhs: None,
                    });
                }
            };
            _unwrap_expr(stack, sub_expr.as_ref(), func_data);
        }
    }
}

// AST conversion
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
        let mut instr_stack: Vec<_KoopaBinaryInst> = vec![];
        _unwrap_expr(&mut instr_stack, &self.retv, func_data);
        let mut var: Option<Value> = None;
        let mut instructions: Vec<Value> = vec![];
        for inst in instr_stack.iter().rev() {
            if let Some(op) = inst.op {
                let lhs = inst.lhs.unwrap();
                let new_var = func_data
                    .dfg_mut()
                    .new_value()
                    .binary(op, lhs, var.unwrap());
                var = Some(new_var);
                instructions.push(new_var);
            } else {
                // The first number before any operation.
                var = inst.lhs;
            }
        }
        instructions.push(func_data.dfg_mut().new_value().ret(var));
        func_data
            .layout_mut()
            .bb_mut(*bb.unwrap())
            .insts_mut()
            .extend(instructions);
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
        // Only the main function is supported.
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
    comp_unit.func_def.add_to_program(&mut program, None, None)?;
    Ok(program)
}
