use std::collections::HashMap;

// use crate::reg::RegAllocator;
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{BinaryOp, Program, Value, ValueKind};

// pub trait GenerateASM {
//     fn to_riscv(self) -> String;
// }

pub trait DebugASM {
    fn print_debug_inst(&self, dfg: &DataFlowGraph);
}

static INDENT: &'static str = "  ";

impl DebugASM for Value {
    fn print_debug_inst(&self, dfg: &DataFlowGraph) {
        let vd = dfg.value(self.clone());
        println!("{:?}", vd);
        match vd.kind() {
            ValueKind::Return(r) => {
                let ret_v = r.value();
                if let Some(v) = ret_v {
                    println!("return {:?}", dfg.value(v.clone()));
                } else {
                    println!("return");
                }
            }
            ValueKind::Binary(b) => {
                println!(
                    "Binary operation: {:?} {:?} {:?}",
                    dfg.value(b.lhs()),
                    b.op(),
                    dfg.value(b.rhs())
                );
            }
            ValueKind::Integer(i) => {
                println!("Integer: {}", i.value());
            }
            _ => {
                panic!("Not implemented");
            }
        }
    }
}

fn binary_op_to_riscv_instr(op: &BinaryOp) -> &str {
    match op {
        BinaryOp::Add => "add",
        BinaryOp::Sub => "sub",
        BinaryOp::Mul => "mul",
        BinaryOp::Div => "div",
        BinaryOp::Mod => "rem",
        BinaryOp::Lt => "slt",
        BinaryOp::Gt => "sgt",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
        _ => panic!("Not implemented"),
    }
}
#[derive(Debug)]
struct StackFrame {
    size: usize,
    sp: usize,
    vars: HashMap<Value, usize>,
}

impl StackFrame {
    fn new(size: usize) -> Self {
        StackFrame {
            size: size,
            sp: 0,
            vars: HashMap::new(),
        }
    }

    fn allocate(&mut self, dfg: &DataFlowGraph, value: Value) -> Result<usize, String> {
        let value_data = dfg.value(value);
        if value_data.ty().is_unit() {
            // This instruction does not have a return value. Do nothing.
            return Err("non-return type can't be allocated on the stack".to_string());
        }
        let size = value_data.ty().size();
        let pos = self.sp;
        self.sp += size;
        if self.sp > self.size {
            return Err(format!(
                "Stack overflow on {:?}, size: {}, sp: {}",
                value_data, self.size, self.sp
            ));
        }
        if self.vars.contains_key(&value) {
            return Err(format!("Variable already exists: {:?}", value_data));
        }
        self.vars.insert(value, pos);
        Ok(pos)
    }

    fn get_sp(&self, value: &Value) -> Result<usize, String> {
        self.vars
            .get(value)
            .cloned()
            .ok_or_else(|| format!("Variable not found: {:?}", value))
    }
}

fn load_var_or_const(
    dfg: &DataFlowGraph,
    value: &Value,
    stack_frame: &StackFrame,
    instrs: &mut Vec<String>,
    reg: &String,
) {
    match dfg.value(value.clone()).kind() {
        ValueKind::Integer(i) => {
            instrs.push(format!("li {}, {}", reg, i.value()));
        }
        _ => {
            let sp = stack_frame.get_sp(value).unwrap();
            instrs.push(format!("lw {}, {}(sp)", reg, sp));
        }
    }
}
// Generate RISC-V ASM for one instruction, given the handle of this IR.
fn generate_one_inst(
    dfg: &DataFlowGraph,
    value: &Value,
    stack_frame: &mut StackFrame,
    instrs: &mut Vec<String>,
) {
    let vd = dfg.value(value.clone());
    match vd.kind() {
        ValueKind::Alloc(_) => {
            // "alloc" does not correspond to any RISC-V instruction.
            stack_frame.allocate(dfg, value.clone()).unwrap();
        }
        ValueKind::Store(s) => {
            load_var_or_const(dfg, &s.value(), stack_frame, instrs, &"t0".to_string());
            instrs.push(format!(
                "sw {}, {}(sp)",
                "t0",
                stack_frame.get_sp(&s.dest()).unwrap()
            ));
        }
        ValueKind::Load(l) => {
            instrs.push(format!(
                "lw {}, {}(sp)",
                "t0",
                stack_frame.get_sp(&l.src()).unwrap()
            ));
            let sp = stack_frame.allocate(dfg, value.clone()).unwrap();
            instrs.push(format!("sw {}, {}(sp)", "t0", sp));
        }
        ValueKind::Return(r) => {
            let ret_v = r.value();
            if let Some(v) = ret_v {
                load_var_or_const(dfg, &v, stack_frame, instrs, &"a0".to_string());
            }
        }
        ValueKind::Branch(b) => {
            let reg = "t0".to_string();
            load_var_or_const(dfg, &b.cond(), stack_frame, instrs, &reg);
            instrs.push(format!(
                "bnez {}, {}",
                reg,
                dfg.bb(b.true_bb()).name().as_ref().unwrap()[1..].to_string()
            ));
            instrs.push(format!(
                "j {}",
                dfg.bb(b.false_bb()).name().as_ref().unwrap()[1..].to_string()
            ));
        }
        ValueKind::Jump(j) => {
            instrs.push(format!(
                "j {}",
                dfg.bb(j.target()).name().as_ref().unwrap()[1..].to_string()
            ));
        }
        ValueKind::Binary(b) => {
            let lhs_v = "t0".to_string();
            let rhs_v = "t1".to_string();
            load_var_or_const(dfg, &b.lhs(), stack_frame, instrs, &lhs_v);
            load_var_or_const(dfg, &b.rhs(), stack_frame, instrs, &rhs_v);

            let regout: String = "t0".to_string();

            match b.op() {
                BinaryOp::Add
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::Mul
                | BinaryOp::Sub
                | BinaryOp::Gt
                | BinaryOp::Lt
                | BinaryOp::And
                | BinaryOp::Or => {
                    instrs.push(format!(
                        "{} {}, {}, {}",
                        binary_op_to_riscv_instr(&b.op()),
                        regout,
                        lhs_v,
                        rhs_v,
                    ));
                }
                BinaryOp::Ge | BinaryOp::Le => {
                    let ins: &str;
                    if matches!(b.op(), BinaryOp::Ge) {
                        ins = "slt";
                    } else {
                        ins = "sgt";
                    }
                    instrs.push(format!("{} {}, {}, {}", ins, regout, lhs_v, rhs_v,));
                    instrs.push(format!("seqz {}, {}", regout, regout));
                }
                BinaryOp::Eq | BinaryOp::NotEq => {
                    let ins: &str;
                    if matches!(b.op(), BinaryOp::Eq) {
                        ins = "seqz";
                    } else {
                        ins = "snez";
                    }
                    if lhs_v == "x0" && rhs_v == "x0" {
                        instrs.push(format!("li {}, 1", regout));
                    } else if lhs_v != "x0" && rhs_v != "x0" {
                        instrs.push(format!("xor {}, {}, {}", regout, lhs_v, rhs_v));
                        instrs.push(format!("{} {}, {}", ins, regout, regout));
                    } else if lhs_v == "x0" {
                        instrs.push(format!("{} {}, {}", ins, regout, rhs_v));
                    } else {
                        instrs.push(format!("{} {}, {}", ins, regout, lhs_v));
                    }
                }
                _ => unreachable!("Not implemented"),
            }

            let sp = stack_frame.allocate(dfg, value.clone()).unwrap();
            instrs.push(format!("sw {}, {}(sp)", regout, sp));
        }
        _ => {
            panic!("Not implemented");
        }
    }
}

pub fn build_riscv(program: &Program) -> String {
    let mut riscv: Vec<String> = vec![];
    riscv.push(format!("{}.text", INDENT));
    riscv.push(format!("{}.globl main", INDENT));
    for (_, func_data) in program.funcs() {
        // Function name. Remove the starting "@" or "%".
        riscv.push(format!("{}:", &func_data.name()[1..]));
        // Get the global DFG handle.
        let dfg = func_data.dfg();

        // prologue
        let mut ss = 0usize;
        for (_, bb_node) in func_data.layout().bbs() {
            for (inst, _) in bb_node.insts() {
                let t = dfg.value(inst.clone()).ty();
                let s = t.size();
                ss += s * (1 - t.is_unit() as usize);
            }
        }
        ss = (ss + 15) / 16 * 16; // Round to multiple of 16.
        if ss > 0 {
            riscv.push(format!("{}addi sp, sp, -{}", INDENT, ss));
        }

        // Instruction placeholders.
        let mut func_instrs: Vec<Vec<String>> = vec![];
        let mut func_bbs: Vec<String> = vec![];
        let mut stack_frame = StackFrame::new(ss);

        // Basic blocks.
        for (bb, bb_node) in func_data.layout().bbs() {
            // BB name. Remove the starting "@" or "%".
            func_bbs.push(dfg.bb(bb.clone()).name().as_ref().unwrap()[1..].to_string());
            // BB instructions.
            // For now, re-allocate all temporary registers for each BB.
            // let mut regalloc = RegAllocator::new();
            let mut bb_instrs: Vec<String> = vec![];
            for (inst_v, _) in bb_node.insts() {
                generate_one_inst(dfg, inst_v, &mut stack_frame, &mut bb_instrs);
                if matches!(dfg.value(inst_v.clone()).kind(), ValueKind::Return(_)) {
                    // epilogue
                    if ss > 0 {
                        bb_instrs.push(format!("addi sp, sp, {}", ss));
                    }
                    bb_instrs.push("ret".to_string());
                    break;
                }
            }
            func_instrs.push(bb_instrs);
        }

        for (i, (bb_instrs, bb_name)) in func_instrs.into_iter().zip(func_bbs).enumerate() {
            if i > 0 {
                riscv.push(format!("{}:", bb_name));
            }
            for instr in bb_instrs.into_iter() {
                riscv.push(format!("{}{}", INDENT, instr));
            }
        }
    }
    riscv.join("\n")
}
