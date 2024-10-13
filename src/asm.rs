use crate::reg::RegAllocator;
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

// Generate RISC-V ASM for one instruction, given the handle of this IR.
fn generate_one_inst(
    dfg: &DataFlowGraph,
    value: &Value,
    regalloc: &mut RegAllocator,
    instrs: &mut Vec<String>,
) {
    let vd = dfg.value(value.clone());
    match vd.kind() {
        ValueKind::Return(r) => {
            let ret_v = r.value();
            if let Some(v) = ret_v {
                match dfg.value(v).kind() {
                    ValueKind::Integer(i) => {
                        if i.value() != 0 {
                            instrs.push(format!("li a0, {}", i.value()));
                        } else {
                            instrs.push("mv a0, x0".to_string());
                        }
                    }
                    _ => {
                        let reg = regalloc.get(&v, dfg).unwrap();
                        if reg != "a0" {
                            instrs.push(format!("mv a0, {}", reg));
                        }
                    }
                }
            }
            instrs.push("ret".to_string());
        }
        ValueKind::Binary(b) => {
            // Load or reate registers for operands.
            // We have to judge whether the operand of a Koopa instruction is an integer
            // because interger operands is allowed in koopa but not in RISC-V.
            // We must manually add additional "li" instructions.
            let lhs_v: String;
            if matches!(dfg.value(b.lhs()).kind(), ValueKind::Integer(i) if i.value() != 0) {
                lhs_v = regalloc.allocate(b.lhs(), dfg);
                if let ValueKind::Integer(i) = dfg.value(b.lhs()).kind() {
                    instrs.push(format!("li {}, {}", lhs_v, i.value()));
                }
            } else {
                lhs_v = regalloc.get(&b.lhs(), dfg).unwrap();
            }
            let rhs_v: String;
            if matches!(dfg.value(b.rhs()).kind(), ValueKind::Integer(i) if i.value() != 0) {
                rhs_v = regalloc.allocate(b.rhs(), dfg);
                if let ValueKind::Integer(i) = dfg.value(b.rhs()).kind() {
                    instrs.push(format!("li {}, {}", rhs_v, i.value()));
                }
            } else {
                rhs_v = regalloc.get(&b.rhs(), dfg).unwrap();
            }

            // Allocate output register.
            let regout: String = regalloc.allocate(value.clone(), dfg);

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
        let mut func_instrs: Vec<String> = vec![];

        // Get the global DFG handle.
        let dfg = func_data.dfg();

        // Basic blocks.
        for (_, bb_node) in func_data.layout().bbs() {
            // BB instructions.
            // For now, re-allocate all temporary registers for each BB.
            let mut regalloc = RegAllocator::new();
            let mut bb_instrs: Vec<String> = vec![];
            for (inst_v, _) in bb_node.insts() {
                generate_one_inst(dfg, inst_v, &mut regalloc, &mut bb_instrs);
            }
            func_instrs.extend(bb_instrs);
        }

        // Function name. Remove the starting "@" or "%".
        riscv.push(format!("{}:", &func_data.name()[1..]));
        for instr in func_instrs.into_iter() {
            riscv.push(format!("{}{}", INDENT, instr));
        }
    }
    riscv.join("\n")
}
