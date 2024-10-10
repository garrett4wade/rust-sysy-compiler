use std::collections::HashMap;

use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{BinaryOp, Program, Value, ValueKind};

// pub trait GenerateASM {
//     fn to_riscv(self) -> String;
// }

pub trait DebugASM {
    fn print_debug_inst(&self, dfg: &DataFlowGraph);
}

static INDENT: &'static str = "  ";
static TMP_REGS: [&'static str; 14] = [
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6",
];

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

struct RegAllocator {
    offset: usize,
    regidx: HashMap<Value, usize>,
}

impl RegAllocator {
    fn new() -> Self {
        RegAllocator {
            offset: 0,
            regidx: HashMap::new(),
        }
    }

    fn allocate(&mut self, value: Value) -> String {
        let idx = self.offset;
        self.regidx.insert(value, idx);
        self.offset = self.offset + 1;
        TMP_REGS[idx].to_string()
    }

    fn get(&self, value: &Value) -> String {
        TMP_REGS[*self.regidx.get(value).unwrap()].to_string()
    }

    fn put(&mut self, value: &Value, reg: &String) {
        let idx = TMP_REGS.iter().position(|&x| x == reg).unwrap();
        self.regidx.insert(value.clone(), idx);
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
                if let ValueKind::Integer(i) = dfg.value(v.clone()).kind() {
                    if i.value() == 0 {
                        instrs.push("mv a0, x0".to_string());
                    } else {
                        instrs.push(format!("li a0, {}", i.value()));
                    }
                } else {
                    instrs.push(format!("mv a0, {}", regalloc.get(&v)));
                }
            }
            instrs.push("ret".to_string());
        }
        ValueKind::Binary(b) => {
            let lhs_kd = dfg.value(b.lhs()).kind();
            let rhs_kd = dfg.value(b.rhs()).kind();

            // load integers and create registers
            let lhs_v: String;
            if let ValueKind::Integer(i) = dfg.value(b.lhs()).kind() {
                if i.value() == 0 {
                    lhs_v = "x0".to_string();
                } else {
                    lhs_v = regalloc.allocate(b.lhs());
                    instrs.push(format!("li {}, {}", lhs_v, i.value()));
                }
            } else {
                lhs_v = regalloc.get(&b.lhs());
            }
            let rhs_v: String;
            if let ValueKind::Integer(i) = dfg.value(b.rhs()).kind() {
                if i.value() == 0 {
                    rhs_v = "x0".to_string();
                } else {
                    rhs_v = regalloc.allocate(b.rhs());
                    instrs.push(format!("li {}, {}", rhs_v, i.value()));
                }
            } else {
                rhs_v = regalloc.get(&b.rhs());
            }

            // decide the output register name
            let regout: String;
            if matches!(lhs_kd, ValueKind::Integer(_i) if _i.value() != 0) {
                regout = regalloc.get(&b.lhs());
                regalloc.put(value, &regout);
            } else if matches!(rhs_kd, ValueKind::Integer(_i) if _i.value() != 0) {
                regout = regalloc.get(&b.rhs());
                regalloc.put(value, &regout);
            } else {
                // both not integer or both zero
                regout = regalloc.allocate(value.clone());
            }

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
