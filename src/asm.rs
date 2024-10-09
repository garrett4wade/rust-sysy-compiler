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
static TMP_REGS: [&'static str; 7] = ["t0", "t1", "t2", "t3", "t4", "t5", "t6"];

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

fn resolve_binary_op_inst(
    value: &Value,
    op: &BinaryOp,
    lhs: &String,
    rhs: &String,
    regalloc: &mut RegAllocator,
    instrs: &mut Vec<String>,
) {
    match op {
        BinaryOp::Sub => {
            instrs.push(format!(
                "sub {}, {}, {}",
                regalloc.allocate(value.clone()),
                lhs,
                rhs
            ));
        }
        BinaryOp::Eq => {
            let mut out: &String = lhs;
            if out == "x0" {
                out = rhs;
            }
            instrs.push(format!("xor {}, {}, {}", out, lhs, rhs));
            instrs.push(format!("seqz {}, {}", out, out));
            regalloc.put(value, out);
        }
        _ => unreachable!("Not implemented"),
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
                    rhs_v = regalloc.allocate(b.lhs());
                    instrs.push(format!("li {}, {}", rhs_v, i.value()));
                }
            } else {
                rhs_v = regalloc.get(&b.rhs());
            }
            resolve_binary_op_inst(&value, &b.op(), &lhs_v, &rhs_v, regalloc, instrs);
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
