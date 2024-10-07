use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{Program, Value, ValueKind};

pub trait GenerateASM {
    fn to_riscv(self) -> String;
}

const INDENT: &str = "  ";

fn unwrap_value(dfg: &DataFlowGraph, value: &Value) -> String {
    let vd = dfg.value(value.clone());
    match vd.kind() {
        ValueKind::Return(r) => {
            let ret_v = unwrap_value(dfg, &r.value().unwrap());
            format!("{}li a0, {}\n{}ret\n", INDENT, ret_v, INDENT)
        }
        ValueKind::Integer(i) => {
            format!("{}", i.value())
        }
        _ => {
            panic!("Not implemented");
        }
    }
}

impl GenerateASM for Program {
    fn to_riscv(self) -> String {
        let mut result = String::new();
        result.push_str(&format!("{}.text\n", INDENT));
        result.push_str(&format!("{}.globl main\n", INDENT));
        for (_, func_data) in self.funcs() {
            // Get the global DFG handle.
            let dfg = func_data.dfg();

            // Function name.
            result.push_str(&format!("{}:\n", &func_data.name()[1..]));

            // Basic blocks.
            for (_, bb_node) in func_data.layout().bbs() {
                // BB instructions.
                for (inst_v, _) in bb_node.insts() {
                    result.push_str(&format!("{}\n", unwrap_value(dfg, inst_v)));
                }
            }
        }
        result
    }
}
