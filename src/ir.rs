use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{Program, Value, ValueKind};

pub trait DumpableKoopa {
    fn to_str(self) -> String;
}

fn unwrap_koopa_value(dfg: &DataFlowGraph, value: &Value) -> String {
    let vd = dfg.value(value.clone());
    match vd.kind() {
        ValueKind::Return(r) => {
            let ret_v = unwrap_koopa_value(dfg, &r.value().unwrap());
            format!("ret {}", ret_v)
        }
        ValueKind::Integer(i) => {
            format!("{}", i.value())
        }
        _ => {
            panic!("Not implemented");
        }
    }
}

impl DumpableKoopa for Program {
    fn to_str(self) -> String {
        let mut result = String::new();
        for (_, func_data) in self.funcs() {
            // Get the global DFG handle.
            let dfg = func_data.dfg();

            // Function signature.
            result.push_str(&format!("fun {}", func_data.name()));
            for (i, param) in func_data.params().iter().enumerate() {
                if i != 0 {
                    result.push_str(", ");
                }
                result.push_str(&format!("{:?}", param));
            }
            result.push_str(&format!("{:?} {{\n", func_data.ty()));

            // Basic blocks.
            for (bb, bb_node) in func_data.layout().bbs() {
                // BB name.
                result.push_str(&format!(
                    "{}:\n",
                    dfg.bb(bb.clone()).name().as_ref().unwrap()
                ));
                // BB instructions.
                for (inst_v, _) in bb_node.insts() {
                    result.push_str(&format!("    {}\n", unwrap_koopa_value(dfg, inst_v)));
                }
            }

            // End of function.
            result.push_str("}\n");
        }
        result
    }
}
