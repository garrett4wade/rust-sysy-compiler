use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{entities::ValueData, Value, ValueKind};
use std::collections::{BTreeSet, HashMap, HashSet};

static TMP_REGS: [&'static str; 14] = [
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6",
];

pub struct RegAllocator {
    slots: BTreeSet<usize>,
    regidx: HashMap<Value, usize>,
    used_by: HashMap<Value, HashSet<Value>>,
}

impl RegAllocator {
    pub fn new() -> Self {
        let mut x = RegAllocator {
            slots: BTreeSet::new(),
            regidx: HashMap::new(),
            used_by: HashMap::new(),
        };
        for i in 0..14 {
            x.slots.insert(i);
        }
        x
    }

    // Allocate a new register for a value.
    pub fn allocate(&mut self, value: Value, dfg: &DataFlowGraph) -> String {
        let idx: usize;
        let data = dfg.value(value.clone());
        match data.kind() {
            ValueKind::Integer(i) => {
                if i.value() == 0 {
                    return "x0".to_string();
                }
                idx = self.allocate_idx(value, data);
            }
            ValueKind::Binary(b) => {
                self.use_value(b.lhs(), dfg.value(b.lhs()), value.clone());
                self.use_value(b.rhs(), dfg.value(b.rhs()), value.clone());
                idx = self.allocate_idx(value, data);
            }
            _ => {
                panic!("Unsupported value kind: {:?}", data.kind());
            }
        }
        TMP_REGS[idx].to_string()
    }

    fn allocate_idx(&mut self, value: Value, data: &ValueData) -> usize {
        let idx = self.slots.iter().next().cloned().unwrap();
        self.slots.remove(&idx);
        self.regidx.insert(value.clone(), idx);
        self.used_by.insert(value.clone(), data.used_by().clone());
        idx
    }

    fn use_value(&mut self, value: Value, data: &ValueData, user: Value) {
        if matches!(data.kind(), ValueKind::Integer(i) if i.value() == 0) {
            return;
        }
        assert!(self.regidx.contains_key(&value));
        self.used_by.get_mut(&value).unwrap().remove(&user);
        let vub = self.used_by.get(&value).unwrap();
        if vub.len() == 0 {
            self.used_by.remove(&value);
            let idx = self.regidx.get(&value).cloned().unwrap();
            self.regidx.remove(&value);
            self.slots.insert(idx);
        }
    }

    pub fn get(&self, value: &Value, dfg: &DataFlowGraph) -> Result<String, String> {
        if matches!(dfg.value(value.clone()).kind(), ValueKind::Integer(i) if i.value() == 0) {
            return Ok("x0".to_string());
        }
        let idx = self.regidx.get(value).cloned();
        idx.map(|i| TMP_REGS[i].to_string()).ok_or_else(|| {
            format!(
                "{:?} does not exist in the register allocator",
                dfg.value(value.clone())
            )
        })
    }
}
