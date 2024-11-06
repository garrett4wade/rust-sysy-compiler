use std::collections::HashMap;

// use crate::reg::RegAllocator;
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{entities::ValueData, BinaryOp, Program, TypeKind, Value, ValueKind};

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
    arg_cnt: usize,
    vars: HashMap<Value, usize>,
}

impl StackFrame {
    fn new(size: usize, arg_offset: usize) -> Self {
        assert!(size >= arg_offset);
        StackFrame {
            size: size,
            sp: arg_offset,
            arg_cnt: 0,
            vars: HashMap::new(),
        }
    }

    fn step_arg(&mut self) {
        self.arg_cnt += 1;
    }

    fn allocate(&mut self, dfg: &DataFlowGraph, value: Value) -> Result<usize, String> {
        let value_data = dfg.value(value);
        if value_data.ty().is_unit() {
            // This instruction does not have a return value. Do nothing.
            return Err("non-return type can't be allocated on the stack".to_string());
        }
        let size = value_data.ty().size();
        let pos = self.sp;
        if let ValueKind::Alloc(_) = value_data.kind() {
            self.sp += ptr_data_size(value_data);
        } else {
            self.sp += size;
        }
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

fn ptr_data_size(data: &ValueData) -> usize {
    // Alloc will produce a pointer type,
    // but we need to count its allocated size.
    if let TypeKind::Pointer(p) = data.ty().kind() {
        return p.size();
    } else {
        panic!("Alloc should produce a pointer type");
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
    program: &Program,
    dfg: &DataFlowGraph,
    value: &Value,
    stack_frame: &mut StackFrame,
    instrs: &mut Vec<String>,
) {
    let vd = dfg.value(value.clone());
    match vd.kind() {
        ValueKind::Call(call) => {
            // Arguments
            call.args().iter().enumerate().for_each(|(i, arg)| {
                if i < 8 {
                    let reg = format!("a{}", i);
                    load_var_or_const(dfg, arg, stack_frame, instrs, &reg);
                } else {
                    load_var_or_const(dfg, arg, stack_frame, instrs, &"t0".to_string());
                    instrs.push(format!("sw {}, {}(sp)", "t0", (i - 8) * 4,));
                }
            });
            // Call
            let func_data = program.func(call.callee());
            let callee_name = func_data.name()[1..].to_string();
            instrs.push(format!("call {}", callee_name));
            // Return
            if !vd.ty().is_unit() {
                let sp = stack_frame.allocate(dfg, value.clone()).unwrap();
                instrs.push(format!("sw a0, {}(sp)", sp));
            }
        }
        ValueKind::Alloc(_) => {
            // "alloc" does not correspond to any RISC-V instruction.
            stack_frame.allocate(dfg, value.clone()).unwrap();
        }
        ValueKind::Store(s) => {
            let vname = dfg.value(s.value()).name().clone();
            let dname;
            let is_global_var;
            if program.borrow_values().contains_key(&s.dest()) {
                dname = program.borrow_value(s.dest()).name().clone();
                is_global_var = true;
            } else {
                dname = dfg.value(s.dest()).name().clone();
                is_global_var = false;
            }
            if vname.is_some()
                && dname.is_some()
                && vname.as_ref().unwrap()[0..1] != dname.as_ref().unwrap()[0..1]
                && vname.unwrap()[1..] == dname.unwrap()[1..]
            {
                // Store function parameters.
                if stack_frame.arg_cnt < 8 {
                    let reg = format!("a{}", stack_frame.arg_cnt);
                    instrs.push(format!(
                        "sw {}, {}(sp)",
                        reg,
                        stack_frame.get_sp(&s.dest()).unwrap()
                    ));
                } else {
                    instrs.push(format!(
                        "lw t0, {}(sp)",
                        stack_frame.size + (stack_frame.arg_cnt - 8) * 4
                    ));
                    instrs.push(format!(
                        "sw t0, {}(sp)",
                        stack_frame.get_sp(&s.dest()).unwrap()
                    ));
                }
                stack_frame.step_arg();
            } else {
                load_var_or_const(dfg, &s.value(), stack_frame, instrs, &"t0".to_string());
                if is_global_var {
                    instrs.push(format!(
                        "la {}, {}",
                        "t1",
                        program.borrow_value(s.dest()).name().as_ref().unwrap()[1..].to_string()
                    ));
                    instrs.push("sw t0, 0(t1)".to_string());
                } else {
                    instrs.push(format!(
                        "sw {}, {}(sp)",
                        "t0",
                        stack_frame.get_sp(&s.dest()).unwrap()
                    ));
                }
            }
        }
        ValueKind::Load(l) => {
            if program.borrow_values().contains_key(&l.src()) {
                if let ValueKind::GlobalAlloc(_) = program.borrow_value(l.src().clone()).kind() {
                    instrs.push(format!(
                        "la {}, {}",
                        "t0",
                        program
                            .borrow_value(l.src().clone())
                            .name()
                            .as_ref()
                            .unwrap()[1..]
                            .to_string()
                    ));
                    instrs.push("lw t0, 0(t0)".to_string());
                }
            } else {
                instrs.push(format!(
                    "lw {}, {}(sp)",
                    "t0",
                    stack_frame.get_sp(&l.src()).unwrap()
                ));
            }
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

    // Global variables.
    for v in program.inst_layout().iter() {
        let vd = program.borrow_value(v.clone());
        if let ValueKind::GlobalAlloc(alloc) = vd.kind() {
            let mut seg: Vec<String> = vec![];
            let size = ptr_data_size(&vd);
            let name = vd.name().as_ref().unwrap()[1..].to_string();
            seg.push(format!("{}.data", INDENT));
            seg.push(format!("{}.globl {}", INDENT, name));
            seg.push(format!("{}:", name));
            let initv = alloc.init();
            if let ValueKind::Integer(i) = program.borrow_value(initv).kind() {
                if i.value() == 0 {
                    seg.push(format!("{}.zero {}", INDENT, size));
                } else {
                    seg.push(format!("{}.word {}", INDENT, i.value()));
                }
            } else {
                panic!("Global variable is not an integer.")
            }
            riscv.push(seg.join("\n"))
        } else {
            panic!("Only support GlobalAlloc in the global scope")
        }
    }

    // Functions.
    for (_, func_data) in program.funcs() {
        // Ignore library functions that only have a declaration.
        if func_data.layout().entry_bb().is_none() {
            continue;
        }

        let mut func_riscv: Vec<String> = vec![];

        // Function name. Remove the starting "@" or "%".
        func_riscv.push(format!("{}.text", INDENT));
        func_riscv.push(format!("{}.globl {}", INDENT, &func_data.name()[1..]));
        func_riscv.push(format!("{}:", &func_data.name()[1..]));

        // Get the global DFG handle.
        let dfg = func_data.dfg();

        // Prologue
        // needs to save all temporary variables
        let mut ss_var = 0i64;
        // needs to save $ra if there's any function calling
        let mut ss_ra = 0i64;
        // needs to save function arguments if #arguments > 8
        let mut ss_arg = 0i64;
        for (_, bb_node) in func_data.layout().bbs() {
            for (inst, _) in bb_node.insts() {
                let inst_data = dfg.value(inst.clone());
                match inst_data.kind() {
                    ValueKind::Call(call) => {
                        ss_ra = 4;
                        ss_arg = ss_arg.max((call.args().len() as i64 - 8) * 4);
                        ss_var += inst_data.ty().size() as i64;
                    }
                    ValueKind::Binary(..) | ValueKind::Load(..) => {
                        ss_var += inst_data.ty().size() as i64;
                    }
                    ValueKind::Alloc(_) => {
                        ss_var += ptr_data_size(inst_data) as i64;
                    }
                    ValueKind::Branch(..)
                    | ValueKind::Jump(..)
                    | ValueKind::Return(..)
                    | ValueKind::Store(..) => {}
                    _ => {
                        panic!("Unknown value kind: {:?}", inst_data.kind())
                    }
                }
            }
        }
        let mut ss = ss_var + ss_ra + ss_arg;
        ss = (ss + 15) / 16 * 16; // Round to multiple of 16.
        if ss > 0 {
            func_riscv.push(format!("{}addi sp, sp, -{}", INDENT, ss));
        }
        if ss_ra > 0 {
            func_riscv.push(format!("{}sw ra, {}(sp)", INDENT, ss - ss_ra));
        }

        // Instruction placeholders.
        let mut func_instrs: Vec<Vec<String>> = vec![];
        let mut func_bbs: Vec<String> = vec![];
        assert!(ss >= 0 && ss_ra >= 0 && ss_arg >= 0);
        let mut stack_frame = StackFrame::new(ss as usize, ss_arg as usize);

        // Basic blocks.
        for (bb, bb_node) in func_data.layout().bbs() {
            // BB name. Remove the starting "@" or "%".
            func_bbs.push(dfg.bb(bb.clone()).name().as_ref().unwrap()[1..].to_string());
            // BB instructions.
            // For now, re-allocate all temporary registers for each BB.
            // let mut regalloc = RegAllocator::new();
            let mut bb_instrs: Vec<String> = vec![];
            for (inst_v, _) in bb_node.insts() {
                generate_one_inst(&program, dfg, inst_v, &mut stack_frame, &mut bb_instrs);
                if matches!(dfg.value(inst_v.clone()).kind(), ValueKind::Return(_)) {
                    // Epilogue
                    // Recover $ra and the stack frame.
                    if ss_ra > 0 {
                        bb_instrs.push(format!("lw ra, {}(sp)", ss - ss_ra));
                    }
                    if ss > 0 {
                        bb_instrs.push(format!("addi sp, sp, {}", ss));
                    }
                    bb_instrs.push("ret".to_string());
                    break;
                }
            }
            func_instrs.push(bb_instrs);
        }

        // Organize BBs and push to the global program.
        for (i, (bb_instrs, bb_name)) in func_instrs.into_iter().zip(func_bbs).enumerate() {
            if i > 0 {
                func_riscv.push(format!("{}:", bb_name));
            }
            for instr in bb_instrs.into_iter() {
                func_riscv.push(format!("{}{}", INDENT, instr));
            }
        }
        riscv.push(func_riscv.join("\n"));
    }
    riscv.join("\n\n")
}
