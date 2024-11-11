use crate::symtable::{SymEntry, SymTable};
use koopa::ir::builder::{
    BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder,
};
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, Value, ValueKind};

pub trait KoopaContext {
    fn symtable(&self) -> &SymTable;
    fn symtable_mut(&mut self) -> &mut SymTable;
    fn set_value_name(&mut self, value: &Value, name: &String);
    fn new_instr(&mut self, value: Value) -> Value;
}

pub struct KoopaGlobalContext {
    pub program: Program,
    pub symtable: SymTable,
}

impl KoopaContext for KoopaGlobalContext {
    fn symtable(&self) -> &SymTable {
        &self.symtable
    }

    fn symtable_mut(&mut self) -> &mut SymTable {
        &mut self.symtable
    }

    fn set_value_name(&mut self, value: &Value, name: &String) {
        self.program
            .set_value_name(value.clone(), Some(name.clone()));
    }

    fn new_instr(&mut self, value: Value) -> Value {
        value
    }
}

impl KoopaGlobalContext {
    pub fn new() -> Self {
        KoopaGlobalContext {
            program: Program::new(),
            symtable: SymTable::new(),
        }
    }

    pub fn new_func(&mut self, func_data: FunctionData) {
        let name = func_data.name()[1..].to_string();
        let function = self.program.new_func(func_data);
        self.symtable
            .insert(name, SymEntry::Func(function))
            .unwrap();
    }

    pub fn global_alloc(&mut self, init_v: Value) -> Value {
        self.program.new_value().global_alloc(init_v)
    }

    pub fn zero_init(&mut self, ty: Type) -> Value {
        self.program.new_value().zero_init(ty)
    }

    pub fn integer(&mut self, n: i32) -> Value {
        self.program.new_value().integer(n)
    }

    pub fn aggregate(&mut self, values: Vec<Value>) -> Value {
        self.program.new_value().aggregate(values)
    }
}

pub struct KoopaLocalContext<'a> {
    pub program: &'a mut Program,
    pub symtable: &'a mut SymTable,
    pub func: Function,
    pub bb: BasicBlock,
    pub break_dst: Option<BasicBlock>,
    pub conti_dst: Option<BasicBlock>,
}

impl<'a> KoopaContext for KoopaLocalContext<'a> {
    fn symtable(&self) -> &SymTable {
        self.symtable
    }

    fn symtable_mut(&mut self) -> &mut SymTable {
        self.symtable
    }

    fn new_instr(&mut self, value: Value) -> Value {
        self.program
            .func_mut(self.func)
            .layout_mut()
            .bb_mut(self.bb)
            .insts_mut()
            .extend([value]);
        value
    }

    fn set_value_name(&mut self, value: &Value, name: &String) {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .set_value_name(value.clone(), Some(name.clone()));
    }

    
}

impl<'a> KoopaLocalContext<'a> {
    pub fn new(ctx: &'a mut KoopaGlobalContext, funcname: &String) -> Self {
        let func = ctx.symtable.get(funcname).unwrap().get_func();
        // Fork the symtable to insert function parameters in this scope.
        ctx.symtable.fork();

        // Create the entry bb.
        let bb = ctx
            .program
            .func_mut(func)
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        ctx.program
            .func_mut(func)
            .layout_mut()
            .bbs_mut()
            .extend([bb.clone()]);
        KoopaLocalContext {
            program: &mut ctx.program,
            symtable: &mut ctx.symtable,
            func,
            bb,
            break_dst: None,
            conti_dst: None,
        }
    }

    pub fn destroy(self) {
        // Destroy the symtable.
        self.symtable.join().unwrap();
    }

    // Check whether the basic block calls "return".
    pub fn returned(&'a self) -> bool {
        let insts = (&self.program)
            .func(self.func)
            .layout()
            .bbs()
            .node(&self.bb)
            .unwrap()
            .insts();
        insts
            .back_key()
            .map(|x| {
                matches!(
                    (&self.program)
                        .func(self.func)
                        .dfg()
                        .value(x.clone())
                        .kind(),
                    ValueKind::Return(_)
                )
            })
            .unwrap_or(false)
    }

    // Shrinks the basic block by eliminating instructions
    // after the first jump, branch, or return instruction.
    pub fn shrink_instrs(&mut self) {
        // TODO: find a more elegant method calling approach
        // Take instructions until the first jump, branch, or return instruction.
        let mut shrinked = self
            .program
            .func(self.func)
            .layout()
            .bbs()
            .node(&self.bb)
            .unwrap()
            .insts()
            .keys()
            .take_while(|&&v| {
                !matches!(
                    self.program.func(self.func).dfg().value(v.clone()).kind(),
                    ValueKind::Return(_) | ValueKind::Jump(_) | ValueKind::Branch(..)
                )
            })
            .copied()
            .collect::<Vec<Value>>();
        // If it has not reached the end, take the next instruction.
        if let Some(&ins) = self
            .program
            .func(self.func)
            .layout()
            .bbs()
            .node(&self.bb)
            .unwrap()
            .insts()
            .keys()
            .skip(shrinked.len())
            .next()
        {
            shrinked.push(ins);
        }
        self.program
            .func_mut(self.func)
            .layout_mut()
            .bb_mut(self.bb.clone())
            .insts_mut()
            .clear();
        self.program
            .func_mut(self.func)
            .layout_mut()
            .bb_mut(self.bb.clone())
            .insts_mut()
            .extend(shrinked);
    }

    // Basic block manipulation.
    pub fn new_bb(&mut self, name: &String) -> BasicBlock {
        let bb = self
            .program
            .func_mut(self.func)
            .dfg_mut()
            .new_bb()
            .basic_block(Some(name.clone()));
        self.program
            .func_mut(self.func)
            .layout_mut()
            .bbs_mut()
            .extend([bb]);
        bb
    }

    pub fn switch(&mut self, bb: BasicBlock) {
        self.bb = bb;
    }

    pub fn break_(&mut self) {
        self.break_dst
            .map(|d| {
                let jump_v = self.jump(d);
                self.new_instr(jump_v);
                self.break_dst = None;
            })
            .expect("No break destination");
    }

    pub fn continue_(&mut self) {
        self.conti_dst
            .map(|d| {
                let jump_v = self.jump(d);
                self.new_instr(jump_v);
                self.conti_dst = None;
            })
            .expect("No continue destination");
    }

    pub fn set_break_dst(&mut self, bb: BasicBlock) {
        self.break_dst = Some(bb);
    }

    pub fn set_conti_dst(&mut self, bb: BasicBlock) {
        self.conti_dst = Some(bb);
    }

    // Re-implemented instructions.
    pub fn alloc(&mut self, ty: Type) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .alloc(ty)
    }

    pub fn integer(&mut self, n: i32) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .integer(n)
    }

    pub fn binary(&mut self, op: koopa::ir::BinaryOp, lhs: Value, rhs: Value) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .binary(op, lhs, rhs)
    }

    pub fn jump(&mut self, bb: BasicBlock) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .jump(bb)
    }

    pub fn branch(&mut self, cond: Value, then_bb: BasicBlock, else_bb: BasicBlock) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .branch(cond, then_bb, else_bb)
    }

    pub fn load(&mut self, ptr: Value) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .load(ptr)
    }

    pub fn call(&mut self, func: Function, args: Vec<Value>) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .call(func, args)
    }

    pub fn ret(&mut self, value: Option<Value>) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .ret(value)
    }

    pub fn get_elem_ptr(&mut self, ptr: Value, idx: Value) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .get_elem_ptr(ptr, idx)
    }

    pub fn store(&mut self, value: Value, ptr: Value) -> Value {
        self.program
            .func_mut(self.func)
            .dfg_mut()
            .new_value()
            .store(value, ptr)
    }
}
