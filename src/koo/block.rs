use std::sync::atomic::{AtomicUsize, Ordering};

use crate::koo::ctx::{KoopaContext, KoopaLocalContext};
use crate::koo::expr::KoopaExpr;
use crate::koo::traits::{KoopaAssignment, KoopaLocalDeclaration, KoopaLocalInit};

// Simple instrucions.
pub enum KoopaInstr {
    LocalDecl(Box<dyn KoopaLocalDeclaration>),
    LocalInit(Box<dyn KoopaLocalInit>),
    Assign(Box<dyn KoopaAssignment>, Box<KoopaExpr>),
    Ret(Option<Box<KoopaExpr>>),
    Expr(Box<KoopaExpr>),
}

impl KoopaInstr {
    fn implement(&self, ctx: &mut KoopaLocalContext) {
        match self {
            KoopaInstr::LocalDecl(v) => v.local_decl(ctx),
            KoopaInstr::LocalInit(v) => v.local_init(ctx),
            KoopaInstr::Assign(a, e) => {
                let new_v = e.unroll(ctx);
                a.assign(ctx, new_v);
            }
            KoopaInstr::Ret(e) => {
                let retv = e.as_ref().map(|e| e.unroll(ctx));
                let ret = ctx.ret(retv);
                ctx.new_instr(ret);
            }
            KoopaInstr::Expr(e) => {
                let v = e.unroll(ctx);
                ctx.new_instr(v);
            }
        }
    }
}

// Control flow
pub enum KoopaControlFlow {
    If {
        cond: Box<KoopaExpr>,
        then_block: Box<KoopaBlock>,
        else_block: Option<Box<KoopaBlock>>,
    },
    While {
        cond: Box<KoopaExpr>,
        while_block: Box<KoopaBlock>,
    },
    Break,
    Continue,
}

impl KoopaControlFlow {
    fn implement(&self, ctx: &mut KoopaLocalContext) {
        match self {
            KoopaControlFlow::If {
                cond,
                then_block,
                else_block,
            } => {
                static IF_COUNTER: AtomicUsize = AtomicUsize::new(0);
                let if_cnt = IF_COUNTER.fetch_add(1, Ordering::Relaxed);

                // Create basic blocks for the if-else statement.
                let then_bb = ctx.new_bb(&format!("%if{}", if_cnt));
                let else_bb = ctx.new_bb(&format!("%if{}else", if_cnt));
                let end_bb = ctx.new_bb(&format!("%fi{}", if_cnt));

                // Create the branching instruction and complete the current basic block.
                let condv = cond.unroll(ctx);
                let branchv = ctx.branch(condv, then_bb, else_bb);
                ctx.new_instr(branchv);

                // Handle the "then" block.
                ctx.switch(then_bb);
                then_block.implement(ctx);
                let jump_v = ctx.jump(end_bb);
                ctx.new_instr(jump_v);
                ctx.shrink_instrs();

                // Handle the "else" block.
                // If there's no "else", create an empty block for the jump instruction.
                ctx.switch(else_bb);
                if else_block.is_some() {
                    else_block.as_ref().unwrap().implement(ctx);
                }
                let jump_v = ctx.jump(end_bb);
                ctx.new_instr(jump_v);
                ctx.shrink_instrs();

                // Finally, change the current basic block to "end_bb".
                ctx.switch(end_bb);
            }
            KoopaControlFlow::While { cond, while_block } => {
                static WHILE_COUNTER: AtomicUsize = AtomicUsize::new(0);
                let while_cnt = WHILE_COUNTER.fetch_add(1, Ordering::Relaxed);

                // Create basic blocks for the while statement.
                let cond_bb = ctx.new_bb(&format!("%while{}entry", while_cnt));
                let body_bb = ctx.new_bb(&format!("%while{}body", while_cnt));
                let end_bb = ctx.new_bb(&format!("%while{}fi", while_cnt));

                // Create the jump instruction and complete the current basic block.
                let jp = ctx.jump(cond_bb);
                ctx.new_instr(jp);

                // Handle the "cond" block.
                ctx.switch(cond_bb);
                let condv = cond.unroll(ctx);
                let branchv = ctx.branch(condv, body_bb, end_bb);
                ctx.new_instr(branchv);

                // Handle the "body" block.
                // Wrap it as a block and call Block::add_to_bb recursively.
                ctx.switch(body_bb);
                ctx.set_break_dst(end_bb);
                ctx.set_conti_dst(cond_bb);
                while_block.implement(ctx);
                let jump_v = ctx.jump(cond_bb);
                ctx.new_instr(jump_v);
                ctx.shrink_instrs();

                // Finally, change the current basic block to "end_bb".
                ctx.switch(end_bb);
            }
            KoopaControlFlow::Break => ctx.break_(),
            KoopaControlFlow::Continue => ctx.continue_(),
        }
    }
}

// Block
pub enum KoopaBlockItem {
    Instr(KoopaInstr),
    Control(KoopaControlFlow),
    Block(Box<KoopaBlock>),
}

pub struct KoopaBlock {
    pub items: Vec<KoopaBlockItem>,
}

impl KoopaBlock {
    pub fn implement(&self, ctx: &mut KoopaLocalContext) {
        for item in self.items.iter() {
            use KoopaBlockItem::*;
            match item {
                Instr(ins) => ins.implement(ctx),
                Control(cf) => cf.implement(ctx),
                Block(block) => {
                    ctx.symtable.fork();
                    block.implement(ctx);
                    ctx.symtable.join().unwrap();
                }
            }
            if ctx.returned() {
                break;
            }
        }
    }
}
