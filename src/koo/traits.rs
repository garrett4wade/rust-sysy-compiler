use crate::koo::ctx::{KoopaGlobalContext, KoopaLocalContext};
use koopa::ir::Value;

pub trait KoopaGlobalDeclaration {
    fn global_decl(&self, ctx: &mut KoopaGlobalContext);
}

pub trait KoopaLocalDeclaration {
    fn local_decl(&self, ctx: &mut KoopaLocalContext);
}

pub trait KoopaGlobalInit {
    fn global_init(&self, ctx: &mut KoopaGlobalContext);
}

pub trait KoopaLocalInit {
    fn local_init(&self, ctx: &mut KoopaLocalContext);
}

pub trait KoopaAssignment {
    fn assign(&self, ctx: &mut KoopaLocalContext, new_v: Value);
}
