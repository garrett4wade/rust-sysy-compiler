use core::panic;

use koopa::ir::builder::GlobalInstBuilder;
use koopa::ir::{Type, Value};

use crate::koo::ctx::{KoopaContext, KoopaGlobalContext, KoopaLocalContext};
use crate::koo::expr::KoopaExpr;
use crate::koo::traits::{
    KoopaAssignment, KoopaGlobalDeclaration, KoopaGlobalInit, KoopaLocalDeclaration, KoopaLocalInit,
};
use crate::symtable::SymEntry;
use num::PrimInt;

#[derive(Clone, Debug)]
pub struct KoopaConstArray {
    pub name: String,
    pub dims: Vec<usize>,
    pub init_list: KoopaArrayInitList,
}

#[derive(Clone, Debug)]
pub struct KoopaVarArray {
    pub name: String,
    pub dims: Vec<usize>,
    pub init_list: Option<KoopaArrayInitList>,
}

#[derive(Clone, Debug)]
pub struct KoopaArrayElem {
    pub name: String,
    pub indices: Vec<KoopaExpr>,
}

pub trait KoopaArray {
    fn dims(&self) -> &[usize];

    fn name(&self) -> &String;

    fn init_list(&self) -> &KoopaArrayInitList;

    fn sym_entry(&self, value: Value) -> SymEntry;

    fn coord<I: PrimInt + TryInto<usize>>(&self, idx: I) -> Vec<usize> {
        let dims = self.dims();
        let mut idx: usize = idx.try_into().ok().unwrap();
        let mut res: Vec<usize> = vec![];
        for &dim in dims.iter().rev() {
            res.push(idx % dim);
            idx = idx / dim;
        }
        res.reverse();
        res
    }

    fn ty(&self) -> Type {
        let mut ty = Type::get_array(Type::get_i32(), self.dims()[self.dims().len() - 1]);
        for &len in self.dims().iter().rev().skip(1) {
            ty = Type::get_array(ty, len);
        }
        ty
    }

    fn get<'a, I: PrimInt + TryInto<usize>>(
        &self,
        ctx: &'a mut KoopaLocalContext,
        idx: I,
    ) -> Value {
        self.get_coord(ctx, &self.coord(idx))
    }

    fn get_coord(&self, ctx: &mut KoopaLocalContext, idx: &[usize]) -> Value {
        let indices = idx
            .iter()
            .map(|&i| KoopaExpr::Number(i.try_into().unwrap()))
            .collect::<Vec<KoopaExpr>>();
        self.get_coord_var(ctx, &indices)
    }

    fn get_coord_var(&self, ctx: &mut KoopaLocalContext, idx: &[KoopaExpr]) -> Value {
        let array = (&ctx).symtable.get(self.name()).unwrap().get_array();
        let mut ptr = array;
        for i in idx {
            let j = i.unroll(ctx);
            ptr = ctx.get_elem_ptr(ptr, j);
            ctx.new_instr(ptr);
        }
        ptr
    }
}

// Initialization
impl<A: KoopaArray> KoopaLocalDeclaration for A {
    fn local_decl(&self, ctx: &mut KoopaLocalContext) {
        let v = ctx.alloc(self.ty());
        ctx.set_value_name(&v, &format!("@{}", self.name()));
        ctx.new_instr(v);
        ctx.symtable_mut()
            .insert(self.name().clone(), self.sym_entry(v))
            .unwrap();
    }
}

impl<A: KoopaArray> KoopaGlobalDeclaration for A {
    fn global_decl(&self, ctx: &mut KoopaGlobalContext) {
        let iv = ctx.zero_init(self.ty());
        let v = ctx.global_alloc(iv);
        ctx.set_value_name(&v, &format!("@{}", self.name()));
        ctx.new_instr(v);
        ctx.symtable_mut()
            .insert(self.name().clone(), self.sym_entry(v))
            .unwrap();
    }
}

// Declaration
impl<A: KoopaArray> KoopaLocalInit for A {
    fn local_init(&self, ctx: &mut KoopaLocalContext) {
        // Get the flattened initialization value.
        let n_elements = self.dims().iter().product();
        let mut res = vec![0; n_elements];
        self.init_list().resolve(ctx, self.dims(), &mut res);
        if res.iter().all(|&x| x == 0) {
            return;
        }

        // Set the initialization value to the array.
        for (i, v) in res.iter().enumerate() {
            let init_v = ctx.integer(*v);
            let ptr = self.get(ctx, i);
            let store = ctx.store(init_v, ptr);
            ctx.new_instr(store);
        }
    }
}

impl<A: KoopaArray> KoopaGlobalInit for A {
    fn global_init(&self, ctx: &mut KoopaGlobalContext) {
        // Get the flattened initialization value.
        let n_elements = self.dims().iter().product();
        let mut res = vec![0; n_elements];
        self.init_list().resolve(ctx, self.dims(), &mut res);
        if res.iter().all(|&x| x == 0) {
            return;
        }

        // Make an aggregation as the initialization value.
        let mut agg = res.iter().map(|&x| ctx.integer(x)).collect::<Vec<Value>>();
        for &l in self.dims().iter().rev().take(self.dims().len() - 1) {
            agg = agg.chunks(l).map(|xs| ctx.aggregate(xs.to_vec())).collect();
        }
        let init_v = ctx.aggregate(agg);

        // Replace the original zero-inited array.
        let old_v = ctx.symtable.get(self.name()).unwrap().get_array();
        ctx.program.remove_value(old_v);
        let v = ctx.program.new_value().global_alloc(init_v);
        ctx.program
            .set_value_name(v, Some(format!("@{}", self.name())));
        ctx.symtable
            .replace(self.name().clone(), self.sym_entry(v))
            .unwrap();
    }
}

// Assignment
impl KoopaAssignment for KoopaArrayElem {
    fn assign(&self, ctx: &mut KoopaLocalContext, new_v: Value) {
        if matches!(
            ctx.symtable.get(&self.name).unwrap(),
            SymEntry::ConstArray(..)
        ) {
            panic!("Cannot assign to a constant array.");
        }
        let ptr = KoopaVarArray::empty(self.name.clone()).get_coord_var(ctx, &self.indices);
        let store = ctx.store(new_v, ptr);
        ctx.new_instr(store);
    }
}

impl KoopaVarArray {
    pub fn empty(name: String) -> Self {
        KoopaVarArray {
            name,
            dims: vec![0],
            init_list: None,
        }
    }
}

// trait implementation
impl KoopaArray for KoopaVarArray {
    fn dims(&self) -> &[usize] {
        &self.dims
    }

    fn name(&self) -> &String {
        &self.name
    }

    fn sym_entry(&self, value: Value) -> SymEntry {
        SymEntry::Array(value, self.dims.clone())
    }

    fn init_list(&self) -> &KoopaArrayInitList {
        self.init_list
            .as_ref()
            .expect("The array does not have an initializer list.")
    }
}

impl KoopaArray for KoopaConstArray {
    fn dims(&self) -> &[usize] {
        &self.dims
    }

    fn name(&self) -> &String {
        &self.name
    }

    fn sym_entry(&self, value: Value) -> SymEntry {
        SymEntry::ConstArray(value, self.dims.clone())
    }

    fn init_list(&self) -> &KoopaArrayInitList {
        &self.init_list
    }
}

// Parsing the complex initializer list.
#[derive(Clone, Debug)]
pub enum KoopaArrayInitListItem {
    Num(i32),
    List(Vec<KoopaArrayInitListItem>),
}

impl KoopaArrayInitListItem {
    pub fn get_num(&self) -> i32 {
        if let KoopaArrayInitListItem::Num(n) = self {
            *n
        } else {
            panic!("Cannot get number from a list.")
        }
    }

    pub fn is_num(&self) -> bool {
        matches!(self, KoopaArrayInitListItem::Num(..))
    }

    pub fn get_list(&self) -> &Vec<KoopaArrayInitListItem> {
        if let KoopaArrayInitListItem::List(list) = self {
            list
        } else {
            panic!("Cannot get list from a number.")
        }
    }
}

#[derive(Clone, Debug)]
pub struct KoopaArrayInitList(pub Vec<KoopaArrayInitListItem>);

impl KoopaArrayInitList {
    fn pack(&self, binsize: usize) -> Self {
        let init = &self.0;
        let mut i = 0;
        let mut res = vec![];
        while i < init.len() {
            match &init[i] {
                KoopaArrayInitListItem::List(_) => {
                    res.push(init[i].clone());
                    i += 1;
                }
                KoopaArrayInitListItem::Num(_) => {
                    let mut j = i;
                    while j - i < binsize && j < init.len() && init[j].is_num() {
                        j += 1;
                    }
                    if (j - i) % binsize != 0 {
                        panic!("Invalid initilizer list");
                    }
                    let mut items = vec![];
                    for k in i..j {
                        items.push(init[k].clone());
                    }
                    res.push(KoopaArrayInitListItem::List(items));
                    i = j;
                }
            }
        }
        KoopaArrayInitList(res)
    }

    fn resolve<C: KoopaContext>(&self, ctx: &mut C, dims: &[usize], res: &mut [i32]) {
        let init = &self.0;

        // Fill the first several numbers.
        let head_item_cnt = init
            .iter()
            .enumerate()
            .take_while(|(_, x)| x.is_num())
            .map(|(i, x)| {
                res[i] = x.get_num();
            })
            .count();
        if head_item_cnt == init.len() {
            return;
        }

        // dims [2, 3, 4] => accum_dims [4, 12, 24]
        let accum_dims: Vec<usize> = dims
            .iter()
            .rev()
            .scan(1, |s, &x| {
                *s *= x;
                Some(*s)
            })
            .collect();
        // accum_dims [24, 12, 4]
        let accum_dims: Vec<usize> = accum_dims.into_iter().rev().collect();
        let recur_axis: usize;
        if head_item_cnt == 0 {
            recur_axis = 1;
        } else {
            recur_axis = accum_dims
                .iter()
                .take_while(|&d| head_item_cnt % d != 0)
                .count();
        }

        if recur_axis == dims.len() {
            panic!("Invalid initializer list");
        }

        // The initializer list may contain numbers after the first list.
        // Pack these numbers into new lists.
        let init =
            KoopaArrayInitList(init[head_item_cnt..].to_owned()).pack(accum_dims[recur_axis]);
        for (i, list_init) in init.0.iter().enumerate() {
            let recur_step = accum_dims[recur_axis];
            let elems = list_init.get_list().to_owned();
            let s = head_item_cnt + i * recur_step;
            let e = s + recur_step;
            KoopaArrayInitList(elems).resolve::<C>(ctx, &dims[recur_axis..], &mut res[s..e]);
        }
    }
}
