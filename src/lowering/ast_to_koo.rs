use koopa::ir::Type;

use crate::ast::{
    Block, BlockItem, CompUnit, CompUnitDecl, Expr, InitListElem, LVal, SymbolValue, SysYType,
};
use crate::koo::array::{
    KoopaArrayElem, KoopaArrayInitList, KoopaArrayInitListItem, KoopaConstArray, KoopaVarArray,
};
use crate::koo::block::{KoopaBlock, KoopaBlockItem, KoopaControlFlow, KoopaInstr};
use crate::koo::expr::KoopaExpr;
use crate::koo::func::KoopaFunc;
use crate::koo::var::{KoopaConst, KoopaVar};
use crate::koo::{KoopaFuncGlobalVarDecl, KoopaProgram};
use crate::symtable::{SymEntry, SymTable};

// A helper trait that uses SymTable for translation.
pub trait DeduceFrom<T> {
    fn deduce_from(t: T, symtable: &SymTable) -> Self;
}

pub trait DeduceInto<T> {
    fn deduce_into(self, symtable: &SymTable) -> T;
}

impl<U, T> DeduceInto<T> for U
where
    T: DeduceFrom<U>,
{
    fn deduce_into(self, symtable: &SymTable) -> T {
        T::deduce_from(self, symtable)
    }
}

impl<U, T> DeduceFrom<Box<U>> for T
where
    T: DeduceFrom<U>,
{
    fn deduce_from(t: Box<U>, symtable: &SymTable) -> Self {
        T::deduce_from(*t, symtable)
    }
}

// Type conversion.
impl From<&SysYType> for Type {
    fn from(ty: &SysYType) -> Type {
        match ty {
            SysYType::Int => Type::get_i32(),
            SysYType::Void => Type::get_unit(),
        }
    }
}

// Expression conversion.
impl DeduceFrom<&Expr> for KoopaExpr {
    fn deduce_from(t: &Expr, symtable: &SymTable) -> Self {
        use KoopaExpr::*;
        match t {
            Expr::Number(n) => Number(*n),
            Expr::Unary(op, sub_expr) => Unary(
                op.clone(),
                Box::new(sub_expr.as_ref().deduce_into(symtable)),
            ),
            Expr::Binary(lhs, op, rhs) => Binary(
                Box::new(lhs.as_ref().deduce_into(symtable)),
                op.clone(),
                Box::new(rhs.as_ref().deduce_into(symtable)),
            ),
            Expr::Symbol(lval) => match lval {
                LVal::Ident(name) => {
                    let symv = symtable.get(name).unwrap();
                    match symv {
                        SymEntry::Const(v) => Const(v),
                        SymEntry::Var(..) => Var(name.clone()),
                        SymEntry::FuncParam(..) => {
                            let ty = symtable.get(name).unwrap().get_func_param_ty();
                            FuncParam(name.clone(), ty)
                        }
                        SymEntry::Array(..) | SymEntry::ConstArray(..) => {
                            panic!("Unknown how to deal with an array in expression.")
                        }
                        SymEntry::Func(..) => {
                            panic!("Function call in expression but expressed as Expr::Symbol.")
                        }
                    }
                }
                LVal::ArrayElem(name, indices) => ArrayElem(
                    name.clone(),
                    indices
                        .into_iter()
                        .map(|x| x.as_ref().deduce_into(symtable))
                        .collect(),
                ),
            },
            Expr::FuncCall { funcname, args } => FuncCall(
                funcname.clone(),
                args.into_iter()
                    .map(|x| x.as_ref().deduce_into(symtable))
                    .collect(),
            ),
        }
    }
}

// Block conversion.
impl DeduceFrom<&Block> for KoopaBlock {
    fn deduce_from(t: &Block, symtable: &SymTable) -> Self {
        use KoopaInstr::*;
        let mut koopa_items = vec![];
        for item in t.items.iter() {
            match item {
                BlockItem::Decl(symbols) => {
                    for s in symbols.iter() {
                        match &s.value {
                            SymbolValue::Arr { lens, init } => {
                                let v = KoopaVarArray {
                                    name: s.name.clone(),
                                    dims: lens
                                        .iter()
                                        .map(|e| e.reduce(symtable).try_into().unwrap())
                                        .collect::<Vec<usize>>(),
                                    init_list: init
                                        .as_ref()
                                        .map(|x| KoopaArrayInitList::deduce_from(x, symtable)),
                                };
                                koopa_items
                                    .push(KoopaBlockItem::Instr(LocalDecl(Box::new(v.clone()))));
                                if init.is_some() {
                                    koopa_items.push(KoopaBlockItem::Instr(LocalInit(Box::new(v))));
                                }
                            }
                            SymbolValue::Const(c) => koopa_items.push(KoopaBlockItem::Instr(
                                LocalDecl(Box::new(KoopaConst(s.name.clone(), c.reduce(symtable)))),
                            )),
                            SymbolValue::Var(c) => {
                                let item = KoopaVar(
                                    s.name.clone(),
                                    c.as_ref().map(|x| x.reduce(symtable)),
                                );
                                koopa_items
                                    .push(KoopaBlockItem::Instr(LocalDecl(Box::new(item.clone()))));
                                if c.is_some() {
                                    koopa_items
                                        .push(KoopaBlockItem::Instr(LocalInit(Box::new(item))));
                                }
                            }
                            SymbolValue::ConstArr { lens, init } => {
                                let v = KoopaConstArray {
                                    name: s.name.clone(),
                                    dims: lens
                                        .iter()
                                        .map(|e| e.reduce(symtable).try_into().unwrap())
                                        .collect::<Vec<usize>>(),
                                    init_list: KoopaArrayInitList::deduce_from(init, symtable),
                                };
                                koopa_items
                                    .push(KoopaBlockItem::Instr(LocalDecl(Box::new(v.clone()))));
                                koopa_items.push(KoopaBlockItem::Instr(LocalInit(Box::new(v))));
                            }
                        }
                    }
                }
                BlockItem::Assign(lval, expr) => {
                    let exprv = KoopaExpr::deduce_from(expr.as_ref(), symtable);
                    match lval {
                        LVal::Ident(name) => {
                            let v = symtable.get(&name).unwrap();
                            match v {
                                SymEntry::Var(..) => {
                                    koopa_items.push(KoopaBlockItem::Instr(Assign(
                                        Box::new(KoopaVar::empty(name.clone())),
                                        Box::new(exprv),
                                    )));
                                }
                                SymEntry::Func(_) => {
                                    panic!("Cannot assign to a function: {}", name)
                                }
                                SymEntry::FuncParam(..) => {
                                    panic!("Cannot assign to a function parameter: {}", name)
                                }
                                SymEntry::Const(_) => {
                                    panic!("Cannot assign to a constant: {}", name)
                                }
                                SymEntry::Array(..) | SymEntry::ConstArray(..) => {
                                    panic!("Cannot assign to an array: {}", name)
                                }
                            }
                        }
                        LVal::ArrayElem(name, indices) => {
                            let indices = indices
                                .iter()
                                .map(|x| KoopaExpr::deduce_from(x.as_ref(), symtable))
                                .collect::<Vec<KoopaExpr>>();
                            let dst = Box::new(KoopaArrayElem {
                                name: name.clone(),
                                indices,
                            });
                            koopa_items.push(KoopaBlockItem::Instr(Assign(dst, Box::new(exprv))));
                        }
                    }
                }
                BlockItem::Ret(expr) => {
                    // Create instructions with recursion.
                    koopa_items.push(KoopaBlockItem::Instr(Ret(expr
                        .as_ref()
                        .map(|e| Box::new(e.as_ref().deduce_into(symtable))))));
                }
                BlockItem::Expr(e) => {
                    if e.is_some() {
                        koopa_items.push(KoopaBlockItem::Instr(Expr(Box::new(
                            KoopaExpr::deduce_from(&(*e.as_ref().unwrap().clone()), symtable),
                        ))));
                    }
                }
                BlockItem::If {
                    cond,
                    then_block,
                    else_block,
                } => {
                    let then_block = Box::new(KoopaBlock::deduce_from(
                        &Block {
                            items: vec![*then_block.clone()],
                        },
                        symtable,
                    ));
                    let else_block = else_block.as_ref().map(|b| {
                        Box::new(KoopaBlock::deduce_from(
                            &Block {
                                items: vec![*b.clone()],
                            },
                            symtable,
                        ))
                    });
                    koopa_items.push(KoopaBlockItem::Control(KoopaControlFlow::If {
                        cond: Box::new(KoopaExpr::deduce_from(cond.as_ref(), symtable)),
                        then_block,
                        else_block,
                    }));
                }
                BlockItem::Block(b) => {
                    koopa_items.push(KoopaBlockItem::Block(Box::new(KoopaBlock::deduce_from(
                        b, symtable,
                    ))));
                }
                BlockItem::While { cond, while_block } => {
                    let while_block = Box::new(KoopaBlock::deduce_from(
                        &Block {
                            items: vec![*while_block.clone()],
                        },
                        symtable,
                    ));
                    koopa_items.push(KoopaBlockItem::Control(KoopaControlFlow::While {
                        cond: Box::new(KoopaExpr::deduce_from(cond.as_ref(), symtable)),
                        while_block,
                    }));
                }
                BlockItem::Break => {
                    koopa_items.push(KoopaBlockItem::Control(KoopaControlFlow::Break));
                }
                BlockItem::Continue => {
                    koopa_items.push(KoopaBlockItem::Control(KoopaControlFlow::Continue));
                }
            }
        }
        KoopaBlock { items: koopa_items }
    }
}

// Function conversion.
impl DeduceFrom<&CompUnitDecl> for KoopaFunc {
    fn deduce_from(t: &CompUnitDecl, symtable: &SymTable) -> Self {
        match t {
            CompUnitDecl::FuncDef {
                type_,
                ident,
                params,
                block,
            } => {
                let param_names = params.iter().map(|p| Some(p.ident.clone())).collect();
                KoopaFunc {
                    name: ident.clone(),
                    param_types: params.iter().map(|p| (&p.type_).into()).collect(),
                    param_names,
                    ret_type: type_.into(),
                    block: Some(KoopaBlock::deduce_from(block, symtable)),
                }
            }
            _ => panic!("Invalid conversion"),
        }
    }
}

// Array conversion.
impl DeduceFrom<&InitListElem> for KoopaArrayInitListItem {
    fn deduce_from(value: &InitListElem, symtable: &SymTable) -> Self {
        match &value {
            InitListElem::Item(expr) => KoopaArrayInitListItem::Num(expr.reduce(&symtable)),
            InitListElem::List(lis) => KoopaArrayInitListItem::List(
                lis.iter()
                    .map(|e| KoopaArrayInitListItem::deduce_from(e, symtable))
                    .collect(),
            ),
        }
    }
}

impl DeduceFrom<&InitListElem> for KoopaArrayInitList {
    fn deduce_from(value: &InitListElem, symtable: &SymTable) -> Self {
        match &value {
            InitListElem::Item(_) => panic!("Can't initialize an array with a scalar"),
            InitListElem::List(_) => KoopaArrayInitList(
                KoopaArrayInitListItem::deduce_from(value, symtable)
                    .get_list()
                    .to_owned(),
            ),
        }
    }
}

// Program conversion.
impl From<&CompUnit> for KoopaProgram {
    fn from(comp_unit: &CompUnit) -> Self {
        let mut program_items = vec![];
        let mut symtable = SymTable::new();
        for decl in comp_unit.defs.iter() {
            match decl {
                CompUnitDecl::FuncDef { .. } => {
                    let item =
                        KoopaFuncGlobalVarDecl::Func(KoopaFunc::deduce_from(decl, &mut symtable));
                    program_items.push(item);
                }
                CompUnitDecl::VarDecl(symbols) => {
                    for s in symbols {
                        let item = match &s.value {
                            SymbolValue::Const(expr) => KoopaFuncGlobalVarDecl::Const(KoopaConst(
                                s.name.clone(),
                                expr.reduce(&symtable),
                            )),
                            SymbolValue::Var(e) => KoopaFuncGlobalVarDecl::Var(KoopaVar(
                                s.name.clone(),
                                e.as_ref().map(|e| e.reduce(&symtable)),
                            )),
                            SymbolValue::Arr { lens, init } => {
                                let mut _init: InitListElem;
                                if init.is_none() {
                                    _init = InitListElem::List(vec![InitListElem::Item(Box::new(
                                        Expr::Number(0),
                                    ))])
                                } else {
                                    _init = init.as_ref().unwrap().clone();
                                }
                                KoopaFuncGlobalVarDecl::VarArray(KoopaVarArray {
                                    name: s.name.clone(),
                                    dims: lens
                                        .iter()
                                        .map(|e| e.reduce(&symtable).try_into().unwrap())
                                        .collect::<Vec<usize>>(),
                                    init_list: Some(KoopaArrayInitList::deduce_from(
                                        &_init,
                                        &mut symtable,
                                    )),
                                })
                            }
                            SymbolValue::ConstArr { lens, init } => {
                                KoopaFuncGlobalVarDecl::ConstArray(KoopaConstArray {
                                    name: s.name.clone(),
                                    dims: lens
                                        .iter()
                                        .map(|e| e.reduce(&symtable).try_into().unwrap())
                                        .collect::<Vec<usize>>(),
                                    init_list: KoopaArrayInitList::deduce_from(init, &symtable),
                                })
                            }
                        };
                        program_items.push(item);
                    }
                }
            }
        }
        KoopaProgram {
            defs: program_items,
        }
    }
}
