use koopa::ir::Type;

use crate::ast::{Block, BlockItem, CompUnit, CompUnitDecl, Expr, InitListElem, LVal, SymbolValue};
use crate::koo::array::{
    KoopaArray, KoopaArrayElem, KoopaArrayInitList, KoopaArrayInitListItem, KoopaConstArray,
    KoopaVarArray,
};
use crate::koo::block::{KoopaBlock, KoopaBlockItem, KoopaControlFlow, KoopaInstr};
use crate::koo::expr::KoopaExpr;
use crate::koo::func::KoopaFunc;
use crate::koo::var::{KoopaConst, KoopaVar};
use crate::koo::{KoopaFuncGlobalVarDecl, KoopaProgram};
use crate::symtable::{SymEntry, SymTable};

// A helper trait that uses SymTable for translation.
pub trait DeduceFrom<T> {
    fn deduce_from(t: T, symtable: &mut SymTable) -> Self;
}

pub trait DeduceInto<T> {
    fn deduce_into(self, symtable: &mut SymTable) -> T;
}

impl<U, T> DeduceInto<T> for U
where
    T: DeduceFrom<U>,
{
    fn deduce_into(self, symtable: &mut SymTable) -> T {
        T::deduce_from(self, symtable)
    }
}

impl<U, T> DeduceFrom<Box<U>> for T
where
    T: DeduceFrom<U>,
{
    fn deduce_from(t: Box<U>, symtable: &mut SymTable) -> Self {
        T::deduce_from(*t, symtable)
    }
}

// Expression conversion.
impl DeduceFrom<&Expr> for KoopaExpr {
    fn deduce_from(t: &Expr, symtable: &mut SymTable) -> Self {
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
                        SymEntry::VarType(..) => Var(name.clone()),
                        _ => {
                            panic!("Invalid expression element.")
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
            Expr::FuncCall { funcname, args } => {
                let mut new_args = vec![];
                for arg in args.iter() {
                    let arg_ = match arg.as_ref() {
                        Expr::Symbol(lval) => match lval {
                            LVal::Ident(name) => {
                                let symv = symtable.get(name).unwrap();
                                match symv {
                                    SymEntry::Const(v) => Const(v),
                                    SymEntry::VarType(..) => Var(name.clone()),
                                    SymEntry::PtrType(_, _dims) => {
                                        ArrayElemParam(name.clone(), vec![])
                                    }
                                    _ => {
                                        panic!("Invalid expression element.")
                                    }
                                }
                            }
                            LVal::ArrayElem(name, indices) => ArrayElemParam(
                                name.clone(),
                                indices
                                    .into_iter()
                                    .map(|x| x.as_ref().deduce_into(symtable))
                                    .collect(),
                            ),
                        },
                        _ => arg.as_ref().deduce_into(symtable),
                    };
                    new_args.push(arg_)
                }
                FuncCall(funcname.clone(), new_args)
            }
        }
    }
}

// Block conversion.
impl DeduceFrom<&Block> for KoopaBlock {
    fn deduce_from(t: &Block, symtable: &mut SymTable) -> Self {
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
                                v.decl(symtable);
                                koopa_items
                                    .push(KoopaBlockItem::Instr(LocalDecl(Box::new(v.clone()))));
                                if init.is_some() {
                                    koopa_items.push(KoopaBlockItem::Instr(LocalInit(Box::new(v))));
                                }
                            }
                            SymbolValue::Const(c) => {
                                let x = KoopaConst(s.name.clone(), c.reduce(symtable));
                                x.decl(symtable);
                                koopa_items.push(KoopaBlockItem::Instr(LocalDecl(Box::new(x))))
                            }
                            SymbolValue::Var(c) => {
                                let item = KoopaVar(s.name.clone(), None);
                                item.decl(symtable);
                                koopa_items
                                    .push(KoopaBlockItem::Instr(LocalDecl(Box::new(item.clone()))));
                                if c.is_some() {
                                    koopa_items.push(KoopaBlockItem::Instr(Assign(
                                        Box::new(item),
                                        Box::new(KoopaExpr::deduce_from(
                                            c.as_ref().unwrap().as_ref(),
                                            symtable,
                                        )),
                                    )));
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
                                v.decl(symtable);
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
                                SymEntry::VarType(_) => {
                                    koopa_items.push(KoopaBlockItem::Instr(Assign(
                                        Box::new(KoopaVar::empty(name.clone())),
                                        Box::new(exprv),
                                    )));
                                }
                                _ => {
                                    panic!("Invalid assignment target: {}", name)
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
                    symtable.fork();
                    koopa_items.push(KoopaBlockItem::Block(Box::new(KoopaBlock::deduce_from(
                        b, symtable,
                    ))));
                    symtable.join().unwrap();
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
    fn deduce_from(t: &CompUnitDecl, symtable: &mut SymTable) -> Self {
        match t {
            CompUnitDecl::FuncDef {
                type_,
                ident,
                params,
                block,
            } => {
                let param_names = params
                    .iter()
                    .map(|p| Some(p.name().to_string()))
                    .collect::<Vec<Option<String>>>();
                let param_types = params.iter().map(|p| p.ty(symtable)).collect::<Vec<Type>>();
                symtable.fork();
                for (p, ty) in param_names.iter().zip(param_types.iter()) {
                    if ty.is_i32() || ty.is_unit() {
                        symtable
                            .insert(p.clone().unwrap(), SymEntry::VarType(ty.clone()))
                            .unwrap();
                    } else {
                        // Pointer/Array type.
                        symtable
                            .insert(
                                p.clone().unwrap(),
                                SymEntry::PtrType(ty.clone(), KoopaVarArray::dims_from_ty(&ty)),
                            )
                            .unwrap();
                    }
                }
                let block = KoopaBlock::deduce_from(block, symtable);
                symtable.join().unwrap();
                KoopaFunc {
                    name: ident.clone(),
                    param_types,
                    param_names,
                    ret_type: type_.into(),
                    block: Some(block),
                }
            }
            _ => panic!("Invalid conversion"),
        }
    }
}

// Array conversion.
impl DeduceFrom<&InitListElem> for KoopaArrayInitListItem {
    fn deduce_from(value: &InitListElem, symtable: &mut SymTable) -> Self {
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
    fn deduce_from(value: &InitListElem, symtable: &mut SymTable) -> Self {
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
                            SymbolValue::Const(expr) => {
                                let c = KoopaConst(s.name.clone(), expr.reduce(&symtable));
                                c.decl(&mut symtable);
                                KoopaFuncGlobalVarDecl::Const(c)
                            }
                            SymbolValue::Var(e) => {
                                let va = KoopaVar(
                                    s.name.clone(),
                                    e.as_ref().map(|e| e.reduce(&symtable)),
                                );
                                va.decl(&mut symtable);
                                KoopaFuncGlobalVarDecl::Var(va)
                            }
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
                                    init_list: KoopaArrayInitList::deduce_from(init, &mut symtable),
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
