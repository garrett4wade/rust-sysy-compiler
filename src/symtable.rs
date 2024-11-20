use koopa::ir::{Function, Type, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum SymEntry {
    Const(i32),
    Var(Value, Type),
    Func(Function),
    FuncParam(Value, Type),
    Array(Value, Vec<usize>),
    ConstArray(Value, Vec<usize>),
    // Used for conversion
    VarType,
    FuncParamType,
}

impl SymEntry {
    pub fn get_const(&self) -> i32 {
        match self {
            SymEntry::Const(i) => *i,
            _ => panic!("SymEntry is not a constant"),
        }
    }

    pub fn get_var(&self) -> Value {
        match self {
            SymEntry::Var(v, _) => v.clone(),
            _ => panic!("SymEntry is not a variable"),
        }
    }

    pub fn get_var_ty(&self) -> Type {
        match self {
            SymEntry::Var(_, ty) => ty.clone(),
            _ => panic!("SymEntry is not a variable"),
        }
    }

    pub fn get_func(&self) -> Function {
        match self {
            SymEntry::Func(f) => f.clone(),
            _ => panic!("SymEntry is not a function"),
        }
    }

    pub fn get_func_param(&self) -> Value {
        match self {
            SymEntry::FuncParam(v, _) => v.clone(),
            _ => panic!("SymEntry is not a function parameter"),
        }
    }

    pub fn get_func_param_ty(&self) -> Type {
        match self {
            SymEntry::FuncParam(_, ty) => ty.clone(),
            _ => panic!("SymEntry is not a function parameter"),
        }
    }

    pub fn get_var_array(&self) -> Value {
        match self {
            SymEntry::Array(v, _) => v.clone(),
            _ => panic!("SymEntry is not an array"),
        }
    }

    pub fn new_var_array(v: Value, dims: Vec<usize>) -> Self {
        SymEntry::Array(v, dims)
    }

    pub fn get_const_array(&self) -> Value {
        match self {
            SymEntry::ConstArray(v, _) => v.clone(),
            _ => panic!("SymEntry is not a constant array"),
        }
    }

    pub fn new_const_array(v: Value, dims: Vec<usize>) -> Self {
        SymEntry::ConstArray(v, dims)
    }

    pub fn get_array(&self) -> Value {
        match self {
            SymEntry::Array(v, _) | SymEntry::ConstArray(v, _) => v.clone(),
            _ => panic!("SymEntry is not an array"),
        }
    }

    pub fn get_array_dims(&self) -> &Vec<usize> {
        match self {
            SymEntry::Array(_, dims) | SymEntry::ConstArray(_, dims) => dims,
            _ => panic!("SymEntry is not an array"),
        }
    }
}
pub struct SymTable {
    parent: Vec<Rc<RefCell<HashMap<String, SymEntry>>>>,
    table: Rc<RefCell<HashMap<String, SymEntry>>>,
}

impl SymTable {
    pub fn new() -> Self {
        SymTable {
            parent: vec![],
            table: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn fork(&mut self) {
        self.parent.push(self.table.clone());
        self.table = Rc::new(RefCell::new(HashMap::new()));
    }

    pub fn join(&mut self) -> Result<(), String> {
        self.table = self
            .parent
            .pop()
            .ok_or_else(|| "Cannot join the root symbol table".to_string())?;
        Ok(())
    }

    pub fn get(&self, name: &String) -> Result<SymEntry, String> {
        for tab in iter::once(&self.table).chain(self.parent.iter().rev()) {
            if let Some(v) = tab.borrow().get(name) {
                return Ok(v.clone());
            }
        }
        Err(format!("Undefined symbol: {}", name))
    }

    pub fn insert(&mut self, k: String, v: SymEntry) -> Result<(), String> {
        if self.table.borrow().contains_key(&k) {
            return Err(format!("Symbol {} cannot be defined twice", k));
        }
        self.table.borrow_mut().insert(k, v);
        Ok(())
    }

    pub fn replace(&mut self, k: String, v: SymEntry) -> Result<SymEntry, String> {
        if !self.table.borrow().contains_key(&k) {
            return Err(format!("Symbol {} has not been defined", k));
        }
        Ok(self.table.borrow_mut().insert(k, v).unwrap())
    }
}
