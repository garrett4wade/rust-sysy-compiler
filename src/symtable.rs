use koopa::ir::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum SymEntry {
    Const(i32),
    Var(Value),
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
}
