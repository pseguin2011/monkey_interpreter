use std::{collections::HashMap, rc::Rc, sync::Mutex};

use super::Objects;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Objects>,
    outer: Option<Rc<Mutex<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<Mutex<Self>>) -> Self {
        let mut env = Self::new();
        env.outer = Some(outer);
        env
    }

    pub fn get(&self, name: &str) -> Option<Objects> {
        if let Some(inner) = self.store.get(name) {
            return Some(inner.clone());
        }
        if let Some(outer) = self.outer.clone() {
            return outer.lock().unwrap().get(name);
        }
        None
    }

    pub fn set(&mut self, name: String, val: Objects) {
        self.store.insert(name, val);
    }
}
