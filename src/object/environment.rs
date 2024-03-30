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

    pub fn get(&self, name: &str) -> Option<&Objects> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: String, val: Objects) {
        self.store.insert(name, val);
    }
}
