use std::collections::HashMap;

use super::Objects;

pub struct Environment {
    store: HashMap<String, Objects>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Objects> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: String, val: Objects) {
        self.store.insert(name, val);
    }
}
