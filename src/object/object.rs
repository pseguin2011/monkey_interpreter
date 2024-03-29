pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

struct Integer {
    value: u64,
}

impl Object for Integer {
    fn inspect(&self) -> String {
        return self.value.to_string();
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer
    }
}

struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Boolean
    }
}

struct Null {}

impl Object for Null {
    fn inspect(&self) -> String {
        "null".to_string()
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }
}
