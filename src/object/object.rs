use std::rc::Rc;

pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    Return,
}

pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Objects {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    ReturnValue(Rc<ReturnValue>),
}

impl Object for Objects {
    fn inspect(&self) -> String {
        match self {
            Self::Integer(i) => i.inspect(),
            Self::Boolean(b) => b.inspect(),
            Self::Null(n) => n.inspect(),
            Self::ReturnValue(r) => r.inspect(),
        }
    }
    fn object_type(&self) -> ObjectType {
        match self {
            Self::Integer(i) => i.object_type(),
            Self::Boolean(b) => b.object_type(),
            Self::Null(n) => n.object_type(),
            Self::ReturnValue(r) => r.object_type(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn inspect(&self) -> String {
        return self.value.to_string();
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Boolean
    }
}

#[derive(Debug, Clone)]
pub struct Null {}

impl Object for Null {
    fn inspect(&self) -> String {
        "null".to_string()
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }
}

#[derive(Debug, Clone)]
pub struct ReturnValue {
    pub value: Objects,
}

impl Object for ReturnValue {
    fn object_type(&self) -> ObjectType {
        ObjectType::Return
    }
    fn inspect(&self) -> String {
        self.value.inspect()
    }
}
