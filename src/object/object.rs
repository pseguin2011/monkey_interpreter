use std::{fmt::Display, rc::Rc};

#[derive(Debug, PartialEq, Eq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    Return,
    Error,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Boolean => "BOOLEAN",
            Self::Return => "RETURN",
            Self::Null => "NULL",
            Self::Integer => "INTEGER",
            Self::Error => "ERROR",
        })
    }
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
    Error(Error),
}

impl Object for Objects {
    fn inspect(&self) -> String {
        match self {
            Self::Integer(i) => i.inspect(),
            Self::Boolean(b) => b.inspect(),
            Self::Null(n) => n.inspect(),
            Self::ReturnValue(r) => r.inspect(),
            Self::Error(e) => e.inspect(),
        }
    }
    fn object_type(&self) -> ObjectType {
        match self {
            Self::Integer(i) => i.object_type(),
            Self::Boolean(b) => b.object_type(),
            Self::Null(n) => n.object_type(),
            Self::ReturnValue(r) => r.object_type(),
            Self::Error(e) => e.object_type(),
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

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
}

impl Object for Error {
    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
    fn object_type(&self) -> ObjectType {
        return ObjectType::Error;
    }
}
