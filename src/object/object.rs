pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}
pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug)]
pub enum Objects {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}

impl Object for Objects {
    fn inspect(&self) -> String {
        match self {
            Self::Integer(i) => i.inspect(),
            Self::Boolean(b) => b.inspect(),
            Self::Null(n) => n.inspect(),
        }
    }
    fn object_type(&self) -> ObjectType {
        match self {
            Self::Integer(i) => i.object_type(),
            Self::Boolean(b) => b.object_type(),
            Self::Null(n) => n.object_type(),
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Null {}

impl Object for Null {
    fn inspect(&self) -> String {
        "null".to_string()
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }
}
