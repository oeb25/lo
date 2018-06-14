use std::{collections::BTreeMap, borrow::Cow};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol<'a>(Cow<'a, str>);

impl<'a, T> From<T> for Symbol<'a>
where
    T: Into<Cow<'a, str>>,
{
    fn from(t: T) -> Symbol<'a> {
        Symbol(t.into())
    }
}

impl<'a> ::std::fmt::Display for Symbol<'a> {
    fn fmt(&self, writer: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        write!(writer, "{}", self.0)
    }
}

impl<'a> ::std::ops::Deref for Symbol<'a> {
    type Target = str;
    fn deref<'b>(&'b self) -> &'b str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'a> {
    String(Cow<'a, str>),
    Int(u64),
    Float(f64),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Divide,
    Equal,
    DoubleEqual,
    Range,
    Dot,
    Star,
    Plus,
    Hyphen,
    PlusEqual,
    ExclamationMark,
    NotEqual,
    Greater,
    Less,
}

impl Operator {
    pub fn to_str(&self) -> &str {
        use ast::Operator::*;
        match self {
            Divide => "/",
            Equal => "=",
            Dot => ".",
            Star => "*",
            Plus => "+",
            Hyphen => "-",
            Range => "..",
            PlusEqual => "+=",
            DoubleEqual => "==",
            ExclamationMark => "!",
            NotEqual => "!=",
            Greater => ">",
            Less => "<",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Seperator {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Colon,
    SimiColon,
    Comma,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Primitive {
    Void,
    Boolean,
    Int,
    Float,
    Str,
    Sampler2D,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a> {
    Primitive(Primitive),
    UserDefined(&'a str),
}

impl<'a> Type<'a> {
    pub fn is_numeric(&self) -> bool {
        self.is_float() || self.is_int()
    }
    pub fn is_int(&self) -> bool {
        match self {
            Type::Primitive(Primitive::Int) => true,
            _ => false,
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            Type::Primitive(Primitive::Float) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef<'a> {
    pub name: &'a str,
    pub fields: Vec<(&'a str, Type<'a>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind<'a> {
    Literal(Literal<'a>),
    Symbol(Symbol<'a>),
    Field(Box<Expression<'a>>, &'a str),
    Operator(Box<Expression<'a>>, Operator, Box<Expression<'a>>),
    FunctionCall {
        name: Symbol<'a>,
        args: Vec<Expression<'a>>,
        return_type: Type<'a>,
    },
    LetDef {
        name: Symbol<'a>,
        initializer: Result<Box<Expression<'a>>, Type<'a>>,
    },
    If {
        condition: Box<Expression<'a>>,
        if_block: Block<'a>,
        else_block: Option<Block<'a>>,
    },
    For {
        name: Symbol<'a>,
        from: Box<Expression<'a>>,
        to: Box<Expression<'a>>,
        body: Block<'a>,
    },
    Block(Block<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'a> {
    pub kind: ExpressionKind<'a>,
    pub typ: Type<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    pub statements: Vec<Expression<'a>>,
    pub return_item: Option<Box<Expression<'a>>>,
}

impl<'a> Block<'a> {
    pub fn return_type(&self) -> &Type<'a> {
        match &self.return_item {
            Some(a) => &a.typ,
            None => &Type::Primitive(Primitive::Void),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: Symbol<'a>,
    pub args: Vec<(Symbol<'a>, Type<'a>)>,
    pub return_type: Type<'a>,
    pub body: Block<'a>,
}

impl<'a> Expression<'a> {
    pub fn new(kind: ExpressionKind<'a>, typ: Type<'a>) -> Expression<'a> {
        Expression { kind, typ }
    }
}

// TODO: Maybe add scope/namespace to definitions
#[derive(Debug)]
pub struct Program<'a> {
    pub structs: BTreeMap<&'a str, StructDef<'a>>,
    pub uniforms: BTreeMap<&'a str, Type<'a>>,
    pub functions: BTreeMap<Symbol<'a>, FunctionDef<'a>>,
}
