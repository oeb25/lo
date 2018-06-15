use parser;
use std::{borrow::Cow, collections::BTreeMap};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeRef(usize);

impl TypeRef {
    pub const fn void() -> TypeRef {
        TypeRef(0)
    }
    pub const fn int() -> TypeRef {
        TypeRef(1)
    }
    pub const fn float() -> TypeRef {
        TypeRef(2)
    }
    pub const fn str() -> TypeRef {
        TypeRef(3)
    }
    pub const fn boolean() -> TypeRef {
        TypeRef(4)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef<'a> {
    pub name: &'a str,
    pub fields: Vec<(&'a str, TypeRef)>,
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
        return_type: TypeRef,
    },
    LetDef {
        name: Symbol<'a>,
        initializer: Result<Box<Expression<'a>>, TypeRef>,
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
    pub typ: TypeRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    pub statements: Vec<Expression<'a>>,
    pub return_item: Option<Box<Expression<'a>>>,
}

impl<'a> Block<'a> {
    pub fn return_type(&self) -> TypeRef {
        match &self.return_item {
            Some(a) => a.typ,
            None => TypeRef::void(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: Symbol<'a>,
    pub args: Vec<(Symbol<'a>, TypeRef)>,
    pub return_type: TypeRef,
    pub body: Block<'a>,
}

impl<'a> Expression<'a> {
    pub fn new(kind: ExpressionKind<'a>, typ: TypeRef) -> Expression<'a> {
        Expression { kind, typ }
    }
}

// TODO: Maybe add scope/namespace to definitions
#[derive(Debug)]
pub struct Program<'a> {
    pub typer: Typer<'a>,
    pub structs: BTreeMap<&'a str, StructDef<'a>>,
    pub uniforms: BTreeMap<&'a str, TypeRef>,
    pub functions: BTreeMap<Symbol<'a>, FunctionDef<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Origin<'a> {
    Builtin,
    UserDefined {
        file: Cow<'a, str>,
        range: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef<'a> {
    Struct {
        name: Cow<'a, str>,
        fields: Vec<(Cow<'a, str>, TypeRef)>,
        origin: Origin<'a>,
    },
    Void,
    Int,
    Float,
    Str,
    Boolean,
}

#[derive(Debug, Clone)]
pub struct Typer<'a> {
    types: Vec<TypeDef<'a>>,
}

impl<'a> Typer<'a> {
    pub fn new() -> Typer<'a> {
        use self::TypeDef::*;
        let types = vec![Void, Int, Float, Str, Boolean];

        Typer { types }
    }
    pub fn look_up_type_def(&self, type_def: &TypeDef) -> Option<TypeRef> {
        self.types
            .iter()
            .position(|x| x == type_def)
            .map(|x| TypeRef(x))
    }
    pub fn void(&self) -> TypeRef {
        TypeRef::void()
    }
    pub fn int(&self) -> TypeRef {
        TypeRef::int()
    }
    pub fn float(&self) -> TypeRef {
        TypeRef::float()
    }
    pub fn str(&self) -> TypeRef {
        TypeRef::str()
    }
    pub fn boolean(&self) -> TypeRef {
        TypeRef::boolean()
    }
    pub fn register_type(&mut self, type_def: TypeDef<'a>) -> TypeRef {
        let type_ref = TypeRef(self.types.len());
        self.types.push(type_def);
        type_ref
    }
    pub fn convert(&self, typ: &parser::Type<'a>) -> TypeRef {
        match typ {
            parser::Type::Name(name) => self.find(name),
        }
    }
    pub fn get(&self, type_ref: &TypeRef) -> &TypeDef<'a> {
        &self.types[type_ref.0]
    }
    pub fn find(&self, find_name: &str) -> TypeRef {
        match find_name {
            "void" => self.void(),
            "int" => self.int(),
            "float" => self.float(),
            "str" => self.str(),
            find_name => {
                for (i, typ) in self.types.iter().enumerate() {
                    match typ {
                        TypeDef::Struct { name, .. } => if name == find_name {
                            return TypeRef(i);
                        },
                        _ => {}
                    }
                }
                unimplemented!("Type {} not found", find_name)
            }
        }
    }
}
