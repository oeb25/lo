use ast::{
    Block, Expression, ExpressionKind, FunctionDef, Literal, Operator, Primitive, Program,
    StructDef, Type, Symbol,
};
pub use parser;

use std::collections::BTreeMap;

impl<'a> Expression<'a> {
    fn infer(scope: &mut Scope<'a>, expr: &parser::Expression<'a>) -> Expression<'a> {
        match expr {
            parser::Expression::Field(expr, field) => {
                let expr = Expression::infer(scope, expr);

                let s = match expr.typ {
                    Type::UserDefined(name) => scope
                        .get_struct(name)
                        .expect(&format!("unable to find struct {:?}", name)),
                    _ => unimplemented!(),
                };

                let (_, typ) = s
                    .fields
                    .iter()
                    .find(|(name, _)| name == field)
                    .expect(&format!("field {:?} does not exist on {:?}", field, s.name));

                Expression::new(ExpressionKind::Field(box expr, field), typ.clone())
            }
            parser::Expression::FuncCall { fun, args } => {
                let fun: &parser::Expression = fun;
                let name = match fun {
                    parser::Expression::Symbol(name) => name.clone(),
                    x => unimplemented!("{:?}", x),
                };

                let args: Vec<_> = args
                    .iter()
                    .map(|arg| Expression::infer(scope, arg))
                    .collect();

                let signature = FunctionSignature {
                    name: name.clone(),
                    args: args.iter().map(|arg| arg.typ.clone()).collect(),
                };

                match scope.get_function(&signature) {
                    Some(return_type) => Expression::new(
                        ExpressionKind::FunctionCall {
                            name,
                            args,
                            return_type: return_type.clone(),
                        },
                        return_type.clone(),
                    ),
                    None => unimplemented!("function {:?} not found in scope", signature),
                }
            }
            parser::Expression::Operator(lhs, op, rhs) => {
                let lhs = Expression::infer(scope, lhs);
                let rhs = Expression::infer(scope, rhs);
                let typ = match (&lhs.typ, op, &rhs.typ) {
                    (a, Operator::Plus, b) | (a, Operator::Hyphen, b) => match (a, b) {
                        (&INT, &INT) => INT,
                        (a, b) if a.is_numeric() && b.is_numeric() => FLOAT,
                        (&VEC2, &VEC2) => VEC2,
                        (&VEC3, &VEC3) => VEC3,
                        _ => unimplemented!("add {:?} {:?}", a, b),
                    },
                    (a, Operator::PlusEqual, b) => (match (a, b) {
                        (&INT, &INT) => &INT,
                        (a, b) if a.is_numeric() && b.is_numeric() => a,
                        (&VEC2, &VEC2) => a,
                        (&VEC3, &VEC3) => a,
                        _ => unimplemented!("add assign {:?} {:?}", a, b),
                    }).clone(),
                    (a, Operator::Star, b) => {
                        if a.is_int() && b.is_int() {
                            INT
                        } else if a.is_float() && b.is_float() {
                            FLOAT
                        } else if a.is_numeric() && b.is_numeric() {
                            FLOAT
                        } else {
                            unimplemented!("mul {:?} {:?}", a, b)
                        }
                    }
                    (a, Operator::Equal, b)
                    | (a, Operator::NotEqual, b)
                    | (a, Operator::DoubleEqual, b)
                    | (a, Operator::Greater, b) => {
                        if (a.is_numeric() && b.is_numeric()) || (a == b) {
                            Type::Primitive(Primitive::Boolean)
                        } else {
                            unimplemented!("Type mismatch: {:?} == {:?}", a, b);
                        }
                    }
                    (a, Operator::Divide, b) => match (a, b) {
                        (&FLOAT, &VEC2) => Type::UserDefined("vec2"),
                        (&INT, &INT) => INT,
                        x => unimplemented!("divide {:?}", x),
                    },
                    x => unimplemented!("{:?}", x),
                };

                Expression::new(ExpressionKind::Operator(box lhs, *op, box rhs), typ)
            }
            parser::Expression::Symbol(name) => {
                let typ = scope
                    .get_variable(name)
                    .expect(&format!("variable {:?} not found in scope", name));
                Expression::new(ExpressionKind::Symbol(name.clone()), typ.clone())
            }
            parser::Expression::If {
                condition,
                if_block,
                else_block,
            } => {
                let condition = box Expression::infer(scope, condition);

                let if_block = Block::infer(scope, if_block);
                let else_block = match else_block {
                    Some(else_block) => Some(Block::infer(scope, else_block)),
                    None => None,
                };

                let if_return_type = if_block.return_type();
                let else_return_type = else_block
                    .as_ref()
                    .map(|e| e.return_type())
                    .unwrap_or(&VOID);
                assert_eq!(if_return_type, else_return_type);

                let return_type = if_return_type.clone();

                Expression::new(
                    ExpressionKind::If {
                        condition,
                        if_block,
                        else_block,
                    },
                    return_type,
                )
            }
            parser::Expression::Literal(a) => {
                let typ = match a {
                    Literal::Int(_) => Type::Primitive(Primitive::Int),
                    Literal::String(_) => Type::Primitive(Primitive::Str),
                    Literal::Float(_) => Type::Primitive(Primitive::Float),
                };
                Expression::new(ExpressionKind::Literal(a.clone()), typ)
            }
            parser::Expression::LetDef {
                name,
                typ,
                initializer,
            } => {
                let initializer = match (initializer, typ) {
                    (Some(initializer), _) => {
                        let infered_initializer = Expression::infer(scope, initializer);
                        if let Some(typ) = typ {
                            assert_eq!(&infered_initializer.typ, typ);
                        }

                        Ok(box infered_initializer)
                    }
                    (None, Some(typ)) => Err(typ.clone()),
                    _ => unimplemented!("soz"),
                };

                scope.register_variable(
                    name.clone(),
                    match &initializer {
                        Ok(i) => i.typ.clone(),
                        Err(typ) => typ.clone(),
                    },
                );

                Expression::new(ExpressionKind::LetDef { name: name.clone(), initializer }, VOID)
            }
            parser::Expression::For {
                name,
                from,
                to,
                body,
            } => {
                let from = box Expression::infer(scope, from);
                let to = box Expression::infer(scope, to);

                let mut scope = scope.get_child();
                scope.register_variable(name.clone(), Primitive::Int.into());
                let body = Block::infer(&mut scope, body);

                Expression::new(
                    ExpressionKind::For {
                        name: name.clone(),
                        from,
                        to,
                        body,
                    },
                    VOID,
                )
            }
            parser::Expression::Block(block) => {
                let block = Block::infer(scope, block);
                let typ = block.return_type().clone();
                Expression::new(ExpressionKind::Block(block), typ)
            }
        }
    }
}

impl<'a> Block<'a> {
    fn infer(scope: &mut Scope<'a>, block: &parser::Block<'a>) -> Block<'a> {
        let mut block_scope = scope.get_child();

        let mut statements = vec![];

        for stmt in &block.statements {
            statements.push(Expression::infer(&mut block_scope, stmt));
        }

        let return_item = block
            .return_item
            .as_ref()
            .map(|r| box Expression::infer(&mut block_scope, r));

        Block {
            statements,
            return_item,
        }
    }
}

impl<'a> FunctionDef<'a> {
    fn infer(scope: &mut Scope<'a>, func_decl: &parser::FuncDef<'a>) -> FunctionDef<'a> {
        let name = func_decl.name.clone();
        let args = func_decl.args.clone();
        let return_type = func_decl.return_type.clone();
        let body = Block::infer(scope, &func_decl.body);

        assert_eq!(&func_decl.return_type, body.return_type());

        FunctionDef {
            name,
            args,
            return_type,
            body,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct FunctionSignature<'a> {
    name: Symbol<'a>,
    args: Vec<Type<'a>>,
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    parent: Option<Box<Scope<'a>>>,
    structs: BTreeMap<&'a str, StructDef<'a>>,
    functions: BTreeMap<FunctionSignature<'a>, Type<'a>>,
    variables: BTreeMap<Symbol<'a>, Type<'a>>,
}

impl<'a> Scope<'a> {
    fn new() -> Scope<'a> {
        let parent = None;
        let structs = BTreeMap::new();
        let functions = BTreeMap::new();
        let variables = BTreeMap::new();

        Scope {
            parent,
            structs,
            functions,
            variables,
        }
    }

    fn register_struct(&mut self, name: &'a str, s: StructDef<'a>) {
        self.structs.insert(name, s);
    }

    fn register_function(&mut self, func: FunctionSignature<'a>, return_type: Type<'a>) {
        self.functions.insert(func, return_type);
    }

    fn register_variable(&mut self, name: Symbol<'a>, typ: Type<'a>) {
        self.variables.insert(name, typ);
    }

    fn get_struct(&self, name: &'a str) -> Option<&StructDef<'a>> {
        match self.structs.get(name) {
            Some(s) => Some(s),
            None => match &self.parent {
                Some(parent) => parent.get_struct(name),
                None => None,
            },
        }
    }

    fn get_function(&self, func: &FunctionSignature<'a>) -> Option<&Type<'a>> {
        match self.functions.get(func) {
            Some(func) => Some(func),
            None => match &self.parent {
                Some(parent) => parent.get_function(func),
                None => None,
            },
        }
    }

    fn get_variable(&self, name: &Symbol<'a>) -> Option<&Type<'a>> {
        match self.variables.get(name) {
            Some(typ) => Some(typ),
            None => match &self.parent {
                Some(parent) => parent.get_variable(name),
                None => None,
            },
        }
    }

    fn get_child(&self) -> Scope<'a> {
        let mut scope = Scope::new();

        scope.parent = Some(box self.clone());

        scope
    }
}

const VOID: Type = Type::Primitive(Primitive::Void);
const INT: Type = Type::Primitive(Primitive::Int);
const FLOAT: Type = Type::Primitive(Primitive::Float);
const VEC2: Type = Type::UserDefined("vec2");
const VEC3: Type = Type::UserDefined("vec3");

#[derive(Debug)]
pub struct Sources<'a> {
    pub documents: BTreeMap<&'a str, parser::Document<'a>>,
}

impl<'a> Sources<'a> {
    pub fn type_check(self) -> Program<'a> {
        // TODO: Add structs to the resulting Program
        let mut structs = BTreeMap::new();
        let mut uniforms = BTreeMap::new();
        let mut functions = BTreeMap::new();

        let mut scope = Scope::new();

        macro_rules! builtins {
            {$($name:ident($($arg:ident),*): $ret:ident)*} => {$(
                scope.register_function(
                    FunctionSignature {
                        name: stringify!($name).into(),
                        args: vec![$(
                            Type::from_str(stringify!($arg)),
                        )*],
                    },
                    Type::from_str(stringify!($ret)),
                );
            )*}
        }

        builtins!{
            texture(sampler2D, vec2): vec4
            textureSize(sampler2D, int): vec2

            vec2(float, float): vec2
            vec2(float, int): vec2
            vec2(int, float): vec2

            vec3(vec4): vec3
            vec3(float, float, float): vec3
            vec3(int): vec3

            dot(vec3, vec3): float
            mod(float, int): float

            // This is a mess...
            print(str): void
            print(str, int): void
            print(str, str): void
            print(str, str, str): void
            print(str, int, str): void
            print(str, int, int): void
            print(str, str, int): void
            print(str, str, str, str): void
            print(str, int, str, str): void
            print(str, int, int, str): void
            print(str, str, int, str): void
            print(str, str, str, int): void
            print(str, int, str, int): void
            print(str, int, int, int): void
            print(str, str, int, int): void
        };

        scope.register_struct(
            "vec2".into(),
            StructDef {
                name: "vec2",
                fields: vec![("x", FLOAT), ("y", FLOAT)],
            },
        );

        for (_name, doc) in self.documents {
            for decl in &doc.declerations {
                match decl {
                    parser::Decleration::FuncDef(func_decl) => {
                        scope.register_function(
                            FunctionSignature {
                                name: func_decl.name.clone(),
                                args: func_decl.args.iter().map(|(_, typ)| typ.clone()).collect(),
                            },
                            func_decl.return_type.clone(),
                        );
                    }
                    parser::Decleration::Uniforms { uniforms } => {
                        for (name, typ) in uniforms {
                            scope.register_variable((*name).into(), typ.clone())
                        }
                    }
                    parser::Decleration::Struct(parser::StructDef { name, fields }) => {
                        scope.register_struct(
                            name,
                            StructDef {
                                name,
                                fields: fields.clone(),
                            },
                        );
                    }
                }
            }

            for decl in &doc.declerations {
                match decl {
                    parser::Decleration::FuncDef(func_decl) => {
                        let mut scope = scope.get_child();

                        for (name, typ) in &func_decl.args {
                            scope.register_variable(name.clone(), typ.clone());
                        }
                        let func = FunctionDef::infer(&mut scope, func_decl);
                        functions.insert(func_decl.name.clone(), func);
                    }
                    parser::Decleration::Struct(s) => {
                        let s = StructDef {
                            name: s.name,
                            fields: s.fields.clone(),
                        };
                        structs.insert(s.name, s);
                    }
                    parser::Decleration::Uniforms { uniforms: us } => {
                        for (name, typ) in us {
                            uniforms.insert(*name, typ.clone());
                        }
                    }
                }
            }
        }

        Program {
            structs,
            uniforms,
            functions,
        }
    }
}
