use ast::{
    Block, Expression, ExpressionKind, FunctionDef, Literal, Operator, Origin, Program,
    StructDef, Symbol, TypeDef, TypeRef, Typer,
};
pub use parser;

use std::collections::BTreeMap;

impl<'a> Expression<'a> {
    fn infer(
        typer: &Typer<'a>,
        scope: &mut Scope<'a>,
        expr: &parser::Expression<'a>,
    ) -> Expression<'a> {
        match expr {
            parser::Expression::Field(expr, field) => {
                let expr = Expression::infer(typer, scope, expr);

                let (name, fields) = match typer.get(&expr.typ) {
                    TypeDef::Struct { name, fields, .. } => (name, fields),
                    _ => unimplemented!("unable to find struct"),
                };

                let (_, typ) = fields
                    .iter()
                    .find(|(name, _)| name == field)
                    .expect(&format!("field {:?} does not exist on {:?}", field, name));

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
                    .map(|arg| Expression::infer(typer, scope, arg))
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
                let lhs = Expression::infer(typer, scope, lhs);
                let rhs = Expression::infer(typer, scope, rhs);
                // TODO
                let typ = lhs.typ;
                // let typ = match (&lhs.typ, op, &rhs.typ) {
                //     (a, Operator::Plus, b) | (a, Operator::Hyphen, b) => match (*a, *b) {
                //         (INT, INT) => INT,
                //         (INT, INT) | (INT, FLOAT) | (FLOAT, FLOAT) | (FLOAT, INT) => FLOAT,
                //         (vec2, vec2) => vec2,
                //         (vec3, vec3) => vec3,
                //         _ => unimplemented!("add {:?} {:?}", a, b),
                //     },
                //     (a, Operator::PlusEqual, b) => (match (a, b) {
                //         (INT, INT) => INT,
                //         (a, b) if a.is_numeric() && b.is_numeric() => a,
                //         (vec2, vec2) => a,
                //         (vec3, vec3) => a,
                //         _ => unimplemented!("add assign {:?} {:?}", a, b),
                //     }).clone(),
                //     (a, Operator::Star, b) => {
                //         if a.is_INT() && b.is_INT() {
                //             INT
                //         } else if a.is_float() && b.is_float() {
                //             FLOAT
                //         } else if a.is_numeric() && b.is_numeric() {
                //             FLOAT
                //         } else {
                //             unimplemented!("mul {:?} {:?}", a, b)
                //         }
                //     }
                //     (a, Operator::Equal, b)
                //     | (a, Operator::NotEqual, b)
                //     | (a, Operator::DoubleEqual, b)
                //     | (a, Operator::Greater, b) => {
                //         if (a.is_numeric() && b.is_numeric()) || (a == b) {
                //             Type::Primitive(Primitive::Boolean)
                //         } else {
                //             unimplemented!("Type mismatch: {:?} == {:?}", a, b);
                //         }
                //     }
                //     (a, Operator::Divide, b) => match (a, b) {
                //         (float, vec2) => Type::UserDefined("vec2"),
                //         (INT, INT) => scope.typer,
                //         x => unimplemented!("divide {:?}", x),
                //     },
                //     x => unimplemented!("{:?}", x),
                // };

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
                let condition = box Expression::infer(typer, scope, condition);

                let if_block = Block::infer(typer, scope, if_block);
                let else_block = match else_block {
                    Some(else_block) => Some(Block::infer(typer, scope, else_block)),
                    None => None,
                };

                let if_return_type = if_block.return_type();
                let else_return_type = else_block
                    .as_ref()
                    .map(|e| e.return_type())
                    .unwrap_or(typer.void());
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
                    Literal::Int(_) => typer.int(),
                    Literal::Float(_) => typer.float(),
                    Literal::String(_) => typer.str(),
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
                        let infered_initializer = Expression::infer(typer, scope, initializer);
                        if let Some(typ) = typ {
                            assert_eq!(infered_initializer.typ, typer.convert(typ));
                        }

                        Ok(box infered_initializer)
                    }
                    (None, Some(typ)) => Err(typer.convert(typ)),
                    _ => unimplemented!("soz"),
                };

                scope.register_variable(
                    name.clone(),
                    match &initializer {
                        Ok(i) => i.typ,
                        Err(typ) => *typ,
                    },
                );

                Expression::new(
                    ExpressionKind::LetDef {
                        name: name.clone(),
                        initializer,
                    },
                    typer.void(),
                )
            }
            parser::Expression::For {
                name,
                from,
                to,
                body,
            } => {
                let from = box Expression::infer(typer, scope, from);
                let to = box Expression::infer(typer, scope, to);

                let mut scope = scope.get_child();
                scope.register_variable(name.clone(), typer.int());
                let body = Block::infer(typer, &mut scope, body);

                Expression::new(
                    ExpressionKind::For {
                        name: name.clone(),
                        from,
                        to,
                        body,
                    },
                    typer.void(),
                )
            }
            parser::Expression::Block(block) => {
                let block = Block::infer(typer, scope, block);
                let typ = block.return_type().clone();
                Expression::new(ExpressionKind::Block(block), typ)
            }
        }
    }
}

impl<'a> Block<'a> {
    fn infer(typer: &Typer<'a>, scope: &mut Scope<'a>, block: &parser::Block<'a>) -> Block<'a> {
        let mut block_scope = scope.get_child();

        let mut statements = vec![];

        for stmt in &block.statements {
            statements.push(Expression::infer(typer, &mut block_scope, stmt));
        }

        let return_item = block
            .return_item
            .as_ref()
            .map(|r| box Expression::infer(typer, &mut block_scope, r));

        Block {
            statements,
            return_item,
        }
    }
}

impl<'a> FunctionDef<'a> {
    fn infer(
        typer: &Typer<'a>,
        scope: &mut Scope<'a>,
        func_decl: &parser::FuncDef<'a>,
    ) -> FunctionDef<'a> {
        let name = func_decl.name.clone();
        let args = func_decl
            .args
            .iter()
            .map(|(name, typ)| (name.clone(), typer.convert(typ)))
            .collect();
        let return_type = typer.convert(&func_decl.return_type);
        let body = Block::infer(typer, scope, &func_decl.body);

        assert_eq!(return_type, body.return_type());

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
    args: Vec<TypeRef>,
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    parent: Option<Box<Scope<'a>>>,
    functions: BTreeMap<FunctionSignature<'a>, TypeRef>,
    variables: BTreeMap<Symbol<'a>, TypeRef>,
}

impl<'a> Scope<'a> {
    fn new() -> Scope<'a> {
        let parent = None;
        let functions = BTreeMap::new();
        let variables = BTreeMap::new();

        Scope {
            parent,
            functions,
            variables,
        }
    }

    fn register_function(&mut self, func: FunctionSignature<'a>, return_type: TypeRef) {
        self.functions.insert(func, return_type);
    }

    fn register_variable(&mut self, name: Symbol<'a>, typ: TypeRef) {
        self.variables.insert(name, typ);
    }

    fn get_function(&self, func: &FunctionSignature<'a>) -> Option<TypeRef> {
        match self.functions.get(func) {
            Some(func) => Some(*func),
            None => match &self.parent {
                Some(parent) => parent.get_function(func),
                None => None,
            },
        }
    }

    fn get_variable(&self, name: &Symbol<'a>) -> Option<TypeRef> {
        match self.variables.get(name) {
            Some(typ) => Some(*typ),
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

#[derive(Debug)]
pub struct Sources<'a> {
    pub documents: BTreeMap<&'a str, parser::Document<'a>>,
}

macro_rules! builtins {
    ($scope:expr, {$(fn $name:ident($($arg:expr),*): $ret:expr;)*}) => {$(
        $scope.register_function(
            FunctionSignature {
                name: stringify!($name).into(),
                args: vec![$($arg,)*],
            },
            $ret,
        );
    )*};
    ($typer:expr, {$(struct $name:ident {$($field:ident: $typ:expr,)*})*}) => {$(
        #[allow(non_snake_case, unused)]
        let $name = $typer.register_type(
            TypeDef::Struct {
                name: stringify!($name).into(),
                fields: vec![$((stringify!($field).into(), $typ),)*],
                origin: Origin::Builtin,
            },
        );
    )*};
}

impl<'a> Sources<'a> {
    pub fn type_check(self) -> Program<'a> {
        // TODO: Add structs to the resulting Program
        let mut structs = BTreeMap::new();
        let mut uniforms = BTreeMap::new();
        let mut functions = BTreeMap::new();

        let mut typer = Typer::new();
        let mut scope = Scope::new();

        let void = typer.void();
        let int = typer.int();
        let float = typer.float();
        let str = typer.str();

        builtins!(typer, {
            struct vec2 {
                x: float,
                y: float,
            }
            struct vec3 {
                x: float,
                y: float,
            }
            struct vec4 {
                x: float,
                y: float,
            }
            struct sampler2D {}
            struct Me {
                pos: vec3,
            }
        });

        builtins!(scope, {
            fn texture(sampler2D, vec2): vec4;
            fn textureSize(sampler2D, int): vec2;

            fn vec2(float, float): vec2;
            fn vec2(float, int): vec2;
            fn vec2(int, float): vec2;

            fn vec3(vec4): vec3;
            fn vec3(float, float, float): vec3;
            fn vec3(int): vec3;

            fn dot(vec3, vec3): float;
            fn mod(float, int): float;

            // This is a mess...
            fn print(str): void;
            fn print(str, int): void;
            fn print(str, str): void;
            fn print(str, str, str): void;
            fn print(str, int, str): void;
            fn print(str, int, int): void;
            fn print(str, str, int): void;
            fn print(str, str, str, str): void;
            fn print(str, int, str, str): void;
            fn print(str, int, int, str): void;
            fn print(str, str, int, str): void;
            fn print(str, str, str, int): void;
            fn print(str, int, str, int): void;
            fn print(str, int, int, int): void;
            fn print(str, str, int, int): void;
        });

        for (_name, doc) in self.documents {
            for decl in &doc.declerations {
                match decl {
                    parser::Decleration::FuncDef(func_decl) => {
                        scope.register_function(
                            FunctionSignature {
                                name: func_decl.name.clone(),
                                args: func_decl
                                    .args
                                    .iter()
                                    .map(|(_, typ)| typer.convert(typ))
                                    .collect(),
                            },
                            typer.convert(&func_decl.return_type),
                        );
                    }
                    parser::Decleration::Uniforms { uniforms } => {
                        for (name, typ) in uniforms {
                            scope.register_variable((*name).into(), typer.convert(typ))
                        }
                    }
                    parser::Decleration::Struct(parser::StructDef { name, fields }) => {
                        typer.register_type(TypeDef::Struct {
                                name: (*name).into(),
                                fields: fields
                                    .iter()
                                    .map(|(name, typ)| ((*name).into(), typer.convert(typ)))
                                    .collect(),

                                // TODO
                                origin: Origin::Builtin,
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
                            scope.register_variable(name.clone(), typer.convert(typ));
                        }
                        let func = FunctionDef::infer(&typer, &mut scope, func_decl);
                        functions.insert(func_decl.name.clone(), func);
                    }
                    parser::Decleration::Struct(s) => {
                        let s = StructDef {
                            name: s.name,
                            fields: s
                                .fields
                                .iter()
                                .map(|(name, typ)| (*name, typer.convert(typ)))
                                .collect(),
                        };
                        structs.insert(s.name, s);
                    }
                    parser::Decleration::Uniforms { uniforms: us } => {
                        for (name, typ) in us {
                            uniforms.insert(*name, typer.convert(typ));
                        }
                    }
                }
            }
        }

        Program {
            typer,
            structs,
            uniforms,
            functions,
        }
    }
}
