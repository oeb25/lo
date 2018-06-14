use itertools::Itertools;

use infer::{Block, Document, Expression, ExpressionKind, Literal, Operator, Primitive, Type};

fn indent(s: &str) -> String {
    s.lines()
        .map(|s| format!("    {}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

#[derive(Debug)]
enum CLExpression<'a> {
    Symbol(&'a str),
    Index(Box<CExpression<'a>>, Box<CExpression<'a>>),
}

#[derive(Debug)]
enum CExpression<'a> {
    L(CLExpression<'a>),
    Literal(Literal<'a>),
}

#[derive(Debug)]
enum Statement<'a> {
    Expression(CExpression<'a>),
    If {
        condition: CExpression<'a>,
        if_block: CBlock<'a>,
        else_block: Option<CBlock<'a>>,
    },
}

#[derive(Debug)]
struct CBlock<'a> {
    statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
struct Function<'a> {
    block: CBlock<'a>,
}

impl Primitive {
    pub fn to_c(&self) -> String {
        use Primitive::*;
        match self {
            Void => "void",
            Boolean => "bool",
            Int => "int",
            Float => "float",
            Str => "str",
            Sampler2D => "sampler2D",
        }.to_string()
    }
}

impl<'a> Type<'a> {
    pub fn to_c(&self) -> String {
        match self {
            Type::Primitive(p) => p.to_c(),
            Type::UserDefined(p) => p.to_string(),
        }
    }
}

impl<'a> Literal<'a> {
    pub fn to_c(self) -> String {
        match self {
            Literal::String(s) => format!("{}", s),
            Literal::Int(i) => format!("{}", i),
            Literal::Float(f) => format!("{}", f),
        }
    }
}

impl<'a> Expression<'a> {
    pub fn to_c_at_precedence(self, return_case: ReturnCase, level: usize) -> String {
        let ret = match return_case {
            ReturnCase::Ignore => "".to_string(),
            ReturnCase::Return => "return ".to_string(),
            ReturnCase::AssignTo(name) => format!("{} = ", name),
        };

        match self.kind {
            ExpressionKind::Symbol(s) => format!("{}{}", ret, s.to_string()),
            ExpressionKind::Literal(lit) => format!("{}{}", ret, lit.to_c()),
            ExpressionKind::LetDef { name, initializer } => match initializer {
                Ok(expr) => format!(
                    "{} {} = {}",
                    expr.typ.to_c(),
                    name,
                    expr.to_c(ReturnCase::Ignore)
                ),
                Err(typ) => format!("{} {}", typ.to_c(), name),
            },
            ExpressionKind::If {
                condition,
                if_block,
                else_block,
            } => format!(
                "if ({}) {{{};\n}}{}",
                condition.to_c(ReturnCase::Ignore),
                indent(&if_block.to_c(return_case)),
                match else_block {
                    Some(else_block) => {
                        format!(" else {{{};\n}}", indent(&else_block.to_c(return_case)))
                    }
                    None => "".to_string(),
                }
            ),
            ExpressionKind::FunctionCall {
                name,
                args,
                return_type: _,
            } => {
                let args = args
                    .into_iter()
                    .map(|expr| expr.to_c(ReturnCase::Ignore))
                    .join(", ");
                format!("{}{}({})", ret, name, args)
            }
            ExpressionKind::Operator(lhs, op, rhs) => {
                let pres = op.precedence();
                let lhs = lhs.to_c_at_precedence(ReturnCase::Ignore, pres);
                let rhs = rhs.to_c_at_precedence(ReturnCase::Ignore, pres);
                if pres <= level {
                    format!("{}({} {} {})", ret, lhs, op, rhs)
                } else {
                    format!("{}{} {} {}", ret, lhs, op, rhs)
                }
            }
            ExpressionKind::Block(block) => format!("{{\n{}\n}}", indent(&block.to_c(return_case))),
            x => unimplemented!("{:?}", x),
        }
    }
    pub fn to_c(self, return_case: ReturnCase) -> String {
        self.to_c_at_precedence(return_case, 0)
    }
    pub fn prepare(self, return_case: ReturnCase) -> (Expression<'a>, Vec<Expression<'a>>) {
        match self.kind {
            ExpressionKind::Symbol(_) => (self, vec![]),
            ExpressionKind::Literal(_) => (self, vec![]),
            ExpressionKind::LetDef { name, initializer } => {
                let (initializer, prerequisites) = match initializer {
                    Ok(expr) => {
                        let (expr, prerequisites) = expr.prepare(return_case);
                        (Ok(box expr), prerequisites)
                    }
                    Err(typ) => (Err(typ), vec![]),
                };
                (
                    Expression::new(ExpressionKind::LetDef { name, initializer }, self.typ),
                    prerequisites,
                )
            }
            ExpressionKind::FunctionCall {
                name,
                args,
                return_type,
            } => {
                let mut prerequisites = vec![];
                let args = args
                    .into_iter()
                    .map(|x| {
                        let (x, mut p) = x.prepare(return_case);
                        prerequisites.append(&mut p);
                        x
                    })
                    .collect();
                (
                    Expression::new(
                        ExpressionKind::FunctionCall {
                            name,
                            args,
                            return_type,
                        },
                        self.typ,
                    ),
                    prerequisites,
                )
            }
            ExpressionKind::If {
                condition,
                if_block,
                else_block: _,
            } => {
                let name = "random_if_name_1";

                let mut prerequisites = vec![];
                let predefine = Expression::new(
                    ExpressionKind::LetDef {
                        name,
                        initializer: Err(self.typ.clone()),
                    },
                    Primitive::Void.into(),
                );
                let mut stmts: Vec<_> = if_block
                    .statements
                    .into_iter()
                    .map(|stmt| {
                        let (stmt, mut ps) = stmt.prepare(ReturnCase::Ignore);
                        prerequisites.append(&mut ps);
                        stmt
                    })
                    .collect();
                prerequisites.push(predefine);

                let ret_typ = self.typ.clone();

                stmts.push(Expression::new(
                    ExpressionKind::Operator(
                        box Expression::new(ExpressionKind::Symbol(name), self.typ.clone()),
                        Operator::Equal,
                        if_block.return_item.unwrap(),
                    ),
                    ret_typ,
                ));

                (
                    Expression::new(ExpressionKind::Symbol(name), self.typ),
                    prerequisites,
                )
            }
            ExpressionKind::Block(block) => {
                let name = "random_block_name_1";
                match block.return_item {
                    Some(return_item) => {
                        let mut prerequisites = vec![];
                        let predefine = Expression::new(
                            ExpressionKind::LetDef {
                                name,
                                initializer: Err(self.typ.clone()),
                            },
                            Primitive::Void.into(),
                        );
                        let mut stmts: Vec<_> = block
                            .statements
                            .into_iter()
                            .map(|stmt| {
                                let (stmt, mut ps) = stmt.prepare(ReturnCase::Ignore);
                                prerequisites.append(&mut ps);
                                stmt
                            })
                            .collect();
                        prerequisites.push(predefine);

                        let ret_typ = return_item.typ.clone();

                        stmts.push(Expression::new(
                            ExpressionKind::Operator(
                                box Expression::new(ExpressionKind::Symbol(name), self.typ.clone()),
                                Operator::Equal,
                                return_item,
                            ),
                            ret_typ,
                        ));
                        prerequisites.push(Expression::new(
                            ExpressionKind::Block(Block {
                                statements: stmts,
                                return_item: None,
                            }),
                            Primitive::Void.into(),
                        ));
                        (
                            Expression::new(ExpressionKind::Symbol(name), self.typ),
                            prerequisites,
                        )
                    }
                    None => unimplemented!(),
                }
            }
            x => unimplemented!("{:?}", x),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ReturnCase<'a> {
    Ignore,
    Return,
    AssignTo(&'a str),
}

impl<'a> Block<'a> {
    pub fn prepare(self, return_case: ReturnCase<'a>) -> Block<'a> {
        let mut stmts = self
            .statements
            .into_iter()
            .flat_map(|stmt| {
                let (stmt, mut prerequisites) = stmt.prepare(ReturnCase::Ignore);

                prerequisites.push(stmt);
                prerequisites
            }).collect();

        let return_item = self
            .return_item
            .map(|stmt| {
                let (stmt, mut prerequisites) = stmt.prepare(ReturnCase::Ignore);

                if prerequisites.len() > 0 {
                    prerequisites.push(stmt);
                    unimplemented!("{:?}", prerequisites)
                } else {
                    box stmt
                }
            });

        // TODO: Return Item

        Block {
            statements: stmts,
            return_item,
        }
    }
    pub fn to_c(self, return_case: ReturnCase<'a>) -> String {
        let stmts = self
            .statements
            .into_iter()
            .map(|stmt| {
                // let (stmt, prerequisites) = stmt.prepare(ReturnCase::Ignore);

                // let ps = prerequisites
                //     .into_iter()
                //     .map(|p| p.to_c(ReturnCase::Ignore))
                //     .join("\n;");
                // format!("{};\n{}", ps, stmt.to_c(ReturnCase::Ignore))
                format!("{}", stmt.to_c(ReturnCase::Ignore))
            })
            .join(";\n");
        let ret = self
            .return_item
            .map(|r| {
                let (stmt, prerequisites) = r.prepare(ReturnCase::Ignore);

                let ps = prerequisites
                    .into_iter()
                    .map(|p| p.to_c(ReturnCase::Ignore))
                    .join("\n;");
                format!("{};\n{}", ps, stmt.to_c(return_case))
            })
            .unwrap_or_else(|| "".to_string());
        format!("{}\n{}", stmts, ret)
    }
}

impl<'a> Document<'a> {
    pub fn to_c(self) -> String {
        let structs = self
            .structs
            .iter()
            .map(|(name, s)| {
                let fields = s
                    .fields
                    .iter()
                    .map(|(name, typ)| format!("{} {};", typ.to_c(), name))
                    .join("\n");

                format!(
                    "typedef struct {name} {{{fields}}} {name};\n",
                    name = name,
                    fields = indent(&fields)
                )
            })
            .join("\n");

        let functions = self
            .functions
            .into_iter()
            .map(|(name, func)| {
                let args = func
                    .args
                    .iter()
                    .map(|(name, typ)| format!("{} {}", typ.to_c(), name))
                    .join(",");
                let body = func.body.prepare(ReturnCase::Return).to_c(ReturnCase::Return);
                format!(
                    "{} {}({}) {{\n{}\n}}",
                    func.return_type.to_c(),
                    name,
                    args,
                    indent(&body)
                )
            })
            .join("\n");

        format!("{}\n{}\n", structs, functions)
    }
}
