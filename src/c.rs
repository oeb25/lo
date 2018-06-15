#[allow(unused)]
use std::borrow::Cow;

use ast::{self, Literal, Operator, StructDef, Symbol, TypeDef, TypeRef, Typer};

use itertools::Itertools;

#[derive(Debug, Clone)]
enum LValue<'a> {
    Symbol(Symbol<'a>),
    Field(Box<Expression<'a>>, Symbol<'a>),
    Index(Box<Expression<'a>>, Box<Expression<'a>>),
    FunctionCall(Box<Expression<'a>>, Vec<Expression<'a>>),
}

#[derive(Debug, Clone)]
enum Expression<'a> {
    LValue(LValue<'a>),
    Literal(Literal<'a>),
    Operator(Box<Expression<'a>>, Operator, Box<Expression<'a>>),
    FunctionCall(Symbol<'a>, Vec<Expression<'a>>),
}

impl<'a> Into<Expression<'a>> for LValue<'a> {
    fn into(self) -> Expression<'a> {
        Expression::LValue(self)
    }
}

#[derive(Debug)]
enum Statement<'a> {
    Expression(Expression<'a>),
    Return(Expression<'a>),
    If {
        condition: Expression<'a>,
        if_block: Block<'a>,
        else_block: Option<Block<'a>>,
    },
    Declaration(TypeRef, Symbol<'a>, Option<Expression<'a>>),
    Block(Block<'a>),
    For {
        name: Symbol<'a>,
        from: Expression<'a>,
        to: Expression<'a>,
        body: Block<'a>,
    },
}

#[derive(Debug)]
struct Block<'a> {
    statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
struct Function<'a> {
    name: Symbol<'a>,
    arguments: Vec<(Symbol<'a>, TypeRef)>,
    return_type: TypeRef,
    body: Block<'a>,
}

#[derive(Debug)]
pub struct Program<'a> {
    // In C the decleration order is important, so we store in a Vec, instead of a map
    structs: Vec<StructDef<'a>>,
    functions: Vec<Function<'a>>,
}

#[derive(Debug, Default)]
struct Namer {
    used: ::std::collections::HashSet<String>,
    counter: usize,
}

impl Namer {
    fn gen_name<'a>(&mut self, prefix: &str) -> Symbol<'a> {
        let name = format!("{}{}", prefix, self.counter);
        self.counter += 1;
        if self.used.contains(&name) {
            self.gen_name(prefix)
        } else {
            self.used.insert(name.clone());
            name.into()
        }
    }
}

impl<'a> ast::Program<'a> {
    pub fn to_c(&self) -> Program {
        let mut namer = Namer::default();

        let mut functions = vec![];

        for (_, func) in &self.functions {
            let name = func.name.clone();
            let arguments = func
                .args
                .iter()
                .map(|(name, typ)| (name.clone(), typ.clone()))
                .collect();
            let return_type = func.return_type.clone();

            let mut statements = vec![];

            for stmt in &func.body.statements {
                let (mut stmts, expr) = stmt.to_c_statements(&self.typer, &mut namer);
                statements.append(&mut stmts);
                if let Some(expr) = expr {
                    statements.push(Statement::Expression(expr));
                }
            }

            if let Some(stmt) = &func.body.return_item {
                let (mut stmts, expr) = stmt.to_c_statements(&self.typer, &mut namer);
                statements.append(&mut stmts);
                if let Some(expr) = expr {
                    statements.push(Statement::Return(expr));
                }
            }

            let body = Block { statements };

            let function = Function {
                name,
                arguments,
                return_type,
                body,
            };
            functions.push(function);
        }

        Program {
            functions,
            structs: self.structs.iter().map(|(_, s)| s.clone()).collect(),
        }
    }
}

const EXPRESSION_REQUIED: &str = "Expression in expression-only place did not return an expression";

impl<'a> ast::Expression<'a> {
    fn to_c_statements(
        &self,
        typer: &Typer<'a>,
        namer: &mut Namer,
    ) -> (Vec<Statement<'a>>, Option<Expression<'a>>) {
        use ast::ExpressionKind as Ek;

        match &self.kind {
            Ek::Literal(lit) => (vec![], Some(Expression::Literal(lit.clone()))),
            Ek::LetDef { name, initializer } => match initializer {
                Ok(initializer) => {
                    let typ = initializer.typ.clone();
                    let (mut pre, expr) = initializer.to_c_statements(typer, namer);

                    pre.push(Statement::Declaration(typ, name.clone(), expr));

                    (pre, None)
                }
                Err(typ) => (
                    vec![Statement::Declaration(typ.clone(), name.clone(), None)],
                    None,
                ),
            },
            Ek::Symbol(name) => (vec![], Some(LValue::Symbol(name.clone()).into())),
            Ek::Operator(lhs, op, rhs) => {
                let (mut lpre, lhs) = lhs.to_c_statements(typer, namer);
                let (mut rpre, rhs) = rhs.to_c_statements(typer, namer);

                let (lhs, rhs) = (
                    lhs.expect(EXPRESSION_REQUIED),
                    rhs.expect(EXPRESSION_REQUIED),
                );

                lpre.append(&mut rpre);

                (
                    lpre,
                    Some(Expression::Operator(box lhs, op.clone(), box rhs)),
                )
            }
            Ek::Block(block) => {
                let (block, new_variable) =
                    block.to_c_statements(namer.gen_name("block_name"), typer, namer);
                match new_variable {
                    Some((pre, new_variable)) => {
                        (vec![pre, Statement::Block(block)], Some(new_variable))
                    }
                    None => (vec![Statement::Block(block)], None),
                }
            }
            Ek::FunctionCall {
                name,
                args,
                return_type,
            } => {
                if *name == "print".into() {
                    let mut args_i = args.iter();

                    let s = match args_i.next().expect("no initial string passed to print") {
                        ast::Expression {
                            kind: Ek::Literal(Literal::String(s)),
                            ..
                        } => s,
                        x => {
                            unimplemented!("first argument of print must me string, found {:?}", x)
                        }
                    };

                    let mut splitted = s.split("%");
                    let mut f_s = splitted.next().unwrap().to_string();

                    for segment in splitted {
                        let next = args_i
                            .next()
                            .expect(&format!("not enourgh arguments for print: {:?}", args));
                        f_s += match typer.get(&next.typ) {
                            TypeDef::Int => "%d",
                            TypeDef::Str => "%s",
                            x => unimplemented!("Cannot print type: {:?}", x),
                        };
                        f_s += segment;
                    }

                    let mut args = args.clone();
                    args[0] =
                        ast::Expression::new(Ek::Literal(Literal::String(f_s.into())), typer.str());

                    return ast::Expression::new(
                        Ek::FunctionCall {
                            name: "printf".into(),
                            args,
                            return_type: return_type.clone(),
                        },
                        return_type.clone(),
                    ).to_c_statements(typer, namer);
                }

                let mut pre = vec![];
                let mut new_args = vec![];

                for arg in args {
                    let (mut arg_pre, arg) = arg.to_c_statements(typer, namer);
                    pre.append(&mut arg_pre);
                    new_args.push(arg.expect(EXPRESSION_REQUIED));
                }

                (pre, Some(Expression::FunctionCall(name.clone(), new_args)))
            }
            Ek::If {
                condition,
                if_block,
                else_block,
            } => {
                let (mut stmts, condition) = condition.to_c_statements(typer, namer);
                let condition = condition.expect(EXPRESSION_REQUIED);
                let name = namer.gen_name("if_block_name_");

                let (if_block, new_variable) = if_block.to_c_statements(name.clone(), typer, namer);
                let else_block = match else_block {
                    Some(else_block) => {
                        // Ignore seconds variable, since we get all we need from the first block.
                        let (else_block, _) =
                            else_block.to_c_statements(name.clone(), typer, namer);
                        Some(else_block)
                    }
                    None => None,
                };

                match new_variable {
                    Some((definition, expr)) => {
                        stmts.push(definition);
                        stmts.push(Statement::If {
                            condition,
                            if_block,
                            else_block,
                        });
                        (stmts, Some(expr))
                    }
                    None => {
                        stmts.push(Statement::If {
                            condition,
                            if_block,
                            else_block,
                        });
                        (stmts, None)
                    }
                }
            }
            Ek::Field(expr, field) => {
                let (pre, expr) = expr.to_c_statements(typer, namer);
                let expr = expr.expect(EXPRESSION_REQUIED);

                (pre, Some(LValue::Field(box expr, (*field).into()).into()))
            }
            Ek::For {
                name,
                from,
                to,
                body,
            } => {
                let (mut pre, from) = from.to_c_statements(typer, namer);
                let (mut to_pre, to) = to.to_c_statements(typer, namer);

                pre.append(&mut to_pre);

                // Ignore return item of loop body
                let (body, _) = body.to_c_statements("for_block_".into(), typer, namer);

                let (from, to, body) = (
                    from.expect(EXPRESSION_REQUIED),
                    to.expect(EXPRESSION_REQUIED),
                    body,
                );

                pre.push(Statement::For {
                    name: name.clone(),
                    from,
                    to,
                    body,
                });

                (pre, None)
            }
        }
    }
}

impl<'a> ast::Block<'a> {
    fn to_c_statements(
        &self,
        name: Symbol<'a>,
        typer: &Typer<'a>,
        namer: &mut Namer,
    ) -> (Block<'a>, Option<(Statement<'a>, Expression<'a>)>) {
        let mut statements = vec![];

        let new_variable: Option<(_, Expression, _)> = if let Some(return_item) = &self.return_item
        {
            Some((
                Statement::Declaration(self.return_type().clone(), name.clone(), None),
                LValue::Symbol(name).into(),
                return_item,
            ))
        } else {
            None
        };

        for stmt in &self.statements {
            let (mut stmts, expr) = stmt.to_c_statements(typer, namer);
            statements.append(&mut stmts);
            if let Some(expr) = expr {
                statements.push(Statement::Expression(expr));
            }
        }

        if let Some((_, sym, stmt)) = &new_variable {
            let (mut stmts, expr) = stmt.to_c_statements(typer, namer);
            statements.append(&mut stmts);
            if let Some(expr) = expr {
                let sym: Expression = sym.clone();
                let assignment = Expression::Operator(box sym, Operator::Equal, box expr);
                statements.push(Statement::Expression(assignment));
            }
        }

        let block = Block { statements };

        (block, new_variable.map(|(a, b, _)| (a, b)))
    }
}

fn indent(s: &str) -> String {
    s.lines()
        .map(|s| format!("    {}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

impl<'a> Program<'a> {
    pub fn print(&self, typer: &Typer) -> String {
        let mut out = vec!["#include <stdio.h>".to_string()];

        for s in &self.structs {
            let fields = s
                .fields
                .iter()
                .map(|(name, typ)| format!("{} {};", typ.print(typer), name))
                .join("\n");

            let s_str = format!(
                "typedef struct {} {{\n{}\n}} {};",
                s.name,
                indent(&fields),
                s.name,
            );
            out.push(s_str);
        }

        for func in &self.functions {
            out.push(func.print(typer));
        }

        out.join("\n\n")
    }
}

impl<'a> Function<'a> {
    fn print(&self, typer: &Typer) -> String {
        let args = self
            .arguments
            .iter()
            .map(|(name, typ)| format!("{} {}", typ.print(typer), name))
            .join(", ");
        let body = self.body.print(typer);
        format!(
            "{} {}({}) {}",
            self.return_type.print(typer),
            self.name,
            args,
            body
        )
    }
}

impl<'a> Block<'a> {
    fn print(&self, typer: &Typer) -> String {
        let body = self
            .statements
            .iter()
            .map(|stmt| stmt.print(typer))
            .join(";\n") + ";\n";
        format!("{{\n{}\n}}", indent(&body))
    }
}

impl<'a> Statement<'a> {
    fn print(&self, typer: &Typer) -> String {
        match self {
            Statement::Declaration(typ, name, expr) => match expr {
                Some(expr) => format!("{} {} = {}", typ.print(typer), name, expr.print(typer)),
                None => format!("{} {}", typ.print(typer), name),
            },
            Statement::If {
                condition,
                if_block,
                else_block,
            } => {
                let mut out = vec![];

                out.push(format!("if ({})", condition.print(typer)));
                out.push(if_block.print(typer));
                if let Some(else_block) = else_block {
                    out.push(format!("else {}", else_block.print(typer)));
                }

                out.join(" ")
            }
            Statement::Expression(expr) => expr.print(typer),
            Statement::Return(expr) => format!("return {}", expr.print(typer)),
            Statement::Block(block) => block.print(typer),
            Statement::For {
                name,
                from,
                to,
                body,
            } => format!(
                "for (int {} = {}; {} < {}; ++{}) {}",
                name,
                from.print(typer),
                name,
                to.print(typer),
                name,
                body.print(typer)
            ),
        }
    }
}

impl<'a> Expression<'a> {
    fn print_with_precedence(&self, typer: &Typer, precedence: usize) -> String {
        match self {
            Expression::Operator(lhs, op, rhs) => {
                let lhs = lhs.print_with_precedence(typer, op.precedence() + 1);
                let rhs = rhs.print_with_precedence(typer, op.precedence() + 1);

                if precedence > 0 {
                    format!("({} {} {})", lhs, op.to_str(), rhs)
                } else {
                    format!("{} {} {}", lhs, op.to_str(), rhs)
                }
            }
            Expression::LValue(l) => l.print(typer),
            Expression::Literal(l) => l.print(),
            Expression::FunctionCall(name, args) => format!(
                "{}({})",
                name,
                args.iter().map(|arg| arg.print(typer)).join(", ")
            ),
        }
    }
    fn print(&self, typer: &Typer) -> String {
        self.print_with_precedence(typer, 0)
    }
}

impl<'a> Literal<'a> {
    pub fn print(&self) -> String {
        match self {
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Int(i) => format!("{}", i),
            Literal::Float(f) => format!("{}", f),
        }
    }
}

impl<'a> LValue<'a> {
    fn print(&self, typer: &Typer) -> String {
        match self {
            LValue::Symbol(name) => name.to_string(),
            LValue::Field(expr, field) => format!("{}.{}", expr.print(typer), field),
            x => unimplemented!("{:?}", x),
        }
    }
}

impl<'a> TypeRef {
    fn print(&self, typer: &Typer<'a>) -> Cow<'a, str> {
        use self::TypeDef::*;
        match typer.get(self) {
            Void => "void".into(),
            Int => "int".into(),
            Float => "float".into(),
            Str => "char*".into(),
            Boolean => "bool".into(),
            Struct { name, .. } => name.clone(),
        }
    }
}
