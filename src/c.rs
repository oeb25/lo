#[allow(unused)]
use itertools::Itertools;

use ast::{self, Literal};

#[derive(Debug)]
enum LValue<'a> {
    Symbol(&'a str),
    Index(Box<Expression<'a>>, Box<Expression<'a>>),
    FunctionCall(Box<Expression<'a>>, Vec<Expression<'a>>),
}

#[derive(Debug)]
enum Expression<'a> {
    LValue(LValue<'a>),
    Literal(Literal<'a>),
}

#[derive(Debug)]
enum Statement<'a> {
    Expression(Expression<'a>),
    If {
        condition: Expression<'a>,
        if_block: Block<'a>,
        else_block: Option<Block<'a>>,
    },
}

#[derive(Debug)]
struct Block<'a> {
    statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
struct Function<'a> {
    block: Block<'a>,
}

impl<'a> ast::Program<'a> {

}
