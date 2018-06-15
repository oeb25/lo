use ast::{Literal, Operator, Seperator, Symbol};
use std::{borrow::Cow, fmt, vec};

impl Operator {
    fn try_from_char(c: char) -> Option<Operator> {
        use ast::Operator::*;
        match c {
            '/' => Some(Divide),
            '=' => Some(Equal),
            '.' => Some(Dot),
            '*' => Some(Star),
            '+' => Some(Plus),
            '-' => Some(Hyphen),
            '!' => Some(ExclamationMark),
            '>' => Some(Greater),
            '<' => Some(Less),
            _ => None,
        }
    }
    fn try_extend(self, c: char) -> Result<Operator, (Operator, char)> {
        use ast::Operator::*;
        match (self, c) {
            (Plus, '=') => Ok(PlusEqual),
            (Equal, '=') => Ok(DoubleEqual),
            (ExclamationMark, '=') => Ok(NotEqual),
            (o, c) => Err((o, c)),
        }
    }
    pub fn precedence(&self) -> usize {
        use ast::Operator::*;
        match self {
            Equal => 0,
            Dot => 0,
            Divide => 2,
            Star => 2,
            Plus => 0,
            Hyphen => 1,
            Range => 10,
            PlusEqual => 0,
            DoubleEqual => 0,
            ExclamationMark => 0,
            NotEqual => 0,
            Greater => 0,
            Less => 0,
        }
    }
}

impl Seperator {
    fn try_from_char(c: char) -> Option<Seperator> {
        use ast::Seperator::*;
        match c {
            '(' => Some(OpenParen),
            ')' => Some(CloseParen),
            '{' => Some(OpenBrace),
            '}' => Some(CloseBrace),
            ':' => Some(Colon),
            ';' => Some(SimiColon),
            ',' => Some(Comma),
            _ => None,
        }
    }
}

impl<'a> Into<Token<'a>> for Operator {
    fn into(self) -> Token<'a> {
        Token::Operator(self)
    }
}

impl<'a> Into<Token<'a>> for Seperator {
    fn into(self) -> Token<'a> {
        Token::Seperator(self)
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use ast::Operator::*;
        write!(
            fmt,
            "{}",
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
        )
    }
}

#[derive(Debug, PartialEq)]
enum Token<'a> {
    Ident(&'a str),
    String(&'a str),
    Number(&'a str),
    Float(&'a str),
    Seperator(Seperator),
    Operator(Operator),
}

#[derive(Debug)]
enum LexState {
    Initial(usize),
    Operator(Operator),
    String(char, bool, usize),
    Ident(usize),
    Number(usize),
    Float(usize),
    MultiLineComment(usize, bool),
}

fn lex<'a>(input: &'a str) -> Tokens<'a> {
    use self::LexState::*;
    use ast::Operator as Op;

    let mut tokens = vec![];

    let mut lex_state = Initial(0);

    for (i, c) in input.chars().enumerate() {
        lex_state = match lex_state {
            Initial(s) => match c {
                ' ' | '\n' | '\t' => Initial(s),
                c if c.is_alphabetic() => Ident(i),
                c if c.is_numeric() => Number(i),
                '\'' => String(c, false, i),
                '"' => String(c, false, i),
                c => {
                    if let Some(sep) = Seperator::try_from_char(c) {
                        tokens.push(sep.into());
                        Initial(i)
                    } else if let Some(op) = Op::try_from_char(c) {
                        Operator(op)
                    } else {
                        unimplemented!("{:?} {:?}", lex_state, c)
                    }
                }
            },
            MultiLineComment(s, last_was_star) => match c {
                '/' if last_was_star => {
                    // TODO: Maybe push the comment
                    Initial(i)
                }
                '*' => MultiLineComment(s, true),
                _ => MultiLineComment(s, false),
            },
            Ident(s) => match c {
                c if c.is_alphanumeric() => Ident(s),
                '_' => Ident(s),
                ' ' | '\n' => {
                    tokens.push(Token::Ident(&input[s..i]));
                    Initial(i)
                }
                c => {
                    tokens.push(Token::Ident(&input[s..i]));
                    if let Some(sep) = Seperator::try_from_char(c) {
                        tokens.push(sep.into());
                        Initial(i)
                    } else if let Some(op) = Op::try_from_char(c) {
                        Operator(op)
                    } else {
                        unimplemented!("{:?} {:?}", lex_state, c)
                    }
                }
            },
            String(cc, true, s) => String(cc, false, s),
            String(cc, _, s) if c == '\\' => String(cc, true, s),
            String(cc, _, s) if c == cc => {
                tokens.push(Token::String(&input[s + 1..i]));
                Initial(i)
            }
            String(cc, _, s) => String(cc, false, s),
            Number(s) => match c {
                c if c.is_numeric() => Number(s),
                '.' => Float(s),
                '\n' | ' ' => {
                    tokens.push(Token::Number(&input[s..i]));
                    Initial(i)
                }
                c => {
                    tokens.push(Token::Number(&input[s..i]));
                    if let Some(sep) = Seperator::try_from_char(c) {
                        tokens.push(sep.into());
                        Initial(i)
                    } else if let Some(op) = Op::try_from_char(c) {
                        Operator(op)
                    } else {
                        unimplemented!("{:?} {:?}", lex_state, c)
                    }
                }
            },
            Float(s) => match c {
                c if c.is_numeric() => Float(s),
                '\n' | ' ' => {
                    tokens.push(Token::Float(&input[s..i]));
                    Initial(i)
                }
                '.' if s + 2 == i => {
                    tokens.push(Token::Number(&input[s..i - 1]));
                    tokens.push(Op::Range.into());
                    Initial(i)
                }
                c => {
                    tokens.push(Token::Float(&input[s..i]));
                    if let Some(sep) = Seperator::try_from_char(c) {
                        tokens.push(sep.into());
                    } else if let Some(op) = Op::try_from_char(c) {
                        tokens.push(op.into());
                    } else {
                        unimplemented!("{:?} {:?}", lex_state, c)
                    }
                    Initial(i)
                }
            },
            Operator(op) => match (op, c) {
                (self::Operator::Divide, '*') => MultiLineComment(i - 1, false),
                (_, c) => match op.try_extend(c) {
                    Ok(op) => Operator(op),
                    Err((op, c)) => match c {
                        ' ' => {
                            tokens.push(op.into());
                            Initial(i)
                        }
                        c if c.is_alphanumeric() => {
                            tokens.push(op.into());
                            Ident(i)
                        }
                        c => unimplemented!("{:?} {:?}", op, c),
                    },
                },
            },
        }
    }

    Tokens {
        tokens: tokens.into_iter(),
        peeked: None,
    }
}

struct Tokens<'a> {
    pub tokens: vec::IntoIter<Token<'a>>,
    pub peeked: Option<Token<'a>>,
}

impl<'a> Tokens<'a> {
    fn next(&mut self) -> Option<Token<'a>> {
        if self.peeked.is_some() {
            self.peeked.take()
        } else {
            self.tokens.next()
        }
    }
    fn peek(&mut self) -> Option<&Token<'a>> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }
        self.peeked.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a> {
    Name(Cow<'a, str>),
}

impl<'a> Type<'a> {
    pub fn from_str(s: &'a str) -> Type<'a> {
        Type::Name(s.into())
    }
}

#[derive(Debug)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
    Symbol(Symbol<'a>),
    Operator(Box<Expression<'a>>, Operator, Box<Expression<'a>>),
    Field(Box<Expression<'a>>, &'a str),
    Block(Block<'a>),
    FuncCall {
        fun: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
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
    LetDef {
        name: Symbol<'a>,
        typ: Option<Type<'a>>,
        initializer: Option<Box<Expression<'a>>>,
    },
}

#[derive(Debug)]
pub struct Block<'a> {
    pub statements: Vec<Expression<'a>>,
    pub return_item: Option<Box<Expression<'a>>>,
}

#[derive(Debug)]
pub struct StructDef<'a> {
    pub name: &'a str,
    pub fields: Vec<(&'a str, Type<'a>)>,
}

#[derive(Debug)]
pub enum Decleration<'a> {
    FuncDef(FuncDef<'a>),
    Struct(StructDef<'a>),
    Uniforms { uniforms: Vec<(&'a str, Type<'a>)> },
}

#[derive(Debug)]
pub struct FuncDef<'a> {
    pub name: Symbol<'a>,
    pub args: Vec<(Symbol<'a>, Type<'a>)>,
    pub return_type: Type<'a>,
    pub body: Block<'a>,
    pub is_default: bool,
}

fn parse<'a>(input: &mut Tokens<'a>) -> Document<'a> {
    let mut declerations = vec![];

    while input.peek().is_some() {
        let decl = parse_decleration(input);
        declerations.push(decl)
    }

    Document { declerations }
}

#[derive(Debug)]
pub struct Document<'a> {
    pub declerations: Vec<Decleration<'a>>,
}

fn parse_decleration<'a>(input: &mut Tokens<'a>) -> Decleration<'a> {
    macro_rules! expect {
        () => {
            input.next().unwrap()
        };
        ($t:expr) => {
            let x: Token = $t.into();
            assert_eq!(x, input.next().unwrap())
        };
    }

    match input.next().unwrap() {
        Token::Ident("fun") => {
            let name = if let Token::Ident(name) = input.next().unwrap() {
                name.into()
            } else {
                unimplemented!()
            };

            expect!(Seperator::OpenParen);

            let mut args = vec![];

            'args_loop: loop {
                match input.next().unwrap() {
                    Token::Ident(arg_name) => {
                        expect!(Seperator::Colon);
                        let typ = parse_type(input);
                        args.push((arg_name.into(), typ));
                        match input.next().unwrap() {
                            Token::Seperator(Seperator::Comma) => {}
                            Token::Seperator(Seperator::CloseParen) => {
                                break 'args_loop;
                            }
                            x => unimplemented!("{:?}", x),
                        }
                    }
                    Token::Seperator(Seperator::CloseParen) => {
                        break 'args_loop;
                    }
                    x => unimplemented!("{:?}", x),
                }
            }

            let (return_type, body) = match input.next().unwrap() {
                Token::Seperator(Seperator::Colon) => {
                    let typ = parse_type(input);
                    match input.next().unwrap() {
                        Token::Seperator(Seperator::OpenBrace) => (typ, parse_block(input)),
                        x => unimplemented!("{:?}", x),
                    }
                }
                Token::Seperator(Seperator::OpenBrace) => {
                    (Type::Name("void".into()), parse_block(input))
                }
                x => unimplemented!("{:?}", x),
            };

            Decleration::FuncDef(FuncDef {
                name,
                args,
                return_type,
                body,
                is_default: false,
            })
        }
        Token::Ident("struct") => {
            let name = if let Token::Ident(name) = input.next().unwrap() {
                name
            } else {
                unimplemented!()
            };

            expect!(Seperator::OpenBrace);

            let mut fields = vec![];

            'fields_loop: loop {
                match input.next().unwrap() {
                    Token::Ident(field_name) => {
                        expect!(Seperator::Colon);
                        let typ = parse_type(input);
                        fields.push((field_name, typ));
                        match input.next().unwrap() {
                            Token::Seperator(Seperator::Comma) => {}
                            Token::Seperator(Seperator::CloseBrace) => {
                                break 'fields_loop;
                            }
                            x => unimplemented!("{:?}", x),
                        }
                    }
                    Token::Seperator(Seperator::CloseBrace) => {
                        break 'fields_loop;
                    }
                    x => unimplemented!("{:?}", x),
                }
            }

            Decleration::Struct(StructDef { name, fields })
        }
        Token::Ident("uniforms") => {
            expect!(Seperator::OpenBrace);

            let mut uniforms = vec![];

            'uniforms_loop: loop {
                match input.next().unwrap() {
                    Token::Ident(uniform_name) => {
                        expect!(Seperator::Colon);
                        let typ = parse_type(input);
                        uniforms.push((uniform_name, typ));
                        match input.next().unwrap() {
                            Token::Seperator(Seperator::Comma) => {}
                            Token::Seperator(Seperator::CloseBrace) => {
                                break 'uniforms_loop;
                            }
                            x => unimplemented!("{:?}", x),
                        }
                    }
                    Token::Seperator(Seperator::CloseBrace) => {
                        break 'uniforms_loop;
                    }
                    x => unimplemented!("{:?}", x),
                }
            }

            Decleration::Uniforms { uniforms }
        }
        x => unimplemented!("{:?}", x),
    }
}

fn parse_type<'a>(input: &mut Tokens<'a>) -> Type<'a> {
    match input.next().unwrap() {
        Token::Ident(type_name) => Type::Name(type_name.into()),
        x => unimplemented!("{:?}", x),
    }
}

fn parse_block<'a>(mut input: &mut Tokens<'a>) -> Block<'a> {
    let mut statements = vec![];

    'statement_loop: loop {
        let stmt = match input.peek().unwrap() {
            Token::Seperator(Seperator::CloseBrace) => {
                input.next();
                return Block {
                    statements,
                    return_item: None,
                };
            }
            _ => parse_expr(&mut input),
        };

        match input.next().unwrap() {
            Token::Seperator(Seperator::CloseBrace) => {
                return Block {
                    statements,
                    return_item: Some(box stmt),
                }
            }
            Token::Seperator(Seperator::SimiColon) => {
                statements.push(stmt);
            }
            x => unimplemented!("{:?}", x),
        }
    }
}

fn rec_parse_expr<'a>(input: &mut Tokens<'a>, level: usize) -> Expression<'a> {
    macro_rules! expect {
        () => {
            input.next().unwrap()
        };
        ($t:expr) => {
            let t: Token = $t.into();
            assert_eq!(t, input.next().unwrap())
        };
    }

    match level {
        0 | 1 | 2 | 3 => {
            let mut subject = rec_parse_expr(input, level + 1);

            loop {
                match input.peek() {
                    Some(Token::Operator(op)) if op.precedence() == level => {
                        let op = *op;
                        input.next();
                        let rhs = rec_parse_expr(input, level + 1);
                        subject = Expression::Operator(box subject, op, box rhs)
                    }
                    _ => return subject,
                }
            }
        }
        4 => {
            let mut subject = rec_parse_expr(input, level + 1);

            'this_loop: loop {
                match input.peek() {
                    Some(Token::Seperator(Seperator::OpenParen)) => {
                        input.next();

                        let mut args = vec![];

                        'args_loop: loop {
                            match input.peek().unwrap() {
                                Token::Seperator(Seperator::CloseParen) => {
                                    input.next();
                                    break 'args_loop;
                                }
                                _ => {
                                    let expr = parse_expr(input);
                                    args.push(expr);
                                    match input.next().unwrap() {
                                        Token::Seperator(Seperator::CloseParen) => {
                                            break 'args_loop;
                                        }
                                        Token::Seperator(Seperator::Comma) => {}
                                        x => unimplemented!("{:?}", x),
                                    };
                                }
                            }
                        }

                        subject = Expression::FuncCall {
                            fun: box subject,
                            args,
                        };
                    }
                    Some(Token::Seperator(Seperator::Colon)) => unimplemented!("heh"),
                    Some(Token::Operator(Operator::Dot)) => {
                        input.next();

                        let field = match input.next() {
                            Some(Token::Ident(field)) => field,
                            x => unimplemented!("{:?}", x),
                        };

                        subject = Expression::Field(box subject, field);
                    }
                    _ => {
                        break 'this_loop;
                    }
                }
            }

            subject
        }
        5 => match input.next().unwrap() {
            Token::Seperator(Seperator::OpenBrace) => {
                let block = parse_block(input);
                Expression::Block(block)
            }
            Token::Ident("if") => {
                let condition = box parse_expr(input);
                expect!(Seperator::OpenBrace);
                let if_block = parse_block(input);

                let else_block = match input.peek() {
                    Some(Token::Ident("else")) => {
                        input.next();
                        expect!(Seperator::OpenBrace);
                        Some(parse_block(input))
                    }
                    _ => None,
                };

                Expression::If {
                    condition,
                    if_block,
                    else_block,
                }
            }
            Token::Ident("for") => {
                let name = if let Some(Token::Ident(name)) = input.next() {
                    name.into()
                } else {
                    unimplemented!()
                };

                expect!(Token::Ident("in"));

                let from = box parse_expr(input);
                expect!(Operator::Range);
                let to = box parse_expr(input);
                expect!(Seperator::OpenBrace);

                let body = parse_block(input);

                Expression::For {
                    name,
                    from,
                    to,
                    body,
                }
            }
            Token::Ident("let") => {
                let name = match input.next() {
                    Some(Token::Ident(name)) => name.into(),
                    x => unimplemented!("{:?}", x),
                };

                let typ = match input.peek() {
                    Some(Token::Seperator(Seperator::Colon)) => {
                        input.next();
                        Some(parse_type(input))
                    }
                    _ => None,
                };

                let initializer = match input.peek() {
                    Some(Token::Operator(Operator::Equal)) => {
                        input.next();
                        Some(Box::new(parse_expr(input)))
                    }
                    _ => None,
                };

                Expression::LetDef {
                    name,
                    typ,
                    initializer,
                }
            }
            Token::Ident(name) => Expression::Symbol(name.into()),
            Token::Number(a) => Expression::Literal(Literal::Int(a.parse().unwrap())),
            Token::String(a) => Expression::Literal(Literal::String(a.into())),
            Token::Float(a) => Expression::Literal(Literal::Float(a.parse().unwrap())),
            Token::Seperator(Seperator::OpenParen) => {
                let inner = rec_parse_expr(input, 0);
                expect!(Seperator::CloseParen);
                inner
            }
            x => unimplemented!("expr {:?}", x),
        },
        x => unimplemented!("recurvice decent parser level {}", x),
    }
}

fn parse_expr<'a>(input: &mut Tokens<'a>) -> Expression<'a> {
    rec_parse_expr(input, 0)
}

pub fn parse_str<'a>(src: &'a str) -> Document<'a> {
    let mut tokens = lex(&src);
    parse(&mut tokens)
}
