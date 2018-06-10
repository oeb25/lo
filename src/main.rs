#![feature(nll, box_syntax)]

#[derive(Debug, PartialEq, Clone, Copy)]
enum Operator {
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

#[derive(Debug, PartialEq, Clone, Copy)]
enum Seperator {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Colon,
    SimiColon,
    Comma,
}

impl Operator {
    fn try_from_char(c: char) -> Option<Operator> {
        use Operator::*;
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
        use Operator::*;
        match (self, c) {
            (Plus, '=') => Ok(PlusEqual),
            (Equal, '=') => Ok(DoubleEqual),
            (ExclamationMark, '=') => Ok(NotEqual),
            (o, c) => Err((o, c)),
        }
    }
    fn precedence(&self) -> usize {
        use Operator::*;
        match self {
            Divide => 0,
            Equal => 0,
            Dot => 0,
            Star => 0,
            Plus => 0,
            Hyphen => 0,
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
        use Seperator::*;
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

impl std::fmt::Display for Operator {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        use Operator::*;
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
    String(char, usize),
    Ident(usize),
    Number(usize),
    Float(usize),
}

fn lex<'a>(input: &'a str) -> Tokens<'a> {
    use LexState::*;
    use Operator as Op;

    let mut tokens = vec![];

    let mut lex_state = Initial(0);

    for (i, c) in input.chars().enumerate() {
        lex_state = match lex_state {
            Initial(s) => match c {
                ' ' | '\n' | '\t' => Initial(s),
                c if c.is_alphabetic() => Ident(i),
                c if c.is_numeric() => Number(i),
                '\'' => String(c, i),
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
            String(cc, s) if c == cc => {
                tokens.push(Token::String(&input[s + 1..i]));
                Initial(i)
            }
            String(cc, s) => String(cc, s),
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
            Operator(op) => match op.try_extend(c) {
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
        }
    }

    Tokens {
        tokens: tokens.into_iter(),
        peeked: None,
    }
}

struct Tokens<'a> {
    tokens: std::vec::IntoIter<Token<'a>>,
    peeked: Option<Token<'a>>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum Primitive {
    Void,
    Boolean,
    Int,
    Float,
    Str,
    Sampler2D,
}

impl Primitive {
    fn try_from(name: &str) -> Option<Primitive> {
        match name {
            "bool" => Some(Primitive::Boolean),
            "int" => Some(Primitive::Int),
            "float" => Some(Primitive::Float),
            "str" => Some(Primitive::Str),
            "sampler2D" => Some(Primitive::Sampler2D),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type<'a> {
    Primitive(Primitive),
    UserDefined(&'a str),
}

impl<'a> Into<Type<'a>> for Primitive {
    fn into(self) -> Type<'a> {
        Type::Primitive(self)
    }
}

#[derive(Debug)]
enum Literal<'a> {
    String(&'a str),
    Int(u64),
    Float(f64),
}

#[derive(Debug)]
enum Expression<'a> {
    Literal(Literal<'a>),
    Symbol(&'a str),
    Operator(Box<Expression<'a>>, Operator, Box<Expression<'a>>),
    Field(Box<Expression<'a>>, &'a str),
    FuncCall {
        fun: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
    },
    If {
        condition: Box<Expression<'a>>,
        if_block: Box<Block<'a>>,
        else_block: Option<Box<Block<'a>>>,
    },
    For {
        name: &'a str,
        from: Box<Expression<'a>>,
        to: Box<Expression<'a>>,
        body: Box<Block<'a>>,
    },
    LetDef {
        name: &'a str,
        typ: Option<Type<'a>>,
        initializer: Option<Box<Expression<'a>>>,
    },
}

#[derive(Debug)]
struct Block<'a> {
    statements: Vec<Expression<'a>>,
    return_item: Option<Expression<'a>>,
}

#[derive(Debug)]
struct StructDef<'a> {
    name: &'a str,
    fields: Vec<(&'a str, Type<'a>)>,
}

#[derive(Debug)]
enum Decleration<'a> {
    FuncDef(FuncDef<'a>),
    Struct(StructDef<'a>),
    Uniforms { uniforms: Vec<(&'a str, Type<'a>)> },
}

fn parse<'a>(input: &mut Tokens<'a>) -> Document<'a> {
    let mut declerations = vec![];

    while input.peek().is_some() {
        let decl = parse_decleration(input);
        declerations.push(decl)
    }

    Document { declerations }
}

fn parse_decleration<'a>(input: &mut Tokens<'a>) -> Decleration<'a> {
    macro_rules! next {
        () => {
            input.next()
        };
    }
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
                name
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
                        args.push((arg_name, typ));
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
                    (Primitive::Void.into(), parse_block(input))
                }
                x => unimplemented!("{:?}", x),
            };

            Decleration::FuncDef(FuncDef {
                name,
                args,
                return_type,
                body,
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
        Token::Ident(type_name) => {
            if let Some(prim) = Primitive::try_from(type_name) {
                prim.into()
            } else {
                Type::UserDefined(type_name)
            }
        }
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
                    return_item: Some(stmt),
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
        0 => {
            let lhs = rec_parse_expr(input, level + 1);
            match input.peek() {
                Some(Token::Operator(op)) if op.precedence() == level => {
                    let op = *op;
                    input.next();
                    let rhs = rec_parse_expr(input, level + 1);
                    Expression::Operator(box lhs, op, box rhs)
                }
                _ => lhs,
            }
        }
        1 => {
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
        2 => match input.next().unwrap() {
            Token::Ident("if") => {
                let condition = box parse_expr(input);
                expect!(Seperator::OpenBrace);
                let if_block = box parse_block(input);

                let else_block = match input.peek() {
                    Some(Token::Ident("else")) => {
                        input.next();
                        expect!(Seperator::OpenBrace);
                        Some(box parse_block(input))
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
                    name
                } else {
                    unimplemented!()
                };

                expect!(Token::Ident("in"));

                let from = box parse_expr(input);
                expect!(Operator::Range);
                let to = box parse_expr(input);
                expect!(Seperator::OpenBrace);

                let body = box parse_block(input);

                Expression::For {
                    name,
                    from,
                    to,
                    body,
                }
            }
            Token::Ident("let") => {
                let name = match input.next() {
                    Some(Token::Ident(name)) => name,
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
            Token::Ident(name) => Expression::Symbol(name),
            Token::Number(a) => Expression::Literal(Literal::Int(a.parse().unwrap())),
            Token::String(a) => Expression::Literal(Literal::String(a)),
            Token::Float(a) => Expression::Literal(Literal::Float(a.parse().unwrap())),
            x => unimplemented!("expr {:?}", x),
        },
        x => unimplemented!("recurvice decent parser level {}", x),
    }
}

fn parse_expr<'a>(input: &mut Tokens<'a>) -> Expression<'a> {
    rec_parse_expr(input, 0)
}

fn indent(s: &str) -> String {
    s.lines()
        .map(|s| format!("\t{}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

impl<'a> Type<'a> {
    fn to_c(&self, scope: &Scope<'a>) -> String {
        match self {
            Type::Primitive(p) => match p {
                Primitive::Void => "void",
                Primitive::Boolean => "bool",
                Primitive::Int => "int",
                Primitive::Float => "float",
                Primitive::Str => "char*",
                Primitive::Sampler2D => "sampler2D",
            }.to_owned(),
            Type::UserDefined(name) => name.to_string(),
        }
    }
    fn is_numeric(&self) -> bool {
        match self {
            Type::Primitive(Primitive::Int) | Type::Primitive(Primitive::Float) => true,
            _ => false,
        }
    }
}

use std::collections::HashMap;

#[derive(Debug)]
struct FuncDef<'a> {
    name: &'a str,
    args: Vec<(&'a str, Type<'a>)>,
    return_type: Type<'a>,
    body: Block<'a>,
}

impl<'a> Expression<'a> {
    fn infer_type<'b>(&self, scope: &'b Scope<'a>) -> Option<&'b Type> {
        match self {
            Expression::Field(_, _) => None,
            Expression::FuncCall { fun, .. } => {
                let fun: &Expression = fun;
                match fun {
                    Expression::Symbol(name) => match scope.funcs.get(name) {
                        Some(f) => Some(&f.return_type),
                        None => unimplemented!("function {:?} not found in scope", name),
                    },
                    x => unimplemented!("{:?}", x),
                }
            }
            Expression::Operator(lhs, op, rhs) => {
                let lhs_typ = lhs.infer_type(scope).expect("unable to infer type");
                let rhs_typ = rhs.infer_type(scope).expect("unable to infer type");
                match (lhs_typ, op, rhs_typ) {
                    (a, Operator::DoubleEqual, b) => {
                        if (a.is_numeric() && b.is_numeric()) || (a == b) {
                            Some(&Type::Primitive(Primitive::Boolean))
                        } else {
                            unimplemented!("Type mismatch: {:?} == {:?}", a, b);
                        }
                    }
                    (a, Operator::Divide, b) => match (a, b) {
                        (Type::Primitive(Primitive::Float), Type::UserDefined("vec2")) => {
                            Some(&Type::UserDefined("vec2"))
                        }
                        x => unimplemented!("{:?}", x),
                    },
                    x => unimplemented!("{:?}", x),
                }
            }
            Expression::Symbol(name) => scope.lookup_value(name),
            Expression::If {
                if_block,
                else_block: _,
                ..
            } => {
                match &if_block.return_item {
                    Some(return_item) => {
                        // TODO: Make sure else_block has same return type if present, and if
                        // not make sure both are returning void, or atleat returning.
                        return_item.infer_type(scope)
                    }
                    None => None,
                }
            }
            Expression::Literal(a) => match a {
                Literal::Int(_) => Some(&Type::Primitive(Primitive::Int)),
                Literal::String(_) => Some(&Type::Primitive(Primitive::Str)),
                Literal::Float(_) => Some(&Type::Primitive(Primitive::Float)),
            },
            x => unimplemented!("{:?}", x),
        }
    }
    fn to_c(&self, scope: &Scope<'a>) -> String {
        match self {
            Expression::Literal(lit) => match lit {
                Literal::String(s) => format!(r#""{}""#, s),
                Literal::Int(s) => format!("{:?}", s),
                Literal::Float(s) => format!("{:?}", s),
            },
            Expression::Operator(lhs, op, rhs) => {
                format!("{} {} {}", lhs.to_c(scope), op, rhs.to_c(scope))
            }
            Expression::Field(subject, field) => format!("{}.{}", subject.to_c(scope), field),
            Expression::LetDef {
                name,
                typ,
                initializer,
            } => {
                let typ = match typ {
                    Some(typ) => typ.to_c(scope),
                    None => {
                        let initializer = initializer
                            .as_ref()
                            .expect("cannot infer type when no initializer was provided");

                        let typ = initializer.infer_type(scope).expect("could not infer :/");
                        typ.to_c(scope)
                    }
                };

                match initializer {
                    None => format!("{} {}", typ, name),
                    Some(initializer) => match initializer.as_ref() {
                        Expression::If {
                            condition,
                            if_block,
                            else_block,
                        } => {
                            let decl = format!("{} {};", typ, name);
                            format!(
                                "{}\nif ({}) {{{}\n}}{}",
                                decl,
                                condition.to_c(scope),
                                indent(&if_block.to_c_assign_to(scope, &Expression::Symbol(name))),
                                match else_block {
                                    Some(else_block) => format!(
                                        " else {{{}\n}}",
                                        indent(
                                            &else_block
                                                .to_c_assign_to(scope, &Expression::Symbol(name))
                                        )
                                    ),
                                    None => "".to_owned(),
                                }
                            )
                        }
                        initializer => format!("{} {} = {}", typ, name, initializer.to_c(scope)),
                    },
                }
            }
            Expression::If {
                condition,
                if_block,
                else_block,
            } => format!(
                "if ({}) {{{}\n}}{}",
                condition.to_c(scope),
                indent(&if_block.to_c(scope)),
                match else_block {
                    Some(else_block) => {
                        format!(" else {{\n{}\n}}", indent(&else_block.to_c(scope)))
                    }
                    None => "".to_owned(),
                }
            ),
            Expression::For {
                name,
                from,
                to,
                body,
            } => format!(
                "for (int {name} = {from}; {name} < {to}; ++{name}) {{\n{body}\n}}",
                name = name,
                from = from.to_c(scope),
                to = to.to_c(scope),
                body = indent(&body.to_c(scope)),
            ),
            Expression::FuncCall { fun, args } => {
                let args = args
                    .into_iter()
                    .map(|expr| format!("{}", expr.to_c(scope)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", fun.to_c(scope), args)
            }
            Expression::Symbol(name) => name.to_string(),
        }
    }
}

impl<'a> Block<'a> {
    fn to_c(&self, scope: &Scope<'a>) -> String {
        let mut block = self
            .statements
            .iter()
            .map(|expr| format!("{};", expr.to_c(scope)))
            .collect::<Vec<_>>()
            .join("\n");

        if let Some(return_item) = &self.return_item {
            block += &format!("\nreturn {};", return_item.to_c(scope));
        }

        block
    }
    fn to_c_assign_to(&self, scope: &Scope<'a>, target: &Expression<'a>) -> String {
        let mut block = self
            .statements
            .iter()
            .map(|expr| format!("{};", expr.to_c(scope)))
            .collect::<Vec<_>>()
            .join("\n");

        if let Some(return_item) = &self.return_item {
            block += &format!("\n{} = {};", target.to_c(scope), return_item.to_c(scope));
        }

        block
    }
}

impl<'a> Decleration<'a> {
    fn to_c(&self, scope: &Scope<'a>) -> String {
        match self {
            Decleration::FuncDef(func_def) => func_def.to_c(scope),
            Decleration::Struct(s) => s.to_c(scope),
            Decleration::Uniforms { uniforms } => uniforms
                .into_iter()
                .map(|(name, typ)| format!("uniform {} {};", typ.to_c(scope), name))
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }
}

impl<'a> FuncDef<'a> {
    fn to_c(&self, scope: &Scope<'a>) -> String {
        let args = self
            .args
            .iter()
            .map(|(name, typ)| format!("{} {}", typ.to_c(scope), name))
            .collect::<Vec<_>>()
            .join(", ");
        let return_type = self.return_type.to_c(scope);
        let mut program = format!("{} {}({}) {{\n", return_type, self.name, args);
        program += &self
            .body
            .to_c(scope)
            .lines()
            .map(|l| format!("\t{}", l))
            .collect::<Vec<_>>()
            .join("\n");
        program += "\n}";

        program
    }
}

impl<'a> StructDef<'a> {
    fn to_c(&self, scope: &Scope<'a>) -> String {
        let fields = self
            .fields
            .iter()
            .map(|(name, typ)| format!("\t{} {};", typ.to_c(scope), self.name))
            .collect::<Vec<_>>()
            .join("\n");
        // format!("typedef struct {} {{\n{}\n}} {};", name, fields, name)
        format!("struct {} {{\n{}\n}};", self.name, fields)
    }
}

impl<'a> Document<'a> {
    fn to_c(&self, scope: &Scope<'a>) -> String {
        let mut program = "".to_owned();

        for decl in &self.declerations {
            program += &decl.to_c(scope);
            program += "\n\n";
        }

        let head = r#"
            #version 420

            out vec4 FragColor;

            in vec2 TexCoords;
        "#;

        let foot = r#"
            void main() {
                FragColor = vec4(frag(TexCoords), 1.0);
            }
        "#;

        format!("{}{}{}", head, program, foot)
    }
}

#[derive(Debug)]
struct Document<'a> {
    declerations: Vec<Decleration<'a>>,
}

impl<'a> Document<'a> {
    fn generate_scope(self) -> Scope<'a> {
        let mut uniforms = HashMap::new();
        let mut funcs = HashMap::new();
        let mut structs = HashMap::new();

        for decl in self.declerations.into_iter() {
            match decl {
                Decleration::Uniforms { uniforms: us } => {
                    for (name, typ) in us {
                        uniforms.insert(name, typ);
                    }
                }
                Decleration::FuncDef(func_def) => {
                    funcs.insert(func_def.name, func_def);
                }
                Decleration::Struct(s) => {
                    structs.insert(s.name, s);
                }
            }
        }

        Scope {
            uniforms,
            funcs,
            structs,
        }
    }
}

#[derive(Debug)]
struct Scope<'a> {
    uniforms: HashMap<&'a str, Type<'a>>,
    funcs: HashMap<&'a str, FuncDef<'a>>,
    structs: HashMap<&'a str, StructDef<'a>>,
}

impl<'a> Scope<'a> {
    fn generate_defaults(&mut self) {
        let vec3 = FuncDef {
            name: "vec3",
            args: vec![],
            return_type: Type::UserDefined("vec3"),
            body: Block {
                statements: vec![],
                return_item: None,
            },
        };
        self.funcs.insert("vec3", vec3);
        let vec2 = FuncDef {
            name: "vec2",
            args: vec![],
            return_type: Type::UserDefined("vec2"),
            body: Block {
                statements: vec![],
                return_item: None,
            },
        };
        self.funcs.insert("vec2", vec2);
        let dot = FuncDef {
            name: "dot",
            args: vec![],
            return_type: Primitive::Float.into(),
            body: Block {
                statements: vec![],
                return_item: None,
            },
        };
        self.funcs.insert("dot", dot);
        let fmod = FuncDef {
            name: "mod",
            args: vec![],
            return_type: Primitive::Float.into(),
            body: Block {
                statements: vec![],
                return_item: None,
            },
        };
        self.funcs.insert("mod", fmod);
        let texture_size = FuncDef {
            name: "textureSize",
            args: vec![],
            return_type: Type::UserDefined("vec2"),
            body: Block {
                statements: vec![],
                return_item: None,
            },
        };
        self.funcs.insert("textureSize", texture_size);
    }

    fn to_vertex(&self) -> String {
        let mut structs = String::new();
        let mut uniforms = String::new();
        let mut functions = String::new();

        for (_, s) in &self.structs {
            structs += &s.to_c(self);
        }

        for (name, typ) in &self.uniforms {
            uniforms += &format!("uniform {} {};\n", typ.to_c(self), name);
        }

        for (_, func) in &self.funcs {
            functions += &func.to_c(self);
            functions += "\n";
        }

        format!("{}\n{}\n{}", structs, uniforms, functions)
    }
    fn lookup_value(&self, name: &str) -> Option<&Type<'a>> {
        unimplemented!("{:?}", name)
    }
}

fn main() {
    use std::{env, fs};

    let path = env::args().nth(1).unwrap();
    let sample_program = fs::read_to_string(path).unwrap();

    let mut lexer = lex(&sample_program);
    let ast = parse(&mut lexer);

    let mut scope = ast.generate_scope();

    scope.generate_defaults();

    println!("{}", scope.to_vertex());
    // print!("{}", ast.to_c(&scope));
}
