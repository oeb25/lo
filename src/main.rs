#![feature(nll, box_syntax)]

extern crate itertools;

pub mod ast;
pub mod c;
pub mod parser;
pub mod typer;

fn main() -> Result<(), Box<std::error::Error>> {
    use std::{env, fs, path::Path};

    let path = env::args().nth(1).expect("no file path was passed to compiler");
    let path: &Path = path.as_ref();
    let source = fs::read_to_string(path)?;

    let ast = parser::parse_str(&source);

    let mut documents = std::collections::BTreeMap::new();
    documents.insert(path.to_str().unwrap(), ast);

    let sources = typer::Sources { documents };

    let program = sources.type_check();

    println!("{:?}", program);

    Ok(())
}
