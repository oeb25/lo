#![feature(nll, box_syntax, const_fn)]

extern crate itertools;

pub mod ast;
pub mod c;
pub mod parser;
pub mod typer;
pub mod instruction;

#[derive(Debug)]
struct Args {
    files: Vec<String>,
    gcc: Option<String>,
    run: bool,
}

fn main() -> Result<(), Box<std::error::Error>> {
    use std::{
        collections::BTreeMap, env, fs, io, path::Path, process::{Command, Stdio},
    };

    let mut args = Args {
        files: vec![],
        gcc: None,
        run: false,
    };

    let mut a = env::args();
    a.next();

    while let Some(arg) = a.next() {
        if arg == "--gcc" {
            let out = a
                .next()
                .expect("expected output path following `--gcc` flag");
            args.gcc = Some(out);
        } else if arg == "--run" {
            args.run = true;
        } else {
            args.files.push(arg);
        }
    }

    if args.files.len() == 0 {
        panic!("No input files were specified");
    }

    let mut src_files: BTreeMap<&str, String> = BTreeMap::new();
    let mut documents = BTreeMap::new();

    for file in &args.files {
        let path: &Path = file.as_ref();
        let source = fs::read_to_string(path)?;

        src_files.insert(file, source);
    }

    for file in &args.files {
        let file: &str = file;
        let src = &src_files.get(file).unwrap();
        let ast = parser::parse_str(src);

        documents.insert(file, ast);
    }

    let sources = typer::Sources { documents };

    let program = sources.type_check();
    let c_program = program.to_c();
    let c_src = c_program.print(&program.typer);

    if args.run {
        unimplemented!("run")
    } else if let Some(out) = args.gcc {
        let mut gcc = Command::new("gcc")
            .args(format!("-o {} -xc -", out).split(' '))
            .current_dir(env::current_dir().expect("failed to get current dir"))
            .stdin(Stdio::piped())
            .spawn()
            .expect("Failed to spawn gcc");

        io::copy(&mut c_src.as_bytes(), gcc.stdin.as_mut().unwrap())
            .expect("failed to pipe src to gcc");

        gcc.wait_with_output().expect("gcc wait faield");
    } else {
        println!("{}", c_src);
    }

    Ok(())
}
