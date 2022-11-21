#![feature(test)]

mod ast;
mod eval;
mod lexer;
mod object;
mod parser;

use eval::{eval_ast, Environment, Error};
use std::{
    fs,
    io::{self, Write},
    path::Path,
};

fn repl() {
    let mut buffer = String::with_capacity(512);
    let mut env = Environment::new();

    loop {
        buffer.clear();
        print!(">>> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut buffer).unwrap();

        let ast = match parser::parse(&buffer) {
            Ok(ast) => ast,
            Err(e) => return eprintln!("{:?}", e),
        };

        match eval_ast(&ast, None) {
            Ok(obj) => println!("{}", obj),
            Err(e) => println!("{:?}", e),
        }
    }
}

fn file(f: &Path) {
    let program = fs::read_to_string(f).unwrap();
    let ast = match parser::parse(&program) {
        Ok(ast) => ast,
        Err(e) => return eprintln!("{:?}", e),
    };

    match eval_ast(&ast, None) {
        Ok(obj) => println!("{}", obj),
        Err(e) => println!("{:?}", e),
    }
}

fn main() -> io::Result<()> {
    let args = std::env::args();

    if args.len() <= 1 {
        repl()
    } else {
        let f = args.skip(1).next().unwrap();
        let path = Path::new(&f);
        file(path);
    }

    Ok(())
}
