#![feature(test)]

mod ast;
mod builtins;
mod compiler;
mod eval;
mod lexer;
mod object;
mod parser;
mod vm;

use eval::eval_program;
use std::{
    fs,
    io::{self, Write},
    path::Path,
};
use vm::run_str;

macro_rules! byte {
    ($value:expr, $position:literal) => {
        (($value >> (8 * $position)) & 0xff) as u8
    };
}

fn repl() {
    let mut buffer = String::with_capacity(512);
    loop {
        buffer.clear();
        print!(">>> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut buffer).unwrap();

        match run_str(&buffer) {
            Ok(obj) => println!("{}", obj),
            Err(e) => println!("{:?}", e),
        }
    }
}

fn file(f: &Path) {
    let program = fs::read_to_string(f).unwrap();
    match eval_program(&program, None) {
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
