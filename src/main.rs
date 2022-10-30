#![feature(test)]

mod ast;
mod builtins;
mod compiler;
mod eval;
mod lexer;
mod object;
mod parser;
mod vm;

use std::fs;
use std::io::{self, Write};
use std::mem::{size_of, size_of_val};
use std::path::Path;
use std::rc::Rc;

use eval::eval_program;
use vm::run_str;

use crate::object::NlObject;

fn repl() {
    let mut buffer = String::with_capacity(512);
    loop {
        buffer.clear();
        print!(">>> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut buffer).unwrap();

        match run_str(&buffer) {
            Ok(obj) => println!("{}", obj),
            Err(e) => eprintln!("{:?}", e),
        }
    }
}

fn file(f: &Path) {
    let program = fs::read_to_string(f).unwrap();
    match eval_program(&program, None) {
        Ok(obj) => println!("{}", obj),
        Err(e) => eprintln!("{:?}", e),
    }
}

fn main() -> io::Result<()> {
    assert!(
        size_of::<NlObject>() <= 16,
        "NlObject is larger than 16 bytes. Don't cut corners Danny boy."
    );

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
