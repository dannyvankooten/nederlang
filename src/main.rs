#![feature(test)]

mod ast;
mod eval;
mod lexer;
mod object;
mod parser;

use eval::{eval_program, Environment};
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let mut buffer = String::with_capacity(512);
    let mut env = Environment::new();

    loop {
        buffer.clear();
        print!(">>> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut buffer)?;

        match eval_program(&buffer, Some(&mut env)) {
            Ok(obj) => println!("{}", obj),
            Err(e) => println!("{:?}", e),
        }
    }

    Ok(())
}
