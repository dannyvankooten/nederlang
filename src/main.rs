#![feature(test)]

mod ast;
mod eval;
mod lexer;
mod object;
mod parser;

use eval::{eval_program, Environment};
use std::io;

fn main() -> io::Result<()> {
    let mut buffer = String::with_capacity(512);
    let mut env = Environment::new();

    loop {
        buffer.clear();
        io::stdin().read_line(&mut buffer)?;
        println!("In: {:?}", buffer.trim());

        let obj = eval_program(&buffer, Some(&mut env));
        println!("Out: {:?}", obj);
        println!();
    }

    Ok(())
}
