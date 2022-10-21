#![feature(test)]

mod eval;
mod lexer;
mod object;
mod parser;

use object::NlObject;
use std::io;

fn run(program: &str) -> NlObject {
    let ast = parser::parse(program);
    eval::eval(&ast)
}

fn main() -> io::Result<()> {
    let mut buffer = String::with_capacity(512);

    loop {
        buffer.clear();
        io::stdin().read_line(&mut buffer)?;
        println!("In: {:?}", buffer.trim());

        let obj = run(&buffer);
        println!("Out: {:?}", obj);
        println!("");
    }

    Ok(())
}
