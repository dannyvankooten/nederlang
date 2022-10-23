#![feature(test)]

mod eval;
mod lexer;
mod object;
mod parser;

use eval::Eval;
use object::NlObject;
use std::io;

fn run(program: &str) -> NlObject {
    match parser::parse(program) {
        Ok(ast) => ast.eval(),
        Err(e) => {
            eprintln!("{}", e.message);
            NlObject::Null
        }
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::with_capacity(512);

    loop {
        buffer.clear();
        io::stdin().read_line(&mut buffer)?;
        println!("In: {:?}", buffer.trim());

        let obj = run(&buffer);
        println!("Out: {:?}", obj);
        println!();
    }

    Ok(())
}
