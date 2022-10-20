#![feature(test)]

use std::io;
mod lexer;
mod parser;

fn main() -> io::Result<()> {
    let input = std::fs::read_to_string("./examples/cargo-sample.rs").unwrap();
    let v = lexer::Tokenizer::new(&input).collect::<Vec<lexer::Token>>();
    println!("{:?}", v);

    // loop {
    //     buffer.clear();
    //     io::stdin().read_line(&mut buffer)?;
    //     println!("Input: {buffer}");
    //     let mut tokenizer = lexer::Tokenizer::new(&buffer);
    //     loop {
    //         let tok = tokenizer.next();
    //         if tok.is_none() {
    //             break;
    //         }
    //         let tok = tok.unwrap();
    //         println!("{:?}", tok);
    //     }
    // }
    
    Ok(())
}
