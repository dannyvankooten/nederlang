use std::io;
mod lexer;
mod parser;

fn main() -> io::Result<()> {
    let mut buffer = String::new();

    loop {
        buffer.clear();
        io::stdin().read_line(&mut buffer)?;
        println!("Input: {buffer}");
        let mut tokenizer = lexer::Tokenizer::new(&buffer);
        loop {
            let tok = tokenizer.next();
            if tok.is_none() {
                break;
            }
            let tok = tok.unwrap();
            println!("{:?}", tok);
        }
    }
    
    Ok(())
}
