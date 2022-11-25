use nederlang::compiler::Compiler;
use nederlang::eval;
use nederlang::object::Object;
use nederlang::parser::parse;
use nederlang::vm::VM;
use std::env::args;
use std::fs;
use std::io::{self, Write};
#[cfg(feature = "debug")]
use std::mem::size_of;
use std::path::Path;

fn run_repl() {
    let mut compiler = Compiler::new();
    let mut vm = VM::new();
    let mut buffer = String::with_capacity(512);

    loop {
        buffer.clear();
        print!(">>> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut buffer).unwrap();

        // TODO: Error handling here
        let ast = parse(&buffer).unwrap();
        let code = compiler.compile_ast(&ast).unwrap();

        match vm.run(code) {
            Ok(obj) => {
                if obj != Object::null() {
                    println!("{obj}")
                }
            }
            Err(e) => eprintln!("{e:?}"),
        }
    }
}

fn run_file(f: &Path) {
    let program = fs::read_to_string(f).unwrap();

    match eval(&program) {
        Ok(obj) => println!("{obj}"),
        Err(e) => eprintln!("{e:?}"),
    }
}

fn main() -> io::Result<()> {
    let file = args().skip(1).find(|a| !a.starts_with("--"));
    if let Some(file) = file {
        let path = Path::new(&file);
        run_file(path);
    } else {
        run_repl();
    }
    Ok(())
}
