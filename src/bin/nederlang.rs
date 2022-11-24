use nederlang::object::Object;
use nederlang::vm::run_str;
use std::env::args;
use std::fs;
use std::io::{self, Write};
#[cfg(feature = "debug")]
use std::mem::size_of;
use std::path::Path;

fn run_repl() {
    let mut buffer = String::with_capacity(512);
    loop {
        buffer.clear();
        print!(">>> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut buffer).unwrap();

        match run_str(&buffer) {
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

    match run_str(&program) {
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
