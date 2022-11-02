extern crate nederlang;

use std::fs;
use std::io::{self, Write};
use std::mem::size_of;
use std::path::Path;

use nederlang::object::NlObject;
use nederlang::vm::run_str;
use std::env::args;

fn run_repl() {
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

fn run_from_file(f: &Path) {
    let program = fs::read_to_string(f).unwrap();

    match run_str(&program) {
        Ok(obj) => println!("{}", obj),
        Err(e) => eprintln!("{:?}", e),
    }
}

fn main() -> io::Result<()> {
    assert!(
        size_of::<NlObject>() <= 16,
        "NlObject is larger than 16 bytes. Don't cut corners Danny boy."
    );

    let file = args().skip(1).find(|a| !a.starts_with("--"));
    if let Some(file) = file {
        let path = Path::new(&file);
        run_from_file(path);
    } else {
        run_repl();
    }
    Ok(())
}
