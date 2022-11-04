use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::env::args;
#[cfg(feature = "debug")]
use std::mem::size_of;
#[cfg(feature = "debug")]
use nederlang::object::NlObject;
use nederlang::vm::run_str;

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

fn run_file(f: &Path) {
    let program = fs::read_to_string(f).unwrap();

    match run_str(&program) {
        Ok(obj) => println!("{}", obj),
        Err(e) => eprintln!("{:?}", e),
    }
}

fn main() -> io::Result<()> {
    #[cfg(feature = "debug")]
    println!("Size of NlObject: {} bytes", size_of::<NlObject>());

    let file = args().skip(1).find(|a| !a.starts_with("--"));
    if let Some(file) = file {
        let path = Path::new(&file);
        run_file(path);
    } else {
        run_repl();
    }
    Ok(())
}
