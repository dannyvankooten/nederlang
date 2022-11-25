mod ast;
mod builtins;
mod gc;
mod lexer;
mod symbols;

pub mod compiler;
pub mod object;
pub mod parser;
pub mod vm;

use crate::{compiler::Compiler, object::Error, object::Object, parser::parse, vm::VM};

/// Evaluates the given program string without retaining any state in between calls
/// This may return a heap-allocated object that needs its memory managed
pub fn eval(program: &str) -> Result<Object, Error> {
    let ast = parse(program)?;
    let code = Compiler::new().compile_ast(&ast)?;
    VM::new().run(code)
}
