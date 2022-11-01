use crate::compiler::{OpCode, Program};
use crate::object::Error;
use crate::object::NlObject;
use crate::parser::parse;
use std::ptr;

macro_rules! read_u8_operand {
    ($instructions:expr, $ip:expr) => {
        unsafe { *$instructions.get_unchecked($ip + 1) as usize }
    };
}

macro_rules! read_u16_operand {
    ($instructions:expr, $ip:expr) => {
        unsafe {
            (*$instructions.get_unchecked($ip + 1) as usize)
                + ((*$instructions.get_unchecked($ip + 2) as usize) << 8)
        }
    };
}

struct Frame {
    /// The compiled bytecode
    instructions: Vec<u8>,

    /// Index of the current instruction
    ip: usize,

    /// Pointer to the index of the stack before function call started
    /// This is where the VM returns its stack to after the function returns
    base_pointer: usize,
}

/// Vec::pop, but without checking if it's empty first.
/// This yields a ~25% performance improvement. Removing any of the other bound check do not yield significant performance improvements.
/// So here we choose to only use a custom method for pop().
#[inline]
fn pop(slice: &mut Vec<NlObject>) -> NlObject {
    // Safety: slice is never empty, opcodes that push items on the stack always come before anything that pops
    unsafe {
        let new_len = slice.len() - 1;
        slice.set_len(new_len);
        ptr::read(slice.as_ptr().add(new_len))
    }
}

impl Frame {

    #[inline]
    fn new(instructions: Vec<u8>, base_pointer: usize) -> Self {
        Frame {
            ip: 0,
            instructions,
            base_pointer,
        }
    }
}

pub fn run_str(program: &str) -> Result<NlObject, Error> {
    let ast = parse(program).map_err(Error::SyntaxError)?;
    let program = Program::new(&ast);
    run(program)
}

const OP_CONST: u8 = OpCode::Const as u8;
const OP_POP: u8 = OpCode::Pop as u8;
const OP_NULL: u8 = OpCode::Null as u8;
const OP_TRUE: u8 = OpCode::True as u8;
const OP_FALSE: u8 = OpCode::False as u8;
const OP_GETLOCAL: u8 = OpCode::GetLocal as u8;
const OP_SETLOCAL: u8 = OpCode::SetLocal as u8;
const OP_GETGLOBAL: u8 = OpCode::GetGlobal as u8;
const OP_SETGLOBAL: u8 = OpCode::SetGlobal as u8;
const OP_CALL: u8 = OpCode::Call as u8;
const OP_JUMP: u8 = OpCode::Jump as u8;
const OP_JUMPIFFALSE: u8 = OpCode::JumpIfFalse as u8;
const OP_ADD: u8 = OpCode::Add as u8;
const OP_SUBTRACT: u8 = OpCode::Subtract as u8;
const OP_DIVIDE: u8 = OpCode::Divide as u8;
const OP_MULTIPLY: u8 = OpCode::Multiply as u8;
const OP_GT: u8 = OpCode::Gt as u8;
const OP_GTE: u8 = OpCode::Gte as u8;
const OP_LT: u8 = OpCode::Lt as u8;
const OP_LTE: u8 = OpCode::Lte as u8;
const OP_EQ: u8 = OpCode::Eq as u8;
const OP_NEQ: u8 = OpCode::Neq as u8;
const OP_RETURNVALUE: u8 = OpCode::ReturnValue as u8;
const OP_RETURN: u8 = OpCode::Return as u8;
const OP_HALT: u8 = OpCode::Halt as u8;

fn run(program: Program) -> Result<NlObject, Error> {
    let constants = program.constants;
    let mut stack = Vec::with_capacity(64);
    let mut globals = Vec::with_capacity(8);
    let mut frames = Vec::with_capacity(32);
    frames.push(Frame::new(program.instructions, 0));
    let mut result = NlObject::Null;
    let mut frame = frames.iter_mut().last().unwrap();

    macro_rules! impl_binary_op_method {
        ($op:tt) => {{
            let right = pop(&mut stack);
            let left = pop(&mut stack);
            let result = left.$op(&right)?;
            stack.push(result);
            frame.ip += 1;
        }};
    }

    if frame.instructions.is_empty() {
        return Ok(result.clone());
    }

    loop {
        #[cfg(debug_assertions)]
        #[cfg(not(test))]
        {
            println!("Stack: {:?}", stack);
            println!(
                "Next bytes: {:?} {:?} {:?}",
                OpCode::from(*frame.instructions.get(frame.ip).unwrap()),
                frame.instructions.get(frame.ip + 1),
                frame.instructions.get(frame.ip + 2)
            );
            println!();
        }

        let opcode = unsafe { *frame.instructions.get_unchecked(frame.ip) };
        match opcode {
            OP_CONST => {
                let idx = read_u16_operand!(frame.instructions, frame.ip);
                let obj = unsafe { constants.get_unchecked(idx) };
                stack.push(obj.clone());
                frame.ip += 3;
            }
            OP_POP => {
                result = pop(&mut stack);
                frame.ip += 1;
            }
            OP_NULL => {
                stack.push(NlObject::Null);
                frame.ip += 1;
            }
            OP_JUMP => {
                frame.ip = read_u16_operand!(frame.instructions, frame.ip);
            }
            OP_JUMPIFFALSE => {
                let condition = pop(&mut stack);
                if condition.is_truthy() {
                    frame.ip += 3;
                } else {
                    frame.ip = read_u16_operand!(frame.instructions, frame.ip);
                }
            }
            OP_TRUE => {
                stack.push(NlObject::Bool(true));
                frame.ip += 1;
            }
            OP_FALSE => {
                stack.push(NlObject::Bool(false));
                frame.ip += 1;
            }
            OP_ADD => impl_binary_op_method!(add),
            OP_SUBTRACT => impl_binary_op_method!(sub),
            OP_DIVIDE => impl_binary_op_method!(div),
            OP_MULTIPLY => impl_binary_op_method!(mul),
            OP_GT => impl_binary_op_method!(gt),
            OP_GTE => impl_binary_op_method!(gte),
            OP_LT => impl_binary_op_method!(lt),
            OP_LTE => impl_binary_op_method!(lte),
            OP_EQ => impl_binary_op_method!(eq),
            OP_NEQ => impl_binary_op_method!(neq),
            OP_HALT => return Ok(result.clone()),
            OP_RETURNVALUE => {
                let result = pop(&mut stack);
                stack.truncate(frame.base_pointer);
                stack.push(result);
                frames.truncate(frames.len() - 1);
                frame = frames.iter_mut().last().unwrap();
                frame.ip += 1;
            }
            OP_RETURN => {
                stack.truncate(frame.base_pointer);
                frames.truncate(frames.len() - 1);
                frame = frames.iter_mut().last().unwrap();
                frame.ip += 1;
            }
            OP_CALL => {
                let num_args = read_u8_operand!(frame.instructions, frame.ip);
                let base_pointer = stack.len() - 1 - num_args;
                let instructions = match pop(&mut stack) {
                    NlObject::CompiledFunction(fn_obj) => fn_obj.0,
                    _ => unimplemented!(),
                };
                frame.ip += 1;
                frames.push(Frame::new(instructions, base_pointer));
                frame = frames.iter_mut().last().unwrap();
            }
            OP_SETGLOBAL => {
                let idx = read_u8_operand!(frame.instructions, frame.ip);
                globals.insert(idx, pop(&mut stack));
                frame.ip += 2;
            }
            OP_GETGLOBAL => {
                let idx = read_u8_operand!(frame.instructions, frame.ip);
                let obj = globals.get(idx).unwrap();
                stack.push(obj.clone());
                frame.ip += 2;
            }
            OP_SETLOCAL => {
                let idx = read_u8_operand!(frame.instructions, frame.ip);
                let value = pop(&mut stack);
                let obj = stack.get_mut(frame.base_pointer + idx).unwrap();
                *obj = value;
                frame.ip += 2;
            }
            OP_GETLOCAL => {
                let idx = read_u8_operand!(frame.instructions, frame.ip);
                let value = stack.get(frame.base_pointer + idx).unwrap();
                stack.push(value.clone());
                frame.ip += 2;
            }

            // This panic! is here to hint the compiler that this path is very unlikely to be taken
            _ => panic!(
                "Missing implementation for opcode {:?}",
                OpCode::from(opcode)
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_expression() {
        assert_eq!(run_str("1"), Ok(NlObject::Int(1)));
        assert_eq!(run_str("1; 2"), Ok(NlObject::Int(2)));
    }

    #[test]
    fn test_infix_expression() {
        assert_eq!(run_str("4 + 2"), Ok(NlObject::Int(6)));
        assert_eq!(run_str("4 - 2"), Ok(NlObject::Int(2)));
        assert_eq!(run_str("4 * 2"), Ok(NlObject::Int(8)));
        assert_eq!(run_str("4 / 4"), Ok(NlObject::Int(1)));
        assert_eq!(run_str("4 == 4"), Ok(NlObject::Bool(true)));
        assert_eq!(run_str("4 != 4"), Ok(NlObject::Bool(false)));
        assert_eq!(run_str("4 > 4"), Ok(NlObject::Bool(false)));
        assert_eq!(run_str("4 >= 4"), Ok(NlObject::Bool(true)));
        assert_eq!(run_str("4 < 4"), Ok(NlObject::Bool(false)));
        assert_eq!(run_str("4 <= 4"), Ok(NlObject::Bool(true)));
    }

    #[test]
    fn test_if_expression() {
        assert_eq!(run_str("als ja { 1 }"), Ok(NlObject::Int(1)));
        assert_eq!(run_str("als ja { 1 } anders { 2 }"), Ok(NlObject::Int(1)));
        assert_eq!(run_str("als nee { 1 } anders { 2 }"), Ok(NlObject::Int(2)));
        assert_eq!(
            run_str("als nee { 1 } anders als nee { 2 } anders { 3 + 3 }"),
            Ok(NlObject::Int(6))
        );
        assert_eq!(run_str("als nee { 1 }"), Ok(NlObject::Null));
    }

    #[test]
    fn test_function_expression_calls() {
        assert_eq!(run_str("functie() { 1 }()"), Ok(NlObject::Int(1)));
        assert_eq!(
            run_str("functie() { 1 }() + functie() { 2 }()"),
            Ok(NlObject::Int(3))
        );
        assert_eq!(
            run_str("functie() { functie() { 1 }() }()"),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            run_str("functie() { functie() { 1 }() }() + functie() { 2 }()"),
            Ok(NlObject::Int(3))
        );
        assert_eq!(
            run_str(
                "1 + functie() { 1 + functie() { 1 }() }() + functie() { functie() { 1 }() + 1 }()"
            ),
            Ok(NlObject::Int(5))
        );
    }

    #[test]
    fn test_variables() {
        assert_eq!(run_str("stel a = 1; a"), Ok(NlObject::Int(1)));
        assert_eq!(run_str("stel a = 1; stel b = 2; a"), Ok(NlObject::Int(1)));
        assert_eq!(run_str("stel a = 1; stel b = 2; b"), Ok(NlObject::Int(2)));
        assert_eq!(
            run_str("stel a = 1; stel b = 2; stel c = a; c"),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            run_str("stel a = 1; { stel a = a + 2; } a"),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            run_str("stel a = 1; functie() { stel a = 2; } a"),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            run_str("stel a = 1; functie(a) { antwoord a; }(2)"),
            Ok(NlObject::Int(2))
        );

        // TODO: This should resolve by looking at the outer scope
        assert_eq!(
            run_str("stel a = 1; functie() { a }()"),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            run_str("stel a = 1; functie(a, b) { a * 2 + b }(a, 1)"),
            Ok(NlObject::Int(3))
        );

        // TODO: This should result in a reference error (it panics currently)
        // assert!(run_str("functie() { stel a = 2; } a").is_err());

        // assert!(run_str("{ stel a = 2; } a").is_err());
    }

    #[test]
    fn test_recursion() {  
        assert_eq!(
            run_str("stel fib = functie(n) { als n < 2 { antwoord n; } fib(n - 1 ) + fib(n - 2) }; fib(6);"),
            Ok(NlObject::Int(8))
        );
    }

    #[test]
    fn test_functions_as_argument() {  
        assert_eq!(
            run_str("(functie (a) { a() })(functie() { 100 });"),
            Ok(NlObject::Int(100))
        );
    }
}
