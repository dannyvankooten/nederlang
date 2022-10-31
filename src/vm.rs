use crate::compiler::{OpCode, Program};
use crate::object::Error;
use crate::object::NlObject;
use crate::parser::parse;

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

impl Frame {
    fn new(instructions: Vec<u8>, base_pointer: usize) -> Self {
        Frame {
            ip: 0,
            instructions,
            base_pointer,
        }
    }
}

pub(crate) fn run_str(program: &str) -> Result<NlObject, Error> {
    let ast = parse(program).map_err(Error::SyntaxError)?;
    let program = Program::new(&ast);
    run(program)
}

const OBJECT_NULL: NlObject = NlObject::Null;
const OBJECT_TRUE: NlObject = NlObject::Bool(true);
const OBJECT_FALSE: NlObject = NlObject::Bool(false);

fn run(program: Program) -> Result<NlObject, Error> {
    let constants = program.constants;
    let mut stack: Vec<NlObject> = Vec::with_capacity(64);
    let mut globals: Vec<NlObject> = Vec::with_capacity(64);
    let mut frames: Vec<Frame> = Vec::with_capacity(64);
    frames.push(Frame::new(program.instructions, 0));
    let mut result = OBJECT_NULL;
    let mut frame = frames.iter_mut().last().unwrap();

    macro_rules! impl_binary_op_method {
        ($op:tt) => {{
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
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

        // TODO: Match as u8, or is compiler smart enough?
        // TODO: Computed go-to by dropping to ASM?
        let opcode = OpCode::from(frame.instructions[frame.ip]);
        match opcode {
            OpCode::Const => {
                let idx = read_u16_operand!(frame.instructions, frame.ip);
                let obj = unsafe { constants.get_unchecked(idx) };
                stack.push(obj.clone());
                frame.ip += 3;
            }
            OpCode::Pop => {
                result = stack.pop().unwrap();
                frame.ip += 1;
            }
            OpCode::Null => {
                stack.push(OBJECT_NULL);
                frame.ip += 1;
            }
            OpCode::Jump => {
                frame.ip = read_u16_operand!(frame.instructions, frame.ip);
            }
            OpCode::JumpIfFalse => {
                let condition = stack.pop().unwrap();
                if !condition.is_truthy() {
                    frame.ip = read_u16_operand!(frame.instructions, frame.ip);
                } else {
                    frame.ip += 3;
                }
            }
            OpCode::True => {
                stack.push(OBJECT_TRUE);
                frame.ip += 1;
            }
            OpCode::False => {
                stack.push(OBJECT_FALSE);
                frame.ip += 1;
            }
            OpCode::Add => impl_binary_op_method!(add),
            OpCode::Subtract => impl_binary_op_method!(sub),
            OpCode::Divide => impl_binary_op_method!(div),
            OpCode::Multiply => impl_binary_op_method!(mul),
            OpCode::Gt => impl_binary_op_method!(gt),
            OpCode::Gte => impl_binary_op_method!(gte),
            OpCode::Lt => impl_binary_op_method!(lt),
            OpCode::Lte => impl_binary_op_method!(lte),
            OpCode::Eq => impl_binary_op_method!(eq),
            OpCode::Neq => impl_binary_op_method!(neq),
            OpCode::Halt => return Ok(result.clone()),
            OpCode::ReturnValue => {
                let result = stack.pop().unwrap();
                stack.truncate(frame.base_pointer);
                stack.push(result);
                frames.pop();
                frame = frames.iter_mut().last().unwrap();
                frame.ip += 1;
            }
            OpCode::Return => {
                stack.truncate(frame.base_pointer);
                frames.pop();
                frame = frames.iter_mut().last().unwrap();
                frame.ip += 1;
            }
            OpCode::Call => {
                let num_args = read_u8_operand!(frame.instructions, frame.ip);
                let base_pointer = stack.len() - 1 - num_args;
                let instructions = match stack.pop().unwrap() {
                    NlObject::CompiledFunction(fn_obj) => fn_obj.0,
                    _ => unimplemented!(),
                };
                frame.ip += 1;
                frames.push(Frame::new(instructions, base_pointer));
                frame = frames.iter_mut().last().unwrap();
            }
            OpCode::SetGlobal => {
                let idx = read_u16_operand!(frame.instructions, frame.ip);
                if globals.capacity() < idx {
                    globals.reserve(idx - globals.capacity());
                }
                globals.insert(idx, stack.pop().unwrap());
                frame.ip += 3;
                // let idx = read_uint16()
            }
            OpCode::GetGlobal => {
                let idx = read_u16_operand!(frame.instructions, frame.ip);
                stack.push(globals.get(idx).unwrap().clone());
                frame.ip += 3;
            }
            OpCode::SetLocal => {
                let idx = read_u16_operand!(frame.instructions, frame.ip);
                let value = stack.pop().unwrap();
                let obj = stack.get_mut(frame.base_pointer + idx).unwrap();
                *obj = value;
                frame.ip += 3;
            }
            OpCode::GetLocal => {
                let idx = read_u16_operand!(frame.instructions, frame.ip);
                let value = stack.get(frame.base_pointer + idx).unwrap();
                stack.push(value.clone());
                frame.ip += 3;
            }
            _ => unimplemented!("Missing implementation for opcode {:?}", opcode),
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate test;
    use test::Bencher;

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

    #[bench]
    fn bench_arithmetic(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(
                Ok(NlObject::Int(1337)),
                run_str(include_str!("../examples/benchmark_1.nl")),
            );
        });
    }

    #[bench]
    fn bench_fib_recursive_22(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(
                Ok(NlObject::Int(17711)),
                run_str(
                    "
                    stel fib = functie(n) {
                        als n < 2 {
                            antwoord n
                        } 
                        
                        fib(n - 1) + fib(n - 2)
                    }
                    
                    fib(22)
                "
                ),
            );
        });
    }
}
