use crate::compiler::{compile, CompiledProgram, OpCode};
use crate::object::NlObject;
use crate::parser::{parse, ParseError};

macro_rules! read_uint16 {
    ($one:expr, $two:expr) => {
        ($one as usize) + (($two as usize) << 8)
    };
}

// struct VM {
//     stack_pointer: usize,
//     stack: Vec<NlObject>,
//     constants: Vec<NlObject>,
//     ip: usize,
// }

struct Frame {
    code: Vec<u8>,
    ip: usize,
    base_pointer: usize,
}

impl Frame {
    fn new(bytecode: &Vec<u8>, base_pointer: usize) -> Self {
        Frame {
            ip: 0,
            code: bytecode.clone(),
            base_pointer,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
    // TypeError(String),
    SyntaxError(ParseError),
    ReferenceError(String),
    IndexError(String),
}

pub(crate) fn run_str(program: &str) -> Result<NlObject, Error> {
    let ast = parse(program).map_err(Error::SyntaxError)?;
    let compiled = compile(&ast);
    run(compiled)
}

fn run(program: CompiledProgram) -> Result<NlObject, Error> {
    let constants = program.constants;
    let bytecode = &program.scopes[0].bytecode;
    let mut stack: Vec<NlObject> = Vec::with_capacity(512);
    let mut frames: Vec<Frame> = Vec::with_capacity(64);
    frames.push(Frame::new(bytecode, 0));
    let mut result = NlObject::Null;
    let mut frame = frames.iter_mut().last().unwrap();

    macro_rules! impl_binary_op {
        ($op:tt) => {
            {
                let right = stack.pop().unwrap();
                let left = stack.pop().unwrap();
                let result = (left $op right).unwrap();
                stack.push(result);
                frame.ip += 1;
            }
        };
    }
    macro_rules! impl_binary_op_method {
        ($op:tt) => {{
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let result = left.$op(&right).unwrap();
            stack.push(result);
            frame.ip += 1;
        }};
    }

    if bytecode.is_empty() {
        return Ok(result);
    }

    loop {
        #[cfg(debug_assertions)]
        {
            println!("Stack: {:?}", stack);
            println!(
                "Next bytes: {:?} {:?} {:?}",
                OpCode::from(*frame.code.get(frame.ip).unwrap()),
                frame.code.get(frame.ip + 1),
                frame.code.get(frame.ip + 2)
            );
            println!();
        }

        let opcode = OpCode::from(frame.code[frame.ip]);
        match opcode {
            OpCode::Const => {
                let idx = read_uint16!(frame.code[frame.ip + 1], frame.code[frame.ip + 2]);
                stack.push(constants[idx].clone());
                frame.ip += 3;
            }
            OpCode::Pop => {
                result = stack.pop().unwrap_or(NlObject::Null);
                frame.ip += 1;
            }
            OpCode::Null => {
                stack.push(NlObject::Null);
                frame.ip += 1;
            }
            OpCode::Jump => {
                frame.ip = read_uint16!(frame.code[frame.ip + 1], frame.code[frame.ip + 2])
            }
            OpCode::JumpIfFalse => {
                let condition = stack.pop().unwrap();
                if !condition.is_truthy() {
                    frame.ip = read_uint16!(frame.code[frame.ip + 1], frame.code[frame.ip + 2])
                } else {
                    frame.ip += 3;
                }
            }
            OpCode::True => {
                stack.push(NlObject::Bool(true));
                frame.ip += 1;
            }
            OpCode::False => {
                stack.push(NlObject::Bool(false));
                frame.ip += 1;
            }
            OpCode::Add => impl_binary_op!(+),
            OpCode::Subtract => impl_binary_op!(-),
            OpCode::Divide => impl_binary_op!(/),
            OpCode::Multiply => impl_binary_op!(*),
            OpCode::Gt => impl_binary_op_method!(gt),
            OpCode::Gte => impl_binary_op_method!(gte),
            OpCode::Lt => impl_binary_op_method!(lt),
            OpCode::Lte => impl_binary_op_method!(lte),
            OpCode::Eq => impl_binary_op_method!(eq),
            OpCode::Neq => impl_binary_op_method!(neq),
            OpCode::Halt => return Ok(result),
            OpCode::ReturnValue => {
                let result = stack.pop().unwrap();
                stack.drain(frame.base_pointer..);
                frames.pop();
                frame = frames.iter_mut().last().unwrap();
                frame.ip += 1;
                stack.push(result);
            }
            OpCode::Return => {
                stack.drain(frame.base_pointer..);
                frames.pop();
                frame = frames.iter_mut().last().unwrap();
                frame.ip += 1;
            }
            OpCode::Call => {
                let num_args = frame.code[frame.ip + 1];
                let fn_obj = &stack[stack.len() - 1 - num_args as usize];
                let bytecode = match fn_obj {
                    NlObject::CompiledFunction(code, _num_locals) => code,
                    _ => unimplemented!(),
                };
                frame.ip += 1;
                frames.push(Frame::new(bytecode, stack.len() - 1 - num_args as usize));
                frame = frames.iter_mut().last().unwrap();
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
    }

    #[bench]
    fn bench_arithmetic(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(
                Ok(NlObject::Int(2350)),
                run_str(" 1 * 5 * 10 * 20 - 100 / 2 + 1400"),
            );
        });
    }

    #[bench]
    #[ignore]
    fn bench_fib_recursive_22(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(
                Ok(NlObject::Int(17711)),
                run_str(
                    "
                functie fib(n) {
                    als n < 2 {
                        n
                    } else {
                        fib(n - 1) + fib(n - 2)
                    }
                }
                
                fib(22)
                "
                ),
            );
        });
    }
}
