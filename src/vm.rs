use crate::compiler::{OpCode, Program};
use crate::object::Error;
use crate::object::NlObject;
use crate::parser::parse;
use std::ptr;

#[cfg(feature = "debug")]
use crate::compiler::bytecode_to_human;
#[cfg(feature = "debug")]
use std::time::Duration;

macro_rules! read_u8_operand {
    ($instructions:expr, $ip:expr) => {
        unsafe { *$instructions.get_unchecked($ip + 1) as usize }
    };
}

macro_rules! read_u16_operand {
    ($instructions:expr, $ip:expr) => {
        read_u8_operand!($instructions, $ip)
            + unsafe { ((*$instructions.get_unchecked($ip + 2) as usize) << 8) }
    };
}

struct Frame {
    /// Index of the current instruction
    ip: usize,

    /// Pointer to the index of the stack before function call started
    /// This is where the VM returns its stack to after the function returns
    base_pointer: usize,
}

/// Vec::pop, but without checking if it's empty first.
/// This yields a ~25% performance improvement.
/// As an aside, removing any of the other bound check related to working with the stack does not seen to yield significant performance improvements.
#[inline]
fn pop(slice: &mut Vec<NlObject>) -> NlObject {
    debug_assert!(!slice.is_empty());

    // Safety: slice is never empty, opcodes that push items on the stack always come before anything that pops
    unsafe {
        let new_len = slice.len() - 1;
        slice.set_len(new_len);
        ptr::read(slice.as_ptr().add(new_len))
    }
}

impl Frame {
    #[inline]
    fn new(ip: usize, base_pointer: usize) -> Self {
        Frame { ip, base_pointer }
    }
}

pub fn run_str(program: &str) -> Result<NlObject, Error> {
    let ast = parse(program)?;
    let program = Program::new(&ast)?;
    run(program)
}

fn run(program: Program) -> Result<NlObject, Error> {
    #[cfg(feature = "debug")]
    {
        println!("Bytecode (raw): {:?}", &program.instructions);
        print!(
            "Bytecode (human): {}\n",
            bytecode_to_human(&program.instructions, true)
        );
        println!("Constants: {:?}", program.constants);
    }

    let instructions = program.instructions;
    let constants = program.constants;

    let mut stack = Vec::with_capacity(64);
    let mut globals = Vec::with_capacity(8);
    let mut frames = Vec::with_capacity(32);
    let mut result = NlObject::Null;

    frames.push(Frame::new(0, 0));
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

    macro_rules! impl_binary_const_local_op_method {
        ($op:tt) => {{
            let left = stack[frame.base_pointer + read_u8_operand!(instructions, frame.ip)];
            let right = constants[read_u8_operand!(instructions, frame.ip + 1)];
            stack.push(left.$op(&right)?);
            frame.ip += 3;
        }};
    }

    if instructions.is_empty() {
        return Ok(result);
    }

    loop {
        #[cfg(feature = "debug")]
        {
            println!("-----------------");
            println!(
                "Current instruction: \t{:?}",
                bytecode_to_human(&instructions[frame.ip..], false)
                    .split(" ")
                    .next()
                    .unwrap()
            );
            println!("Globals: \t\t{:?}", globals);
            println!("Stack: \t\t\t{:?}", stack);
            std::thread::sleep(Duration::from_millis(5));
        }

        debug_assert!(instructions.len() > frame.ip);
        let opcode = unsafe { OpCode::from(*instructions.get_unchecked(frame.ip)) };
        match opcode {
            OpCode::Const => {
                let idx = read_u16_operand!(instructions, frame.ip);
                stack.push(constants[idx]);
                frame.ip += 3;
            }
            OpCode::SetGlobal => {
                let idx = read_u8_operand!(instructions, frame.ip);
                let value = pop(&mut stack);
                while globals.len() <= idx {
                    globals.push(NlObject::Null);
                }
                globals[idx] = value;
                frame.ip += 2;
            }
            OpCode::GetGlobal => {
                let idx = read_u8_operand!(instructions, frame.ip);
                stack.push(globals[idx]);
                frame.ip += 2;
            }
            OpCode::SetLocal => {
                let idx = read_u8_operand!(instructions, frame.ip);
                let value = pop(&mut stack);
                stack[frame.base_pointer + idx] = value;
                frame.ip += 2;
            }
            OpCode::GetLocal => {
                let idx = read_u8_operand!(instructions, frame.ip);
                debug_assert!(stack.len() > frame.base_pointer + idx);
                stack.push(unsafe { *stack.get_unchecked(frame.base_pointer + idx) });
                frame.ip += 2;
            }
            // TODO: Make JUMP* opcodes relative
            OpCode::Jump => {
                frame.ip = read_u16_operand!(instructions, frame.ip);
            }
            OpCode::JumpIfFalse => {
                let condition = pop(&mut stack);
                if condition.is_truthy() {
                    frame.ip += 3;
                } else {
                    frame.ip = read_u16_operand!(instructions, frame.ip);
                }
            }
            OpCode::Pop => {
                result = pop(&mut stack);
                frame.ip += 1;
            }
            OpCode::Null => {
                stack.push(NlObject::Null);
                frame.ip += 1;
            }
            OpCode::True => {
                stack.push(NlObject::Bool(true));
                frame.ip += 1;
            }
            OpCode::False => {
                stack.push(NlObject::Bool(false));
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
            OpCode::Modulo => impl_binary_op_method!(rem),
            OpCode::And => impl_binary_op_method!(and),
            OpCode::Or => impl_binary_op_method!(or),
            OpCode::Not => {
                let left = pop(&mut stack);
                let result = left.not()?;
                stack.push(result);
                frame.ip += 1;
            }
            OpCode::Negate => {
                let left = pop(&mut stack);
                let result = left.neg()?;
                stack.push(result);
                frame.ip += 1;
            }
            OpCode::Call => {
                let num_args = read_u8_operand!(instructions, frame.ip);
                let base_pointer = stack.len() - 1 - num_args;
                let (ip, num_locals) = match pop(&mut stack) {
                    NlObject::CompiledFunctionPointer(ip, num_locals) => (ip, num_locals),
                    _ => unimplemented!(),
                };

                // Make room on the stack for any local variables defined inside this function
                for _ in 0..num_locals - num_args as u8 {
                    stack.push(NlObject::Null);
                }

                frame.ip += 1;
                frames.push(Frame::new(ip as usize, base_pointer));
                frame = frames.iter_mut().last().unwrap();
            }
            OpCode::ReturnValue => {
                let result = pop(&mut stack);
                stack.truncate(frame.base_pointer);
                stack.push(result);
                frames.truncate(frames.len() - 1);
                frame = frames.iter_mut().last().unwrap();
                frame.ip += 1;
            }
            OpCode::Return => {
                stack.truncate(frame.base_pointer);
                stack.push(NlObject::Null);
                frames.truncate(frames.len() - 1);
                frame = frames.iter_mut().last().unwrap();
                frame.ip += 1;
            }
            OpCode::GtLocalConst => impl_binary_const_local_op_method!(gt),
            OpCode::GteLocalConst => impl_binary_const_local_op_method!(gte),
            OpCode::LtLocalConst => impl_binary_const_local_op_method!(lt),
            OpCode::LteLocalConst => impl_binary_const_local_op_method!(lte),
            OpCode::EqLocalConst => impl_binary_const_local_op_method!(eq),
            OpCode::NeqLocalConst => impl_binary_const_local_op_method!(neq),
            OpCode::AddLocalConst => impl_binary_const_local_op_method!(add),
            OpCode::SubtractLocalConst => impl_binary_const_local_op_method!(sub),
            OpCode::MultiplyLocalConst => impl_binary_const_local_op_method!(mul),
            OpCode::DivideLocalConst => impl_binary_const_local_op_method!(div),
            OpCode::ModuloLocalConst => impl_binary_const_local_op_method!(rem),
            OpCode::Halt => return Ok(result),
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
    }

    #[test]
    fn test_nested_local_scopes() {
        assert_eq!(
            run_str(
                "1 + functie() { 1 + functie() { 1 }() }() + functie() { functie() { 1 }() + 1 }()"
            ),
            Ok(NlObject::Int(5))
        );
    }

    #[test]
    fn test_variables() {
        for (program, expected_result) in [
            ("stel a = 1; a", NlObject::Int(1)),
            ("stel a = 1; stel b = 2; a", NlObject::Int(1)),
            ("stel a = 1; stel b = 2; b", NlObject::Int(2)),
            (
                "stel a = 1; stel b = 2; stel c = a + b; c",
                NlObject::Int(3),
            ),
        ] {
            let result = run_str(program);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), expected_result, "test program: {program}")
        }
    }

    #[test]
    fn test_referencing_undeclared_vars() {
        for program in [
            "a",
            "{ stel a = 1; } a",
            "{ { stel a = 1; } a }",
            "functie() { stel a = 1; }() a",
        ] {
            assert_eq!(
                run_str(program),
                Err(Error::ReferenceError("a is not defined".to_string()))
            )
        }
    }

    #[test]
    fn test_scoped_variables() {
        for (program, expected_result) in [
            ("functie(a) { a + 1 }(1)", NlObject::Int(2)),
            // Shadow declaration in same scope:
            // ("stel a = 1; stel a = 2; a", NlObject::Int(2)),
            // This is valid, because the scopes differ:
            ("stel a = 1; { stel a = 2; } a", NlObject::Int(1)),
            (
                "stel a = 1; { stel b = 2; } stel c = 3; c",
                NlObject::Int(3),
            ),
            (
                "stel a = 1; { stel b = a; { stel c = b; c } }",
                NlObject::Int(1),
            ),
        ] {
            let result = run_str(program);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), expected_result, "test program: {program}")
        }
    }

    #[test]
    fn test_function_vars() {
        assert_eq!(
            run_str("stel a = 1; functie() { stel a = 2; } a"),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            run_str("stel a = 1; functie(a) { antwoord a; }(2)"),
            Ok(NlObject::Int(2))
        );
        assert_eq!(
            run_str("stel a = 1; functie() { a }()"),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            run_str("stel a = 1; functie(a, b) { a * 2 + b }(a, 1)"),
            Ok(NlObject::Int(3))
        );
    }

    #[test]
    fn test_named_functions() {
        assert_eq!(
            run_str("functie optellen(a, b) { a + b }; optellen(10, 20)"),
            Ok(NlObject::Int(30))
        );
        assert_eq!(
            run_str("functie optellen(a, b) { a + b }; functie aftrekken(a, b) { a - b } aftrekken(optellen(10, 20), 30)"),
            Ok(NlObject::Int(0))
        );

        assert_eq!(
            run_str("stel opt1 = functie opt2(a, b) { a + b }; opt1(1, 2)"),
            Ok(NlObject::Int(3))
        );
        assert_eq!(
            run_str("stel opt1 = functie opt2(a, b) { a + b }; opt2(1, 2)"),
            Ok(NlObject::Int(3))
        );
    }

    #[test]
    fn test_functions_as_argument() {
        assert_eq!(
            run_str("(functie (a) { a() })(functie() { 100 });"),
            Ok(NlObject::Int(100))
        );
    }

    #[test]
    fn test_logical_andor() {
        assert_eq!(run_str("ja en ja"), Ok(NlObject::Bool(true)));
        assert_eq!(run_str("ja en nee"), Ok(NlObject::Bool(false)));
        assert_eq!(run_str("nee en nee"), Ok(NlObject::Bool(false)));
        assert_eq!(run_str("nee of nee"), Ok(NlObject::Bool(false)));
        assert_eq!(run_str("nee of ja"), Ok(NlObject::Bool(true)));
        assert_eq!(run_str("1 > 0 of 0 > 1"), Ok(NlObject::Bool(true)));
    }

    #[test]
    fn test_negating_values() {
        assert_eq!(run_str("-1"), Ok(NlObject::Int(-1)));
        assert_eq!(run_str("-1.00"), Ok(NlObject::Float(-1.00)));
    }

    #[test]
    fn test_not_values() {
        assert_eq!(run_str("!ja"), Ok(NlObject::Bool(false)));
        assert_eq!(run_str("!nee"), Ok(NlObject::Bool(true)));
        assert_eq!(run_str("!!nee"), Ok(NlObject::Bool(false)));
    }

    #[test]
    fn test_fib_recursion() {
        assert_eq!(
            run_str("stel fib = functie(n) { als n < 2 { antwoord n; } fib(n - 1 ) + fib(n - 2) }; fib(6);"),
            Ok(NlObject::Int(8))
        );
    }

    #[test]
    fn test_fib_loop() {
        assert_eq!(
            run_str(include_str!("../examples/fib-loop.nl")),
            Ok(NlObject::Int(9227465))
        );
    }

    #[test]
    fn test_break_statement() {
        assert_eq!(
            run_str("stel a = 0; zolang a < 10 { a = a + 1; als a == 5 { stop } } a"),
            Ok(NlObject::Int(5))
        );
    }

    #[test]
    fn test_continue_statement() {
        assert_eq!(run_str("stel i = 0; stel a = 2; zolang i < 10 { i = i + 1; als i >= 5 { volgende; } a = a * 2; } a"), Ok(NlObject::Int(32)));
    }
}
