use crate::compiler::{OpCode, Program};
use crate::object::Error;
use crate::object::NlObject;
use crate::parser::parse;
use std::io::Write;
use std::ptr;

#[cfg(feature = "debug")]
use crate::compiler::bytecode_to_human;

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
        println!("Bytecode (raw)= \n{:?}", &program.instructions);
        print!(
            "Bytecode (human)= {}\n",
            bytecode_to_human(&program.instructions, true)
        );
        println!("{:16}= {:?}", "Constants", program.constants);
    }

    // Buffer used to capture input from stdin during stepped debugging
    let mut buffer = String::new();

    // Syntactic sugar
    let instructions = program.instructions;
    let constants = program.constants;

    // Storage
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

    let mut debug_pause = 0;

    loop {
        #[cfg(feature = "debug")]
        {
            println!(
                "{:16}= {}/{}: {}",
                "Instruction",
                frame.ip,
                instructions.len(),
                bytecode_to_human(&instructions[frame.ip..], false)
                    .split(" ")
                    .next()
                    .unwrap()
            );
            print!("{:16}= [", "Globals");
            for (i, v) in globals.iter().enumerate() {
                print!("{}{}: {:?}", if i > 0 { ", " } else { "" }, i, v)
            }
            println!("]");
            print!("{:16}= [", "Stack");
            for (i, v) in stack.iter().enumerate() {
                print!("{}{}: {:?}", if i > 0 { ", " } else { "" }, i, v)
            }
            println!("]");

            if debug_pause == 0 {
                print!("{} ", ">".repeat(40));
                std::io::stdout().flush().unwrap();
                buffer.clear();
                std::io::stdin().read_line(&mut buffer).unwrap();
                debug_pause = buffer.trim().parse().unwrap_or(1) - 1;
            } else {
                println!("{} ", ">".repeat(40));
                debug_pause -= 1;
            }
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
