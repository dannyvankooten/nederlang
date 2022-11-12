use crate::builtins::{self, Builtin};
use crate::compiler::{OpCode, Program};
use crate::object::{Error, Object, Type};
use crate::parser::parse;

#[cfg(feature = "debug")]
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
fn pop(slice: &mut Vec<Object>) -> Object {
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

pub fn run_str(program: &str) -> Result<Object, Error> {
    let ast = parse(program)?;
    let program = Program::new(&ast)?;
    run(program)
}

fn run(program: Program) -> Result<Object, Error> {
    #[cfg(feature = "debug")]
    {
        println!("Bytecode (raw)= \n{:?}", &program.instructions);
        print!(
            "Bytecode (human)= {}\n",
            bytecode_to_human(&program.instructions, true)
        );
        println!("{:16}= {:?}", "Constants", program.constants);
    }

    // Keep your friends close
    let instructions = program.instructions;
    let constants = program.constants;
    let mut gc = program.gc;

    // Init stack, globals, frames & tmp value to store final program result
    let mut stack = Vec::with_capacity(32);
    let mut globals = Vec::with_capacity(8);
    let mut frames = Vec::with_capacity(32);
    let mut final_result = Object::null();

    // Push main frame onto the call stack
    frames.push(Frame::new(0, 0));
    let mut frame = frames.iter_mut().last().unwrap();

    macro_rules! impl_binary_op_method {
        ($op:tt) => {{
            let right = pop(&mut stack);
            let left = pop(&mut stack);
            let result = left.$op(right, &mut gc)?;
            stack.push(result);
            frame.ip += 1;
        }};
    }

    macro_rules! impl_binary_const_local_op_method {
        ($op:tt) => {{
            let left = stack[frame.base_pointer + read_u16_operand!(instructions, frame.ip)];
            let right = constants[read_u16_operand!(instructions, frame.ip + 2)];
            let result = left.$op(right, &mut gc)?;
            stack.push(result);
            frame.ip += 5;
        }};
    }

    if instructions.is_empty() {
        return Ok(final_result);
    }

    #[cfg(feature = "debug")]
    let mut debug_pause = 0;

    #[cfg(feature = "debug")]
    // Buffer used to capture input from stdin during stepped debugging
    let mut buffer = String::new();

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
                    globals.push(Object::null());
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

                // collect garbage on every jump instruction
                gc.run(&[&stack, &constants, &globals, &[final_result]]);
            }
            OpCode::JumpIfFalse => {
                let condition = pop(&mut stack);
                let evaluation = match condition.tag() {
                    Type::Bool => Ok(condition.as_bool()),
                    _ => Err(Error::TypeError(format!("Kan object met type {} niet gebruiken als voorwaarde. Gebruik evt. bool() om te type casten naar boolean.", condition.tag()))),
                }?;
                if evaluation == true {
                    frame.ip += 3;
                } else {
                    frame.ip = read_u16_operand!(instructions, frame.ip);
                }
            }
            OpCode::Pop => {
                final_result = pop(&mut stack);
                frame.ip += 1;
            }
            OpCode::Null => {
                stack.push(Object::null());
                frame.ip += 1;
            }
            OpCode::True => {
                stack.push(Object::bool(true));
                frame.ip += 1;
            }
            OpCode::False => {
                stack.push(Object::bool(false));
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
                assert_eq!(left.tag(), Type::Bool);
                let result = Object::bool(!left.as_bool());
                stack.push(result);
                frame.ip += 1;
            }
            OpCode::Negate => {
                let left = pop(&mut stack);
                let result = match left.tag() {
                    Type::Float => unsafe { Object::float(-left.as_f64_unchecked(), &mut gc) },
                    Type::Int => Object::int(-left.as_int()),
                    _ => {
                        return Err(Error::TypeError(format!(
                            "Can not negate object of type {}",
                            left.tag()
                        )))
                    }
                };
                stack.push(result);
                frame.ip += 1;
            }
            OpCode::Call => {
                let num_args = read_u8_operand!(instructions, frame.ip);
                let base_pointer = stack.len() - 1 - num_args;
                let obj = pop(&mut stack);
                if obj.tag() != Type::Function {
                    return Err(Error::TypeError(format!(
                        "object van type {} is not aanroepbaar",
                        obj.tag()
                    )));
                }
                let [ip, num_locals] = obj.as_function();

                // Make room on the stack for any local variables defined inside this function
                for _ in 0..num_locals - num_args as u16 {
                    stack.push(Object::null());
                }

                frame.ip += 1;
                frames.push(Frame::new(ip as usize, base_pointer));
                frame = frames.iter_mut().last().unwrap();
            }
            OpCode::CallBuiltin => {
                let builtin = read_u8_operand!(instructions, frame.ip) as u8;
                let num_args = read_u8_operand!(instructions, frame.ip + 1);
                let mut args = Vec::with_capacity(num_args);
                for _ in 0..num_args {
                    args.push(pop(&mut stack));
                }
                args.reverse();
                let builtin = unsafe { std::mem::transmute::<u8, Builtin>(builtin) };
                let result = builtins::call(builtin, &args, &mut gc)?;
                stack.push(result);
                frame.ip += 3;
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
                stack.push(Object::null());
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
            OpCode::Halt => {
                gc.untrace(final_result);
                return Ok(final_result);
            }
        }
    }
}
