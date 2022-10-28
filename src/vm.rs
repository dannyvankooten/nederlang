use crate::compiler::{compile, CompiledProgram, OpCode};
use crate::object::NlObject;
use crate::parser::parse;

macro_rules! read_uint16 {
    ($one:expr, $two:expr) => {
        ($one as usize) + (($two as usize) << 8)
    };
}

struct VM {
    stack_pointer: usize,
    stack: Vec<NlObject>,
    constants: Vec<NlObject>,
    ip: usize,
}

fn run_str(program: &str) -> Option<NlObject> {
    let ast = parse(program).unwrap();
    let compiled = compile(&ast);
    run(compiled)
}

fn run(program: CompiledProgram) -> Option<NlObject> {
    let mut ip = 0;
    let constants = program.constants;
    let mut stack: Vec<NlObject> = Vec::with_capacity(512);
    let mut result: Option<NlObject> = None;

    macro_rules! impl_binary_op {
        ($op:tt) => {
            {
                let right = stack.pop().unwrap();
                let left = stack.pop().unwrap();
                let result = (left $op right).unwrap();
                stack.push(result);
                ip += 1;
            }
        };
    }

    macro_rules! impl_binary_op_method {
        ($op:tt) => {{
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let result = left.$op(&right).unwrap();
            stack.push(result);
            ip += 1;
        }};
    }

    if program.bytecode.is_empty() {
        return None;
    }

    loop {
        // println!("Stack: {:?}", stack);
        // println!("Next bytes: {:?} {:?} {:?}", OpCode::from(*program.bytecode.get(ip).unwrap()) , program.bytecode.get(ip+1), program.bytecode.get(ip+2));

        let opcode = OpCode::from(program.bytecode[ip]);
        match opcode {
            OpCode::Const => {
                let idx = read_uint16!(program.bytecode[ip + 1], program.bytecode[ip + 2]);
                stack.push(constants[idx].clone());
                ip += 3;
            }
            OpCode::Pop => {
                result = stack.pop();
                ip += 1;
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
            OpCode::Halt => return result,
            _ => unimplemented!("Missing implementation for opcode {:?}", opcode),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_int_expression() {
        assert_eq!(run_str("1"), Some(NlObject::Int(1)));
        assert_eq!(run_str("1; 2"), Some(NlObject::Int(2)));
    }

    #[test]
    fn test_infix_expression() {
        assert_eq!(run_str("4 + 2"), Some(NlObject::Int(6)));
        assert_eq!(run_str("4 - 2"), Some(NlObject::Int(2)));
        assert_eq!(run_str("4 * 2"), Some(NlObject::Int(8)));
        assert_eq!(run_str("4 / 4"), Some(NlObject::Int(1)));

        assert_eq!(run_str("4 == 4"), Some(NlObject::Bool(true)));
        assert_eq!(run_str("4 != 4"), Some(NlObject::Bool(false)));
        assert_eq!(run_str("4 > 4"), Some(NlObject::Bool(false)));
        assert_eq!(run_str("4 >= 4"), Some(NlObject::Bool(true)));
        assert_eq!(run_str("4 < 4"), Some(NlObject::Bool(false)));
        assert_eq!(run_str("4 <= 4"), Some(NlObject::Bool(true)));
    }
}
