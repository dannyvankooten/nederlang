use std::fmt::Display;

use crate::ast::*;
use crate::object::NlObject;

macro_rules! byte {
    ($value:expr, $position:literal) => {
        (($value >> (8 * $position)) & 0xff) as u8
    };
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub(crate) enum OpCode {
    Const = 0,
    Pop,
    True,
    False,
    Add,
    Subtract,
    Divide,
    Multiply,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
    Jump,
    JumpIfFalse,
    Null,
    Halt,
}

const IP_PLACEHOLDER: usize = 99999;

/// Lookup table for quickly converting from u8 to OpCode variant
/// The order here is significant!
static U8_TO_OPCODE_MAP: [OpCode; 18] = [
    OpCode::Const,
    OpCode::Pop,
    OpCode::True,
    OpCode::False,
    OpCode::Add,
    OpCode::Subtract,
    OpCode::Divide,
    OpCode::Multiply,
    OpCode::Gt,
    OpCode::Gte,
    OpCode::Lt,
    OpCode::Lte,
    OpCode::Eq,
    OpCode::Neq,
    OpCode::Jump,
    OpCode::JumpIfFalse,
    OpCode::Null,
    OpCode::Halt,
];

impl From<u8> for OpCode {
    #[inline]
    fn from(value: u8) -> Self {
        unsafe { return *U8_TO_OPCODE_MAP.get_unchecked(value as usize) }
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            OpCode::Const => f.write_str("Const"),
            OpCode::Pop => f.write_str("Pop"),
            OpCode::True => f.write_str("True"),
            OpCode::False => f.write_str("False"),
            OpCode::Add => f.write_str("Add"),
            OpCode::Subtract => f.write_str("Subtract"),
            OpCode::Divide => f.write_str("Divide"),
            OpCode::Multiply => f.write_str("Multiply"),
            OpCode::Gt => f.write_str("Gt"),
            OpCode::Gte => f.write_str("Gte"),
            OpCode::Lt => f.write_str("Lt"),
            OpCode::Lte => f.write_str("Lte"),
            OpCode::Eq => f.write_str("Eq"),
            OpCode::Neq => f.write_str("Neq"),
            OpCode::Jump => f.write_str("Jump"),
            OpCode::JumpIfFalse => f.write_str("JumpIfFalse"),
            OpCode::Null => f.write_str("Null"),
            OpCode::Halt => f.write_str("Halt"),
        }
    }
}

pub(crate) struct CompiledProgram {
    pub(crate) bytecode: Vec<u8>,
    pub(crate) constants: Vec<NlObject>,
}

impl CompiledProgram {
    fn new() -> Self {
        CompiledProgram {
            bytecode: Vec::new(),
            constants: Vec::new(),
        }
    }

    fn add_constant(&mut self, obj: NlObject) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn add_instruction(&mut self, operand: OpCode, value: usize) -> usize {
        let pos = self.bytecode.len();

        match operand {
            // Opcodes with 2 operands (2^16 max value)
            OpCode::Const | OpCode::Jump | OpCode::JumpIfFalse => {
                self.bytecode.push(operand as u8);
                self.bytecode.push(byte!(value, 0));
                self.bytecode.push(byte!(value, 1));
            }
            // OpCodes with 1 operand:
            // -

            // OpCodes with 0 operands:
            OpCode::True | OpCode::False | OpCode::Null => self.bytecode.push(operand as u8),
            _ => unimplemented!("operand {:?} not implemented.", operand),
        }

        pos
    }

    fn remove_last_instruction_if(&mut self, op: OpCode) {
        if self.bytecode[self.bytecode.len() - 1] == op as u8 {
            self.bytecode.pop();
        }
    }

    fn change_instruction_operand_at(&mut self, op: OpCode, pos: usize, new_value: usize) {
        assert_eq!(self.bytecode[pos], op as u8);

        // TODO: For opcodes with less than 2 operands, we need to account for it here.
        self.bytecode[pos + 1] = byte!(new_value, 0);
        self.bytecode[pos + 2] = byte!(new_value, 1);
    }

    fn compile_block_statement(&mut self, stmts: &[Stmt]) {
        for s in stmts {
            self.compile_statement(s);
        }
    }

    fn compile_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expression(expr);
                self.bytecode.push(OpCode::Pop as u8);
            }
            Stmt::Block(stmts) => self.compile_block_statement(stmts),
            _ => unimplemented!("Can not yet compile statements of type {:?}", stmt),
        }
    }

    fn compile_operator(&mut self, operator: &Operator) {
        let opcode = match operator {
            Operator::Add => OpCode::Add,
            Operator::Subtract => OpCode::Subtract,
            Operator::Divide => OpCode::Divide,
            Operator::Multiply => OpCode::Multiply,
            Operator::Gt => OpCode::Gt,
            Operator::Gte => OpCode::Gte,
            Operator::Lt => OpCode::Lt,
            Operator::Lte => OpCode::Lte,
            Operator::Eq => OpCode::Eq,
            Operator::Neq => OpCode::Neq,
            _ => unimplemented!("Operators of type {:?} not yet implemented.", operator),
        };
        self.bytecode.push(opcode as u8);
    }

    fn compile_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Bool(expr) => {
                if expr.value {
                    self.bytecode.push(OpCode::True as u8);
                } else {
                    self.bytecode.push(OpCode::False as u8);
                }
            }
            Expr::Float(expr) => {
                let idx = self.add_constant(NlObject::Float(expr.value));
                self.add_instruction(OpCode::Const, idx);
            }
            Expr::Int(expr) => {
                let idx = self.add_constant(NlObject::Int(expr.value));
                self.add_instruction(OpCode::Const, idx);
            }
            Expr::Infix(expr) => {
                self.compile_expression(&*expr.left);
                self.compile_expression(&*expr.right);
                self.compile_operator(&expr.operator);
            }
            Expr::If(expr) => {
                self.compile_expression(&expr.condition);
                let pos_jump_before_consequence =
                    self.add_instruction(OpCode::JumpIfFalse, IP_PLACEHOLDER);
                self.compile_block_statement(&expr.consequence);
                self.remove_last_instruction_if(OpCode::Pop);

                let pos_jump_after_consequence = self.add_instruction(OpCode::Jump, IP_PLACEHOLDER);

                // Change operand of last JumpIfFalse opcode to where we're currently at
                self.change_instruction_operand_at(
                    OpCode::JumpIfFalse,
                    pos_jump_before_consequence,
                    self.bytecode.len(),
                );

                if let Some(alternative) = &expr.alternative {
                    self.compile_block_statement(alternative);
                    self.remove_last_instruction_if(OpCode::Pop);
                } else {
                    self.add_instruction(OpCode::Null, 0);
                }

                // Change operand of last JumpIfFalse opcode to where we're currently at
                self.change_instruction_operand_at(
                    OpCode::Jump,
                    pos_jump_after_consequence,
                    self.bytecode.len(),
                );
            }
            _ => unimplemented!("Can not yet compile expressions of type {:?}", expr),
        }
    }
}

pub(crate) fn compile(program: &BlockStmt) -> CompiledProgram {
    let mut cp = CompiledProgram::new();
    cp.compile_block_statement(program);
    cp.bytecode.push(OpCode::Halt as u8);
    cp
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn bytecode_to_human(code: &[u8]) -> String {
        let mut ip = 0;
        let mut str = String::with_capacity(256);

        loop {
            let op = OpCode::from(code[ip]);
            match op {
                OpCode::Const | OpCode::Jump | OpCode::JumpIfFalse => {
                    str.push_str(&op.to_string());
                    str.push_str(&format!(
                        "({})",
                        code[ip + 1] as usize + ((code[ip + 2] as usize) << 8)
                    ));
                    ip += 3;
                }
                OpCode::Halt => {
                    str.truncate(str.trim().len());
                    return str;
                }
                _ => {
                    str.push_str(&op.to_string());
                    ip += 1;
                }
            }
            str.push_str(" ");
        }
    }

    fn run(program: &str) -> String {
        let ast = parse(program).unwrap();
        let bytecode = &compile(&ast).bytecode;
        bytecode_to_human(bytecode)
    }

    #[test]
    fn test_macro() {
        assert_eq!(byte!(0, 0), 0);
        assert_eq!(byte!(0, 1), 0);

        assert_eq!(byte!(1, 0), 1);
        assert_eq!(byte!(1, 1), 0);

        assert_eq!(byte!(32, 0), 32);
        assert_eq!(byte!(32, 1), 0);

        assert_eq!(byte!(65535, 0), 255);
        assert_eq!(byte!(65535, 1), 255);

        assert_eq!(byte!(255, 0), 255);
        assert_eq!(byte!(255, 1), 0);
    }

    #[test]
    fn test_int_expression() {
        assert_eq!(run("5"), "Const(0) Pop");
        assert_eq!(run("5; 5"), "Const(0) Pop Const(1) Pop");
    }

    #[test]
    fn test_bool_expression() {
        assert_eq!(run("ja"), "True Pop");
        assert_eq!(run("nee"), "False Pop");
    }

    #[test]
    fn test_float_expression() {
        assert_eq!(run("1.23"), "Const(0) Pop");
        assert_eq!(run("1.23; 1.23"), "Const(0) Pop Const(1) Pop");
    }

    #[test]
    fn test_infix_expression() {
        assert_eq!(run("1 + 2"), "Const(0) Const(1) Add Pop");
        assert_eq!(run("1 - 2"), "Const(0) Const(1) Subtract Pop");
        assert_eq!(run("1 * 2"), "Const(0) Const(1) Multiply Pop");
        assert_eq!(run("1 / 2"), "Const(0) Const(1) Divide Pop");
        assert_eq!(
            run("1 * 2 * 3"),
            "Const(0) Const(1) Multiply Const(2) Multiply Pop"
        );
    }

    #[test]
    fn test_block_statements() {
        assert_eq!(run("{ 1 }"), "Const(0) Pop");
    }

    #[test]
    fn test_if_expression() {
        assert_eq!(
            run("als ja { 1 }"),
            "True JumpIfFalse(10) Const(0) Jump(11) Null Pop"
        );
        assert_eq!(
            run("als ja { 1 } anders { 2 }"),
            "True JumpIfFalse(10) Const(0) Jump(13) Const(1) Pop"
        );
    }
}
