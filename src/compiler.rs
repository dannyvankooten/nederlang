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
    Return,
    ReturnValue,
    Call,
    Halt,
}

const IP_PLACEHOLDER: usize = 99999;

/// Lookup table for quickly converting from u8 to OpCode variant
/// The order here is significant!
static U8_TO_OPCODE_MAP: [OpCode; 21] = [
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
    OpCode::Return,
    OpCode::ReturnValue,
    OpCode::Call,
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
            OpCode::Return => f.write_str("Return"),
            OpCode::ReturnValue => f.write_str("ReturnValue"),
            OpCode::Call => f.write_str("Call"),
            OpCode::Halt => f.write_str("Halt"),
        }
    }
}

pub(crate) struct CompilerScope {
    symbols: Vec<String>,
    pub(crate) bytecode: Vec<u8>,
}

pub(crate) struct CompiledProgram {
    pub(crate) constants: Vec<NlObject>,
    pub(crate) scopes: Vec<CompilerScope>,
    current_scope: usize,
}

impl CompiledProgram {
    fn new() -> Self {
        CompiledProgram {
            constants: Vec::new(),
            scopes: vec![CompilerScope {
                symbols: Vec::new(),
                bytecode: Vec::with_capacity(256),
            }],
            current_scope: 0,
        }
    }

    fn add_constant(&mut self, obj: NlObject) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn add_instruction(&mut self, operand: OpCode, value: usize) -> usize {
        let pos = self.current_scope().bytecode.len();

        match operand {
            // Opcodes with 2 operands (2^16 max value)
            OpCode::Const | OpCode::Jump | OpCode::JumpIfFalse => {
                self.current_scope_mut().bytecode.push(operand as u8);
                self.current_scope_mut().bytecode.push(byte!(value, 0));
                self.current_scope_mut().bytecode.push(byte!(value, 1));
            }
            // OpCodes with 1 operand:
            OpCode::Call => {
                self.current_scope_mut().bytecode.push(operand as u8);
                self.current_scope_mut().bytecode.push(byte!(value, 0));
            }

            // OpCodes with 0 operands:
            _ => self.current_scope_mut().bytecode.push(operand as u8),
        }

        pos
    }

    pub(crate) fn current_scope_mut(&mut self) -> &mut CompilerScope {
        self.scopes.iter_mut().last().unwrap()
    }

    pub(crate) fn current_scope(&self) -> &CompilerScope {
        self.scopes.iter().last().unwrap()
    }

    fn enter_scope(&mut self) {
        self.scopes.push(CompilerScope {
            symbols: Vec::new(),
            bytecode: Vec::with_capacity(256),
        });
        self.current_scope += 1;
    }

    fn leave_scope(&mut self) -> CompilerScope {
        self.current_scope -= 1;
        self.scopes.pop().unwrap()
    }

    fn current_instructions_len(&self) -> usize {
        return self.scopes[self.current_scope].bytecode.len();
    }

    fn last_instruction_is(&self, op: OpCode) -> bool {
        self.current_scope().bytecode.iter().last() == Some(&(op as u8))
    }

    fn replace_last_instruction(&mut self, op: OpCode) {
        let last_instruction = self.current_scope_mut().bytecode.iter_mut().last().unwrap();
        *last_instruction = op as u8;
    }

    fn replace_last_instruction_if(&mut self, old_op: OpCode, new_op: OpCode) {
        if self.last_instruction_is(old_op) {
            self.replace_last_instruction(new_op);
        }
    }

    fn remove_last_instruction(&mut self) {
        self.current_scope_mut().bytecode.pop();
    }

    fn remove_last_instruction_if(&mut self, op: OpCode) {
        if self.last_instruction_is(op) {
            self.remove_last_instruction();
        }
    }

    fn change_instruction_operand_at(&mut self, op: OpCode, pos: usize, new_value: usize) {
        assert_eq!(self.current_scope().bytecode[pos], op as u8);

        // TODO: For opcodes with less than 2 operands, we need to account for it here.
        self.current_scope_mut().bytecode[pos + 1] = byte!(new_value, 0);
        self.current_scope_mut().bytecode[pos + 2] = byte!(new_value, 1);
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
                self.add_instruction(OpCode::Pop, 0);
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
        self.add_instruction(opcode, 0);
    }

    fn compile_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Bool(expr) => {
                if expr.value {
                    self.add_instruction(OpCode::True, 0);
                } else {
                    self.add_instruction(OpCode::False, 0);
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
                    self.current_instructions_len(),
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
                    self.current_instructions_len(),
                );
            }
            Expr::Function(_name, parameters, body) => {
                self.enter_scope();

                for p in parameters {
                    self.current_scope_mut().symbols.push(p.clone());
                }

                self.compile_block_statement(body);

                // TODO: If last instruction is OpCode::Pop, replace it with OpCode::ReturnValue
                // TODO: Else, if last instruction is not OpCode::ReturnValue, add OpCode::Return (for speed)?
                if self.last_instruction_is(OpCode::Pop) {
                    self.replace_last_instruction(OpCode::ReturnValue);
                } else if !self.last_instruction_is(OpCode::ReturnValue) {
                    self.add_instruction(OpCode::Return, 0);
                }

                let compiled_function = self.leave_scope();
                let id = self.add_constant(NlObject::CompiledFunction(
                    compiled_function.bytecode,
                    compiled_function.symbols.len() as u8,
                ));
                self.add_instruction(OpCode::Const, id);
            }
            Expr::Call(expr) => {
                self.compile_expression(&expr.left);

                for a in &expr.arguments {
                    self.compile_expression(a);
                }

                self.add_instruction(OpCode::Call, expr.arguments.len());
            }

            _ => unimplemented!("Can not yet compile expressions of type {:?}", expr),
        }
    }
}

pub(crate) fn compile(program: &BlockStmt) -> CompiledProgram {
    let mut cp = CompiledProgram::new();
    cp.compile_block_statement(program);
    cp.add_instruction(OpCode::Halt, 0);
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
                // OpCodes with 2 operands:
                OpCode::Const | OpCode::Jump | OpCode::JumpIfFalse => {
                    str.push_str(&op.to_string());
                    str.push_str(&format!(
                        "({})",
                        code[ip + 1] as usize + ((code[ip + 2] as usize) << 8)
                    ));
                    ip += 3;
                }

                // OpCodes with 1 operand:
                OpCode::Call => {
                    str.push_str(&op.to_string());
                    str.push_str(&format!("({})", code[ip + 1]));
                    ip += 2;
                }

                // Halt (end of program, return str)
                OpCode::Halt => {
                    str.truncate(str.trim().len());
                    return str;
                }

                // Single opcode (no operands)
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
        let compiled = compile(&ast);
        let bytecode = &compiled.current_scope().bytecode;
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

    #[test]
    fn test_function_expression() {
        // Const(0) is inside the function
        // Const(1) is the compiled function
        assert_eq!(run("functie() { 1 }"), "Const(1) Pop");
    }

    #[test]
    fn test_call_expression() {
        // Const(0) is inside the function
        // Const(1) is the compiled function
        assert_eq!(
            run("functie(a, b) { 1 }(1, 2)"),
            "Const(1) Const(2) Const(3) Call(2) Pop"
        );
    }
}
