use std::fmt::Display;
use std::fmt::Write;

use crate::ast::*;
use crate::object::Error;
use crate::object::Object;
use crate::symbols::*;

macro_rules! byte {
    ($value:expr, $position:literal) => {
        (($value >> (8 * $position)) & 0xff) as u8
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

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
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
    And,
    Or,
    Not,
    Modulo,
    Negate,
    Jump,
    JumpIfFalse,
    Null,
    Return,
    ReturnValue,
    Call,
    GetLocal,
    SetLocal,
    GetGlobal,
    SetGlobal,
    GtLocalConst,
    GteLocalConst,
    LtLocalConst,
    LteLocalConst,
    EqLocalConst,
    NeqLocalConst,
    AddLocalConst,
    SubtractLocalConst,
    MultiplyLocalConst,
    DivideLocalConst,
    ModuloLocalConst,
    Halt,
}

const IP_PLACEHOLDER: usize = 99999;
const JUMP_PLACEHOLDER_BREAK: usize = 9999 + 1;

impl From<u8> for OpCode {
    #[inline(always)]
    fn from(value: u8) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}

impl OpCode {
    /// Returns the number of operands for the OpCode variant
    fn num_operands(&self) -> usize {
        match self {
            // OpCodes with 2 operands:
            OpCode::Const
            | OpCode::Jump
            | OpCode::JumpIfFalse

            // TODO: The following should ideally have 2 operands of 2 bytes each
            | OpCode::GtLocalConst
            | OpCode::GteLocalConst
            | OpCode::LtLocalConst
            | OpCode::LteLocalConst
            | OpCode::EqLocalConst
            | OpCode::NeqLocalConst
            | OpCode::AddLocalConst
            | OpCode::SubtractLocalConst
            | OpCode::MultiplyLocalConst
            | OpCode::DivideLocalConst
            | OpCode::ModuloLocalConst => 2,

            // OpCodes with 1 operand:
            OpCode::Call
            | OpCode::SetLocal
            | OpCode::GetGlobal
            | OpCode::SetGlobal
            | OpCode::GetLocal => 1,

            // Single opcode (no operands)
            _ => 0,
        }
    }
}

struct LoopContext {
    /// Points to the first instruction of the (current) loop condition
    /// This is where continue statements should jump to
    start: usize,

    /// Stores the index of all JUMP instructions within the current loop context that originate from a break statement
    /// Once this loop context ends, these instructions should have their operands updated to the first instruction that follows this loop
    break_instructions: Vec<usize>,
}

impl LoopContext {
    fn new(start: usize) -> Self {
        Self {
            start,
            break_instructions: Vec::new(),
        }
    }
}

pub(crate) struct Compiler {
    symbols: SymbolTable,
    constants: Vec<Object>,
    instructions: Vec<u8>,
    last_instruction: Option<OpCode>,
    loop_contexts: Vec<LoopContext>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            instructions: Vec::with_capacity(64),
            constants: Vec::with_capacity(64),
            last_instruction: None,
            loop_contexts: Vec::new(),
        }
    }

    fn add_instruction(&mut self, op: OpCode, value: usize) -> usize {
        let bytecode = &mut self.instructions;
        let pos = bytecode.len();

        // push OpCode itself
        bytecode.push(op as u8);

        // push operands of OpCode
        match op.num_operands() {
            // Opcodes with 2 operands (2^16 max value)
            2 => {
                bytecode.push(byte!(value, 0));
                bytecode.push(byte!(value, 1));
            }
            // OpCodes with a single operand (2^8 max value)
            1 => {
                bytecode.push(byte!(value, 0));
            }

            // OpCodes with 0 operands:
            0 => {
                // In case we call add_instruction for an opcode that should have a value, throw a helpful panic here
                assert_eq!(value, 0);
            }

            _ => panic!("Invalid operator: {op:?}"),
        }

        // store last instruction so we can match on it
        self.last_instruction = Some(op);

        pos
    }

    #[inline]
    fn last_instruction_is(&self, op: OpCode) -> bool {
        self.last_instruction == Some(op)
    }

    /// Replace the last instruction with the given opcode
    /// Note that this currently only works with OpCodes that do not take any operand values
    #[inline]
    fn replace_last_instruction(&mut self, op: OpCode) {
        assert_eq!(op.num_operands(), 0);
        let idx = self.instructions.len() - 1;
        self.instructions[idx] = op as u8;
        self.last_instruction = Some(op);
    }

    #[inline]
    fn remove_last_instruction(&mut self) {
        assert!(self.last_instruction.is_some());
        assert_eq!(self.last_instruction.unwrap().num_operands(), 0);
        self.instructions.pop();
        self.last_instruction = None;
    }

    #[inline]
    fn remove_last_instruction_if(&mut self, op: OpCode) {
        if self.last_instruction_is(op) {
            self.remove_last_instruction();
        }
    }

    fn change_instruction_operand_at(&mut self, op: OpCode, pos: usize, new_value: usize) {
        assert_eq!(self.instructions[pos], op as u8);
        assert_eq!(op.num_operands(), 2);

        // TODO: For opcodes with less than 2 operands, we need to account for it here.
        self.instructions[pos + 1] = byte!(new_value, 0);
        self.instructions[pos + 2] = byte!(new_value, 1);
    }

    fn compile_block_statement(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        // if block statement does not contain any other statements or expressions
        // simply push a NULL onto the stack
        if stmts.len() == 0 {
            self.add_instruction(OpCode::Null, 0);
            return Ok(());
        }

        self.symbols.enter_scope();
        for s in stmts {
            self.compile_statement(s)?;
        }
        self.symbols.leave_scope();
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expression(expr)?;
                self.add_instruction(OpCode::Pop, 0);
            }
            Stmt::Block(stmts) => self.compile_block_statement(stmts)?,
            Stmt::Let(name, value) => {
                let symbol = self.symbols.define(name);
                self.compile_expression(value)?;

                if symbol.scope == Scope::Global {
                    self.add_instruction(OpCode::SetGlobal, symbol.index);
                } else {
                    self.add_instruction(OpCode::SetLocal, symbol.index);
                }
            }
            Stmt::Return(expr) => {
                // TODO: Allow expression to be omitted (needs work in parser first)
                self.compile_expression(expr)?;
                self.add_instruction(OpCode::ReturnValue, 0);
            }
            Stmt::Break => {
                self.add_instruction(OpCode::Null, 0);
                let ip = self.add_instruction(OpCode::Jump, JUMP_PLACEHOLDER_BREAK);

                let ctx = match self.loop_contexts.iter_mut().last() {
                    Some(ctx) => ctx,
                    None => return Err(Error::SyntaxError(format!("Foutief gebruik van 'stop'."))),
                };
                ctx.break_instructions.push(ip);
            }
            Stmt::Continue => {
                self.add_instruction(OpCode::Null, 0);

                match self.loop_contexts.iter().last() {
                    Some(ctx) => {
                        self.add_instruction(OpCode::Jump, ctx.start);
                    }
                    None => {
                        return Err(Error::SyntaxError(format!(
                            "Foutief gebruik van 'volgende'."
                        )))
                    }
                }
            }
        }

        Ok(())
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
            Operator::Modulo => OpCode::Modulo,
            Operator::Not => OpCode::Not,
            Operator::Negate => OpCode::Negate,
            Operator::And => OpCode::And,
            Operator::Or => OpCode::Or,
            _ => unimplemented!("Operators of type {operator:?} not yet implemented."),
        };
        self.add_instruction(opcode, 0);
    }

    fn compile_const_var_infix_expression(
        &mut self,
        varname: &str,
        const_value: &ExprInt,
        operator: &Operator,
    ) -> Result<(), Error> {
        let idx_constant = self.add_constant(Object::from(const_value.value));
        let symbol = self.symbols.resolve(varname);
        match symbol {
            Some(symbol) => {
                let op = match (operator, symbol.scope) {
                    (Operator::Add, Scope::Local) => OpCode::AddLocalConst,
                    (Operator::Subtract, Scope::Local) => OpCode::SubtractLocalConst,
                    (Operator::Lt, Scope::Local) => OpCode::LtLocalConst,
                    (Operator::Lte, Scope::Local) => OpCode::LteLocalConst,
                    (Operator::Gt, Scope::Local) => OpCode::GtLocalConst,
                    (Operator::Gte, Scope::Local) => OpCode::GteLocalConst,
                    (Operator::Eq, Scope::Local) => OpCode::EqLocalConst,
                    (Operator::Neq, Scope::Local) => OpCode::NeqLocalConst,
                    (Operator::Multiply, Scope::Local) => OpCode::MultiplyLocalConst,
                    (Operator::Divide, Scope::Local) => OpCode::DivideLocalConst,
                    (Operator::Modulo, Scope::Local) => OpCode::ModuloLocalConst,
                    _ => {
                        return Err(Error::ReferenceError(format!(
                        "Optimized variant of this operator & scope type is not yet implemented."
                    )))
                    }
                };
                self.instructions.push(op as u8);
                self.instructions.push(symbol.index as u8);
                self.instructions.push(idx_constant as u8);
            }
            None => return Err(Error::ReferenceError(format!("{varname} is not defined"))),
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expr) -> Result<(), Error> {
        match expr {
            Expr::Bool(expr) => {
                if expr.value {
                    self.add_instruction(OpCode::True, 0);
                } else {
                    self.add_instruction(OpCode::False, 0);
                }
            }
            Expr::Float(expr) => {
                let idx = self.add_constant(Object::from(expr.value));
                self.add_instruction(OpCode::Const, idx);
            }
            Expr::Int(expr) => {
                let idx = self.add_constant(Object::from(expr.value));
                self.add_instruction(OpCode::Const, idx);
            }
            Expr::String(expr) => {
                let idx = self.add_constant(Object::from(expr.value.clone()));
                self.add_instruction(OpCode::Const, idx);
            }
            Expr::Identifier(name) => {
                let symbol = self.symbols.resolve(name);
                match symbol {
                    Some(symbol) => {
                        if symbol.scope == Scope::Global {
                            self.add_instruction(OpCode::GetGlobal, symbol.index);
                        } else {
                            self.add_instruction(OpCode::GetLocal, symbol.index);
                        }
                    }
                    None => return Err(Error::ReferenceError(format!("{name} is not defined"))),
                }
            }
            Expr::Prefix(expr) => {
                self.compile_expression(&expr.right)?;

                match expr.operator {
                    Operator::Negate | Operator::Subtract => {
                        self.add_instruction(OpCode::Negate, 0);
                    }
                    Operator::Not => {
                        self.add_instruction(OpCode::Not, 0);
                    }

                    _ => {
                        return Err(Error::TypeError(format!(
                            "Invalid operator for prefix expression: {:?}",
                            expr.operator
                        )))
                    }
                }
            }
            Expr::Assign(expr) => {
                let name = match &*expr.left {
                    Expr::Identifier(name) => name,
                    _ => {
                        return Err(Error::TypeError(format!(
                            "Can not assign to expression of type {:?}",
                            expr.left
                        )))
                    }
                };

                let symbol = self.symbols.resolve(name);
                match symbol {
                    Some(symbol) => {
                        self.compile_expression(&expr.right)?;

                        if symbol.scope == Scope::Global {
                            // TODO: Create superinstruction for this?
                            self.add_instruction(OpCode::SetGlobal, symbol.index);
                            self.add_instruction(OpCode::GetGlobal, symbol.index);
                        } else {
                            self.add_instruction(OpCode::SetLocal, symbol.index);
                            self.add_instruction(OpCode::GetLocal, symbol.index);
                        }
                    }
                    None => return Err(Error::ReferenceError(format!("{name} is not defined"))),
                }
            }
            Expr::Infix(expr) => {
                // If this expression is a combination of a constant & a variable, create an optimized instruction for it that skips the stack
                match (&*expr.left, &*expr.right) {
                    (Expr::Identifier(name), Expr::Int(inner_expr))
                    | (Expr::Int(inner_expr), Expr::Identifier(name)) => {
                        let res = self.compile_const_var_infix_expression(
                            name,
                            inner_expr,
                            &expr.operator,
                        );
                        if res.is_ok() {
                            return res;
                        }
                    }
                    _ => (),
                }

                // If that failed because we haven't implemented a specialized instruction yet, compile it as a sequence of normal instructions
                self.compile_expression(&expr.left)?;
                self.compile_expression(&expr.right)?;
                self.compile_operator(&expr.operator);
            }
            Expr::If(expr) => {
                self.compile_expression(&expr.condition)?;
                let pos_jump_before_consequence =
                    self.add_instruction(OpCode::JumpIfFalse, IP_PLACEHOLDER);

                self.compile_block_statement(&expr.consequence)?;
                self.remove_last_instruction_if(OpCode::Pop);

                let pos_jump_after_consequence = self.add_instruction(OpCode::Jump, IP_PLACEHOLDER);

                // Change operand of last JumpIfFalse opcode to where we're currently at
                self.change_instruction_operand_at(
                    OpCode::JumpIfFalse,
                    pos_jump_before_consequence,
                    self.instructions.len(),
                );

                if let Some(alternative) = &expr.alternative {
                    self.compile_block_statement(alternative)?;
                    self.remove_last_instruction_if(OpCode::Pop);
                } else {
                    self.add_instruction(OpCode::Null, 0);
                }

                // Change operand of last JumpIfFalse opcode to where we're currently at
                self.change_instruction_operand_at(
                    OpCode::Jump,
                    pos_jump_after_consequence,
                    self.instructions.len(),
                );
            }
            Expr::While(expr) => {
                // TODO: Can we get rid of this now that empty block statement emit a NULL?
                self.add_instruction(OpCode::Null, 0);
                self.loop_contexts
                    .push(LoopContext::new(self.instructions.len()));
                let pos_before_condition = self.instructions.len();
                self.compile_expression(&expr.condition)?;

                let pos_jump_if_false = self.add_instruction(OpCode::JumpIfFalse, IP_PLACEHOLDER);
                self.add_instruction(OpCode::Pop, 0);
                self.compile_block_statement(&expr.body)?;

                if self.last_instruction_is(OpCode::Pop) {
                    self.remove_last_instruction();
                } else {
                    self.add_instruction(OpCode::Null, 0);
                }

                self.add_instruction(OpCode::Jump, pos_before_condition);

                // Update jump statement for when initial condition evaluated to false (should skip over entire loop)
                self.change_instruction_operand_at(
                    OpCode::JumpIfFalse,
                    pos_jump_if_false,
                    self.instructions.len(),
                );

                // Update jump statements for every break statement inside this loop
                let ctx = self.loop_contexts.pop().unwrap();
                for ip in ctx.break_instructions {
                    self.change_instruction_operand_at(OpCode::Jump, ip, self.instructions.len());
                }
            }
            Expr::Function(fn_name, parameters, body) => {
                let symbol = if fn_name != "" {
                    Some(self.symbols.define(fn_name))
                } else {
                    None
                };
                let jump_over_fn = self.add_instruction(OpCode::Jump, 0);

                // Compile function in a new scope
                self.symbols.new_context();
                for p in parameters {
                    self.symbols.define(p);
                }

                let pos_start_function = self.instructions.len();

                self.compile_block_statement(body)?;

                if self.last_instruction_is(OpCode::Pop) {
                    self.replace_last_instruction(OpCode::ReturnValue);
                } else if !self.last_instruction_is(OpCode::ReturnValue) {
                    self.add_instruction(OpCode::Return, 0);
                }

                self.change_instruction_operand_at(
                    OpCode::Jump,
                    jump_over_fn,
                    self.instructions.len(),
                );

                // Switch back to previous scope again
                let num_locals = self.symbols.leave_context() as u8;
                let info = (pos_start_function << 16) as u32 + num_locals as u32;
                let idx = self.add_constant(Object::from(info as i64));

                self.add_instruction(OpCode::Const, idx);

                // If this function received a name, define it in the scope
                if let Some(symbol) = symbol {
                    if symbol.scope == Scope::Global {
                        self.add_instruction(OpCode::SetGlobal, symbol.index);
                    } else {
                        self.add_instruction(OpCode::SetLocal, symbol.index);
                    }
                    self.add_instruction(OpCode::Const, idx);
                }
            }
            Expr::Call(expr) => {
                for a in &expr.arguments {
                    self.compile_expression(a)?;
                }
                self.compile_expression(&expr.left)?;
                self.add_instruction(OpCode::Call, expr.arguments.len());
            }

            _ => unimplemented!("Can not yet compile expressions of type {expr:?}"),
        }

        Ok(())
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        // re-use already defined constants
        if let Some(pos) = self
            .constants
            .iter()
            .position(|c| c.tag() == obj.tag() && c == &obj)
        {
            return pos;
        }

        let idx = self.constants.len();
        self.constants.push(obj);
        idx
    }
}

pub(crate) struct Program {
    pub(crate) constants: Vec<Object>,
    pub(crate) instructions: Vec<u8>,
}

impl Program {
    pub(crate) fn new(ast: &BlockStmt) -> Result<Self, Error> {
        let mut compiler = Compiler::new();
        compiler.compile_block_statement(ast)?;
        compiler.add_instruction(OpCode::Halt, 0);

        // Copy all instructions for compiled functions over to main scope (after OpCode::Halt)
        let mut instructions = compiler.instructions;
        let mut constants = compiler.constants;

        // Shrink everything to least possible size
        instructions.shrink_to_fit();
        constants.shrink_to_fit();

        Ok(Self {
            constants,
            instructions,
        })
    }
}

/// We use a string representation of OpCodes to make testing a little easier
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
            OpCode::And => f.write_str("And"),
            OpCode::Or => f.write_str("Or"),
            OpCode::Not => f.write_str("Not"),
            OpCode::Modulo => f.write_str("Modulo"),
            OpCode::Negate => f.write_str("Negate"),
            OpCode::Jump => f.write_str("Jump"),
            OpCode::JumpIfFalse => f.write_str("JumpIfFalse"),
            OpCode::Null => f.write_str("Null"),
            OpCode::Return => f.write_str("Return"),
            OpCode::ReturnValue => f.write_str("ReturnValue"),
            OpCode::Call => f.write_str("Call"),
            OpCode::GetLocal => f.write_str("GetLocal"),
            OpCode::SetLocal => f.write_str("SetLocal"),
            OpCode::GetGlobal => f.write_str("GetGlobal"),
            OpCode::SetGlobal => f.write_str("SetGlobal"),
            OpCode::GtLocalConst => f.write_str("GtLocalConst"),
            OpCode::GteLocalConst => f.write_str("GteLocalConst"),
            OpCode::LtLocalConst => f.write_str("LtLocalConst"),
            OpCode::LteLocalConst => f.write_str("LteLocalConst"),
            OpCode::EqLocalConst => f.write_str("EqLocalConst"),
            OpCode::NeqLocalConst => f.write_str("NeqLocalConst"),
            OpCode::AddLocalConst => f.write_str("AddLocalConst"),
            OpCode::SubtractLocalConst => f.write_str("SubtractLocalConst"),
            OpCode::MultiplyLocalConst => f.write_str("MultiplyLocalConst"),
            OpCode::DivideLocalConst => f.write_str("DivideLocalConst"),
            OpCode::ModuloLocalConst => f.write_str("ModuloLocalConst"),
            OpCode::Halt => f.write_str("Halt"),
        }
    }
}

// Converts an array of bytes to a string representation consisting of the OpCode along with their u16 values
// For example: [OpCode::Const, 1, 0] -> "Const(1)"
#[allow(dead_code)]
pub fn bytecode_to_human(code: &[u8], positions: bool) -> String {
    let mut ip = 0;
    let mut str = String::with_capacity(256);

    while ip < code.len() {
        if ip > 0 {
            str.push(' ');
        }
        let op = OpCode::from(code[ip]);

        if positions {
            write!(str, "\n{ip:4} ").unwrap();
        }
        str.push_str(&op.to_string());

        match op.num_operands() {
            // OpCodes with 2 operands:
            2 => {
                write!(str, "({})", read_u16_operand!(code, ip)).unwrap();
            }

            // OpCodes with 1 operand:
            1 => {
                write!(str, "({})", code[ip + 1]).unwrap();
            }

            _ => (),
        }

        ip += 1 + op.num_operands();
    }

    str
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn run(program: &str) -> String {
        let ast = parse(program).unwrap();
        let program = Program::new(&ast).unwrap();
        bytecode_to_human(&program.instructions, false)
    }

    fn assert_bytecode_eq(program: &str, expected: &str) {
        let ast = parse(program).unwrap();
        let code = Program::new(&ast).unwrap();
        assert_eq!(
            bytecode_to_human(&code.instructions, false),
            expected,
            "\nInput: \t{program}\nBytecode: \t{}",
            bytecode_to_human(&code.instructions, true)
        );
    }

    #[test]
    fn test_byte_macro() {
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
        assert_eq!(run("5"), "Const(0) Pop Halt");
        assert_eq!(run("5; 5"), "Const(0) Pop Const(0) Pop Halt");
        assert_eq!(
            run("5; 6; 5"),
            "Const(0) Pop Const(1) Pop Const(0) Pop Halt"
        );
    }

    #[test]
    fn test_bool_expression() {
        assert_eq!(run("ja"), "True Pop Halt");
        assert_eq!(run("ja; ja"), "True Pop True Pop Halt");
        assert_eq!(run("nee"), "False Pop Halt");
    }

    #[test]
    fn test_float_expression() {
        assert_eq!(run("1.23"), "Const(0) Pop Halt");
        assert_eq!(run("1.23; 1.23"), "Const(0) Pop Const(0) Pop Halt");
        assert_eq!(
            run("5.00; 6.00; 5.00"),
            "Const(0) Pop Const(1) Pop Const(0) Pop Halt"
        );
    }

    #[test]
    fn test_infix_expression() {
        assert_eq!(run("1 + 2"), "Const(0) Const(1) Add Pop Halt");
        assert_eq!(run("1 - 2"), "Const(0) Const(1) Subtract Pop Halt");
        assert_eq!(run("1 * 2"), "Const(0) Const(1) Multiply Pop Halt");
        assert_eq!(run("1 / 2"), "Const(0) Const(1) Divide Pop Halt");
        assert_eq!(
            run("1 * 2 * 3"),
            "Const(0) Const(1) Multiply Const(2) Multiply Pop Halt"
        );
    }

    #[test]
    fn test_block_statements() {
        assert_eq!(run("{ 1 }"), "Const(0) Pop Halt");
    }

    #[test]
    fn test_if_expression() {
        assert_bytecode_eq(
            "als ja { 1 }",
            "True JumpIfFalse(10) Const(0) Jump(11) Null Pop Halt",
        );
        assert_bytecode_eq(
            "als ja { 1 } anders { 2 }",
            "True JumpIfFalse(10) Const(0) Jump(13) Const(1) Pop Halt",
        );
    }

    #[test]
    fn test_if_expression_empty_body() {
        assert_bytecode_eq(
            "als ja { }",
            "True JumpIfFalse(8) Null Jump(9) Null Pop Halt",
        );

        assert_bytecode_eq(
            "als ja { } anders { 1 }",
            "True JumpIfFalse(8) Null Jump(11) Const(0) Pop Halt",
        );
    }

    #[test]
    fn test_if_expression_empty_else() {
        assert_bytecode_eq(
            "als ja { 1 } anders {}",
            "True JumpIfFalse(10) Const(0) Jump(11) Null Pop Halt",
        );
    }

    #[test]
    fn test_function_expression() {
        assert_bytecode_eq(
            "functie() { 1 }",
            "Jump(7) Const(0) ReturnValue Const(1) Pop Halt",
        );

        assert_bytecode_eq(
            "functie() { 1 } functie() { 2 }",
            "Jump(7) Const(0) ReturnValue Const(1) Pop Jump(18) Const(2) ReturnValue Const(3) Pop Halt"
        );
    }

    #[test]
    fn test_call_expression() {
        assert_bytecode_eq(
            "functie(a, b) { 1 }(1, 2)",
            "Const(0) Const(1) Jump(13) Const(0) ReturnValue Const(2) Call(2) Pop Halt",
        );
    }

    #[test]
    fn test_declare_statement() {
        assert_eq!(run("stel a = 1;"), "Const(0) SetGlobal(0) Halt");

        assert_eq!(
            run("stel a = 1; stel b = 2;"),
            "Const(0) SetGlobal(0) Const(1) SetGlobal(1) Halt"
        );

        // TODO: Test scoped variables
    }

    #[test]
    fn test_ident_expressions() {
        assert_eq!(
            run("stel a = 1; a"),
            "Const(0) SetGlobal(0) GetGlobal(0) Pop Halt"
        );

        assert_eq!(
            run("stel a = 1; stel b = 2; a; b;"),
            "Const(0) SetGlobal(0) Const(1) SetGlobal(1) GetGlobal(0) Pop GetGlobal(1) Pop Halt"
        );

        // TODO: Test scoped variables
    }
}
