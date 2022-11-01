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
    GetLocal,
    SetLocal,
    GetGlobal,
    SetGlobal,
    Halt,
}

const IP_PLACEHOLDER: usize = 99999;

/// Lookup table for quickly converting from u8 to OpCode variant
/// The order here is significant!
static U8_TO_OPCODE_MAP: [OpCode; 25] = [
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
    OpCode::GetLocal,
    OpCode::SetLocal,
    OpCode::GetGlobal,
    OpCode::SetGlobal,
    OpCode::Halt,
];

impl From<u8> for OpCode {
    #[inline]
    fn from(value: u8) -> Self {
        unsafe { return *U8_TO_OPCODE_MAP.get_unchecked(value as usize) }
    }
}

pub(crate) struct CompilerScope<'a> {
    symbol_table: &'a mut SymbolTable,
    constants: &'a mut Vec<NlObject>,
    instructions: Vec<u8>,
}

struct SymbolTable {
    scopes: Vec<Vec<String>>,
}
struct Symbol {
    scope: Scope,
    index: usize,
}
impl SymbolTable {
    fn new() -> Self {
        let mut scopes: Vec<Vec<String>> = Vec::with_capacity(4);
        scopes.push(Vec::with_capacity(4));
        SymbolTable { scopes }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn leave_scope(&mut self) -> usize {
        let symbols = self.scopes.pop().unwrap();
        symbols.len()
    }

    fn define(&mut self, name: &str) -> Symbol {
        let scope = if self.scopes.len() > 1 {
            Scope::Local
        } else {
            Scope::Global
        };
        let symbols = self.scopes.iter_mut().last().unwrap();
        symbols.push(name.to_string());
        Symbol {
            scope: scope,
            index: symbols.len() - 1,
        }
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(index) = scope.iter().position(|n| n == name) {
                return Some(Symbol {
                    scope: if i < (self.scopes.len() - 1) {
                        Scope::Local
                    } else {
                        Scope::Global
                    },
                    index,
                });
            }
        }

        None
    }
}

impl<'a> CompilerScope<'a> {
    /// Creates a new compiler scope to compile in
    fn new(constants: &'a mut Vec<NlObject>, symbol_table: &'a mut SymbolTable) -> Self {
        CompilerScope {
            symbol_table,
            instructions: Vec::with_capacity(64),
            constants,
        }
    }

    fn add_instruction(&mut self, operand: OpCode, value: usize) -> usize {
        let bytecode = &mut self.instructions;
        let pos = bytecode.len();

        match operand {
            // Opcodes with 2 operands (2^16 max value)
            OpCode::Const | OpCode::Jump | OpCode::JumpIfFalse => {
                bytecode.push(operand as u8);
                bytecode.push(byte!(value, 0));
                bytecode.push(byte!(value, 1));
            }
            // OpCodes with 1 operand:
            OpCode::Call
            | OpCode::SetLocal
            | OpCode::GetLocal
            | OpCode::SetGlobal
            | OpCode::GetGlobal => {
                bytecode.push(operand as u8);
                bytecode.push(byte!(value, 0));
            }

            // OpCodes with 0 operands:
            _ => bytecode.push(operand as u8),
        }

        pos
    }

    #[inline]
    fn current_instructions_len(&self) -> usize {
        return self.instructions.len();
    }

    #[inline]
    fn last_instruction_is(&self, op: OpCode) -> bool {
        self.instructions.iter().last() == Some(&(op as u8))
    }

    #[inline]
    fn replace_last_instruction(&mut self, op: OpCode) {
        let last_instruction = self.instructions.iter_mut().last().unwrap();
        *last_instruction = op as u8;
    }

    #[inline]
    fn remove_last_instruction(&mut self) {
        self.instructions.pop();
    }

    #[inline]
    fn remove_last_instruction_if(&mut self, op: OpCode) {
        if self.last_instruction_is(op) {
            self.remove_last_instruction();
        }
    }

    fn change_instruction_operand_at(&mut self, op: OpCode, pos: usize, new_value: usize) {
        debug_assert_eq!(self.instructions[pos], op as u8);

        // TODO: For opcodes with less than 2 operands, we need to account for it here.
        self.instructions[pos + 1] = byte!(new_value, 0);
        self.instructions[pos + 2] = byte!(new_value, 1);
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
            Stmt::Let(name, value) => {
                let symbol = self.symbol_table.define(name);
                self.compile_expression(value);

                // TODO: Emit SetLocal if this is a local scope?
                if symbol.scope == Scope::Global {
                    self.add_instruction(OpCode::SetGlobal, symbol.index);
                } else {
                    self.add_instruction(OpCode::SetLocal, symbol.index);
                }
            }
            Stmt::Return(expr) => {
                // TODO: Allow expression to be omitted (needs work in parser first)
                self.compile_expression(expr);
                self.add_instruction(OpCode::ReturnValue, 0);
            }
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
            Expr::Identifier(name) => {
                let symbol = self.symbol_table.resolve(name);
                match symbol {
                    Some(symbol) => {
                        if symbol.scope == Scope::Global {
                            self.add_instruction(OpCode::GetGlobal, symbol.index);
                        } else {
                            self.add_instruction(OpCode::GetLocal, symbol.index);
                        }
                    }
                    None => panic!("Invalid identifier."),
                }
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
                // Compile function in a new scope
                let mut scope = CompilerScope::new(self.constants, self.symbol_table);
                scope.symbol_table.enter_scope();
                for p in parameters {
                    scope.symbol_table.define(p);
                }

                scope.compile_block_statement(body);

                if scope.last_instruction_is(OpCode::Pop) {
                    scope.replace_last_instruction(OpCode::ReturnValue);
                } else if !scope.last_instruction_is(OpCode::ReturnValue) {
                    scope.add_instruction(OpCode::Return, 0);
                }

                let num_locals = scope.symbol_table.leave_scope() as u8;
                // Shrink instructions to least possible size
                let mut instructions = scope.instructions;
                instructions.shrink_to_fit();

                let id = self.add_constant(NlObject::CompiledFunction(Box::new((
                    instructions,
                    num_locals,
                ))));
                self.add_instruction(OpCode::Const, id);
            }
            Expr::Call(expr) => {
                for a in &expr.arguments {
                    self.compile_expression(a);
                }
                self.compile_expression(&expr.left);

                self.add_instruction(OpCode::Call, expr.arguments.len());
            }

            _ => unimplemented!("Can not yet compile expressions of type {:?}", expr),
        }
    }

    fn add_constant(&mut self, obj: NlObject) -> usize {
        // re-use already defined constants
        if let Some(pos) = self.constants.iter().position(|c| c == &obj) {
            return pos;
        }

        self.constants.push(obj);
        self.constants.len() - 1
    }
}

pub(crate) struct Program {
    pub(crate) constants: Vec<NlObject>,
    pub(crate) instructions: Vec<u8>,
}

#[derive(PartialEq)]
enum Scope {
    Local,
    Global,
}

impl Program {
    pub(crate) fn new(ast: &BlockStmt) -> Self {
        let mut symbol_table = SymbolTable::new();
        let mut constants = Vec::with_capacity(64);
        let mut scope = CompilerScope::new(&mut constants, &mut symbol_table);
        scope.compile_block_statement(ast);
        scope.add_instruction(OpCode::Halt, 0);

        // Shrink instructions to least possible size
        scope.instructions.shrink_to_fit();
        let instructions = scope.instructions;

        // Shrink constants to least possible size
        constants.shrink_to_fit();

        Self {
            constants,
            instructions,
        }
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
            OpCode::Halt => f.write_str("Halt"),
        }
    }
}

// Converts an array of bytes to a string representation consisting of the OpCode along with their u16 values
// For example: [OpCode::Const, 1, 0] -> "Const(1)"
#[allow(dead_code)]
pub fn bytecode_to_human(code: &[u8]) -> String {
    let mut ip = 0;
    let mut str = String::with_capacity(256);

    loop {
        let op = OpCode::from(code[ip]);
        match op {
            // Halt (end of program, return str)
            OpCode::Halt => break,
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
            OpCode::Call
            | OpCode::SetLocal
            | OpCode::GetGlobal
            | OpCode::SetGlobal
            | OpCode::GetLocal => {
                str.push_str(&op.to_string());
                str.push_str(&format!("({})", code[ip + 1]));
                ip += 2;
            }

            // Single opcode (no operands)
            _ => {
                str.push_str(&op.to_string());
                ip += 1;
            }
        }
        str.push_str(" ");
    }

    // trim trailing whitespace while modifying original string in place
    str.truncate(str.trim().len());
    return str;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn run(program: &str) -> String {
        let ast = parse(program).unwrap();
        let program = Program::new(&ast);
        bytecode_to_human(&program.instructions)
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
        assert_eq!(run("5"), "Const(0) Pop");
        assert_eq!(run("5; 5"), "Const(0) Pop Const(0) Pop");
        assert_eq!(run("5; 6; 5"), "Const(0) Pop Const(1) Pop Const(0) Pop");
    }

    #[test]
    fn test_bool_expression() {
        assert_eq!(run("ja"), "True Pop");
        assert_eq!(run("ja; ja"), "True Pop True Pop");
        assert_eq!(run("nee"), "False Pop");
    }

    #[test]
    fn test_float_expression() {
        assert_eq!(run("1.23"), "Const(0) Pop");
        assert_eq!(run("1.23; 1.23"), "Const(0) Pop Const(0) Pop");
        assert_eq!(
            run("5.00; 6.00; 5.00"),
            "Const(0) Pop Const(1) Pop Const(0) Pop"
        );
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
        // Const(0) = 1
        // Const(1) = 2
        // Const(2) = functie(a, b) { ... }
        // Call(2) = call last object on stack with 2 args
        assert_eq!(
            run("functie(a, b) { 1 }(1, 2)"),
            "Const(0) Const(1) Const(2) Call(2) Pop"
        );
    }

    #[test]
    fn test_declare_statement() {
        assert_eq!(run("stel a = 1;"), "Const(0) SetGlobal(0)");

        assert_eq!(
            run("stel a = 1; stel b = 2;"),
            "Const(0) SetGlobal(0) Const(1) SetGlobal(1)"
        );

        // TODO: Test scoped variables
    }

    #[test]
    fn test_ident_expressions() {
        assert_eq!(
            run("stel a = 1; a"),
            "Const(0) SetGlobal(0) GetGlobal(0) Pop"
        );

        assert_eq!(
            run("stel a = 1; stel b = 2; a; b;"),
            "Const(0) SetGlobal(0) Const(1) SetGlobal(1) GetGlobal(0) Pop GetGlobal(1) Pop"
        );

        // TODO: Test scoped variables
    }
}
