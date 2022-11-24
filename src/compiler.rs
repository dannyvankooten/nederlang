use std::fmt::Display;
use std::fmt::Write;

use crate::ast::*;
use crate::builtins;
use crate::gc::GC;
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
        ($instructions[$ip + 1] as usize) | (($instructions[$ip + 2] as usize) << 8)
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
    CallBuiltin,
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
    Array,
    IndexGet,
    IndexSet,
    Halt,
}

const JUMP_PLACEHOLDER: u16 = 1337;

impl From<u8> for OpCode {
    #[inline(always)]
    fn from(value: u8) -> Self {
        // Safety: Since we convert OpCode to u8 (with repr(u8)) the reverse should also be safe
        unsafe { std::mem::transmute(value) }
    }
}

impl OpCode {
    fn operands(&self) -> &[usize] {
        match self {
            // OpCodes with 1 operand of 2 bytes
            OpCode::Const | OpCode::Jump | OpCode::JumpIfFalse | OpCode::Array => &[2],

            // OpCodes with 2 operands of 2 bytes
            OpCode::GtLocalConst
            | OpCode::GteLocalConst
            | OpCode::LtLocalConst
            | OpCode::LteLocalConst
            | OpCode::EqLocalConst
            | OpCode::NeqLocalConst
            | OpCode::AddLocalConst
            | OpCode::SubtractLocalConst
            | OpCode::MultiplyLocalConst
            | OpCode::DivideLocalConst
            | OpCode::ModuloLocalConst => &[2, 2],

            // OpCodes with 2 operands of 1 bytes each
            OpCode::CallBuiltin => &[1, 1],

            // OpCodes with 1 operand op 1 byte:
            OpCode::Call => &[1],

            OpCode::SetLocal | OpCode::GetGlobal | OpCode::SetGlobal | OpCode::GetLocal => &[2],

            // OpCodes with no operands
            OpCode::Pop
            | OpCode::True
            | OpCode::False
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Divide
            | OpCode::Multiply
            | OpCode::Gt
            | OpCode::Gte
            | OpCode::Lt
            | OpCode::Lte
            | OpCode::Eq
            | OpCode::Neq
            | OpCode::And
            | OpCode::Or
            | OpCode::Not
            | OpCode::Modulo
            | OpCode::Negate
            | OpCode::Null
            | OpCode::Return
            | OpCode::ReturnValue
            | OpCode::IndexGet
            | OpCode::IndexSet
            | OpCode::Halt => &[],
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
    gc: GC,
}

impl Compiler {
    fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            instructions: Vec::with_capacity(64),
            constants: Vec::with_capacity(64),
            last_instruction: None,
            loop_contexts: Vec::new(),
            gc: GC::new(),
        }
    }

    #[inline]
    fn emit_opcode(&mut self, op: OpCode) {
        self.instructions.push(op as u8);
        self.last_instruction = Some(op);
    }

    #[inline]
    fn emit_u8(&mut self, v: u8) {
        self.instructions.push(v)
    }

    #[inline]
    fn emit_u16(&mut self, v: u16) {
        self.instructions.push((v & 0xFF) as u8);
        self.instructions.push(((v >> 8) & 0xFF) as u8);
    }

    #[inline]
    fn last_instruction_is(&self, op: OpCode) -> bool {
        self.last_instruction == Some(op)
    }

    #[inline]
    fn remove_last_instruction(&mut self) {
        debug_assert!(self.last_instruction.is_some());
        debug_assert_eq!(self.last_instruction.unwrap().operands().len(), 0);
        self.instructions.pop();
        self.last_instruction = None;
    }

    fn change_instruction_operand_at(&mut self, op: OpCode, pos: usize, new_value: usize) {
        assert_eq!(self.instructions[pos], op as u8);
        assert_eq!(op.operands().len(), 1);
        assert_eq!(op.operands().first(), Some(&2));

        // TODO: For opcodes with less than 2 operands, we need to account for it here.
        self.instructions[pos + 1] = byte!(new_value, 0);
        self.instructions[pos + 2] = byte!(new_value, 1);
    }

    fn compile_block_statement(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        // if block statement does not contain any other statements or expressions
        // simply push a NULL onto the stack
        if stmts.len() == 0 {
            self.emit_opcode(OpCode::Null);
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
                self.compile_expression(&expr)?;
                self.emit_opcode(OpCode::Pop);
            }
            Stmt::Block(stmts) => self.compile_block_statement(stmts)?,
            Stmt::Let(name, value) => {
                let symbol = self.symbols.define(&name);
                self.compile_expression(&value)?;
                let op = if symbol.scope == Scope::Global {
                    OpCode::SetGlobal
                } else {
                    OpCode::SetLocal
                };
                self.emit_opcode(op);
                self.emit_u16(symbol.index);
            }
            Stmt::Return(expr) => {
                // TODO: Allow expression to be omitted (needs work in parser first)
                self.compile_expression(&expr)?;
                self.emit_opcode(OpCode::ReturnValue);
            }
            Stmt::Break => {
                self.emit_opcode(OpCode::Null);
                let pos = self.instructions.len();
                self.emit_opcode(OpCode::Jump);
                self.emit_u16(JUMP_PLACEHOLDER);
                let ctx = match self.loop_contexts.last_mut() {
                    Some(ctx) => ctx,
                    None => return Err(Error::SyntaxError(format!("foutief gebruik van 'stop'"))),
                };
                ctx.break_instructions.push(pos);
            }
            Stmt::Continue => {
                self.emit_opcode(OpCode::Null);

                let pos = match self.loop_contexts.iter().last() {
                    Some(ctx) => Ok(ctx.start),
                    None => Err(Error::SyntaxError(format!(
                        "foutief gebruik van 'volgende'"
                    ))),
                }?;
                self.emit_opcode(OpCode::Jump);
                self.emit_u16(pos.try_into().unwrap());
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
            _ => panic!("unexpected operator of type {operator:?}"),
        };
        self.emit_opcode(opcode);
    }

    fn compile_const_var_infix_expression(
        &mut self,
        varname: &str,
        const_value: isize,
        operator: &Operator,
    ) -> Result<(), Error> {
        let idx_constant = self.add_constant(Object::int(const_value));
        let symbol = self.symbols.resolve(varname);
        match symbol {
            Some(symbol) => {
                let opcode = match (operator, symbol.scope) {
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
                        // This is just for other part of compiler to signal it should emit a normal instruction sequence
                        return Err(Error::ReferenceError(format!(
                        "Optimized variant of this operator & scope type is not yet implemented."
                    )));
                    }
                };

                self.emit_opcode(opcode);
                self.emit_u16(symbol.index);
                self.emit_u16(idx_constant);
            }
            None => {
                return Err(Error::ReferenceError(format!(
                    "{varname} is niet gedefinieerd"
                )))
            }
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expr) -> Result<(), Error> {
        match expr {
            Expr::Bool { value } => {
                let opcode = if *value { OpCode::True } else { OpCode::False };
                self.emit_opcode(opcode);
            }
            Expr::Float { value } => {
                let obj = Object::float(*value, &mut self.gc);
                let idx = self.add_constant(obj);
                self.emit_opcode(OpCode::Const);
                self.emit_u16(idx);
            }
            Expr::Int { value } => {
                let idx = self.add_constant(Object::int(*value));
                self.emit_opcode(OpCode::Const);
                self.emit_u16(idx);
            }
            Expr::String { value } => {
                let obj = Object::string(&value, &mut self.gc);
                let idx = self.add_constant(obj);
                self.emit_opcode(OpCode::Const);
                self.emit_u16(idx);
            }
            Expr::Identifier(name) => {
                let symbol = self.symbols.resolve(&name);
                match symbol {
                    Some(symbol) => {
                        let opcode = if symbol.scope == Scope::Global {
                            OpCode::GetGlobal
                        } else {
                            OpCode::GetLocal
                        };
                        self.emit_opcode(opcode);
                        self.emit_u16(symbol.index);
                    }
                    None => {
                        return Err(Error::ReferenceError(format!(
                            "{name} is niet gedefinieerd"
                        )))
                    }
                }
            }
            Expr::Prefix { operator, right } => {
                self.compile_expression(&*right)?;

                match operator {
                    Operator::Negate | Operator::Subtract => {
                        self.emit_opcode(OpCode::Negate);
                    }
                    Operator::Not => {
                        self.emit_opcode(OpCode::Not);
                    }

                    _ => {
                        return Err(Error::TypeError(format!(
                            "foutieve operator voor prefix expressie: {:?}",
                            operator
                        )))
                    }
                }
            }
            Expr::Assign { left, right } => {
                let name = match &**left {
                    Expr::Identifier(name) => name,
                    Expr::Index { left, index } => {
                        self.compile_expression(&*left)?;
                        self.compile_expression(&*index)?;
                        self.compile_expression(&*right)?;
                        self.emit_opcode(OpCode::IndexSet);
                        return Ok(());
                    }
                    _ => {
                        return Err(Error::TypeError(format!(
                            "kan geen waarde toewijzen aan expressies van type {:?}",
                            left
                        )))
                    }
                };

                let symbol = self.symbols.resolve(&name);
                match symbol {
                    Some(symbol) => {
                        self.compile_expression(&*right)?;

                        match symbol.scope {
                            Scope::Global => {
                                self.emit_opcode(OpCode::SetGlobal);
                                self.emit_u16(symbol.index);
                                self.emit_opcode(OpCode::GetGlobal);
                                self.emit_u16(symbol.index);
                            }

                            Scope::Local => {
                                self.emit_opcode(OpCode::SetLocal);
                                self.emit_u16(symbol.index);
                                self.emit_opcode(OpCode::GetLocal);
                                self.emit_u16(symbol.index);
                            }
                        }
                    }
                    None => {
                        return Err(Error::ReferenceError(format!(
                            "{name} is niet gedefinieerd"
                        )))
                    }
                }
            }
            Expr::Infix {
                left,
                operator,
                right,
            } => {
                // If this expression is a combination of a constant & a variable, create an optimized instruction for it that skips the stack
                match (&**left, &**right) {
                    (Expr::Identifier(name), Expr::Int { value })
                    | (Expr::Int { value }, Expr::Identifier(name)) => {
                        let res = self.compile_const_var_infix_expression(&name, *value, &operator);
                        if res.is_ok() {
                            return res;
                        }
                    }
                    _ => (),
                }

                // If that failed because we haven't implemented a specialized instruction yet, compile it as a sequence of normal instructions
                self.compile_expression(&*left)?;
                self.compile_expression(&*right)?;
                self.compile_operator(&operator);
            }
            Expr::If {
                condition,
                consequence,
                alternative,
            } => {
                self.compile_expression(&*condition)?;
                let pos_jump_if_false = self.instructions.len();
                self.emit_opcode(OpCode::JumpIfFalse);
                self.emit_u16(JUMP_PLACEHOLDER);

                self.compile_block_statement(consequence)?;

                if self.last_instruction_is(OpCode::Pop) {
                    self.remove_last_instruction();
                }

                let pos_jump = self.instructions.len();
                self.emit_opcode(OpCode::Jump);
                self.emit_u16(JUMP_PLACEHOLDER);

                // Change operand of last JumpIfFalse opcode to where we're currently at
                self.change_instruction_operand_at(
                    OpCode::JumpIfFalse,
                    pos_jump_if_false,
                    self.instructions.len(),
                );

                if let Some(alternative) = alternative {
                    self.compile_block_statement(alternative)?;
                    if self.last_instruction_is(OpCode::Pop) {
                        self.remove_last_instruction();
                    }
                } else {
                    self.emit_opcode(OpCode::Null);
                }

                // Change operand of last JumpIfFalse opcode to where we're currently at
                self.change_instruction_operand_at(OpCode::Jump, pos_jump, self.instructions.len());
            }
            Expr::While { condition, body } => {
                // TODO: Can we get rid of this now that empty block statement emit a NULL?
                self.emit_opcode(OpCode::Null);
                self.loop_contexts
                    .push(LoopContext::new(self.instructions.len()));
                let pos_before_condition = self.instructions.len();
                self.compile_expression(&*condition)?;

                let pos_jump_if_false = self.instructions.len();
                self.emit_opcode(OpCode::JumpIfFalse);
                self.emit_u16(JUMP_PLACEHOLDER);
                self.emit_opcode(OpCode::Pop);
                self.compile_block_statement(body)?;

                if self.last_instruction_is(OpCode::Pop) {
                    self.remove_last_instruction();
                } else {
                    self.emit_opcode(OpCode::Null);
                }

                // emit jump instruction to loop condition
                self.emit_opcode(OpCode::Jump);
                self.emit_u16(pos_before_condition.try_into().unwrap());

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
            Expr::Function {
                name,
                parameters,
                body,
            } => {
                let symbol = if name != "" {
                    Some(self.symbols.define(&name))
                } else {
                    None
                };

                let pos_jump = self.instructions.len();
                self.emit_opcode(OpCode::Jump);
                self.emit_u16(JUMP_PLACEHOLDER);

                // Compile function in a new scope
                self.symbols.new_context();
                for p in parameters {
                    self.symbols.define(&p);
                }

                let pos_start_function = self.instructions.len();

                self.compile_block_statement(body)?;

                if self.last_instruction_is(OpCode::Pop) {
                    self.remove_last_instruction();
                    self.emit_opcode(OpCode::ReturnValue);
                } else if !self.last_instruction_is(OpCode::ReturnValue) {
                    self.emit_opcode(OpCode::Return);
                }

                self.change_instruction_operand_at(OpCode::Jump, pos_jump, self.instructions.len());

                // Switch back to previous scope again
                let num_locals = self.symbols.leave_context();

                // Create function object and store as constant
                let obj = Object::function(pos_start_function.try_into().unwrap(), num_locals.try_into().unwrap());
                let idx = self.add_constant(obj);
                self.emit_opcode(OpCode::Const);
                self.emit_u16(idx);

                // If this function received a name, define it in the scope
                if let Some(symbol) = symbol {
                    let opcode = if symbol.scope == Scope::Global {
                        OpCode::SetGlobal
                    } else {
                        OpCode::SetLocal
                    };
                    self.emit_opcode(opcode);
                    self.emit_u16(symbol.index);

                    self.emit_opcode(OpCode::Const);
                    self.emit_u16(idx);
                }
            }
            Expr::Call { left, arguments } => 'compile_call: {
                for a in arguments {
                    self.compile_expression(a)?;
                }

                if let Expr::Identifier(name) = &**left {
                    if let Some(builtin) = builtins::resolve(name) {
                        self.emit_opcode(OpCode::CallBuiltin);
                        self.emit_u8(builtin as u8);
                        self.emit_u8(arguments.len().try_into().unwrap());
                        break 'compile_call;
                    }
                }
                self.compile_expression(&*left)?;
                self.emit_opcode(OpCode::Call);
                self.emit_u8(arguments.len().try_into().unwrap());
            }

            Expr::Array { values } => {
                for v in values {
                    self.compile_expression(v)?;
                }
                self.emit_opcode(OpCode::Array);
                self.emit_u16(values.len().try_into().unwrap());
            }

            Expr::Index { left, index } => {
                self.compile_expression(&*left)?;
                self.compile_expression(&*index)?;
                self.emit_opcode(OpCode::IndexGet);
            }
        }

        Ok(())
    }

    fn add_constant(&mut self, obj: Object) -> u16 {
        // re-use already defined constants
        if let Some(pos) = self
            .constants
            .iter()
            .position(|c| c.tag() == obj.tag() && c == &obj)
        {
            return pos.try_into().unwrap();
        }

        let idx = self.constants.len();
        self.constants.push(obj);
        idx.try_into().unwrap()
    }
}

pub(crate) struct Program {
    pub(crate) constants: Vec<Object>,
    pub(crate) instructions: Vec<u8>,
    pub(crate) gc: GC,
}

impl Program {
    pub(crate) fn new(ast: BlockStmt) -> Result<Self, Error> {
        let mut compiler = Compiler::new();

        compiler.compile_block_statement(&ast)?;
        compiler.emit_opcode(OpCode::Halt);

        // Copy all instructions for compiled functions over to main scope (after OpCode::Halt)
        let mut instructions = compiler.instructions;
        let mut constants = compiler.constants;

        // Shrink everything to least possible size
        instructions.shrink_to_fit();
        constants.shrink_to_fit();

        Ok(Self {
            constants,
            instructions,
            gc: compiler.gc,
        })
    }
}

/// We use a string representation of OpCodes to make testing a little easier
impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpCode::*;
        let s = match &self {
            Const => "Const",
            Pop => "Pop",
            True => "True",
            False => "False",
            Add => "Add",
            Subtract => "Subtract",
            Divide => "Divide",
            Multiply => "Multiply",
            Gt => "Gt",
            Gte => "Gte",
            Lt => "Lt",
            Lte => "Lte",
            Eq => "Eq",
            Neq => "Neq",
            And => "And",
            Or => "Or",
            Not => "Not",
            Modulo => "Modulo",
            Negate => "Negate",
            Jump => "Jump",
            JumpIfFalse => "JumpIfFalse",
            Null => "Null",
            Return => "Return",
            ReturnValue => "ReturnValue",
            Call => "Call",
            CallBuiltin => "CallBuiltin",
            GetLocal => "GetLocal",
            SetLocal => "SetLocal",
            GetGlobal => "GetGlobal",
            SetGlobal => "SetGlobal",
            GtLocalConst => "GtLocalConst",
            GteLocalConst => "GteLocalConst",
            LtLocalConst => "LtLocalConst",
            LteLocalConst => "LteLocalConst",
            EqLocalConst => "EqLocalConst",
            NeqLocalConst => "NeqLocalConst",
            AddLocalConst => "AddLocalConst",
            SubtractLocalConst => "SubtractLocalConst",
            MultiplyLocalConst => "MultiplyLocalConst",
            DivideLocalConst => "DivideLocalConst",
            ModuloLocalConst => "ModuloLocalConst",
            Array => "Array",
            IndexGet => "IndexGet",
            IndexSet => "IndexSet",
            Halt => "Halt",
        };
        f.write_str(s)
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

        if op.operands().len() > 0 {
            str.push('(');
        }
        for (i, width) in op.operands().iter().enumerate() {
            if i > 0 {
                str.push(',');
            }

            match width {
                2 => write!(str, "{}", read_u16_operand!(code, ip)).unwrap(),
                1 => write!(str, "{}", code[ip + 1]).unwrap(),
                _ => panic!("invalid operand width"),
            };
            ip += width;
        }
        if op.operands().len() > 0 {
            str.push(')');
        }

        ip += 1;
    }

    str
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn run(program: &str) -> String {
        let ast = parse(program).unwrap();
        let program = Program::new(ast).unwrap();
        bytecode_to_human(&program.instructions, false)
    }

    fn assert_bytecode_eq(program: &str, expected: &str) {
        let ast = parse(program).unwrap();
        let code = Program::new(ast).unwrap();
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

    #[test]
    fn test_call_builtin() {
        assert_eq!(
            run("print(\"hallo\")"),
            "Const(0) CallBuiltin(0,1) Pop Halt"
        )
    }
}
