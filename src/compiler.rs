use crate::ast::*;
use crate::object::NlObject;

macro_rules! byte {
    ($value:expr, $position:literal) => {
        ($value >> $position) as u8
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
    Halt,
}

/// Lookup table for quickly converting from u8 to OpCode variant
/// The order here is significant!
static U8_TO_OPCOPDE_MAP: [OpCode; 15] = [
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
    OpCode::Halt,
];

impl From<u8> for OpCode {
    #[inline]
    fn from(value: u8) -> Self {
        unsafe { return *U8_TO_OPCOPDE_MAP.get_unchecked(value as usize) }
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

    fn statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.expression(expr);
                self.bytecode.push(OpCode::Pop as u8);
            }
            _ => unimplemented!("Can not yet compile statements of type {:?}", stmt),
        }
    }

    fn add_instruction(&mut self, operand: OpCode, value: usize) {
        match operand {
            OpCode::Const => {
                self.bytecode.push(operand as u8);
                self.bytecode.push(byte!(value, 0));
                self.bytecode.push(byte!(value, 1));
            }
            _ => unimplemented!(),
        }
    }

    fn operator(&mut self, operator: &Operator) {
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

    fn expression(&mut self, expr: &Expr) {
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
                self.expression(&*expr.left);
                self.expression(&*expr.right);
                self.operator(&expr.operator);
            }
            _ => unimplemented!("Can not yet compile expressions of type {:?}", expr),
        }
    }
}

pub(crate) fn compile(program: &BlockStmt) -> CompiledProgram {
    let mut cp = CompiledProgram::new();
    for statement in program {
        cp.statement(statement);
    }

    // end every program with OpCode::Halt so we can match on it to know when we're finished
    cp.bytecode.push(OpCode::Halt as u8);

    cp
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_int_expression() {
        let ast = parse("5").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [
                OpCode::Const as u8,
                0,
                0,
                OpCode::Pop as u8,
                OpCode::Halt as u8
            ]
        );

        let ast = parse("5; 5").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [
                OpCode::Const as u8,
                0,
                0,
                OpCode::Pop as u8,
                OpCode::Const as u8,
                1,
                0,
                OpCode::Pop as u8,
                OpCode::Halt as u8
            ]
        );
    }

    #[test]
    fn test_bool_expression() {
        let ast = parse("ja").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [OpCode::True as u8, OpCode::Pop as u8, OpCode::Halt as u8]
        );

        let ast = parse("nee").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [OpCode::False as u8, OpCode::Pop as u8, OpCode::Halt as u8]
        );
    }

    #[test]
    fn test_float_expression() {
        let ast = parse("1.23").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [
                OpCode::Const as u8,
                0,
                0,
                OpCode::Pop as u8,
                OpCode::Halt as u8
            ]
        );

        let ast = parse("1.23; 2.23").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [
                OpCode::Const as u8,
                0,
                0,
                OpCode::Pop as u8,
                OpCode::Const as u8,
                1,
                0,
                OpCode::Pop as u8,
                OpCode::Halt as u8
            ]
        );
    }

    #[test]
    fn test_infix_expression() {
        let ast = parse("1 + 2").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [
                OpCode::Const as u8,
                0,
                0,
                OpCode::Const as u8,
                1,
                0,
                OpCode::Add as u8,
                OpCode::Pop as u8,
                OpCode::Halt as u8
            ]
        );

        let ast = parse("1 - 2").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [
                OpCode::Const as u8,
                0,
                0,
                OpCode::Const as u8,
                1,
                0,
                OpCode::Subtract as u8,
                OpCode::Pop as u8,
                OpCode::Halt as u8
            ]
        );

        let ast = parse("1 / 2").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [
                OpCode::Const as u8,
                0,
                0,
                OpCode::Const as u8,
                1,
                0,
                OpCode::Divide as u8,
                OpCode::Pop as u8,
                OpCode::Halt as u8
            ]
        );

        let ast = parse("1 * 2").unwrap();
        assert_eq!(
            compile(&ast).bytecode,
            [
                OpCode::Const as u8,
                0,
                0,
                OpCode::Const as u8,
                1,
                0,
                OpCode::Multiply as u8,
                OpCode::Pop as u8,
                OpCode::Halt as u8
            ]
        );
    }
}
