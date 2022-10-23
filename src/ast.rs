use crate::lexer::Token;

#[derive(PartialEq, Debug)]
pub(crate) enum Expr {
    Infix(ExprInfix),
    Prefix(ExprPrefix),
    Int(ExprInt),
    Float(ExprFloat),
    Bool(ExprBool),
    If(ExprIf),
    Identifier,
    Function,
    Call,
    String(ExprString),
    Array,
    Index,
    For,
    While,
    Assign,
}

#[derive(PartialEq, Debug)]
pub(crate) struct ExprInfix {
    pub(crate) left: Box<Expr>,
    pub(crate) op: Op,
    pub(crate) right: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct ExprInt {
    pub(crate) value: i64,
}

#[derive(Debug, PartialEq)]
pub(crate) struct ExprFloat {
    pub(crate) value: f64,
}

#[derive(Debug, PartialEq)]
pub(crate) struct ExprBool {
    pub(crate) value: bool,
}

#[derive(Debug, PartialEq)]
pub(crate) struct ExprString {
    pub(crate) value: String,
}

#[derive(PartialEq, Debug)]
pub(crate) struct ExprPrefix {
    pub(crate) op: Op,
    pub(crate) right: Box<Expr>,
}

#[derive(PartialEq, Debug)]
pub(crate) struct ExprIf {
    pub(crate) condition: Box<Expr>,
    pub(crate) consequence: BlockStmt,
    pub(crate) alternative: Option<BlockStmt>,
}

#[derive(PartialEq, Debug)]
pub(crate) enum Stmt {
    // Let(String, Expr),
    // Return(Expr),
    Expr(Expr),
    // Break,
    // Continue,
}

pub(crate) type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
    Negate,
    And,
    Or,
    Modulo,
}

impl From<&Token<'_>> for Op {
    fn from(value: &Token) -> Self {
        match value {
            Token::Plus => Op::Add,
            Token::Minus => Op::Subtract,
            Token::Slash => Op::Divide,
            Token::Star => Op::Multiply,
            Token::Percent => Op::Modulo,
            Token::And => Op::And,
            Token::Or => Op::Or,
            Token::Gt => Op::Gt,
            Token::Gte => Op::Gte,
            Token::Lt => Op::Lt,
            Token::Lte => Op::Lte,
            Token::Eq => Op::Eq,
            Token::Neq => Op::Neq,
            Token::Bang => Op::Negate,
            _ => unimplemented!(
                "Parsing token {:?} into operator is not implemented.",
                value
            ),
        }
    }
}

impl ExprPrefix {
    pub fn new(op: Op, right: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Prefix(ExprPrefix { op, right }))
    }
}

impl ExprInfix {
    pub fn new(left: Box<Expr>, op: Op, right: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Infix(ExprInfix { left, op, right }))
    }
}

impl ExprIf {
    pub fn new(
        condition: Box<Expr>,
        consequence: BlockStmt,
        alternative: Option<BlockStmt>,
    ) -> Box<Expr> {
        Box::new(Expr::If(ExprIf {
            condition,
            consequence,
            alternative,
        }))
    }
}

impl ExprInt {
    pub fn new(value: i64) -> Box<Expr> {
        Box::new(Expr::Int(ExprInt { value }))
    }
}

impl ExprFloat {
    pub fn new(value: f64) -> Box<Expr> {
        Box::new(Expr::Float(ExprFloat { value }))
    }
}

impl ExprBool {
    pub fn new(value: bool) -> Box<Expr> {
        Box::new(Expr::Bool(ExprBool { value }))
    }
}

impl ExprString {
    pub fn new(value: String) -> Box<Expr> {
        Box::new(Expr::String(ExprString { value }))
    }
}
