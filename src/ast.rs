use std::fmt::Display;

use crate::lexer::Token;

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub enum Stmt {
    Let(String, Expr),
    Return(Expr),
    Expr(Expr),
    Block(BlockStmt),
    // Break,
    // Continue,
}

pub type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub enum Expr {
    Infix(ExprInfix),
    Prefix(ExprPrefix),
    Int(ExprInt),
    Float(ExprFloat),
    Bool(ExprBool),
    If(ExprIf),
    Identifier(String),
    Function(String, Vec<String>, BlockStmt),
    Call(ExprCall),
    Assign(ExprAssign),
    String(ExprString),
    Array(ExprArray),
    Index(ExprIndex),
    // For(ExprFor),
    While(ExprWhile),
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprIndex {
    pub left: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprInfix {
    pub left: Box<Expr>,
    pub operator: Operator,
    pub right: Box<Expr>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprAssign {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ExprInt {
    pub value: i64,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ExprFloat {
    pub value: f64,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ExprBool {
    pub value: bool,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ExprString {
    pub value: String,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprPrefix {
    pub operator: Operator,
    pub right: Box<Expr>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprIf {
    pub condition: Box<Expr>,
    pub consequence: BlockStmt,
    pub alternative: Option<BlockStmt>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprCall {
    pub left: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprWhile {
    pub condition: Box<Expr>,
    pub body: BlockStmt,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprArray {
    pub values: Vec<Expr>,
}

#[derive(PartialEq, Eq, Debug, PartialOrd, Clone)]
pub enum Operator {
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
    Not,
    Negate,
    And,
    Or,
    Modulo,
    Assign,
}

impl From<Token<'_>> for Operator {
    fn from(value: Token) -> Self {
        match value {
            Token::Plus => Operator::Add,
            Token::Minus => Operator::Subtract,
            Token::Slash => Operator::Divide,
            Token::Star => Operator::Multiply,
            Token::Percent => Operator::Modulo,
            Token::And => Operator::And,
            Token::Or => Operator::Or,
            Token::Gt => Operator::Gt,
            Token::Gte => Operator::Gte,
            Token::Lt => Operator::Lt,
            Token::Lte => Operator::Lte,
            Token::Eq => Operator::Eq,
            Token::Neq => Operator::Neq,
            Token::Bang => Operator::Not,
            Token::Assign => Operator::Assign,
            _ => unimplemented!(
                "Parsing token {:?} into operator is not implemented.",
                value
            ),
        }
    }
}

/// We use a string representation of OpCodes to make testing a little easier
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expr::Identifier(_) => f.write_str("identifier"),
            Expr::Bool(_) => f.write_str("boolean"),
            Expr::Float(_) => f.write_str("float"),
            Expr::Int(_) => f.write_str("int"),
            Expr::String(..) => f.write_str("string"),
            Expr::Function(..) => f.write_str("function"),
            Expr::Array(..) => f.write_str("array"),

            _ => write!(f, "{:?}", self),
        }
    }
}

impl ExprPrefix {
    pub fn new(operator: Operator, right: Expr) -> Expr {
        Expr::Prefix(ExprPrefix {
            operator,
            right: Box::new(right),
        })
    }
}

impl ExprInfix {
    pub fn new(left: Expr, operator: Operator, right: Expr) -> Expr {
        Expr::Infix(ExprInfix {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }
}

impl ExprAssign {
    pub fn new(left: Expr, right: Expr) -> Expr {
        Expr::Assign(ExprAssign {
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

impl ExprIf {
    pub fn new(condition: Expr, consequence: BlockStmt, alternative: Option<BlockStmt>) -> Expr {
        Expr::If(ExprIf {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }
}

impl ExprCall {
    pub fn new(func: Expr, arguments: Vec<Expr>) -> Expr {
        Expr::Call(ExprCall {
            left: Box::new(func),
            arguments: arguments,
        })
    }
}

impl ExprArray {
    pub fn new(values: Vec<Expr>) -> Expr {
        Expr::Array(ExprArray { values })
    }
}

impl ExprWhile {
    pub fn new(condition: Expr, body: BlockStmt) -> Expr {
        Expr::While(ExprWhile {
            condition: Box::new(condition),
            body,
        })
    }
}

impl ExprIndex {
    pub fn new(left: Expr, index: Expr) -> Expr {
        Expr::Index(ExprIndex {
            left: Box::new(left),
            index: Box::new(index),
        })
    }
}

impl ExprInt {
    pub fn new(value: i64) -> Expr {
        Expr::Int(ExprInt { value })
    }
}

impl ExprFloat {
    pub fn new(value: f64) -> Expr {
        Expr::Float(ExprFloat { value })
    }
}

impl ExprBool {
    pub fn new(value: bool) -> Expr {
        Expr::Bool(ExprBool { value })
    }
}

impl ExprString {
    pub fn new(value: String) -> Expr {
        Expr::String(ExprString { value })
    }
}
