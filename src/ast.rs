use std::fmt::Display;

use crate::lexer::Token;

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub enum Stmt {
    Let(String, Expr),
    Return(Expr),
    Expr(Expr),
    Block(BlockStmt),
    Break,
    Continue,
}

pub type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub enum Expr {
    Infix {
        left: Box<Expr>,
        operator: Operator,
        right: Box<Expr>,
    },
    Prefix {
        operator: Operator,
        right: Box<Expr>,
    },
    Int {
        value: i64,
    },
    Float {
        value: f64,
    },
    Bool {
        value: bool,
    },
    If {
        condition: Box<Expr>,
        consequence: BlockStmt,
        alternative: Option<BlockStmt>,
    },
    Identifier(String),
    Function {
        name: String,
        parameters: Vec<String>,
        body: BlockStmt,
    },
    Call {
        left: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Assign {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    String {
        value: String,
    },
    Array {
        values: Vec<Expr>,
    },
    Index {
        left: Box<Expr>,
        index: Box<Expr>,
    },
    // For(ExprFor),
    While {
        condition: Box<Expr>,
        body: BlockStmt,
    },
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

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expr::Identifier(_) => f.write_str("identifier"),
            Expr::Bool { .. } => f.write_str("boolean"),
            Expr::Float { .. } => f.write_str("float"),
            Expr::Int { .. } => f.write_str("int"),
            Expr::String { .. } => f.write_str("string"),
            Expr::Function { .. } => f.write_str("function"),
            Expr::Array { .. } => f.write_str("array"),

            _ => write!(f, "{self:?}"),
        }
    }
}
