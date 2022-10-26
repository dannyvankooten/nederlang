use crate::lexer::Token;

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub(crate) enum Stmt {
    Let(String, Expr),
    Return(Expr),
    Expr(Expr),
    Block(BlockStmt),
    // Break,
    // Continue,
}

pub(crate) type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub(crate) enum Expr {
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
pub(crate) struct ExprIndex {
    pub(crate) left: Box<Expr>,
    pub(crate) index: Box<Expr>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub(crate) struct ExprInfix {
    pub(crate) left: Box<Expr>,
    pub(crate) operator: Operator,
    pub(crate) right: Box<Expr>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub(crate) struct ExprAssign {
    pub(crate) left: Box<Expr>,
    pub(crate) right: Box<Expr>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub(crate) struct ExprInt {
    pub(crate) value: i64,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub(crate) struct ExprFloat {
    pub(crate) value: f64,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub(crate) struct ExprBool {
    pub(crate) value: bool,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub(crate) struct ExprString {
    pub(crate) value: String,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub(crate) struct ExprPrefix {
    pub(crate) operator: Operator,
    pub(crate) right: Box<Expr>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub(crate) struct ExprIf {
    pub(crate) condition: Box<Expr>,
    pub(crate) consequence: BlockStmt,
    pub(crate) alternative: Option<BlockStmt>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub(crate) struct ExprCall {
    pub(crate) left: Box<Expr>,
    pub(crate) arguments: Vec<Expr>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub(crate) struct ExprWhile {
    pub(crate) condition: Box<Expr>,
    pub(crate) body: BlockStmt,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub(crate) struct ExprArray {
    pub(crate) values: Vec<Expr>,
}

#[derive(PartialEq, Eq, Debug, PartialOrd, Clone)]
pub(crate) enum Operator {
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
            Token::Bang => Operator::Negate,
            Token::Assign => Operator::Assign,
            _ => unimplemented!(
                "Parsing token {:?} into operator is not implemented.",
                value
            ),
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

/// Just a helper struct to easily create an Expr::Identifier with owned value
pub(crate) struct ExprIdent;
impl ExprIdent {
    pub fn new(v: &str) -> Expr {
        Expr::Identifier(v.to_owned())
    }
}
