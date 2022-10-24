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
    pub(crate) operator: Operator,
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
    pub(crate) operator: Operator,
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
    Let(String, Expr),
    // Return(Expr),
    Expr(Expr),
    // Break,
    // Continue,
}

pub(crate) type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Eq, Debug)]
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
}

impl From<&Token<'_>> for Operator {
    fn from(value: &Token) -> Self {
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
            _ => unimplemented!(
                "Parsing token {:?} into operator is not implemented.",
                value
            ),
        }
    }
}

impl ExprPrefix {
    pub fn new(operator: Operator, right: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Prefix(ExprPrefix { operator, right }))
    }
}

impl ExprInfix {
    pub fn new(left: Box<Expr>, operator: Operator, right: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Infix(ExprInfix { left, operator, right }))
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
