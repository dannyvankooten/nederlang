use crate::lexer::Token;

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub enum Expr {
    Infix(ExprInfix),
    Prefix(ExprPrefix),
    Int(ExprInt),
    Float(ExprFloat),
    Bool(ExprBool),
    If(ExprIf),
    Identifier(String),
    Function(ExprFunction),
    Call(ExprCall),
    Assign(ExprAssign),
    String(ExprString),
    Declare(ExprDeclare),
    Block(Vec<Expr>),
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprDeclare {
    pub name: String,
    pub value: Box<Expr>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprFunction {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<Expr>,
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
    pub consequence: Vec<Expr>,
    pub alternative: Option<Vec<Expr>>,
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct ExprCall {
    pub func: Box<Expr>,
    pub arguments: Vec<Expr>,
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
    Negate,
    And,
    Or,
    Modulo,
    Assign,
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
    pub fn new(condition: Expr, consequence: Vec<Expr>, alternative: Option<Vec<Expr>>) -> Expr {
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
            func: Box::new(func),
            arguments: arguments,
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
