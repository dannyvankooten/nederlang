use std::boxed::Box;

use crate::lexer::{Tokenizer, Token, TokenKind};

#[derive(PartialEq, Eq, Debug)]
enum Expression {
    Infix{
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    Prefix,
    Integer(i64),
    Float,
    Identifier,
    Boolean,
    Function,
    Call,
    String,
    Array,
    Index,
    For,
    While,
    Assign,
}

enum Statement {
    Let,
    Return,
    Expr,
    Break,
    Continue,
}

#[derive(PartialEq, Eq, Debug)]
enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    GreaterThan,
    GreaterThanOrEquals,
    LessThan,
    LessThanOrEquals,
    Equals,
    NotEquals,
    Negate,
    And,
    Or,
    Modulo,
}

#[derive(PartialOrd, PartialEq, Copy, Clone)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Method,
    Prefix,
    Call,
    Index,
}

impl From<Token<'_>> for Precedence {
    fn from(t: Token) -> Self {
        match t.kind {
            // TODO! Add Neq
            TokenKind::Eq => Precedence::Equals,
            TokenKind::Lt | TokenKind::Gt => Precedence::LessGreater,
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Slash | TokenKind::Star => Precedence::Product,
            TokenKind::OpenParen => Precedence::Call,
            TokenKind::OpenBracket => Precedence::Index,
            TokenKind::Dot => Precedence::Method,
            _ => Precedence::Lowest,

        }
    }
}

impl Precedence {
    fn from_token(t: &Token) -> Precedence {
        match t.kind {
            // TODO! Add Neq
            TokenKind::Eq => Precedence::Equals,
            TokenKind::Lt | TokenKind::Gt => Precedence::LessGreater,
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Slash | TokenKind::Star => Precedence::Product,
            TokenKind::OpenParen => Precedence::Call,
            TokenKind::OpenBracket => Precedence::Index,
            TokenKind::Dot => Precedence::Method,
            _ => Precedence::Lowest,

        }
    }
}

impl From<&Token<'_>> for Operator {
    fn from(value: &Token) -> Self {
        match value.kind {
            TokenKind::Plus => Operator::Add,
            TokenKind::Minus => Operator::Subtract,
            TokenKind::Slash => Operator::Divide,
            TokenKind::Star => Operator::Multiply,
            TokenKind::Percent => Operator::Modulo,
            TokenKind::And => Operator::And,
            TokenKind::Or => Operator::Or,
            _ => todo!(),
        }
    }
}

struct Parser <'a> {
    tokenizer: Tokenizer<'a>,
    current: Token<'a>,
    next: Token<'a>,
}

impl <'a> Parser <'a> {
    fn new(mut tokenizer: Tokenizer) -> Parser {
        let current = tokenizer.next().unwrap_or(Token{ kind: TokenKind::EOF, len: 0 });
        let next = tokenizer.next().unwrap_or(Token{ kind: TokenKind::EOF, len: 0 });
        Parser {
            tokenizer,
            current,
            next,
        }
    }

    fn advance(&mut self) {
        self.current = self.next;
        self.next = self.tokenizer.next().unwrap_or(Token{ kind: TokenKind::EOF, len: 0 });
    }

    fn infix_expression(&mut self, left: Box<Expression>) -> Box<Expression> {
        self.advance();
        let operator = Operator::from(&self.current);

        self.advance();
        return Box::new(Expression::Infix { 
            left,
            operator: operator, 
            right: self.expression(Precedence::from_token(&self.current)) 
        })
    }

    fn expression(&mut self, precedence: Precedence) -> Box<Expression> {
        let mut left = match &self.current.kind {
            TokenKind::Numerical(s) => {
                Box::new(Expression::Integer(s.parse().unwrap()))
            },
            _ => panic!("Unsupported expression type: {:?}", self.current),
        };

        // keep going
        while self.next.kind != TokenKind::Semi && precedence < Precedence::from_token(&self.next) {
            left = match self.next.kind {
                TokenKind::Lt | TokenKind::Gt => self.infix_expression(left),
                TokenKind::Plus | TokenKind::Minus => self.infix_expression(left),
                TokenKind::Slash | TokenKind::Star => self.infix_expression(left),
                _ => return left,
            };
        }
       

        return left;
    }
}


#[derive(PartialEq, Debug)]
pub enum MyObject {
    Int(i64)
}

fn eval(expr: &Expression) -> MyObject {
    match expr {
        Expression::Infix { left, operator, right } => {
            let left = eval(left);
            let right = eval(right);
            match operator {
                Operator::Add => {
                    match (left, right) {
                        (MyObject::Int(a), MyObject::Int(b)) => MyObject::Int(a + b),
                        _ => todo!(),
                    }
                },
                Operator::Subtract => {
                    match (left, right) {
                        (MyObject::Int(a), MyObject::Int(b)) => MyObject::Int(a - b),
                        _ => todo!(),
                    }
                },
                Operator::Multiply => {
                    match (left, right) {
                        (MyObject::Int(a), MyObject::Int(b)) => MyObject::Int(a * b),
                        _ => todo!(),
                    }
                },
                Operator::Divide => {
                    match (left, right) {
                        (MyObject::Int(a), MyObject::Int(b)) => MyObject::Int(a / b),
                        _ => todo!(),
                    }
                },
                _ => todo!()
            }
        },
        Expression::Integer(v) => MyObject::Int(*v),
        _ => todo!(),
    }
}

fn parse(program: &str) -> Box<Expression> {
    let tokenizer = Tokenizer::new(program);
    Parser::new(tokenizer).expression(Precedence::Lowest)
}

mod test {
    use super::*;
    use Expression as E;

    #[test]
    fn test_expression_parsing() {
        let lbox = Box::new(E::Integer(5));
        let rbox = Box::new(E::Integer(1));

        assert_eq!(parse("5 + 1"), Box::new(E::Infix{ 
            left: lbox,
            operator: Operator::Add,
            right: rbox,
        }))
    }

    #[test]
    fn test_eval() {
        assert_eq!(eval(&parse("5+1")), MyObject::Int(6));
        assert_eq!(eval(&parse("5*2")), MyObject::Int(10));
        assert_eq!(eval(&parse("5/5")), MyObject::Int(1));
        assert_eq!(eval(&parse("5-5")), MyObject::Int(0));
        assert_eq!(eval(&parse("1 + 5 * 2")), MyObject::Int(11));
    }
}