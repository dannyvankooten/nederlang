use std::boxed::Box;

use crate::lexer::{Token, Tokenizer};

#[derive(PartialEq, Debug)]
pub(crate) enum Expr {
    Infix(ExprInfix),
    Prefix(ExprPrefix),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Identifier,
    Function,
    Call,
    String(String),
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

#[derive(PartialEq, Debug)]
pub(crate) struct ExprPrefix {
    pub(crate) op: Op,
    pub(crate) right: Box<Expr>,
}

pub(crate) enum Stmt {
    Let,
    Return,
    Expr,
    Break,
    Continue,
}

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

/// Token precedence used for Exprs
/// The order here is important!
#[derive(PartialOrd, PartialEq, Copy, Clone)]
pub enum Precedence {
    Lowest,
    Assign,
    OrAnd,
    Equals,
    LessGreater,
    Sum,
    Product,
    Method,
    Prefix,
    Call,
    Index,
}

impl From<&Token<'_>> for Precedence {
    fn from(t: &Token) -> Self {
        match t {
            Token::Assign => Precedence::Assign,
            Token::Or | Token::And => Precedence::OrAnd,
            Token::Lt | Token::Gt | Token::Lte | Token::Gte => Precedence::LessGreater,
            Token::Eq | Token::Neq => Precedence::Equals,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Star | Token::Percent => Precedence::Product,
            Token::Dot => Precedence::Method,
            Token::OpenParen => Precedence::Call,
            Token::OpenBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
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
            _ => todo!(
                "Parsing token {:?} into operator is not yet implemented.",
                value
            ),
        }
    }
}

struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    current: Token<'a>,
    next: Token<'a>,
}

impl<'a> Parser<'a> {
    fn new(input: &str) -> Parser {
        let mut tokenizer = Tokenizer::new(input);
        let current = Token::Illegal;
        let next = tokenizer.next().unwrap_or(Token::Illegal);
        Parser {
            tokenizer,
            current,
            next,
        }
    }

    fn advance(&mut self) {
        self.current = self.next;
        self.next = self.tokenizer.next().unwrap_or(Token::Illegal);
    }

    fn op(&mut self) -> Op {
        self.advance();
        Op::from(&self.current)
    }

    fn infix_expr(&mut self, left: Box<Expr>) -> Box<Expr> {
        return Box::new(Expr::Infix(ExprInfix {
            left,
            op: self.op(),
            right: self.expr(Precedence::from(&self.current)),
        }));
    }

    fn prefix(&mut self, op: Op) -> Box<Expr> {
        return Box::new(Expr::Prefix(ExprPrefix {
            op: op,
            right: self.expr(Precedence::from(&self.current)),
        }));
    }

    fn expr(&mut self, precedence: Precedence) -> Box<Expr> {
        self.advance();

        let mut left = match &self.current {
            Token::Numerical(s) => {
                if s.contains('.') {
                    Box::new(Expr::Float(s.parse().unwrap()))
                } else {
                    Box::new(Expr::Integer(s.parse().unwrap()))
                }
            }
            Token::True => Box::new(Expr::Boolean(true)),
            Token::False => Box::new(Expr::Boolean(false)),
            Token::String(s) => Box::new(Expr::String(s.to_string())),
            Token::OpenParen => {
                let expr = self.expr(Precedence::Lowest);
                // skip closing parenthesis
                self.advance();
                expr
            }
            Token::Bang | Token::Minus => self.prefix((&self.current).into()),
            _ => todo!("Unsupported expression type: {:?}", self.current),
        };

        // keep going
        while self.next != Token::Semi && precedence < Precedence::from(&self.next) {
            left = match self.next {
                Token::Lt
                | Token::Lte
                | Token::Gt
                | Token::Gte
                | Token::Eq
                | Token::Neq
                | Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Star
                | Token::Percent => self.infix_expr(left),
                _ => return left,
            };
        }

        return left;
    }
}

pub(crate) fn parse(program: &str) -> Box<Expr> {
    Parser::new(program).expr(Precedence::Lowest)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_infix_expressions() {
        for (input, expected_ast) in [
            (
                "5 + 1",
                Expr::Infix(ExprInfix {
                    left: Box::new(Expr::Integer(5)),
                    op: Op::Add,
                    right: Box::new(Expr::Integer(1)),
                }),
            ),
            (
                "5 - 1",
                Expr::Infix(ExprInfix {
                    left: Box::new(Expr::Integer(5)),
                    op: Op::Subtract,
                    right: Box::new(Expr::Integer(1)),
                }),
            ),
            (
                "5 * ( 1 + 1 )",
                Expr::Infix(ExprInfix {
                    left: Box::new(Expr::Integer(5)),
                    op: Op::Multiply,
                    right: Box::new(Expr::Infix(ExprInfix {
                        left: Box::new(Expr::Integer(1)),
                        op: Op::Add,
                        right: Box::new(Expr::Integer(1)),
                    })),
                }),
            ),
        ] {
            assert_eq!(*parse(input), expected_ast)
        }
    }

    #[test]
    fn test_prefix_expressions() {
        for (input, expected_ast) in [(
            "! ja",
            Expr::Prefix(ExprPrefix {
                op: Op::Negate,
                right: Box::new(Expr::Boolean(true)),
            }),
        )] {
            assert_eq!(*parse(input), expected_ast)
        }
    }

    #[test]
    fn test_string_expressions() {
        for (input, expected_ast) in [("\"Wilhelmus\"", Expr::String("Wilhelmus".to_string()))] {
            assert_eq!(*parse(input), expected_ast)
        }
    }
}
