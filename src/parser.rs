use std::boxed::Box;
use crate::lexer::{Token, Tokenizer};

#[derive(PartialEq, Debug)]
pub(crate) enum Expr {
    Infix(ExprInfix),
    Prefix(ExprPrefix),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    If(ExprIf),
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
pub(crate) type Program = BlockStmt;

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
            _ => unimplemented!(
                "Parsing token {:?} into operator is not implemented.",
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
    /// Creates a new Parser from the given input string
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

    /// Advances the parser (reads the next token)
    fn advance(&mut self) {
        self.current = self.next;
        self.next = self.tokenizer.next().unwrap_or(Token::Illegal);
    }

    /// Parses an operator token
    fn op(&mut self) -> Op {
        self.advance();
        Op::from(&self.current)
    }

    /// Parses an infix expression, like [Token::Int(5), Token::Minus, Token::Int(5)]
    fn infix_expr(&mut self, left: Box<Expr>) -> Result<Box<Expr>, ParseError> {
        return Ok(Box::new(Expr::Infix(ExprInfix {
            left,
            op: self.op(),
            right: self.expr(Precedence::from(&self.current))?,
        })));
    }

    /// Parses a prefix expression, like [Token::Minus, Token::Int(5)]
    fn prefix(&mut self, op: Op) -> Result<Box<Expr>, ParseError> {
        return Ok(Box::new(Expr::Prefix(ExprPrefix {
            op: op,
            right: self.expr(Precedence::from(&self.current))?,
        })));
    }

    /// Assert next token is of the given type and skips it
    fn skip(&mut self, t: Token) -> Result<(), ParseError>{
        if self.next != t {
            return Err(ParseError{ message: format!("Unexpected token: expected {:?}, got {:?}", t, self.next).to_owned() });
        }
        assert_eq!(self.next, t);
        self.advance();

        Ok(())
    }

    /// Skips the next token if it is of the given type
    fn maybe_skip(&mut self, t: Token) {
        if self.next == t {
            self.advance()
        }
    }

    /// Parse an expression
    fn expr(&mut self, precedence: Precedence) -> Result<Box<Expr>, ParseError> {
        self.advance();

        let mut left = match self.current {
            Token::Int(s) => Box::new(Expr::Integer(s.parse().unwrap())),
            Token::Float(s) => Box::new(Expr::Float(s.parse().unwrap())),
            Token::True => Box::new(Expr::Boolean(true)),
            Token::False => Box::new(Expr::Boolean(false)),
            Token::String(s) => Box::new(Expr::String(s.to_owned())),
            Token::OpenParen => {
                let expr = self.expr(Precedence::Lowest)?;
                // skip closing parenthesis
                self.skip(Token::CloseParen)?;
                expr
            }
            Token::If => {
                let condition = self.expr(Precedence::Lowest)?;
                let consequence = self.block_statement()?;
                let mut alternative = None;

                if self.next == Token::Else {
                    self.skip(Token::Else)?;
                    alternative = Some(self.block_statement()?);
                }

                let expr = Box::new( Expr::If(ExprIf{
                   condition,
                   consequence,
                   alternative,
                }));
                expr
            },
            Token::Bang | Token::Minus => self.prefix((&self.current).into())?,
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
                | Token::Percent => self.infix_expr(left)?,
                _ => return Ok(left),
            };
        }

        return Ok(left);
    }

    /// Parse a single statement
    fn statement(&mut self) -> Result<Stmt, ParseError> {
        return Ok(Stmt::Expr(*self.expr(Precedence::Lowest)?));
    }

    /// Parse a block (surrounded by curly braces)
    /// Can be an unnamed block, function body, if consequence, etc.
    fn block_statement(&mut self) -> Result<BlockStmt, ParseError> {
        let mut block = BlockStmt::with_capacity(64);
        self.skip(Token::OpenBrace)?;

        while self.next != Token::Illegal && self.next != Token::CloseBrace {
            block.push(self.statement()?);
            self.maybe_skip(Token::Semi);
        }

        self.skip(Token::CloseBrace)?;
        return Ok(block);
    }
}

#[derive(Debug)]
pub(crate) struct ParseError {
    pub(crate) message: String,
}

/// Parses the program string into an AST representation
pub(crate) fn parse(program: &str) -> Result<BlockStmt, ParseError> {
    let mut parser = Parser::new(program);
    let mut block = BlockStmt::with_capacity(64);

    while parser.next != Token::Illegal {
        block.push(parser.statement()?);
        parser.maybe_skip(Token::Semi);
    }

    return Ok(block);
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
            assert_eq!(parse(input).unwrap(), vec![ Stmt::Expr(expected_ast) ])
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
            assert_eq!(parse(input).unwrap(), vec![ Stmt::Expr(expected_ast) ])
        }
    }

    #[test]
    fn test_string_expressions() {
        for (input, expected_ast) in [("\"Wilhelmus\"", Expr::String("Wilhelmus".to_owned()))] {
            assert_eq!(parse(input).unwrap(), vec![ Stmt::Expr(expected_ast) ])
        }
    }
}
