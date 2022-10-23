use crate::ast::*;
use crate::lexer::{Token, Tokenizer};
use std::boxed::Box;

/// Token precedence used for Exprs
/// The order here is important!
#[derive(PartialOrd, PartialEq)]
enum Precedence {
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

impl Token<'_> {
    /// Get the parsing precedence for a given Token
    fn precedence(&self) -> Precedence {
        match self {
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

struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    current_token: Token<'a>,
    next_token: Token<'a>,
}

impl<'a> Parser<'a> {
    /// Creates a new Parser from the given input string
    fn new(input: &str) -> Parser {
        let mut tokenizer = Tokenizer::new(input);
        let current_token = Token::Illegal;
        let next_token = tokenizer.next().unwrap_or(Token::Illegal);
        Parser {
            tokenizer,
            current_token,
            next_token,
        }
    }

    /// Advances the parser (reads the next token)
    fn advance(&mut self) {
        self.current_token = self.next_token;
        self.next_token = self.tokenizer.next().unwrap_or(Token::Illegal);
    }

    /// Parses an operator token
    fn parse_operator(&mut self) -> Op {
        self.advance();
        Op::from(&self.current_token)
    }

    /// Parses an infix expression, like [Token::Int(5), Token::Minus, Token::Int(5)]
    fn parse_infix_expr(&mut self, left: Box<Expr>) -> Result<Box<Expr>, ParseError> {
        let op = self.parse_operator();
        let right = self.parse_expr(self.current_token.precedence())?;
        Ok(ExprInfix::new(left, op, right))
    }

    /// Parses a prefix expression, like [Token::Minus, Token::Int(5)]
    fn parse_prefix_expr(&mut self, op: Op) -> Result<Box<Expr>, ParseError> {
        Ok(ExprPrefix::new(
            op,
            self.parse_expr(self.current_token.precedence())?,
        ))
    }

    /// Parses an if-else (or if-else-if) expression
    fn parse_if_expr(&mut self) -> Result<Box<Expr>, ParseError> {
        let condition = self.parse_expr(Precedence::Lowest)?;
        let consequence = self.parse_block_statement()?;
        let alternative = if self.next_token == Token::Else {
            self.skip(Token::Else)?;

            if self.next_token == Token::If {
                Some(vec![self.parse_statement()?])
            } else {
                Some(self.parse_block_statement()?)
            }
        } else {
            None
        };

        Ok(ExprIf::new(condition, consequence, alternative))
    }

    /// Assert next token is of the given type and skips it
    fn skip(&mut self, t: Token) -> Result<(), ParseError> {
        if self.next_token != t {
            return Err(ParseError {
                message: format!(
                    "Unexpected token: expected {:?}, got {:?}",
                    t, self.next_token
                ),
            });
        }
        self.advance();
        Ok(())
    }

    /// Skips the next token if it is of the given type
    fn skip_optional(&mut self, t: Token) {
        if self.next_token == t {
            self.advance()
        }
    }

    /// Parse an expression
    fn parse_expr(&mut self, precedence: Precedence) -> Result<Box<Expr>, ParseError> {
        self.advance();

        let mut left = match self.current_token {
            Token::Int(s) => ExprInt::new(s.parse().unwrap()),
            Token::Float(s) => ExprFloat::new(s.parse().unwrap()),
            Token::True => ExprBool::new(true),
            Token::False => ExprBool::new(false),
            Token::String(s) => ExprString::new(s.to_owned()),
            Token::OpenParen => {
                let expr = self.parse_expr(Precedence::Lowest)?;
                self.skip(Token::CloseParen)?;
                expr
            }
            Token::If => self.parse_if_expr()?,
            Token::Bang | Token::Minus => self.parse_prefix_expr((&self.current_token).into())?,
            _ => todo!("Unsupported expression type: {:?}", self.current_token),
        };

        // keep going
        while self.next_token != Token::Semi && precedence < self.next_token.precedence() {
            left = match self.next_token {
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
                | Token::Percent => self.parse_infix_expr(left)?,
                _ => return Ok(left),
            };
        }

        Ok(left)
    }

    /// Parse a single statement
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        Ok(Stmt::Expr(*self.parse_expr(Precedence::Lowest)?))
    }

    /// Parse a block (surrounded by curly braces)
    /// Can be an unnamed block, function body, if consequence, etc.
    fn parse_block_statement(&mut self) -> Result<BlockStmt, ParseError> {
        let mut block = BlockStmt::with_capacity(64);
        self.skip(Token::OpenBrace)?;

        while self.next_token != Token::Illegal && self.next_token != Token::CloseBrace {
            block.push(self.parse_statement()?);
            self.skip_optional(Token::Semi);
        }

        self.skip(Token::CloseBrace)?;
        Ok(block)
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

    while parser.next_token != Token::Illegal {
        block.push(parser.parse_statement()?);
        parser.skip_optional(Token::Semi);
    }

    Ok(block)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bool_expression() {
        assert_eq!(parse("ja").unwrap(), vec![Stmt::Expr(*ExprBool::new(true))]);
        assert_eq!(
            parse("nee").unwrap(),
            vec![Stmt::Expr(*ExprBool::new(false))]
        );
    }

    #[test]
    fn test_int_expression() {
        assert_eq!(parse("5").unwrap(), vec![Stmt::Expr(*ExprInt::new(5))])
    }

    #[test]
    fn test_float_expression() {
        assert_eq!(
            parse("5.55").unwrap(),
            vec![Stmt::Expr(*ExprFloat::new(5.55))]
        )
    }

    #[test]
    fn test_string_expressions() {
        for (input, expected_ast) in [
            ("\"foo\"", ExprString::new("foo".to_owned())),
            ("\"foo bar\"", ExprString::new("foo bar".to_owned())),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(*expected_ast)])
        }
    }

    #[test]
    fn test_infix_expressions() {
        for (input, expected_ast) in [
            (
                "5 + 1",
                ExprInfix::new(ExprInt::new(5), Op::Add, ExprInt::new(1)),
            ),
            (
                "5 - 1",
                ExprInfix::new(ExprInt::new(5), Op::Subtract, ExprInt::new(1)),
            ),
            (
                "5 / 1",
                ExprInfix::new(ExprInt::new(5), Op::Divide, ExprInt::new(1)),
            ),
            (
                "5 * 1",
                ExprInfix::new(ExprInt::new(5), Op::Multiply, ExprInt::new(1)),
            ),
            (
                "5 % 1",
                ExprInfix::new(ExprInt::new(5), Op::Modulo, ExprInt::new(1)),
            ),
            (
                "5 > 1",
                ExprInfix::new(ExprInt::new(5), Op::Gt, ExprInt::new(1)),
            ),
            (
                "5 >= 1",
                ExprInfix::new(ExprInt::new(5), Op::Gte, ExprInt::new(1)),
            ),
            (
                "5 == 1",
                ExprInfix::new(ExprInt::new(5), Op::Eq, ExprInt::new(1)),
            ),
            (
                "5 != 1",
                ExprInfix::new(ExprInt::new(5), Op::Neq, ExprInt::new(1)),
            ),
            (
                "5 < 1",
                ExprInfix::new(ExprInt::new(5), Op::Lt, ExprInt::new(1)),
            ),
            (
                "5 <= 1",
                ExprInfix::new(ExprInt::new(5), Op::Lte, ExprInt::new(1)),
            ),
            (
                "5.55 + 1",
                ExprInfix::new(ExprFloat::new(5.55), Op::Add, ExprInt::new(1)),
            ),
            (
                "5.55 - 1",
                ExprInfix::new(ExprFloat::new(5.55), Op::Subtract, ExprInt::new(1)),
            ),
            (
                "5.55 / 1",
                ExprInfix::new(ExprFloat::new(5.55), Op::Divide, ExprInt::new(1)),
            ),
            (
                "5.55 * 1",
                ExprInfix::new(ExprFloat::new(5.55), Op::Multiply, ExprInt::new(1)),
            ),
            (
                "5.55 % 1",
                ExprInfix::new(ExprFloat::new(5.55), Op::Modulo, ExprInt::new(1)),
            ),
            (
                "5 * ( 1 + 1 )",
                ExprInfix::new(
                    ExprInt::new(5),
                    Op::Multiply,
                    ExprInfix::new(ExprInt::new(1), Op::Add, ExprInt::new(1)),
                ),
            ),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(*expected_ast)])
        }
    }

    #[test]
    fn test_prefix_expressions() {
        for (input, expected_ast) in [
            ("! ja", ExprPrefix::new(Op::Negate, ExprBool::new(true))),
            ("-100", ExprPrefix::new(Op::Subtract, ExprInt::new(100))),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(*expected_ast)])
        }
    }
}
