use crate::ast::*;
use crate::lexer::{Token, Tokenizer};

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
}

impl<'a> Parser<'a> {
    /// Creates a new Parser from the given input string
    fn new(input: &str) -> Parser {
        let mut tokenizer = Tokenizer::new(input);
        let current_token = tokenizer.next().unwrap_or(Token::Illegal);
        Parser {
            tokenizer,
            current_token,
        }
    }

    /// Advances the parser (reads the next token)
    fn advance(&mut self) {
        self.current_token = self.tokenizer.next().unwrap_or(Token::Illegal);
    }

    /// Parses an operator token
    fn parse_operator(&mut self) -> Operator {
        Operator::from(&self.current_token)
    }

    /// Parses an infix expression, like [Token::Int(5), Token::Minus, Token::Int(5)]
    fn parse_infix_expr(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let operator = self.parse_operator();
        let precedence = self.current_token.precedence();
        self.advance();
        let right = self.parse_expr(precedence)?;
        Ok(ExprInfix::new(left, operator, right))
    }

    /// Parses a prefix expression, like [Token::Minus, Token::Int(5)]
    fn parse_prefix_expr(&mut self) -> Result<Expr, ParseError> {
        let operator = self.parse_operator();
        let precedence = self.current_token.precedence();
        self.advance();
        Ok(ExprPrefix::new(
            operator,
            self.parse_expr(precedence)?,
        ))
    }

    /// Parses an if-else (or if-else-if) expression
    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        // skip over IF token
        self.advance();

        let condition = self.parse_expr(Precedence::Lowest)?;
        let consequence = self.parse_block_statement()?;
        let alternative = if self.current_token == Token::Else {
            self.advance();

            if self.current_token == Token::If {
                Some(vec![self.parse_statement()?])
            } else {
                Some(self.parse_block_statement()?)
            }
        } else {
            None
        };

        Ok(ExprIf::new(condition, consequence, alternative))
    }

    /// Assert current token is of the given type and skips it
    fn skip(&mut self, t: Token) -> Result<(), ParseError> {
        if self.current_token != t {
            return Err(ParseError::new(format!(
                "Unexpected token: expected {:?}, got {:?}",
                t, self.current_token
            )));
        }
        self.advance();
        Ok(())
    }

    /// Skips the current token if it is of the given type
    fn skip_optional(&mut self, t: Token) {
        if self.current_token == t {
            self.advance()
        }
    }

    fn parse_int_expression(&mut self, strval: &str) -> Expr {
        self.advance();
        ExprInt::new(strval.parse().unwrap()) 
    }

    fn parse_float_expression(&mut self, strval: &str) -> Expr {
        self.advance();
        ExprFloat::new(strval.parse().unwrap()) 
    }

    fn parse_bool_expression(&mut self, value: bool) -> Expr {
        self.advance();
        ExprBool::new(value) 
    }

    fn parse_string_expression(&mut self, value: &str) -> Expr {
        self.advance();
        ExprString::new(value.to_owned())
    }

    /// Parse an expression
    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        let mut left = match self.current_token {
            Token::Int(s) => self.parse_int_expression(s),
            Token::Float(s) => self.parse_float_expression(s),
            Token::True => self.parse_bool_expression(true),
            Token::False => self.parse_bool_expression(false),
            Token::String(s) => self.parse_string_expression(s),
            Token::OpenParen => {
                self.advance();
                let expr = self.parse_expr(Precedence::Lowest)?;
                self.skip(Token::CloseParen)?;
                expr
            }
            Token::If => self.parse_if_expr()?,
            Token::Bang | Token::Minus => self.parse_prefix_expr()?,
            _ => todo!("Unsupported expression type: {:?}", self.current_token),
        };

        // keep going
        while self.current_token != Token::Semi && precedence < self.current_token.precedence() {
            left = match self.current_token {
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
                | Token::Percent => {
                    self.parse_infix_expr(left)?
                },
                _ => return Ok(left),
            };
        }

        Ok(left)
    }

    /// Parse a single statement
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        let stmt = match self.current_token {
            Token::Declare => {
                self.advance();

                let identifier = match self.current_token {
                    Token::Identifier(name) => Ok(name.to_owned()),
                    _ => Err(ParseError::new(format!("Expected identifier, got {:?}", self.current_token))),
                }?;
                self.advance();

                // TODO: Make this part optional, for uninitialized vars?
                self.skip(Token::Assign)?;

                let value = self.parse_expr(Precedence::Lowest)?;
                Stmt::Let(identifier, value)
            },
            _ => Stmt::Expr(self.parse_expr(Precedence::Lowest)?)
        };

        self.skip_optional(Token::Semi);

        Ok(stmt)
    }

    /// Parse a block (surrounded by curly braces)
    /// Can be an unnamed block, function body, if consequence, etc.
    fn parse_block_statement(&mut self) -> Result<BlockStmt, ParseError> {
        let mut block = BlockStmt::with_capacity(64);
        self.skip(Token::OpenBrace)?;

        while self.current_token != Token::Illegal && self.current_token != Token::CloseBrace {
            block.push(self.parse_statement()?);
        }

        self.skip(Token::CloseBrace)?;
        Ok(block)
    }
}

#[derive(Debug)]
pub(crate) struct ParseError {
    pub(crate) message: String,
}

impl ParseError {
    fn new(message: String) -> ParseError {
        ParseError { message }
    }
}

/// Parses the program string into an AST representation
pub(crate) fn parse(program: &str) -> Result<BlockStmt, ParseError> {
    let mut parser = Parser::new(program);
    let mut block = BlockStmt::with_capacity(64);

    while parser.current_token != Token::Illegal {
        block.push(parser.parse_statement()?);
    }

    Ok(block)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bool_expression() {
        assert_eq!(parse("ja").unwrap(), vec![Stmt::Expr(ExprBool::new(true))]);
        assert_eq!(
            parse("nee").unwrap(),
            vec![Stmt::Expr(ExprBool::new(false))]
        );
    }

    #[test]
    fn test_int_expression() {
        assert_eq!(parse("5").unwrap(), vec![Stmt::Expr(ExprInt::new(5))]);
        assert_eq!(parse("1; 2").unwrap(), vec![Stmt::Expr(ExprInt::new(1)), Stmt::Expr(ExprInt::new(2))]);

    }

    #[test]
    fn test_float_expression() {
        assert_eq!(
            parse("5.55").unwrap(),
            vec![Stmt::Expr(ExprFloat::new(5.55))]
        )
    }

    #[test]
    fn test_string_expressions() {
        for (input, expected_ast) in [
            ("\"foo\"", ExprString::new("foo".to_owned())),
            ("\"foo bar\"", ExprString::new("foo bar".to_owned())),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(expected_ast)])
        }
    }

    #[test]
    fn test_infix_expressions() {
        for (input, expected_ast) in [
            (
                "5 + 1",
                ExprInfix::new(ExprInt::new(5), Operator::Add, ExprInt::new(1)),
            ),
            (
                "5 - 1",
                ExprInfix::new(ExprInt::new(5), Operator::Subtract, ExprInt::new(1)),
            ),
            (
                "5 / 1",
                ExprInfix::new(ExprInt::new(5), Operator::Divide, ExprInt::new(1)),
            ),
            (
                "5 * 1",
                ExprInfix::new(ExprInt::new(5), Operator::Multiply, ExprInt::new(1)),
            ),
            (
                "5 % 1",
                ExprInfix::new(ExprInt::new(5), Operator::Modulo, ExprInt::new(1)),
            ),
            (
                "5 > 1",
                ExprInfix::new(ExprInt::new(5), Operator::Gt, ExprInt::new(1)),
            ),
            (
                "5 >= 1",
                ExprInfix::new(ExprInt::new(5), Operator::Gte, ExprInt::new(1)),
            ),
            (
                "5 == 1",
                ExprInfix::new(ExprInt::new(5), Operator::Eq, ExprInt::new(1)),
            ),
            (
                "5 != 1",
                ExprInfix::new(ExprInt::new(5), Operator::Neq, ExprInt::new(1)),
            ),
            (
                "5 < 1",
                ExprInfix::new(ExprInt::new(5), Operator::Lt, ExprInt::new(1)),
            ),
            (
                "5 <= 1",
                ExprInfix::new(ExprInt::new(5), Operator::Lte, ExprInt::new(1)),
            ),
            (
                "5.55 + 1",
                ExprInfix::new(ExprFloat::new(5.55), Operator::Add, ExprInt::new(1)),
            ),
            (
                "5.55 - 1",
                ExprInfix::new(ExprFloat::new(5.55), Operator::Subtract, ExprInt::new(1)),
            ),
            (
                "5.55 / 1",
                ExprInfix::new(ExprFloat::new(5.55), Operator::Divide, ExprInt::new(1)),
            ),
            (
                "5.55 * 1",
                ExprInfix::new(ExprFloat::new(5.55), Operator::Multiply, ExprInt::new(1)),
            ),
            (
                "5.55 % 1",
                ExprInfix::new(ExprFloat::new(5.55), Operator::Modulo, ExprInt::new(1)),
            ),
            (
                "5 * ( 1 + 1 )",
                ExprInfix::new(
                    ExprInt::new(5),
                    Operator::Multiply,
                    ExprInfix::new(ExprInt::new(1), Operator::Add, ExprInt::new(1)),
                ),
            ),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(expected_ast)])
        }
    }

    #[test]
    fn test_prefix_expressions() {
        for (input, expected_ast) in [
            ("! ja", ExprPrefix::new(Operator::Negate, ExprBool::new(true))),
            ("-100", ExprPrefix::new(Operator::Subtract, ExprInt::new(100))),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(expected_ast)])
        }
    }

    #[test]
    fn test_if_expressions() {
        for (input, expected_ast) in [
            ("als ja { 1 }", ExprIf::new(ExprBool::new(true), vec![Stmt::Expr(ExprInt::new(1))], None)),
            ("als ja { 1 } anders { 2 }", ExprIf::new(
                ExprBool::new(true), 
                vec![Stmt::Expr(ExprInt::new(1))], 
                Some(vec![Stmt::Expr(ExprInt::new(2))])),
            )
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(expected_ast)])
        }
    }

    #[test]
    fn test_declare_statements() {
        for (input, expected_ast) in [
            ("stel x = 100", Stmt::Let("x".to_owned(), ExprInt::new(100))),
        ] {
            assert_eq!(parse(input).unwrap(), vec![expected_ast])
        }
    }
}
