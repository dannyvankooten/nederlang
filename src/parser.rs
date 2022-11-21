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
        Ok(ExprPrefix::new(operator, self.parse_expr(precedence)?))
    }

    /// Parses an if-else (or if-else-if) expression
    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        // skip over IF token
        self.advance();

        let condition = self.parse_expr(Precedence::Lowest)?;
        let consequence = self.parse_block()?;
        let alternative = if self.current_token == Token::Else {
            self.advance();

            if self.current_token == Token::If {
                Some(vec![self.parse_expr(Precedence::Lowest)?])
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };

        Ok(ExprIf::new(condition, consequence, alternative))
    }

    fn parse_assign_expr(&mut self, left: Expr) -> Result<Expr, ParseError> {
        // confirm left is of the correct type
        match left {
            Expr::Identifier(_) => (),
            _ => {
                return Err(ParseError::new(format!(
                    "can not assign to expression of type {:?}",
                    left
                )))
            }
        }

        // skip over Token::Assign
        self.advance();
        let right = self.parse_expr(Precedence::Assign)?;
        Ok(ExprAssign::new(left, right))
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

    fn parse_function_expr(&mut self) -> Result<Expr, ParseError> {
        self.advance();

        let name = match self.current_token {
            Token::Identifier(name) => Ok(name),
            _ => Err(ParseError::new("Expected identifier.".to_owned())),
        }?;
        self.advance();

        let mut parameters = vec![];
        self.skip(Token::OpenParen)?;

        // parse list of parameter names
        while self.current_token != Token::CloseParen {
            if let Token::Identifier(name) = self.current_token {
                parameters.push(name.to_owned());
                self.advance();
                self.skip_optional(Token::Comma);
            }
        }
        self.skip(Token::CloseParen)?;

        let body = self.parse_block()?;
        Ok(Expr::Function(ExprFunction {
            name: name.to_owned(),
            parameters,
            body,
        }))
    }

    fn parse_ident(&mut self, name: &str) -> Expr {
        let expr = Expr::Identifier(name.to_owned());
        self.advance();
        expr
    }

    fn parse_call_expr(&mut self, func: Expr) -> Result<Expr, ParseError> {
        if !matches!(func, Expr::Identifier(_)) && !matches!(func, Expr::Function(..)) {
            return Err(ParseError(format!(
                "Expression of type {:?} is not callable.",
                func
            )));
        }

        // skip Token::OpenParen
        self.advance();

        // parse argument list
        let mut arguments = vec![];
        while self.current_token != Token::CloseParen {
            arguments.push(self.parse_expr(Precedence::Lowest)?);
            self.skip_optional(Token::Comma);
        }

        // skip Token::CloseParen
        self.advance();

        Ok(ExprCall::new(func, arguments))
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
            Token::Identifier(name) => self.parse_ident(name),
            Token::Func => self.parse_function_expr()?,
            Token::Declare => self.parse_decl_expr()?,
            Token::OpenBrace => Expr::Block(self.parse_block()?),
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
                | Token::Percent => self.parse_infix_expr(left)?,
                Token::Assign => self.parse_assign_expr(left)?,
                Token::OpenParen => self.parse_call_expr(left)?,
                _ => return Ok(left),
            };
        }

        self.skip_optional(Token::Semi);

        Ok(left)
    }

    fn parse_decl_expr(&mut self) -> Result<Expr, ParseError> {
        // skip Token::Declare
        self.advance();

        let identifier = match self.current_token {
            Token::Identifier(name) => Ok(name.to_owned()),
            _ => Err(ParseError::new(format!(
                "Expected identifier, got {:?}",
                self.current_token
            ))),
        }?;

        // skip Token::Ident
        self.advance();

        // TODO: Make this part optional, for uninitialized vars?
        self.skip(Token::Assign)?;

        let value = self.parse_expr(Precedence::Lowest)?;
        Ok(Expr::Declare(ExprDeclare {
            name: identifier,
            value: Box::new(value),
        }))
    }

    /// Parse a block (surrounded by curly braces)
    /// Can be an unnamed block, function body, if consequence, etc.
    fn parse_block(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut block = Vec::with_capacity(64);
        self.skip(Token::OpenBrace)?;

        while self.current_token != Token::Illegal && self.current_token != Token::CloseBrace {
            block.push(self.parse_expr(Precedence::Lowest)?);
        }

        self.skip(Token::CloseBrace)?;
        Ok(block)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError(String);

impl ParseError {
    fn new(message: String) -> ParseError {
        ParseError(message)
    }
}

/// Parses the program string into an AST representation
pub(crate) fn parse(program: &str) -> Result<Vec<Expr>, ParseError> {
    let mut parser = Parser::new(program);
    let mut block = Vec::with_capacity(64);

    while parser.current_token != Token::Illegal {
        block.push(parser.parse_expr(Precedence::Lowest)?);
    }

    Ok(block)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bool_expression() {
        assert_eq!(parse("ja").unwrap(), vec![ExprBool::new(true)]);
        assert_eq!(parse("nee").unwrap(), vec![ExprBool::new(false)]);
    }

    #[test]
    fn test_int_expression() {
        assert_eq!(parse("5").unwrap(), vec![ExprInt::new(5)]);
        assert_eq!(
            parse("1; 2").unwrap(),
            vec![ExprInt::new(1), ExprInt::new(2)]
        );
    }

    #[test]
    fn test_float_expression() {
        assert_eq!(parse("5.55").unwrap(), vec![ExprFloat::new(5.55)])
    }

    #[test]
    fn test_string_expressions() {
        for (input, expected_ast) in [
            ("\"foo\"", ExprString::new("foo".to_owned())),
            ("\"foo bar\"", ExprString::new("foo bar".to_owned())),
        ] {
            assert_eq!(parse(input).unwrap(), vec![expected_ast])
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
            assert_eq!(parse(input).unwrap(), vec![expected_ast])
        }
    }

    #[test]
    fn test_prefix_expressions() {
        for (input, expected_ast) in [
            (
                "! ja",
                ExprPrefix::new(Operator::Negate, ExprBool::new(true)),
            ),
            (
                "-100",
                ExprPrefix::new(Operator::Subtract, ExprInt::new(100)),
            ),
        ] {
            assert_eq!(parse(input).unwrap(), vec![expected_ast])
        }
    }

    #[test]
    fn test_if_expressions() {
        for (input, expected_ast) in [
            (
                "als ja { 1 }",
                ExprIf::new(ExprBool::new(true), vec![ExprInt::new(1)], None),
            ),
            (
                "als ja { 1 } anders { 2 }",
                ExprIf::new(
                    ExprBool::new(true),
                    vec![ExprInt::new(1)],
                    Some(vec![ExprInt::new(2)]),
                ),
            ),
        ] {
            assert_eq!(parse(input).unwrap(), vec![expected_ast])
        }
    }

    #[test]
    fn test_declare_statements() {
        for (input, expected_ast) in [
            (
                "stel x = 100",
                Expr::Declare(ExprDeclare {
                    name: "x".to_owned(),
                    value: Box::new(ExprInt::new(100)),
                }),
            ),
            (
                "stel x = 100;",
                Expr::Declare(ExprDeclare {
                    name: "x".to_owned(),
                    value: Box::new(ExprInt::new(100)),
                }),
            ),
        ] {
            assert_eq!(parse(input).unwrap(), vec![expected_ast])
        }
    }

    #[test]
    fn test_assign_expressions() {
        for (input, expected_ast) in [(
            "stel a = 1; a = 2;",
            vec![
                Expr::Declare(ExprDeclare {
                    name: "a".to_owned(),
                    value: Box::new(ExprInt::new(1)),
                }),
                ExprAssign::new(Expr::Identifier("a".to_owned()), ExprInt::new(2)),
            ],
        )] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }
    }

    #[test]
    fn test_function_expression() {
        for (input, expected_ast) in [
            (
                "functie fib() { 1 }",
                vec![Expr::Function(ExprFunction {
                    name: "fib".to_owned(),
                    parameters: vec![],
                    body: vec![ExprInt::new(1)],
                })],
            ),
            (
                "functie fib(a) { 1 }",
                vec![Expr::Function(ExprFunction {
                    name: "fib".to_owned(),
                    parameters: vec!["a".to_owned()],
                    body: vec![ExprInt::new(1)],
                })],
            ),
            (
                "functie fib(a, b) { 1 }",
                vec![Expr::Function(ExprFunction {
                    name: "fib".to_owned(),
                    parameters: vec!["a".to_owned(), "b".to_owned()],
                    body: vec![ExprInt::new(1)],
                })],
            ),
        ] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }
    }

    #[test]
    fn test_ident_expression() {
        for (input, expected_ast) in [
            ("foo", vec![Expr::Identifier(String::from("foo"))]),
            (
                "foo; bar",
                vec![
                    Expr::Identifier(String::from("foo")),
                    Expr::Identifier(String::from("bar")),
                ],
            ),
        ] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }
    }

    #[test]
    fn test_call_expression() {
        for (input, expected_ast) in [
            (
                "foo()",
                vec![ExprCall::new(Expr::Identifier("foo".to_owned()), vec![])],
            ),
            (
                "foo(1)",
                vec![ExprCall::new(
                    Expr::Identifier("foo".to_owned()),
                    vec![ExprInt::new(1)],
                )],
            ),
            (
                "foo(1, 2)",
                vec![ExprCall::new(
                    Expr::Identifier("foo".to_owned()),
                    vec![ExprInt::new(1), ExprInt::new(2)],
                )],
            ),
        ] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }

        assert!(parse("1()").is_err());
        assert!(parse("ja()").is_err());
        assert!(parse("\"foo\"()").is_err());
    }
}
