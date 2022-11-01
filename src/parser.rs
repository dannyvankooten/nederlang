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
    #[inline]
    fn advance(&mut self) {
        self.current_token = self.tokenizer.next().unwrap_or(Token::Illegal);
    }

    /// Parses an operator token
    #[inline]
    fn parse_operator(&mut self) -> Operator {
        Operator::from(self.current_token)
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

    fn parse_assign_expr(&mut self, left: Expr) -> Result<Expr, ParseError> {
        // confirm left is of the correct type
        match left {
            Expr::Identifier(_) | Expr::Index(_) => (),
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
    #[inline]
    fn skip_optional(&mut self, t: Token) {
        if self.current_token == t {
            self.advance()
        }
    }

    #[inline]
    fn parse_int_expression(&mut self, strval: &str) -> Expr {
        self.advance();
        ExprInt::new(strval.parse().unwrap())
    }

    #[inline]
    fn parse_float_expression(&mut self, strval: &str) -> Expr {
        self.advance();
        ExprFloat::new(strval.parse().unwrap())
    }

    #[inline]
    fn parse_bool_expression(&mut self, value: bool) -> Expr {
        self.advance();
        ExprBool::new(value)
    }

    #[inline]
    fn parse_string_expression(&mut self, value: &str) -> Expr {
        self.advance();
        ExprString::new(value.to_owned())
    }

    fn parse_function_expr(&mut self) -> Result<Expr, ParseError> {
        // skip over Token::Func
        self.advance();

        // parse optional identifier
        // if this is not given, the function must be stored in a variable
        let name = match self.current_token {
            Token::Identifier(name) => {
                self.advance();
                name
            }
            _ => "",
        };

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

        let body = self.parse_block_statement()?;
        Ok(Expr::Function(name.to_owned(), parameters, body))
    }

    fn parse_ident(&mut self, name: &str) -> Expr {
        let expr = Expr::Identifier(name.to_owned());
        self.advance();
        expr
    }

    fn parse_call_expr(&mut self, func: Expr) -> Result<Expr, ParseError> {
        // validate that left-hand-side is a callable expression type
        match func {
            Expr::Identifier(_) | Expr::Function(_, _, _) => {}
            _ => {
                return Err(ParseError(format!(
                    "Expression of type {:?} is not callable.",
                    func
                )))
            }
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

    fn parse_while_expr(&mut self) -> Result<Expr, ParseError> {
        // skip over Token::While
        self.advance();

        let condition = self.parse_expr(Precedence::Lowest)?;
        let body = self.parse_block_statement()?;
        Ok(ExprWhile::new(condition, body))
    }

    fn parse_array_expr(&mut self) -> Result<Expr, ParseError> {
        // skip over Token::OpenBracket
        self.advance();

        let mut values = Vec::new();
        while self.current_token != Token::CloseBracket {
            values.push(self.parse_expr(Precedence::Lowest)?);
            self.skip_optional(Token::Comma);
        }

        self.skip(Token::CloseBracket)?;
        Ok(ExprArray::new(values))
    }

    fn parse_index_expr(&mut self, left: Expr) -> Result<Expr, ParseError> {
        // skip over Token::OpenBracket
        self.advance();
        let index = self.parse_expr(Precedence::Lowest)?;
        self.skip(Token::CloseBracket)?;
        Ok(ExprIndex::new(left, index))
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
            Token::While => self.parse_while_expr()?,
            Token::OpenBracket => self.parse_array_expr()?,
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
                Token::OpenBracket => self.parse_index_expr(left)?,
                _ => return Ok(left),
            };
        }

        Ok(left)
    }

    fn parse_decl_statement(&mut self) -> Result<Stmt, ParseError> {
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
        Ok(Stmt::Let(identifier, value))
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, ParseError> {
        // skip over Token::Return
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;
        Ok(Stmt::Return(expr))
    }

    /// Parse a single statement
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        let stmt = match self.current_token {
            Token::Declare => self.parse_decl_statement()?,
            Token::OpenBrace => Stmt::Block(self.parse_block_statement()?),
            Token::Return => self.parse_return_statement()?,
            _ => Stmt::Expr(self.parse_expr(Precedence::Lowest)?),
        };

        self.skip_optional(Token::Semi);

        Ok(stmt)
    }

    /// Parse a block (surrounded by curly braces)
    /// Can be an unnamed block, function body, if consequence, etc.
    fn parse_block_statement(&mut self) -> Result<BlockStmt, ParseError> {
        let mut block = BlockStmt::with_capacity(8);
        self.skip(Token::OpenBrace)?;

        while self.current_token != Token::Illegal && self.current_token != Token::CloseBrace {
            block.push(self.parse_statement()?);
        }

        self.skip(Token::CloseBrace)?;
        Ok(block)
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseError(String);

impl ParseError {
    fn new(message: String) -> ParseError {
        ParseError(message)
    }
}

/// Parses the program string into an AST representation
pub fn parse(program: &str) -> Result<BlockStmt, ParseError> {
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


    /// Just a helper struct to easily create an Expr::Identifier with owned value
    pub struct ExprIdent;
    impl ExprIdent {
        pub fn new(v: &str) -> Expr {
            Expr::Identifier(v.to_owned())
        }
    }

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
        assert_eq!(
            parse("1; 2").unwrap(),
            vec![Stmt::Expr(ExprInt::new(1)), Stmt::Expr(ExprInt::new(2))]
        );
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
            (
                "! ja",
                ExprPrefix::new(Operator::Negate, ExprBool::new(true)),
            ),
            (
                "-100",
                ExprPrefix::new(Operator::Subtract, ExprInt::new(100)),
            ),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(expected_ast)])
        }
    }

    #[test]
    fn test_if_expressions() {
        for (input, expected_ast) in [
            (
                "als ja { 1 }",
                ExprIf::new(ExprBool::new(true), vec![Stmt::Expr(ExprInt::new(1))], None),
            ),
            (
                "als ja { 1 } anders { 2 }",
                ExprIf::new(
                    ExprBool::new(true),
                    vec![Stmt::Expr(ExprInt::new(1))],
                    Some(vec![Stmt::Expr(ExprInt::new(2))]),
                ),
            ),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(expected_ast)])
        }
    }

    #[test]
    fn test_declare_statements() {
        for (input, expected_ast) in [
            ("stel x = 100", Stmt::Let("x".to_owned(), ExprInt::new(100))),
            (
                "stel x = 100;",
                Stmt::Let("x".to_owned(), ExprInt::new(100)),
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
                Stmt::Let("a".to_owned(), ExprInt::new(1)),
                Stmt::Expr(ExprAssign::new(
                    Expr::Identifier("a".to_owned()),
                    ExprInt::new(2),
                )),
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
                vec![Stmt::Expr(Expr::Function(
                    "fib".to_owned(),
                    vec![],
                    vec![Stmt::Expr(ExprInt::new(1))],
                ))],
            ),
            (
                "functie fib(a) { 1 }",
                vec![Stmt::Expr(Expr::Function(
                    "fib".to_owned(),
                    vec!["a".to_owned()],
                    vec![Stmt::Expr(ExprInt::new(1))],
                ))],
            ),
            (
                "functie fib(a, b) { 1 }",
                vec![Stmt::Expr(Expr::Function(
                    "fib".to_owned(),
                    vec!["a".to_owned(), "b".to_owned()],
                    vec![Stmt::Expr(ExprInt::new(1))],
                ))],
            ),
        ] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }
    }

    #[test]
    fn test_ident_expression() {
        for (input, expected_ast) in [
            (
                "foo",
                vec![Stmt::Expr(Expr::Identifier(String::from("foo")))],
            ),
            (
                "foo; bar",
                vec![
                    Stmt::Expr(Expr::Identifier(String::from("foo"))),
                    Stmt::Expr(Expr::Identifier(String::from("bar"))),
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
                vec![Stmt::Expr(ExprCall::new(
                    Expr::Identifier("foo".to_owned()),
                    vec![],
                ))],
            ),
            (
                "foo(1)",
                vec![Stmt::Expr(ExprCall::new(
                    Expr::Identifier("foo".to_owned()),
                    vec![ExprInt::new(1)],
                ))],
            ),
            (
                "foo(1, 2)",
                vec![Stmt::Expr(ExprCall::new(
                    Expr::Identifier("foo".to_owned()),
                    vec![ExprInt::new(1), ExprInt::new(2)],
                ))],
            ),
        ] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }

        assert!(parse("1()").is_err());
        assert!(parse("ja()").is_err());
        assert!(parse("\"foo\"()").is_err());
    }

    #[test]
    fn test_return_statements() {
        assert_eq!(
            parse("antwoord 1;"),
            Ok(vec![Stmt::Return(ExprInt::new(1))])
        );
        assert_eq!(
            parse("antwoord 1 + 2;"),
            Ok(vec![Stmt::Return(ExprInfix::new(
                ExprInt::new(1),
                Operator::Add,
                ExprInt::new(2)
            ))])
        );

        assert_eq!(
            parse("antwoord fib(1) + fib(2);"),
            Ok(vec![Stmt::Return(ExprInfix::new(
                ExprCall::new(ExprIdent::new("fib"), vec![ExprInt::new(1)]),
                Operator::Add,
                ExprCall::new(ExprIdent::new("fib"), vec![ExprInt::new(2)]),
            ))])
        );
    }

    #[test]
    fn test_array_expression() {
        for (input, expected_ast) in [
            ("[]", vec![Stmt::Expr(ExprArray::new(vec![]))]),
            (
                "[1]",
                vec![Stmt::Expr(ExprArray::new(vec![ExprInt::new(1)]))],
            ),
            (
                "[1, 2]",
                vec![Stmt::Expr(ExprArray::new(vec![
                    ExprInt::new(1),
                    ExprInt::new(2),
                ]))],
            ),
        ] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }
    }

    #[test]
    fn test_while_expression() {
        for (input, expected_ast) in [
            (
                "zolang ja { }",
                vec![Stmt::Expr(ExprWhile::new(
                    ExprBool::new(true),
                    BlockStmt::new(),
                ))],
            ),
            (
                "zolang ja { 1 }",
                vec![Stmt::Expr(ExprWhile::new(
                    ExprBool::new(true),
                    vec![Stmt::Expr(ExprInt::new(1))],
                ))],
            ),
        ] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }
    }

    #[test]
    fn test_index_expression() {
        for (input, expected_ast) in [
            (
                "a[0]",
                vec![Stmt::Expr(ExprIndex::new(
                    ExprIdent::new("a"),
                    ExprInt::new(0),
                ))],
            ),
            (
                "[1][0]",
                vec![Stmt::Expr(ExprIndex::new(
                    ExprArray::new(vec![ExprInt::new(1)]),
                    ExprInt::new(0),
                ))],
            ),
        ] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }
    }
}
