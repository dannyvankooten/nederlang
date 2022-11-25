use crate::ast::*;
use crate::lexer::{Token, Tokenizer};
use crate::object::Error;

type ParseError = Error;

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
    #[inline]
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
    #[inline(always)]
    fn advance(&mut self) {
        self.current_token = self.tokenizer.next().unwrap_or(Token::Illegal);
    }

    /// Parses an operator token
    fn parse_operator(&mut self) -> Operator {
        Operator::from(self.current_token)
    }

    /// Parses OpAssign expressions, like a += 5
    /// This is just syntactic sugar, internally they are replaced by two seperate expression
    /// Perhaps this deserves its own specialized OpCode in the future to save some instructions
    fn parse_op_assign_expression(
        &mut self,
        left: Expr,
        operator: Operator,
    ) -> Result<Expr, ParseError> {
        self.advance();
        let right = self.parse_expr(Precedence::Lowest)?;
        Ok(Expr::Assign {
            left: Box::new(left.clone()),
            right: Box::new(Expr::Infix {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }),
        })
    }

    /// Parses an infix expression, like [Token::Int(5), Token::Minus, Token::Int(5)]
    fn parse_infix_expr(&mut self, left: Expr) -> Result<Expr, ParseError> {
        match &left {
            Expr::Function { .. } => {
                return Err(ParseError::TypeError(format!(
                "expressies van het type {left} kunnen niet worden gebruikt in een infix-expressie"
            )))
            }
            _ => (),
        }

        let operator = self.parse_operator();
        let precedence = self.current_token.precedence();
        self.advance();

        if self.current_token == Token::Assign && matches!(left, Expr::Identifier(_)) {
            return self.parse_op_assign_expression(left, operator);
        }

        let right = self.parse_expr(precedence)?;
        Ok(Expr::Infix {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    /// Parses a prefix expression, like [Token::Minus, Token::Int(5)]
    fn parse_prefix_expr(&mut self) -> Result<Expr, ParseError> {
        let operator = self.parse_operator();
        let precedence = self.current_token.precedence();
        self.advance();
        Ok(Expr::Prefix {
            operator,
            right: Box::new(self.parse_expr(precedence)?),
        })
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

        Ok(Expr::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_assign_expr(&mut self, left: Expr) -> Result<Expr, ParseError> {
        // confirm left is of the correct type
        match left {
            Expr::Identifier(_) | Expr::Index { .. } => (),
            _ => {
                return Err(ParseError::TypeError(format!(
                    "kan geen waarde toewijzen aan expressies van het type {left}"
                )))
            }
        }

        // skip over Token::Assign
        self.advance();
        let right = self.parse_expr(Precedence::Assign)?;
        Ok(Expr::Assign {
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    /// Assert current token is of the given type and skips it
    #[inline]
    fn skip(&mut self, t: Token) -> Result<(), ParseError> {
        if self.current_token != t {
            return Err(ParseError::SyntaxError(format!(
                "onverwachte token. verwacchte {t:?}, maar kreeg {:?}",
                self.current_token
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
        Expr::Int {
            value: strval.parse().unwrap(),
        }
    }

    #[inline]
    fn parse_float_expression(&mut self, strval: &str) -> Expr {
        self.advance();
        Expr::Float {
            value: strval.parse().unwrap(),
        }
    }

    #[inline]
    fn parse_bool_expression(&mut self, value: bool) -> Expr {
        // consume bool token
        self.advance();
        Expr::Bool { value }
    }

    #[inline]
    fn parse_string_expression(&mut self, value: &str) -> Expr {
        // consume string token
        self.advance();

        // read the string, skipping any escape sequences
        let mut b = value.chars().skip(1);
        let mut s = String::with_capacity(value.len());
        for ch in value.chars() {
            let next = b.next();
            if ch == '\\' && (next == Some('"') || next == Some('\\')) {
                continue;
            }

            s.push(ch);
        }

        // Since program came from user input
        // We have to replace escape sequences with their actual (single-char) value
        s = s.replace("\\n", "\n").replace("\\t", "\t");
        Expr::String { value: s }
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
        Ok(Expr::Function {
            name: name.to_owned(),
            parameters,
            body,
        })
    }

    #[inline]
    fn parse_ident(&mut self, name: &str) -> Expr {
        let expr = Expr::Identifier(name.to_owned());
        self.advance();
        expr
    }

    fn parse_call_expr(&mut self, left: Expr) -> Result<Expr, ParseError> {
        // validate that left-hand-side is a callable expression type
        match left {
            Expr::Identifier(_) | Expr::Function { .. } => {}
            _ => {
                return Err(ParseError::TypeError(format!(
                    "expressies van het type {left} zijn niet aanroepbaar"
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

        Ok(Expr::Call {
            left: Box::new(left),
            arguments,
        })
    }

    fn parse_while_expr(&mut self) -> Result<Expr, ParseError> {
        // skip over Token::While
        self.advance();

        let condition = self.parse_expr(Precedence::Lowest)?;
        let body = self.parse_block_statement()?;
        Ok(Expr::While {
            condition: Box::new(condition),
            body,
        })
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
        Ok(Expr::Array { values })
    }

    fn parse_index_expr(&mut self, left: Expr) -> Result<Expr, ParseError> {
        match &left {
            Expr::Identifier(_) | Expr::Array { .. } | Expr::String { .. } => (),
            _ => {
                return Err(ParseError::TypeError(format!(
                    "kan niet indexeren in expressies van het type {left}"
                )))
            }
        }

        // skip over Token::OpenBracket
        self.advance();
        let index = self.parse_expr(Precedence::Lowest)?;
        self.skip(Token::CloseBracket)?;
        Ok(Expr::Index {
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    /// Parse an expression
    #[inline]
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
            _ => {
                return Err(ParseError::SyntaxError(format!(
                    "onverwachte token. verwachtte een expressie, maar kreeg {:?}",
                    self.current_token
                )))
            }
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
                | Token::And
                | Token::Or
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
            _ => Err(ParseError::SyntaxError(format!(
                "onverwachte token. verwachtte een naam, kreeg een {:?}",
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
    #[inline]
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        let stmt = match self.current_token {
            Token::Declare => self.parse_decl_statement()?,
            Token::OpenBrace => Stmt::Block(self.parse_block_statement()?),
            Token::Return => self.parse_return_statement()?,
            Token::Continue => {
                self.advance();
                Stmt::Continue
            }
            Token::Break => {
                self.advance();
                Stmt::Break
            }
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
        assert_eq!(
            parse("ja").unwrap(),
            vec![Stmt::Expr(Expr::Bool { value: true })]
        );
        assert_eq!(
            parse("nee").unwrap(),
            vec![Stmt::Expr(Expr::Bool { value: false })]
        );
    }

    #[test]
    fn test_int_expression() {
        assert_eq!(
            parse("5").unwrap(),
            vec![Stmt::Expr(Expr::Int { value: 5 })]
        );
        assert_eq!(
            parse("1; 2").unwrap(),
            vec![
                Stmt::Expr(Expr::Int { value: 1 }),
                Stmt::Expr(Expr::Int { value: 2 })
            ]
        );
    }

    #[test]
    fn test_float_expression() {
        assert_eq!(
            parse("5.55").unwrap(),
            vec![Stmt::Expr(Expr::Float { value: 5.55 })]
        )
    }

    #[test]
    fn test_string_expressions() {
        for (input, expected_ast) in [
            (
                "\"foo\"",
                Expr::String {
                    value: "foo".to_owned(),
                },
            ),
            (
                "\"foo bar\"",
                Expr::String {
                    value: "foo bar".to_owned(),
                },
            ),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(expected_ast)])
        }
    }

    #[test]
    fn test_infix_expressions() {
        for (input, expected_ast) in [
            (
                "5 + 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Add,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 - 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Subtract,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 / 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Divide,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 * 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Multiply,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 % 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Modulo,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 > 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Gt,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 >= 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Gte,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 == 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Eq,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 != 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Neq,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 < 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Lt,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 <= 1",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Lte,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5.55 + 1",
                Expr::Infix {
                    left: Box::new(Expr::Float { value: 5.55 }),
                    operator: Operator::Add,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5.55 - 1",
                Expr::Infix {
                    left: Box::new(Expr::Float { value: 5.55 }),
                    operator: Operator::Subtract,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5.55 / 1",
                Expr::Infix {
                    left: Box::new(Expr::Float { value: 5.55 }),
                    operator: Operator::Divide,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5.55 * 1",
                Expr::Infix {
                    left: Box::new(Expr::Float { value: 5.55 }),
                    operator: Operator::Multiply,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5.55 % 1",
                Expr::Infix {
                    left: Box::new(Expr::Float { value: 5.55 }),
                    operator: Operator::Modulo,
                    right: Box::new(Expr::Int { value: 1 }),
                },
            ),
            (
                "5 * ( 1 + 1 )",
                Expr::Infix {
                    left: Box::new(Expr::Int { value: 5 }),
                    operator: Operator::Multiply,
                    right: Box::new(Expr::Infix {
                        left: Box::new(Expr::Int { value: 1 }),
                        operator: Operator::Add,
                        right: Box::new(Expr::Int { value: 1 }),
                    }),
                },
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
                Expr::Prefix {
                    operator: Operator::Not,
                    right: Box::new(Expr::Bool { value: true }),
                },
            ),
            (
                "-100",
                Expr::Prefix {
                    operator: Operator::Subtract,
                    right: Box::new(Expr::Int { value: 100 }),
                },
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
                Expr::If {
                    condition: Box::new(Expr::Bool { value: true }),
                    consequence: vec![Stmt::Expr(Expr::Int { value: 1 })],
                    alternative: None,
                },
            ),
            (
                "als ja { 1 } anders { 2 }",
                Expr::If {
                    condition: Box::new(Expr::Bool { value: true }),
                    consequence: vec![Stmt::Expr(Expr::Int { value: 1 })],
                    alternative: Some(vec![Stmt::Expr(Expr::Int { value: 2 })]),
                },
            ),
        ] {
            assert_eq!(parse(input).unwrap(), vec![Stmt::Expr(expected_ast)])
        }
    }

    #[test]
    fn test_declare_statements() {
        for (input, expected_ast) in [
            (
                "stel x = 100",
                Stmt::Let("x".to_owned(), Expr::Int { value: 100 }),
            ),
            (
                "stel x = 100;",
                Stmt::Let("x".to_owned(), Expr::Int { value: 100 }),
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
                Stmt::Let("a".to_owned(), Expr::Int { value: 1 }),
                Stmt::Expr(Expr::Assign {
                    left: Box::new(Expr::Identifier("a".to_owned())),
                    right: Box::new(Expr::Int { value: 2 }),
                }),
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
                vec![Stmt::Expr(Expr::Function {
                    name: "fib".to_owned(),
                    parameters: vec![],
                    body: vec![Stmt::Expr(Expr::Int { value: 1 })],
                })],
            ),
            (
                "functie fib(a) { 1 }",
                vec![Stmt::Expr(Expr::Function {
                    name: "fib".to_owned(),
                    parameters: vec!["a".to_owned()],
                    body: vec![Stmt::Expr(Expr::Int { value: 1 })],
                })],
            ),
            (
                "functie fib(a, b) { 1 }",
                vec![Stmt::Expr(Expr::Function {
                    name: "fib".to_owned(),
                    parameters: vec!["a".to_owned(), "b".to_owned()],
                    body: vec![Stmt::Expr(Expr::Int { value: 1 })],
                })],
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
                vec![Stmt::Expr(Expr::Call {
                    left: Box::new(Expr::Identifier("foo".to_owned())),
                    arguments: vec![],
                })],
            ),
            (
                "foo(1)",
                vec![Stmt::Expr(Expr::Call {
                    left: Box::new(Expr::Identifier("foo".to_owned())),
                    arguments: vec![Expr::Int { value: 1 }],
                })],
            ),
            (
                "foo(1, 2)",
                vec![Stmt::Expr(Expr::Call {
                    left: Box::new(Expr::Identifier("foo".to_owned())),
                    arguments: vec![Expr::Int { value: 1 }, Expr::Int { value: 2 }],
                })],
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
            Ok(vec![Stmt::Return(Expr::Int { value: 1 })])
        );
        assert_eq!(
            parse("antwoord 1 + 2;"),
            Ok(vec![Stmt::Return(Expr::Infix {
                left: Box::new(Expr::Int { value: 1 }),
                operator: Operator::Add,
                right: Box::new(Expr::Int { value: 2 })
            })])
        );

        assert_eq!(
            parse("antwoord fib(1) + fib(2);"),
            Ok(vec![Stmt::Return(Expr::Infix {
                left: Box::new(Expr::Call {
                    left: Box::new(ExprIdent::new("fib")),
                    arguments: vec![Expr::Int { value: 1 }]
                }),
                operator: Operator::Add,
                right: Box::new(Expr::Call {
                    left: Box::new(ExprIdent::new("fib")),
                    arguments: vec![Expr::Int { value: 2 }]
                })
            }),])
        );
    }

    #[test]
    fn test_array_expression() {
        for (input, expected_ast) in [
            ("[]", vec![Stmt::Expr(Expr::Array { values: vec![] })]),
            (
                "[1]",
                vec![Stmt::Expr(Expr::Array {
                    values: vec![Expr::Int { value: 1 }],
                })],
            ),
            (
                "[1, 2]",
                vec![Stmt::Expr(Expr::Array {
                    values: vec![Expr::Int { value: 1 }, Expr::Int { value: 2 }],
                })],
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
                vec![Stmt::Expr(Expr::While {
                    condition: Box::new(Expr::Bool { value: true }),
                    body: BlockStmt::new(),
                })],
            ),
            (
                "zolang ja { 1 }",
                vec![Stmt::Expr(Expr::While {
                    condition: Box::new(Expr::Bool { value: true }),
                    body: vec![Stmt::Expr(Expr::Int { value: 1 })],
                })],
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
                vec![Stmt::Expr(Expr::Index {
                    left: Box::new(ExprIdent::new("a")),
                    index: Box::new(Expr::Int { value: 0 }),
                })],
            ),
            (
                "[1][0]",
                vec![Stmt::Expr(Expr::Index {
                    left: Box::new(Expr::Array {
                        values: vec![Expr::Int { value: 1 }],
                    }),
                    index: Box::new(Expr::Int { value: 0 }),
                })],
            ),
        ] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }
    }

    #[test]
    fn test_op_assign_expression() {
        for (input, expected_ast) in [(
            "a += 5",
            vec![Stmt::Expr(Expr::Assign {
                left: Box::new(ExprIdent::new("a")),
                right: Box::new(Expr::Infix {
                    left: Box::new(ExprIdent::new("a")),
                    operator: Operator::Add,
                    right: Box::new(Expr::Int { value: 5 }),
                }),
            })],
        )] {
            assert_eq!(parse(input).unwrap(), expected_ast)
        }
    }
}
