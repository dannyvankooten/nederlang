use crate::ast::{
    BlockStmt, Expr, ExprAssign, ExprBool, ExprCall, ExprFloat, ExprIf, ExprInfix, ExprInt,
    ExprPrefix, ExprString, Operator, Stmt,
};
use crate::object::*;
use crate::parser;
use parser::ParseError;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
    TypeError(String),
    SyntaxError(ParseError),
    ReferenceError(String),
}

#[derive(Debug)]
pub struct Environment<'a> {
    symbols: RefCell<Vec<(String, NlObject)>>,
    outer: Option<&'a Environment<'a>>,
}

pub(crate) trait Eval {
    fn eval(self, env: &mut Environment) -> Result<NlObject, Error>;
}

impl<'a> Environment<'a> {
    #[inline]
    pub fn new() -> Self {
        Environment {
            symbols: RefCell::new(Vec::with_capacity(4)),
            outer: None,
        }
    }

    pub fn new_from(env: &'a Environment<'a>) -> Self {
        Environment {
            symbols: RefCell::new(Vec::with_capacity(4)),
            outer: Some(env),
        }
    }

    pub(crate) fn resolve(&self, ident: &str) -> NlObject {
        let symbols = self.symbols.borrow();
        if let Some((_, value)) = symbols.iter().find(|(name, _)| name == ident) {
            return value.clone();
        }

        if let Some(outer) = &self.outer {
            return outer.resolve(ident);
        }

        NlObject::Null
    }

    #[inline]
    pub(crate) fn insert(&self, ident: String, value: NlObject) {
        self.symbols.borrow_mut().push((ident, value));
    }

    pub(crate) fn update(&self, ident: &str, value: NlObject) -> Result<(), Error> {
        let mut symbols = self.symbols.borrow_mut();
        if let Some(pos) = symbols.iter().position(|(name, _)| name == ident) {
            symbols[pos] = (ident.to_owned(), value);
            return Ok(());
        } else if let Some(outer) = &self.outer {
            return outer.update(ident, value);
        }

        Err(Error::ReferenceError(format!(
            "assignment to undeclared variable {}",
            ident
        )))
    }
}

pub(crate) fn eval_program(
    program: &str,
    env: Option<&mut Environment>,
) -> Result<NlObject, Error> {
    let mut default_env = Environment::new();
    let env = env.unwrap_or(&mut default_env);
    let ast = parser::parse(program).map_err(Error::SyntaxError)?;
    ast.eval(env)
}

impl Eval for ExprAssign {
    fn eval(self, env: &mut Environment) -> Result<NlObject, Error> {
        match *self.left {
            Expr::Identifier(name) => {
                let right = self.right.eval(env)?;
                env.update(&name, right)?;
                return Ok(NlObject::Null);
            }
            _ => panic!(),
        }
    }
}

impl Eval for ExprInfix {
    fn eval(self, env: &mut Environment) -> Result<NlObject, Error> {
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;

        match self.operator {
            Operator::Add => left + right,
            Operator::Subtract => left - right,
            Operator::Multiply => left * right,
            Operator::Divide => left / right,
            Operator::Modulo => left % right,
            Operator::Gt => left.gt(&right),
            Operator::Gte => left.gte(&right),
            Operator::Lt => left.lt(&right),
            Operator::Lte => left.lte(&right),
            Operator::Eq => left.eq(&right),
            Operator::Neq => left.neq(&right),
            _ => {
                unimplemented!(
                    "Infix expressions with operator {:?} are not implemented.",
                    &self.operator
                )
            }
        }
    }
}

impl Eval for ExprPrefix {
    fn eval(self, env: &mut Environment) -> Result<NlObject, Error> {
        let right = self.right.eval(env)?;

        match self.operator {
            Operator::Negate => !right,
            Operator::Subtract => -right,
            _ => unimplemented!(
                "Prefix expressions with operator {:?} are not implemented.",
                &self.operator
            ),
        }
    }
}

impl Eval for ExprIf {
    fn eval(self, env: &mut Environment) -> Result<NlObject, Error> {
        let condition = self.condition.eval(env)?;

        if condition.is_truthy() {
            self.consequence.eval(env)
        } else if let Some(alternative) = self.alternative {
            alternative.eval(env)
        } else {
            Ok(NlObject::Null)
        }
    }
}

impl Eval for Stmt {
    fn eval(self, env: &mut Environment) -> Result<NlObject, Error> {
        match self {
            Stmt::Expr(expr) => expr.eval(env),
            Stmt::Let(name, value) => {
                let value = value.eval(env)?;
                env.insert(name, value);
                // TODO: What to return from declare statements?
                Ok(NlObject::Null)
            }
            Stmt::Block(expr) => expr.eval_scoped(env),
            _ => unimplemented!("Statements of type {:?} are not implemented.", self),
        }
    }
}

trait EvalWithScope {
    fn eval_scoped(self, env: &mut Environment) -> Result<NlObject, Error>;
}

impl EvalWithScope for BlockStmt {
    #[inline]
    fn eval_scoped(self, env: &mut Environment) -> Result<NlObject, Error> {
        // BlockStmts get their own scope, so variables declare inside this block are dropped at the end
        let mut new_env = Environment::new_from(env);
        self.eval(&mut new_env)
    }
}

impl Eval for BlockStmt {
    fn eval(self, env: &mut Environment) -> Result<NlObject, Error> {
        let mut last = NlObject::Null;
        for s in self {
            last = s.eval(env)?
        }
        Ok(last)
    }
}

impl ExprInt {
    #[inline(always)]
    fn eval(&self) -> Result<NlObject, Error> {
        Ok(NlObject::Int(self.value))
    }
}

impl ExprFloat {
    #[inline(always)]
    fn eval(&self) -> Result<NlObject, Error> {
        Ok(NlObject::Float(self.value))
    }
}

impl ExprBool {
    #[inline(always)]
    fn eval(&self) -> Result<NlObject, Error> {
        Ok(NlObject::Bool(self.value))
    }
}

impl ExprString {
    #[inline(always)]
    fn eval(self) -> Result<NlObject, Error> {
        Ok(NlObject::String(self.value))
    }
}

impl Eval for ExprCall {
    fn eval(self, env: &mut Environment) -> Result<NlObject, Error> {
        let (parameters, body) = match *self.func {
            Expr::Identifier(name) => {
                let object = env.resolve(&name);
                match object {
                    NlObject::Func(params, body) => (params, body),
                    _ => {
                        return Err(Error::TypeError(format!(
                            "Object of type {:?} is not callable.",
                            object
                        )))
                    }
                }
            }
            Expr::Function(_, parameters, body) => (parameters, body),
            _ => panic!(
                "Expression of type {:?} is not callable. Parser should have picked up on this.",
                self.func
            ),
        };

        // Validate number of arguments
        if self.arguments.len() != parameters.len() {
            // TODO: Supply function name here.
            return Err(Error::TypeError(format!(
                "{} takes exactly {} arguments ({} given)",
                "function",
                parameters.len(),
                self.arguments.len()
            )));
        }

        // Create new environment for this function to run in
        // Declare every argument as the corresponding parameter name
        let mut fn_env = Environment::new();
        for (name, value) in std::iter::zip(parameters, self.arguments) {
            fn_env.insert(name, value.eval(env)?);
        }
        fn_env.outer = Some(env);

        return body.eval(&mut fn_env);
    }
}

impl Eval for Expr {
    fn eval(self, env: &mut Environment) -> Result<NlObject, Error> {
        match self {
            Expr::Infix(expr) => expr.eval(env),
            Expr::Prefix(expr) => expr.eval(env),
            Expr::Assign(expr) => expr.eval(env),
            Expr::If(expr) => expr.eval(env),
            Expr::Int(expr) => expr.eval(),
            Expr::Float(expr) => expr.eval(),
            Expr::Bool(expr) => expr.eval(),
            Expr::String(expr) => expr.eval(),
            Expr::Identifier(name) => Ok(env.resolve(&name)),
            Expr::Function(name, parameters, body) => {
                env.insert(name, NlObject::Func(parameters, body));
                Ok(NlObject::Null)
            }
            Expr::Call(expr) => expr.eval(env),
            _ => unimplemented!(
                "Evaluating expressions of type {:?} is not yet implemented.",
                self
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    extern crate test;

    #[test]
    fn test_int_arithmetic() {
        for (input, expected) in [
            ("6 + 2", NlObject::Int(8)),
            ("6 - 2", NlObject::Int(4)),
            ("6 * 2", NlObject::Int(12)),
            ("6 / 2", NlObject::Int(3)),
            ("6 % 2", NlObject::Int(0)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_int_arithmetic_with_precedence() {
        for (input, expected) in [
            ("6 + 2 * 5", NlObject::Int(16)),
            ("6 + 2 * 5 / 5", NlObject::Int(8)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_float_arithmetic() {
        for (input, expected) in [
            ("6.5 + 2.0", NlObject::Float(8.5)),
            ("6.5 - 2.0", NlObject::Float(4.5)),
            ("6.5 * 2.0", NlObject::Float(13.0)),
            ("6.5 / 2.0", NlObject::Float(3.25)),
            ("6.5 % 2.0", NlObject::Float(0.5)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_mixed_arithmetic() {
        for (input, expected) in [
            ("6.5 + 2", NlObject::Float(8.5)),
            ("6 - 2.0", NlObject::Float(4.0)),
            ("6.5 * 2", NlObject::Float(13.0)),
            ("6.5 / 2", NlObject::Float(3.25)),
            ("6.5 % 2", NlObject::Float(0.5)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_int_boolean_infix_expressions() {
        for (input, expected) in [
            ("5 > 2", NlObject::Bool(true)),
            ("5 < 2", NlObject::Bool(false)),
            ("1 >= 1", NlObject::Bool(true)),
            ("1 <= 1", NlObject::Bool(true)),
            ("1 == 1", NlObject::Bool(true)),
            ("1 != 1", NlObject::Bool(false)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_float_boolean_infix_expressions() {
        for (input, expected) in [
            ("5.5 > 2.5", NlObject::Bool(true)),
            ("5.5 < 2.5", NlObject::Bool(false)),
            ("1.5 >= 1.5", NlObject::Bool(true)),
            ("1.5 <= 1.5", NlObject::Bool(true)),
            ("1.5 == 1.5", NlObject::Bool(true)),
            ("1.5 != 1.5", NlObject::Bool(false)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_bool_boolean_infix_expressions() {
        for (input, expected) in [
            ("ja > nee", NlObject::Bool(true)),
            ("ja < nee", NlObject::Bool(false)),
            ("ja >= nee", NlObject::Bool(true)),
            ("ja <= nee", NlObject::Bool(false)),
            ("ja == nee", NlObject::Bool(false)),
            ("ja != nee", NlObject::Bool(true)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_prefix_expressions() {
        for (input, expected) in [
            ("!ja", NlObject::Bool(false)),
            ("!nee", NlObject::Bool(true)),
            ("-5", NlObject::Int(-5)),
            ("!!ja", NlObject::Bool(true)),
            ("!!!ja", NlObject::Bool(false)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_string_infix_expressions() {
        for (input, expected) in [
            ("\"foo\" + \"bar\"", NlObject::String("foobar".to_owned())),
            ("\"foo\" * 2", NlObject::String("foofoo".to_owned())),
            ("\"foo\" == \"foo\"", NlObject::Bool(true)),
            ("\"foo\" != \"foo\"", NlObject::Bool(false)),
            ("\"foo\" >= \"foo\"", NlObject::Bool(true)),
            ("\"foo\" <= \"foo\"", NlObject::Bool(true)),
            ("\"foo\" > \"foo\"", NlObject::Bool(false)),
            ("\"foo\" < \"foo\"", NlObject::Bool(false)),
            ("\"abc\" > \"xyz\"", NlObject::Bool(false)),
            ("\"abc\" >= \"xyz\"", NlObject::Bool(false)),
            ("\"abc\" < \"xyz\"", NlObject::Bool(true)),
            ("\"abc\" <= \"xyz\"", NlObject::Bool(true)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_multiple_expressions() {
        {
            let (input, expected) = ("5 + 5; 6 + 2", NlObject::Int(8));
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_if_expressions() {
        for (input, expected) in [
            ("als 5 > 4 { 1 }", NlObject::Int(1)),
            ("als 5 < 4 { 1 }", NlObject::Null),
            ("als 5 < 4 { 1 } anders { 2 }", NlObject::Int(2)),
            ("als (5 > 4) { 1 } anders { 2 }", NlObject::Int(1)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_declare_statements() {
        {
            let (input, expected) = ("stel a = 100", NlObject::Null);
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_ident_expressions() {
        for (input, expected) in [
            ("a", NlObject::Null),
            ("stel a = 100; a", NlObject::Int(100)),
            ("stel a = 100; stel b = 2; a", NlObject::Int(100)),
            ("stel a = 100; stel b = 2; a * b", NlObject::Int(200)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_nested_if_expressions() {
        for (input, expected) in [
            ("als 5 > 4 { als 11 > 10 { 1 } }", NlObject::Int(1)),
            (
                "als 5 > 4 { als 10 > 11 { 1 } anders { 2 } }",
                NlObject::Int(2),
            ),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_if_else_if_expression() {
        for (input, expected) in [
            ("als 1 > 2 { 1 } anders als 2 > 1 { 2 }", NlObject::Int(2)),
            (
                "als 1 > 2 { 1 } anders als 2 > 3 { 2 } anders { 3 }",
                NlObject::Int(3),
            ),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_declare_statements_with_scopes() {
        for (input, expected) in [
            ("stel a = 1; { a =  2; } a", NlObject::Int(2)),
            ("{ stel b = 1; } b", NlObject::Null),
            ("stel a = 1; { stel a = 2; } a", NlObject::Int(1)),
            ("stel a = 1; { stel b = 1; a = a + b; } a", NlObject::Int(2)),
        ] {
            assert_eq!(
                Ok(expected),
                eval_program(input, None),
                "eval input: {}",
                input
            );
        }
    }

    #[test]
    fn test_cmp_errors() {
        for input in [
            "5 > \"foo\"",
            "5 >= \"foo\"",
            "5 < \"foo\"",
            "5 <= \"foo\"",
            "\"foo\" > 5",
            "\"foo\" >= 5",
            "\"foo\" < 5",
            "\"foo\" <= 5",
        ] {
            assert!(eval_program(input, None).is_err(), "eval input: {}", input);
        }
    }

    #[bench]
    fn bench_fib_recursive_22(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(
                Ok(NlObject::Int(17711)),
                eval_program(
                    "
                functie fib(n) {
                    als n < 2 {
                        n
                    } anders {
                        fib(n - 1) + fib(n - 2)
                    }    
                }
                
                fib(22)
                ",
                    None
                ),
            );
        });
    }
}
