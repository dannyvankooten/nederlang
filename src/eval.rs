use crate::ast::{
    BlockStmt, Expr, ExprArray, ExprAssign, ExprBool, ExprCall, ExprFloat, ExprIf, ExprIndex,
    ExprInfix, ExprInt, ExprPrefix, ExprString, ExprWhile, Operator, Stmt,
};
use crate::object::*;
use crate::parser;
use parser::ParseError;
use std::cell::RefCell;

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
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error>;
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

    pub(crate) fn resolve(&self, ident: &str) -> Option<NlObject> {
        let symbols = self.symbols.borrow();
        if let Some((_, value)) = symbols.iter().find(|(name, _)| name == ident) {
            return Some(value.clone());
        }

        if let Some(outer) = &self.outer {
            return outer.resolve(ident);
        }

        None
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

impl Eval for Expr {
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error> {
        match &self {
            Expr::Int(expr) => expr.eval(),
            Expr::Float(expr) => expr.eval(),
            Expr::Bool(expr) => expr.eval(),
            Expr::String(expr) => expr.eval(),
            Expr::Identifier(name) => {
                if let Some(object) = env.resolve(&name) {
                    Ok(object)
                } else {
                    return Err(Error::ReferenceError(format!("{} is not defined.", name)));
                }
            }
            Expr::Infix(expr) => expr.eval(env),
            Expr::If(expr) => expr.eval(env),
            Expr::Function(name, parameters, body) => {
                let func = NlFuncObject::new(name.clone(), parameters.clone(), body.clone());
                env.insert(name.clone(), func.clone());
                Ok(func)
            }
            Expr::Call(expr) => expr.eval(env),
            Expr::Prefix(expr) => expr.eval(env),
            Expr::Assign(expr) => expr.eval(env),
            Expr::While(expr) => eval_while_expression(expr, env),
            Expr::Array(expr) => eval_array_expression(expr, env),
            Expr::Index(expr) => eval_index_expression(expr, env),
            _ => unimplemented!(
                "Evaluating expressions of type {:?} is not yet implemented.",
                self
            ),
        }
    }
}

impl Eval for ExprAssign {
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error> {
        match &*self.left {
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
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error> {
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
                    self.operator
                )
            }
        }
    }
}

impl Eval for ExprPrefix {
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error> {
        let right = self.right.eval(env)?;

        match self.operator {
            Operator::Negate => !right,
            Operator::Subtract => -right,
            _ => unimplemented!(
                "Prefix expressions with operator {:?} are not implemented.",
                self.operator
            ),
        }
    }
}

impl Eval for ExprIf {
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error> {
        let condition = self.condition.eval(env)?;

        if condition.is_truthy() {
            self.consequence.eval(env)
        } else if let Some(alternative) = &self.alternative {
            alternative.eval(env)
        } else {
            Ok(NlObject::Null)
        }
    }
}

trait EvalWithScope {
    fn eval_scoped(&self, env: &mut Environment) -> Result<NlObject, Error>;
}

impl EvalWithScope for BlockStmt {
    #[inline]
    fn eval_scoped(&self, env: &mut Environment) -> Result<NlObject, Error> {
        // BlockStmts get their own scope, so variables declare inside this block are dropped at the end
        let mut new_env = Environment::new_from(env);
        self.eval(&mut new_env)
    }
}

impl Eval for BlockStmt {
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error> {
        let mut result = NlObject::Null;
        for s in self {
            result = match s {
                Stmt::Expr(expr) => expr.eval(env)?,
                Stmt::Block(expr) => expr.eval_scoped(env)?,
                Stmt::Let(name, value) => {
                    let value = value.eval(env)?;
                    env.insert(name.clone(), value);
                    // TODO: What to return from declare statements?
                    NlObject::Null
                }
                Stmt::Return(expr) => return Ok(NlObject::Return(Box::new(expr.eval(env)?))),
            };

            // Return values should be returned directly
            // without traversing the rest of the statements in this block
            match result {
                NlObject::Return(value) => {
                    return Ok(*value);
                }
                _ => {}
            }
        }
        Ok(result)
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
    fn eval(&self) -> Result<NlObject, Error> {
        Ok(NlObject::String(self.value.clone()))
    }
}

impl Eval for ExprCall {
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error> {
        let func = match &*self.func {
            Expr::Identifier(name) => {
                let object = env.resolve(name);
                match object {
                    Some(NlObject::Func(func)) => func,
                    Some(_) => {
                        return Err(Error::TypeError(format!(
                            "Object of type {:?} is not callable.",
                            object
                        )))
                    }
                    _ => return Err(Error::ReferenceError(format!("{} is not defined.", name))),
                }
            }
            Expr::Function(name, parameters, body) => NlFuncObject {
                name: name.clone(),
                parameters: parameters.clone(),
                body: body.clone(),
            },
            _ => panic!(
                "Expression of type {:?} is not callable. Parser should have picked up on this.",
                self.func
            ),
        };

        // Validate number of arguments
        if self.arguments.len() != func.parameters.len() {
            // TODO: Supply function name here.
            return Err(Error::TypeError(format!(
                "{} takes exactly {} arguments ({} given)",
                func.name,
                func.parameters.len(),
                self.arguments.len()
            )));
        }

        // Create new environment for this function to run in
        // Declare every argument as the corresponding parameter name
        let mut fn_env = Environment::new();
        for (name, value) in std::iter::zip(func.parameters, &self.arguments) {
            fn_env.insert(name, value.eval(env)?);
        }
        fn_env.outer = Some(env);

        return func.body.eval(&mut fn_env);
    }
}

fn eval_array_expression(expr: &ExprArray, env: &mut Environment) -> Result<NlObject, Error> {
    let mut values = Vec::with_capacity(expr.values.len());
    for e in &expr.values {
        values.push(e.eval(env)?);
    }

    Ok(NlObject::Array(values))
}

fn eval_while_expression(expr: &ExprWhile, env: &mut Environment) -> Result<NlObject, Error> {
    while expr.condition.eval(env)?.is_truthy() {
        expr.body.eval_scoped(env)?;
    }

    Ok(NlObject::Null)
}

fn eval_index_expression(expr: &ExprIndex, env: &mut Environment) -> Result<NlObject, Error> {
    let left = expr.left.eval(env)?;
    let index = expr.index.eval(env)?;

    match left {
        NlObject::Array(values) => match index {
            NlObject::Int(idx) => {
                if idx < 0 || idx as usize >= values.len() {
                    return Err(Error::TypeError(format!(
                        "out of bounds: array has length {}, but index was {}",
                        values.len(),
                        idx
                    )));
                }
                Ok(values[idx as usize].clone())
            }
            _ => {
                return Err(Error::TypeError(format!(
                    "index must be of type int, not {}",
                    index
                )))
            }
        },
        _ => {
            return Err(Error::TypeError(format!(
                "can not index into object of type {:?}",
                left
            )))
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
    fn test_reference_error() {
        assert!(eval_program("a", None).is_err());
        assert!(eval_program("{ stel b = 1; } b", None).is_err());
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

    #[test]
    fn test_return_statements() {
        assert_eq!(
            eval_program("functie a() { antwoord 1; } a();", None),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            eval_program("functie a() { antwoord 1; 2; } a();", None),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            eval_program("functie a() { als ja { antwoord 1; } 2; } a();", None),
            Ok(NlObject::Int(1))
        );
        assert_eq!(
            eval_program("functie a() { als nee { antwoord 1; } 2; } a();", None),
            Ok(NlObject::Int(2))
        );
        assert_eq!(
            eval_program("{ antwoord 1; } 2;", None),
            Ok(NlObject::Int(1))
        );

        // Currently this fails because the value is still wrapped in a Return object.
        // assert_eq!(eval_program("antwoord 1; 2;", None), Ok(NlObject::Int(1)));
    }

    #[test]
    fn test_arrays() {
        assert_eq!(eval_program("[]", None), Ok(NlObject::Array(vec![])));
        assert_eq!(
            eval_program("[1]", None),
            Ok(NlObject::Array(vec![NlObject::Int(1)]))
        );
        assert_eq!(
            eval_program("[1, 2]", None),
            Ok(NlObject::Array(vec![NlObject::Int(1), NlObject::Int(2)]))
        );
    }

    #[test]
    fn test_while() {
        assert_eq!(eval_program("zolang nee { }", None), Ok(NlObject::Null));

        assert_eq!(
            eval_program("stel a = 0; zolang a < 3 { a = a + 1; } a", None),
            Ok(NlObject::Int(3))
        );
    }

    #[test]
    fn test_array_indexing() {
        // a is not defined
        assert!(eval_program("a[0]", None).is_err());
        // out of bounds
        assert!(eval_program("[][0]", None).is_err());

        // OK:
        assert_eq!(eval_program("[1][0]", None), Ok(NlObject::Int(1)));
        assert_eq!(eval_program("[1, 2][0]", None), Ok(NlObject::Int(1)));
        assert_eq!(eval_program("[1, 2][1]", None), Ok(NlObject::Int(2)));
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
                        antwoord n
                    } 
                    fib(n - 1) + fib(n - 2)
                }
                
                fib(22)
                ",
                    None
                ),
            );
        });
    }
}
