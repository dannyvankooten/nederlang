use std::collections::HashMap;

use crate::ast::{
    BlockStmt, Expr, ExprBool, ExprFloat, ExprIf, ExprInfix, ExprInt, ExprPrefix, ExprString,
    Operator, Stmt,
};
use crate::object::*;
use crate::parser;

#[derive(Debug)]
pub struct Environment<'a> {
    symbol_table: HashMap<String, NlObject>,
    outer: Option<&'a Environment<'a>>,
}

pub(crate) trait Eval {
    fn eval(&self, env: &mut Environment) -> NlObject;
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            symbol_table: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_from(env: &'a mut Environment<'_>) -> Self {
        Environment {
            symbol_table: HashMap::new(),
            outer: Some(env),
        }
    }

    pub fn resolve(&self, ident: &str) -> NlObject {
        if self.symbol_table.contains_key(ident) {
            return self.symbol_table.get(ident).unwrap().clone();
        }

        if let Some(outer) = &self.outer {
            return outer.resolve(ident);
        }

        NlObject::Null
    }

    pub fn insert(&mut self, ident: &str, value: NlObject) {
        self.symbol_table.insert(ident.to_owned(), value);
    }

    pub fn update(&mut self, ident: &str, value: NlObject) {
        if self.symbol_table.contains_key(ident) {
            self.insert(ident, value)
        }

        // if let Some(outer) = self.outer {
        //     return outer.update(ident, value);
        // }
    }
}

pub fn eval_program(program: &str, env: Option<&mut Environment>) -> NlObject {
    let mut default_env = Environment::new();
    let env = env.unwrap_or(&mut default_env);
    match parser::parse(program) {
        Ok(ast) => {
            let mut last = NlObject::Null;
            for s in ast {
                last = s.eval(env)
            }
            last
        }
        Err(e) => {
            eprintln!("{}", e.message);
            NlObject::Null
        }
    }
}

impl Eval for ExprInfix {
    fn eval(&self, env: &mut Environment) -> NlObject {
        let left = self.left.eval(env);
        let right = self.right.eval(env);

        match &self.operator {
            Operator::Add => left + right,
            Operator::Subtract => left - right,
            Operator::Multiply => left * right,
            Operator::Divide => left / right,
            Operator::Modulo => left % right,
            Operator::Gt => NlObject::Bool(left > right),
            Operator::Gte => NlObject::Bool(left >= right),
            Operator::Lt => NlObject::Bool(left < right),
            Operator::Lte => NlObject::Bool(left >= right),
            Operator::Eq => NlObject::Bool(left == right),
            Operator::Neq => NlObject::Bool(left != right),
            Operator::Assign => {
                // TODO: Move Assignments into its own expression because we are now
                // needlessly evaluating the lhs
                // Also we could show a parsing error in case left is not Ident
                match &*self.left {
                    Expr::Identifier(name) => {
                        env.update(name, right);
                        NlObject::Null
                    }
                    _ => panic!(),
                }
            }
            _ => unimplemented!(
                "Infix expressions with operator {:?} are not implemented.",
                &self.operator
            ),
        }
    }
}

impl Eval for ExprPrefix {
    fn eval(&self, env: &mut Environment) -> NlObject {
        let right = self.right.eval(env);

        match &self.operator {
            Operator::Negate => !right,
            Operator::Subtract => -right,
            _ => unimplemented!(
                "Infix expressions with operator {:?} are not implemented.",
                &self.operator
            ),
        }
    }
}

impl Eval for ExprIf {
    fn eval(&self, env: &mut Environment) -> NlObject {
        let condition = self.condition.eval(env);

        if condition.is_truthy() {
            self.consequence.eval(env)
        } else if let Some(alternative) = &self.alternative {
            alternative.eval(env)
        } else {
            NlObject::Null
        }
    }
}

impl Eval for Stmt {
    fn eval(&self, env: &mut Environment) -> NlObject {
        match self {
            Stmt::Expr(expr) => expr.eval(env),
            Stmt::Let(name, value) => {
                let value = value.eval(env);
                env.insert(name, value);
                // TODO: What to return from declare statements?
                NlObject::Null
            }
            Stmt::Block(expr) => expr.eval(env),
            _ => unimplemented!("Statements of type {:?} are not implemented.", self),
        }
    }
}

impl Eval for BlockStmt {
    fn eval(&self, env: &mut Environment) -> NlObject {
        // BlockStmts get their own scope, so variables declare inside this block are dropped at the end
        let mut new_env = Environment::new_from(env);
        let mut last = NlObject::Null;
        for s in self {
            last = s.eval(&mut new_env)
        }
        last
    }
}

impl ExprInt {
    fn eval(&self) -> NlObject {
        NlObject::Int(self.value)
    }
}

impl ExprFloat {
    fn eval(&self) -> NlObject {
        NlObject::Float(self.value)
    }
}

impl ExprBool {
    fn eval(&self) -> NlObject {
        NlObject::Bool(self.value)
    }
}

impl ExprString {
    fn eval(&self) -> NlObject {
        NlObject::String(self.value.to_owned())
    }
}

impl Eval for Expr {
    fn eval(&self, env: &mut Environment) -> NlObject {
        match self {
            Expr::Infix(expr) => expr.eval(env),
            Expr::Prefix(expr) => expr.eval(env),
            Expr::If(expr) => expr.eval(env),
            Expr::Int(expr) => expr.eval(),
            Expr::Float(expr) => expr.eval(),
            Expr::Bool(expr) => expr.eval(),
            Expr::String(expr) => expr.eval(),
            Expr::Identifier(name) => env.resolve(name),
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
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
        }
    }

    #[test]
    fn test_int_arithmetic_with_precedence() {
        for (input, expected) in [
            ("6 + 2 * 5", NlObject::Int(16)),
            ("6 + 2 * 5 / 5", NlObject::Int(8)),
        ] {
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
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
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
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
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
        }
    }

    #[test]
    fn test_boolean_infix_expressions() {
        for (input, expected) in [
            ("5 > 2", NlObject::Bool(true)),
            ("5 < 2", NlObject::Bool(false)),
            ("1 >= 1", NlObject::Bool(true)),
            ("1 <= 1", NlObject::Bool(true)),
        ] {
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
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
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
        }
    }

    #[test]
    fn test_string_infix_expressions() {
        for (input, expected) in [
            ("\"foo\" + \"bar\"", NlObject::String("foobar".to_owned())),
            ("\"foo\" * 2", NlObject::String("foofoo".to_owned())),
        ] {
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
        }
    }

    #[test]
    fn test_multiple_expressions() {
        {
            let (input, expected) = ("5 + 5; 6 + 2", NlObject::Int(8));
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
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
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
        }
    }

    #[test]
    fn test_declare_statements() {
        {
            let (input, expected) = ("stel a = 100", NlObject::Null);
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
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
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
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
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
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
            assert_eq!(expected, eval_program(input, None), "eval input: {}", input);
        }
    }

    #[bench]
    #[ignore]
    fn bench_string_addition(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(
                NlObject::String("foo".repeat(10)),
                eval_program("\"foo\" * 10", None),
            );
        });
    }
}
