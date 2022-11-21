use crate::ast::{
    Expr, ExprAssign, ExprBool, ExprCall, ExprDeclare, ExprFloat, ExprFunction, ExprIf, ExprInfix,
    ExprInt, ExprPrefix, ExprString, Operator,
};
use crate::object::*;
use crate::parser;
use parser::ParseError;

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
    TypeError(String),
    SyntaxError(ParseError),
    ReferenceError(String),
}

type Scope<'a> = Vec<(&'a str, Object<'a>)>;

#[derive(Debug)]
pub struct Environment<'a> {
    scopes: Vec<Scope<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            scopes: vec![Vec::new()],
        }
    }

    pub(crate) fn resolve(&self, ident: &str) -> Object<'a> {
        for scope in self.scopes.iter().rev() {
            for (name, value) in scope {
                if *name == ident {
                    return value.clone();
                }
            }
        }

        Object::Null
    }

    pub(crate) fn insert(&mut self, ident: &'a str, value: Object<'a>) {
        let scope = self.scopes.last_mut().unwrap();
        scope.push((ident, value));
    }

    pub(crate) fn update(&mut self, ident: &str, new_value: Object<'a>) -> Result<(), Error> {
        for scope in self.scopes.iter_mut().rev() {
            for (name, value) in scope {
                if *name == ident {
                    *value = new_value.clone();
                    return Ok(());
                }
            }
        }

        Err(Error::ReferenceError(format!(
            "assignment to undeclared variable {}",
            ident
        )))
    }
}

pub(crate) fn eval_ast<'a>(
    root: &'a Vec<Expr>,
    _env: Option<&mut Environment>,
) -> Result<Object<'a>, Error> {
    let mut env = Environment::new();
    // let env = env.unwrap_or(&mut default_env);
    let mut last = Object::Null;
    for s in root {
        last = eval_expr(s, &mut env)?;
    }

    // TODO: Fix this
    Ok(last)
}

fn eval_assign_expr<'a>(
    expr: &'a ExprAssign,
    env: &mut Environment<'a>,
) -> Result<Object<'a>, Error> {
    match &*expr.left {
        Expr::Identifier(name) => {
            let right = eval_expr(&*expr.right, env)?;
            env.update(&name, right)?;
            return Ok(Object::Null);
        }
        _ => panic!(),
    }
}

fn eval_infix_expr<'a>(
    expr: &'a ExprInfix,
    env: &mut Environment<'a>,
) -> Result<Object<'a>, Error> {
    let left = eval_expr(&*expr.left, env)?;
    let right = eval_expr(&*expr.right, env)?;

    match expr.operator {
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
                &expr.operator
            )
        }
    }
}

fn eval_prefix_expr<'a>(
    expr: &'a ExprPrefix,
    env: &mut Environment<'a>,
) -> Result<Object<'a>, Error> {
    let right = eval_expr(&*expr.right, env)?;

    match expr.operator {
        Operator::Negate => !right,
        Operator::Subtract => -right,
        _ => unimplemented!(
            "Prefix expressions with operator {:?} are not implemented.",
            &expr.operator
        ),
    }
}

fn eval_if_expr<'a>(expr: &'a ExprIf, env: &mut Environment<'a>) -> Result<Object<'a>, Error> {
    let condition = eval_expr(&*expr.condition, env)?;

    if condition.is_truthy() {
        eval_block(&expr.consequence, env)
    } else if let Some(alternative) = &expr.alternative {
        eval_block(alternative, env)
    } else {
        Ok(Object::Null)
    }
}

fn eval_block<'a>(block: &'a Vec<Expr>, env: &mut Environment<'a>) -> Result<Object<'a>, Error> {
    let mut last = Object::Null;
    for expr in block {
        last = eval_expr(expr, env)?;
    }
    Ok(last)
}

fn eval_int_expr(expr: &ExprInt) -> Result<Object, Error> {
    Ok(Object::Int(expr.value))
}

fn eval_float_expr(expr: &ExprFloat) -> Result<Object, Error> {
    Ok(Object::Float(expr.value))
}

fn eval_bool_expr(expr: &ExprBool) -> Result<Object, Error> {
    Ok(Object::Bool(expr.value))
}

fn eval_string_expr(expr: &ExprString) -> Result<Object, Error> {
    Ok(Object::String(expr.value.to_owned()))
}

fn eval_function_expr(expr: &ExprFunction) -> Result<Object, Error> {
    Ok(Object::Func(expr))
}

fn eval_declare_expr<'a>(
    expr: &'a ExprDeclare,
    env: &mut Environment<'a>,
) -> Result<Object<'a>, Error> {
    let value = eval_expr(&*expr.value, env)?;
    env.insert(&expr.name, value);
    Ok(Object::Null)
}

fn eval_call_expr<'a>(expr: &'a ExprCall, env: &mut Environment<'a>) -> Result<Object<'a>, Error> {
    let function = match &*expr.func {
        Expr::Identifier(name) => env.resolve(&name),
        Expr::Function(expr) => eval_function_expr(expr)?,
        _ => panic!("Expression of type {:?} is not callable.", expr.func),
    };

    match function {
        Object::Func(func) => {
            if expr.arguments.len() != func.parameters.len() {
                return Err(Error::TypeError(format!(
                    "{} takes exactly {} arguments ({} given)",
                    "function",
                    func.parameters.len(),
                    expr.arguments.len()
                )));
            }

            let mut scope = Scope::with_capacity(func.parameters.len());
            for (name, value_expr) in std::iter::zip(&func.parameters, &expr.arguments) {
                let value = eval_expr(value_expr, env)?;
                scope.push((name, value));
            }
            env.scopes.push(scope);
            let result = eval_block(&func.body, env);
            env.scopes.pop();
            return result;
        }
        _ => {
            return Err(Error::TypeError(format!(
                "Object of type {:?} is not callable.",
                function
            )))
        }
    }
}

fn eval_expr<'a>(expr: &'a Expr, env: &mut Environment<'a>) -> Result<Object<'a>, Error> {
    match expr {
        Expr::Infix(expr) => eval_infix_expr(expr, env),
        Expr::Prefix(expr) => eval_prefix_expr(expr, env),
        Expr::Assign(expr) => eval_assign_expr(expr, env),
        Expr::If(expr) => eval_if_expr(expr, env),
        Expr::Int(expr) => eval_int_expr(expr),
        Expr::Float(expr) => eval_float_expr(expr),
        Expr::Bool(expr) => eval_bool_expr(expr),
        Expr::String(expr) => eval_string_expr(expr),
        Expr::Identifier(name) => Ok(env.resolve(&name)),
        Expr::Function(expr) => {
            let obj = eval_function_expr(expr)?;
            env.insert(expr.name.as_str(), obj.clone());
            Ok(obj)
        }
        Expr::Declare(expr) => eval_declare_expr(expr, env),
        Expr::Call(expr) => eval_call_expr(expr, env),
        _ => unimplemented!(
            "Evaluating expressions of type {:?} is not yet implemented.",
            expr
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    extern crate test;

    fn assert_program_result(program: &str, result: Result<Object, Error>) {
        let ast = match parser::parse(&program) {
            Ok(ast) => ast,
            Err(e) => return eprintln!("{:?}", e),
        };

        assert_eq!(eval_ast(&ast, None), result);
    }

    #[test]
    fn test_int_arithmetic() {
        for (input, expected) in [
            ("6 + 2", Object::Int(8)),
            ("6 - 2", Object::Int(4)),
            ("6 * 2", Object::Int(12)),
            ("6 / 2", Object::Int(3)),
            ("6 % 2", Object::Int(0)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_int_arithmetic_with_precedence() {
        for (input, expected) in [
            ("6 + 2 * 5", Object::Int(16)),
            ("6 + 2 * 5 / 5", Object::Int(8)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_float_arithmetic() {
        for (input, expected) in [
            ("6.5 + 2.0", Object::Float(8.5)),
            ("6.5 - 2.0", Object::Float(4.5)),
            ("6.5 * 2.0", Object::Float(13.0)),
            ("6.5 / 2.0", Object::Float(3.25)),
            ("6.5 % 2.0", Object::Float(0.5)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    #[ignore]
    fn test_mixed_arithmetic() {
        for (input, expected) in [
            ("6.5 + 2", Object::Float(8.5)),
            ("6 - 2.0", Object::Float(4.0)),
            ("6.5 * 2", Object::Float(13.0)),
            ("6.5 / 2", Object::Float(3.25)),
            ("6.5 % 2", Object::Float(0.5)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_int_boolean_infix_expressions() {
        for (input, expected) in [
            ("5 > 2", Object::Bool(true)),
            ("5 < 2", Object::Bool(false)),
            ("1 >= 1", Object::Bool(true)),
            ("1 <= 1", Object::Bool(true)),
            ("1 == 1", Object::Bool(true)),
            ("1 != 1", Object::Bool(false)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_float_boolean_infix_expressions() {
        for (input, expected) in [
            ("5.5 > 2.5", Object::Bool(true)),
            ("5.5 < 2.5", Object::Bool(false)),
            ("1.5 >= 1.5", Object::Bool(true)),
            ("1.5 <= 1.5", Object::Bool(true)),
            ("1.5 == 1.5", Object::Bool(true)),
            ("1.5 != 1.5", Object::Bool(false)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_bool_boolean_infix_expressions() {
        for (input, expected) in [
            ("ja > nee", Object::Bool(true)),
            ("ja < nee", Object::Bool(false)),
            ("ja >= nee", Object::Bool(true)),
            ("ja <= nee", Object::Bool(false)),
            ("ja == nee", Object::Bool(false)),
            ("ja != nee", Object::Bool(true)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_prefix_expressions() {
        for (input, expected) in [
            ("!ja", Object::Bool(false)),
            ("!nee", Object::Bool(true)),
            ("-5", Object::Int(-5)),
            ("!!ja", Object::Bool(true)),
            ("!!!ja", Object::Bool(false)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_string_infix_expressions() {
        for (input, expected) in [
            ("\"foo\" + \"bar\"", Object::String("foobar".to_owned())),
            // ("\"foo\" * 2", Object::String("foofoo".to_owned())),
            ("\"foo\" == \"foo\"", Object::Bool(true)),
            ("\"foo\" != \"foo\"", Object::Bool(false)),
            ("\"foo\" >= \"foo\"", Object::Bool(true)),
            ("\"foo\" <= \"foo\"", Object::Bool(true)),
            ("\"foo\" > \"foo\"", Object::Bool(false)),
            ("\"foo\" < \"foo\"", Object::Bool(false)),
            ("\"abc\" > \"xyz\"", Object::Bool(false)),
            ("\"abc\" >= \"xyz\"", Object::Bool(false)),
            ("\"abc\" < \"xyz\"", Object::Bool(true)),
            ("\"abc\" <= \"xyz\"", Object::Bool(true)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_multiple_expressions() {
        {
            let (input, expected) = ("5 + 5; 6 + 2", Object::Int(8));
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_if_expressions() {
        for (input, expected) in [
            ("als 5 > 4 { 1 }", Object::Int(1)),
            ("als 5 < 4 { 1 }", Object::Null),
            ("als 5 < 4 { 1 } anders { 2 }", Object::Int(2)),
            ("als (5 > 4) { 1 } anders { 2 }", Object::Int(1)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_declare_statements() {
        {
            let (input, expected) = ("stel a = 100", Object::Null);
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_ident_expressions() {
        for (input, expected) in [
            ("a", Object::Null),
            ("stel a = 100; a", Object::Int(100)),
            ("stel a = 100; stel b = 2; a", Object::Int(100)),
            ("stel a = 100; stel b = 2; a * b", Object::Int(200)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_nested_if_expressions() {
        for (input, expected) in [
            ("als 5 > 4 { als 11 > 10 { 1 } }", Object::Int(1)),
            (
                "als 5 > 4 { als 10 > 11 { 1 } anders { 2 } }",
                Object::Int(2),
            ),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_if_else_if_expression() {
        for (input, expected) in [
            ("als 1 > 2 { 1 } anders als 2 > 1 { 2 }", Object::Int(2)),
            (
                "als 1 > 2 { 1 } anders als 2 > 3 { 2 } anders { 3 }",
                Object::Int(3),
            ),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }
}
