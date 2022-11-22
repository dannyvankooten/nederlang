use crate::ast::{
    Expr, ExprAssign, ExprBool, ExprCall, ExprDeclare, ExprFloat, ExprFunction, ExprIf, ExprInfix,
    ExprInt, ExprPrefix, ExprString, Operator,
};
use crate::object::*;

#[derive(Debug)]
pub struct Environment<'a> {
    names: Vec<&'a str>,
    values: Vec<Object>
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            names: Vec::with_capacity(64),
            values: Vec::with_capacity(64)
        }
    }

    pub(crate) fn resolve(&self, ident: &str) -> Object {
        if let Some(pos) = self.names.iter().rev().position(|name| *name == ident) {
            return self.values[self.values.len() - 1 - pos];
        }
        
        Object::null()
    }

    pub(crate) fn insert(&mut self, ident: &'a str, value: Object) {
        self.names.push(ident);
        self.values.push(value);
    }

    pub(crate) fn update(&mut self, ident: &str, new_value: Object) -> Result<(), Error> {
        if let Some(pos) = self.names.iter().rev().position(|name| *name == ident) {
            self.values[pos] = new_value;
            return Ok(());
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
) -> Result<Object, Error> {
    let mut env = Environment::new();
    // let env = env.unwrap_or(&mut default_env);
    let mut last = Object::null();
    for s in root {
        last = eval_expr(s, &mut env)?;
    }

    // TODO: Fix this
    Ok(last)
}

fn eval_assign_expr<'a>(expr: &'a ExprAssign, env: &mut Environment<'a>) -> Result<Object, Error> {
    match &*expr.left {
        Expr::Identifier(name) => {
            let right = eval_expr(&*expr.right, env)?;
            env.update(&name, right)?;
            return Ok(Object::null());
        }
        _ => panic!(),
    }
}

#[inline]
fn eval_infix_expr<'a>(expr: &'a ExprInfix, env: &mut Environment<'a>) -> Result<Object, Error> {
    let left = eval_expr(&*expr.left, env)?;
    let right = eval_expr(&*expr.right, env)?;

    match expr.operator {
        Operator::Add => left.add(right),
        Operator::Subtract => left.sub(right),
        Operator::Multiply => left.mul(right),
        Operator::Divide => left.div(right),
        Operator::Modulo => left.rem(right),
        Operator::Gt => left.gt(right),
        Operator::Gte => left.gte(right),
        Operator::Lt => left.lt(right),
        Operator::Lte => left.lte(right),
        Operator::Eq => left.eq(right),
        Operator::Neq => left.neq(right),
        _ => {
            unimplemented!(
                "Infix expressions with operator {:?} are not implemented.",
                &expr.operator
            )
        }
    }
}

fn eval_prefix_expr<'a>(expr: &'a ExprPrefix, env: &mut Environment<'a>) -> Result<Object, Error> {
    let right = eval_expr(&*expr.right, env)?;

    match expr.operator {
        Operator::Negate => Ok(Object::bool(!right.as_bool())),
        Operator::Subtract => Ok(Object::int(-right.as_int())),
        _ => unimplemented!(
            "Prefix expressions with operator {:?} are not implemented.",
            &expr.operator
        ),
    }
}

#[inline]
fn eval_if_expr<'a>(expr: &'a ExprIf, env: &mut Environment<'a>) -> Result<Object, Error> {
    let condition = eval_expr(&*expr.condition, env)?;

    if condition.is_truthy() {
        eval_block(&expr.consequence, env)
    } else if let Some(alternative) = &expr.alternative {
        eval_block(alternative, env)
    } else {
        Ok(Object::null())
    }
}

#[inline(always)]
fn eval_block<'a>(block: &'a Vec<Expr>, env: &mut Environment<'a>) -> Result<Object, Error> {
    let mut last = Object::null();
    for expr in block {
        last = eval_expr(expr, env)?;
    }
    Ok(last)
}

#[inline(always)]
fn eval_int_expr(expr: &ExprInt) -> Result<Object, Error> {
    Ok(Object::int(expr.value))
}

#[inline(always)]
fn eval_float_expr(expr: &ExprFloat) -> Result<Object, Error> {
    Ok(Object::float(expr.value))
}

#[inline(always)]
fn eval_bool_expr(expr: &ExprBool) -> Result<Object, Error> {
    Ok(Object::bool(expr.value))
}

fn eval_string_expr(expr: &ExprString) -> Result<Object, Error> {
    Ok(Object::string(&expr.value))
}

#[inline(always)]
fn eval_function_expr(expr: &ExprFunction) -> Result<Object, Error> {
    Ok(Object::function(expr))
}

fn eval_declare_expr<'a>(
    expr: &'a ExprDeclare,
    env: &mut Environment<'a>,
) -> Result<Object, Error> {
    let value = eval_expr(&*expr.value, env)?;
    env.insert(&expr.name, value);
    Ok(Object::null())
}

#[inline]
fn eval_call_expr<'a>(expr: &'a ExprCall, env: &mut Environment<'a>) -> Result<Object, Error> {
    let obj = match &*expr.func {
        Expr::Identifier(name) => env.resolve(&name),
        Expr::Function(expr) => eval_function_expr(expr)?,
        _ => panic!("Expression of type {:?} is not callable.", expr.func),
    };

    if obj.tag() != Type::Function {
        return Err(Error::TypeError(format!(
            "Object of type {:?} is not callable.",
            obj
        )));
    }

    let func = obj.as_function();
    if expr.arguments.len() != func.parameters.len() {
        return Err(Error::TypeError(format!(
            "{} takes exactly {} arguments ({} given)",
            "function",
            func.parameters.len(),
            expr.arguments.len()
        )));
    }

    let var_length = env.names.len();
    for value_expr in &expr.arguments {
        let value = eval_expr(value_expr, env)?;
        env.values.push(value);
    }
    for name in &func.parameters {
        env.names.push(name);
    }

    let result = eval_block(&func.body, env);
    env.names.truncate(var_length);
    env.values.truncate(var_length);
    return result;
}

#[inline]
fn eval_expr<'a>(expr: &'a Expr, env: &mut Environment<'a>) -> Result<Object, Error> {
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
            env.insert(expr.name.as_str(), obj);
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
    extern crate test;
    use crate::parser;

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
            ("6 + 2", Object::int(8)),
            ("6 - 2", Object::int(4)),
            ("6 * 2", Object::int(12)),
            ("6 / 2", Object::int(3)),
            ("6 % 2", Object::int(0)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_int_arithmetic_with_precedence() {
        for (input, expected) in [
            ("6 + 2 * 5", Object::int(16)),
            ("6 + 2 * 5 / 5", Object::int(8)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_float_arithmetic() {
        for (input, expected) in [
            ("6.5 + 2.0", Object::float(8.5)),
            ("6.5 - 2.0", Object::float(4.5)),
            ("6.5 * 2.0", Object::float(13.0)),
            ("6.5 / 2.0", Object::float(3.25)),
            ("6.5 % 2.0", Object::float(0.5)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    #[ignore]
    fn test_mixed_arithmetic() {
        for (input, expected) in [
            ("6.5 + 2", Object::float(8.5)),
            ("6 - 2.0", Object::float(4.0)),
            ("6.5 * 2", Object::float(13.0)),
            ("6.5 / 2", Object::float(3.25)),
            ("6.5 % 2", Object::float(0.5)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_int_boolean_infix_expressions() {
        for (input, expected) in [
            ("5 > 2", Object::bool(true)),
            ("5 < 2", Object::bool(false)),
            ("1 >= 1", Object::bool(true)),
            ("1 <= 1", Object::bool(true)),
            ("1 == 1", Object::bool(true)),
            ("1 != 1", Object::bool(false)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_float_boolean_infix_expressions() {
        for (input, expected) in [
            ("5.5 > 2.5", Object::bool(true)),
            ("5.5 < 2.5", Object::bool(false)),
            ("1.5 >= 1.5", Object::bool(true)),
            ("1.5 <= 1.5", Object::bool(true)),
            ("1.5 == 1.5", Object::bool(true)),
            ("1.5 != 1.5", Object::bool(false)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_bool_boolean_infix_expressions() {
        for (input, expected) in [
            ("ja > nee", Object::bool(true)),
            ("ja < nee", Object::bool(false)),
            ("ja >= nee", Object::bool(true)),
            ("ja <= nee", Object::bool(false)),
            ("ja == nee", Object::bool(false)),
            ("ja != nee", Object::bool(true)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_prefix_expressions() {
        for (input, expected) in [
            ("!ja", Object::bool(false)),
            ("!nee", Object::bool(true)),
            ("-5", Object::int(-5)),
            ("!!ja", Object::bool(true)),
            ("!!!ja", Object::bool(false)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    #[ignore]
    fn test_string_infix_expressions() {
        for (input, expected) in [
            ("\"foo\" + \"bar\"", Object::string("foobar")),
            // ("\"foo\" * 2", Object::string("foofoo".to_owned())),
            ("\"foo\" == \"foo\"", Object::bool(true)),
            ("\"foo\" != \"foo\"", Object::bool(false)),
            ("\"foo\" >= \"foo\"", Object::bool(true)),
            ("\"foo\" <= \"foo\"", Object::bool(true)),
            ("\"foo\" > \"foo\"", Object::bool(false)),
            ("\"foo\" < \"foo\"", Object::bool(false)),
            ("\"abc\" > \"xyz\"", Object::bool(false)),
            ("\"abc\" >= \"xyz\"", Object::bool(false)),
            ("\"abc\" < \"xyz\"", Object::bool(true)),
            ("\"abc\" <= \"xyz\"", Object::bool(true)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_multiple_expressions() {
        {
            let (input, expected) = ("5 + 5; 6 + 2", Object::int(8));
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_if_expressions() {
        for (input, expected) in [
            ("als 5 > 4 { 1 }", Object::int(1)),
            ("als 5 < 4 { 1 }", Object::null()),
            ("als 5 < 4 { 1 } anders { 2 }", Object::int(2)),
            ("als (5 > 4) { 1 } anders { 2 }", Object::int(1)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_declare_statements() {
        {
            let (input, expected) = ("stel a = 100", Object::null());
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_ident_expressions() {
        for (input, expected) in [
            ("a", Object::null()),
            ("stel a = 100; a", Object::int(100)),
            ("stel a = 100; stel b = 2; a", Object::int(100)),
            ("stel a = 100; stel b = 2; a * b", Object::int(200)),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_nested_if_expressions() {
        for (input, expected) in [
            ("als 5 > 4 { als 11 > 10 { 1 } }", Object::int(1)),
            (
                "als 5 > 4 { als 10 > 11 { 1 } anders { 2 } }",
                Object::int(2),
            ),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }

    #[test]
    fn test_if_else_if_expression() {
        for (input, expected) in [
            ("als 1 > 2 { 1 } anders als 2 > 1 { 2 }", Object::int(2)),
            (
                "als 1 > 2 { 1 } anders als 2 > 3 { 2 } anders { 3 }",
                Object::int(3),
            ),
        ] {
            assert_program_result(input, Ok(expected));
        }
    }
}
