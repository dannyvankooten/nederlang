use crate::ast::{
    BlockStmt, Expr, ExprArray, ExprAssign, ExprBool, ExprCall, ExprFloat, ExprIf, ExprIndex,
    ExprInfix, ExprInt, ExprPrefix, ExprString, ExprWhile, Operator, Stmt,
};
use crate::parser;
use crate::{builtins, object::*};
use hashbrown::HashMap;
use parser::ParseError;

#[derive(Debug)]
pub struct Environment {
    scopes: Vec<HashMap<String, NlObject>>,
    current_scope_idx: u8,
}

pub(crate) trait Eval {
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error>;
}

const MAX_SCOPES: u8 = 64;

impl Environment {
    #[inline]
    pub fn new() -> Self {
        let mut scopes: Vec<HashMap<String, NlObject>> = Vec::with_capacity(MAX_SCOPES as usize);
        for _ in 0..MAX_SCOPES {
            scopes.push(HashMap::new());
        }

        Environment {
            scopes: scopes,
            current_scope_idx: 0,
        }
    }

    #[inline(always)]
    pub fn enter_scope(&mut self) {
        self.current_scope_idx += 1;

        debug_assert!(self.current_scope_idx < MAX_SCOPES);
    }

    #[inline(always)]
    pub fn leave_scope(&mut self) {
        self.scopes[self.current_scope_idx as usize].clear();
        self.current_scope_idx -= 1;
    }

    #[inline]
    pub(crate) fn resolve(&self, ident: &str) -> Option<NlObject> {
        for idx in (0..self.current_scope_idx + 1).rev() {
            unsafe {
                if let Some(value) = self.scopes.get_unchecked(idx as usize).get(ident) {
                    return Some(value.clone());
                }
            }
        }

        return None;
    }

    #[inline]
    /// Resolves a function
    /// Right now the only difference with resolve is that this looks at the top-scope first
    /// Only if function is not in top scope, it starts looking locally
    /// This also means functions declared in the top-level scope can not be re-declared.
    pub(crate) fn resolve_function(&self, ident: &str) -> Option<NlObject> {
        unsafe {
            // Safety: Scope 0 is always set
            if let Some(value) = self.scopes.get_unchecked(0).get(ident) {
                return Some(value.clone());
            }
        }

        return self.resolve(ident);
    }

    #[inline]
    pub(crate) fn insert(&mut self, ident: String, value: NlObject) {
        unsafe {
            self.scopes
                .get_unchecked_mut(self.current_scope_idx as usize)
                .insert(ident, value);
        }
    }

    pub(crate) fn update(&mut self, ident: &str, new_value: NlObject) -> Result<(), Error> {
        for idx in (0..self.current_scope_idx + 1).rev() {
            if let Some(old_value) = self.scopes[idx as usize].get_mut(ident) {
                *old_value = new_value;
                return Ok(());
            }
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
        match self {
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

#[inline(always)]
fn is_out_of_bounds<T>(values: &Vec<T>, idx: i64) -> bool {
    return idx < 0 || idx as usize >= values.len();
}

impl Eval for ExprAssign {
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error> {
        match &*self.left {
            Expr::Identifier(name) => {
                let right = self.right.eval(env)?;
                env.update(&name, right)?;
                return Ok(NlObject::Null);
            }
            Expr::Index(expr) => {
                match &*expr.left {
                    Expr::Identifier(name) => {
                        let value = env.resolve(&name);
                        if value.is_none() {
                            return Err(Error::ReferenceError(format!("{} is not defined", name)));
                        }
                        let value = value.unwrap();
                        match value {
                            NlObject::Array(mut values) => {
                                let index = expr.index.eval(env)?;
                                match index {
                                    NlObject::Int(idx) => {
                                        if is_out_of_bounds(&values, idx) {
                                            return Err(Error::IndexError(format!(
                                                "Array assignment index out of bounds"
                                            )));
                                        }

                                        let right = self.right.eval(env)?;
                                        values[idx as usize] = right;
                                        env.update(&name, NlObject::Array(values))?;
                                    }
                                    _ => {
                                        return Err(Error::IndexError(format!(
                                            "Can not use object of type {} as index",
                                            index
                                        )))
                                    }
                                }
                            }
                            _ => {
                                return Err(Error::IndexError(format!(
                                    "Can not index into object of type {}",
                                    value
                                )))
                            }
                        }
                    }
                    // assigning to a non-stored value is a no-op
                    // we could also error here?
                    _ => (),
                }
                Ok(NlObject::Null)
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
        env.enter_scope();
        let result = self.eval(env);
        env.leave_scope();
        result
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
        Ok(NlObject::String(Box::new(self.value.clone())))
    }
}

fn eval_builtin_call(
    name: &str,
    args: &[Expr],
    env: &mut Environment,
) -> Option<Result<NlObject, Error>> {
    match name {
        "len" => {
            let arg = args[0].eval(env);
            if arg.is_err() {
                return Some(arg);
            }
            Some(builtins::len(arg.unwrap()))
        }
        "print" => {
            let arg = args[0].eval(env);
            if arg.is_err() {
                return Some(arg);
            }
            Some(builtins::print(&[arg.unwrap()]))
        }
        _ => None,
    }
}

impl Eval for ExprCall {
    fn eval(&self, env: &mut Environment) -> Result<NlObject, Error> {
        let func = match &*self.left {
            // LHS is an identifier
            Expr::Identifier(name) => {
                let object = env.resolve_function(name);
                match object {
                    Some(NlObject::Func(func)) => *func,
                    Some(_) => {
                        return Err(Error::TypeError(format!(
                            "Object of type {:?} is not callable.",
                            object
                        )))
                    }
                    _ => {
                        // No identifier with this name, try a builtin function
                        if let Some(result) = eval_builtin_call(name, &self.arguments, env) {
                            return result;
                        }

                        return Err(Error::ReferenceError(format!("{} is not defined.", name)));
                    }
                }
            }
            // LHS is a direct function expression
            Expr::Function(name, parameters, body) => NlFuncObject {
                name: name.clone(),
                parameters: parameters.clone(),
                body: body.clone(),
            },
            _ => panic!(
                "Expression of type {:?} is not callable. Parser should have picked up on this.",
                self.left
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
        env.enter_scope();
        for (name, value) in std::iter::zip(&func.parameters, &self.arguments) {
            let value = value.eval(env)?;
            env.insert(name.to_string(), value);
        }
        let result = func.body.eval(env)?;
        env.leave_scope();

        if let NlObject::Return(return_value) = result {
            return Ok(*return_value);
        }
        Ok(result)
    }
}

fn eval_array_expression(expr: &ExprArray, env: &mut Environment) -> Result<NlObject, Error> {
    let mut values = Vec::with_capacity(expr.values.len());
    for e in &expr.values {
        values.push(e.eval(env)?);
    }

    Ok(NlObject::Array(Box::new(values)))
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
            (
                "\"foo\" + \"bar\"",
                NlObject::String(Box::new("foobar".to_owned())),
            ),
            (
                "\"foo\" * 2",
                NlObject::String(Box::new("foofoo".to_owned())),
            ),
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
            eval_program(
                "functie b() { antwoord 1 }; functie a() { antwoord b() + b(); }; a();",
                None
            ),
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
        assert_eq!(
            eval_program("[]", None),
            Ok(NlObject::Array(Box::new(vec![])))
        );
        assert_eq!(
            eval_program("[1]", None),
            Ok(NlObject::Array(Box::new(vec![NlObject::Int(1)])))
        );
        assert_eq!(
            eval_program("[1, 2]", None),
            Ok(NlObject::Array(Box::new(vec![
                NlObject::Int(1),
                NlObject::Int(2)
            ])))
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

    #[test]
    fn test_array_index_assign() {
        // a is not defined
        assert!(eval_program("a[0] = 1", None).is_err());
        // The following does not err right now because we don't check anything if left hand side is not an identifier
        // assert!(eval_program("[][0] = 1", None).is_err());
        assert!(eval_program("stel a = [1, 2]; a[-1] = 1", None).is_err());
        assert!(eval_program("stel a = [1, 2]; a[2] = 1", None).is_err());

        // OK:
        assert_eq!(
            eval_program("stel a = [1]; a[0] = 2; a", None),
            Ok(NlObject::Array(Box::new(vec![NlObject::Int(2)])))
        );
        assert_eq!(
            eval_program("stel a = [1, 2]; a[0] = 2; a[1] = 3; a", None),
            Ok(NlObject::Array(Box::new(vec![
                NlObject::Int(2),
                NlObject::Int(3)
            ])))
        );
    }

    #[test]
    fn test_builtins() {
        assert_eq!(eval_program("len([1, 2, 3])", None), Ok(NlObject::Int(3)));
        assert_eq!(eval_program("len(\"foobar\")", None), Ok(NlObject::Int(6)));
    }

    #[bench]
    fn bench_arithmetic(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(
                Ok(NlObject::Int(2350)),
                eval_program(" 1 * 5 * 10 * 20 - 100 / 2 + 1400", None),
            );
        });
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
