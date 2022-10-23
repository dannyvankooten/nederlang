use crate::ast::{
    BlockStmt, Expr, ExprBool, ExprFloat, ExprIf, ExprInfix, ExprInt, ExprPrefix, ExprString, Op,
    Stmt,
};
use crate::object::*;

pub(crate) trait Eval {
    fn eval(&self) -> NlObject;
}

impl Eval for ExprInfix {
    fn eval(&self) -> NlObject {
        let left = self.left.eval();
        let right = self.right.eval();

        match &self.op {
            Op::Add => left + right,
            Op::Subtract => left - right,
            Op::Multiply => left * right,
            Op::Divide => left / right,
            Op::Modulo => left % right,
            Op::Gt => NlObject::Bool(left > right),
            Op::Gte => NlObject::Bool(left >= right),
            Op::Lt => NlObject::Bool(left < right),
            Op::Lte => NlObject::Bool(left >= right),
            Op::Eq => NlObject::Bool(left == right),
            Op::Neq => NlObject::Bool(left != right),
            _ => unimplemented!(
                "Infix expressions with operator {:?} are not implemented.",
                &self.op
            ),
        }
    }
}

impl Eval for ExprPrefix {
    fn eval(&self) -> NlObject {
        let right = self.right.eval();

        match &self.op {
            Op::Negate => !right,
            Op::Subtract => -right,
            _ => unimplemented!(
                "Infix expressions with operator {:?} are not implemented.",
                &self.op
            ),
        }
    }
}

impl Eval for ExprIf {
    fn eval(&self) -> NlObject {
        let condition = self.condition.eval();

        if condition.is_truthy() {
            self.consequence.eval()
        } else if let Some(alternative) = &self.alternative {
            alternative.eval()
        } else {
            NlObject::Null
        }
    }
}

impl Eval for Stmt {
    fn eval(&self) -> NlObject {
        match self {
            Stmt::Expr(expr) => expr.eval(),
            _ => todo!(),
        }
    }
}

impl Eval for BlockStmt {
    fn eval(&self) -> NlObject {
        let mut last = NlObject::Null;
        for s in self {
            last = s.eval()
        }
        last
    }
}

impl Eval for ExprInt {
    fn eval(&self) -> NlObject {
        NlObject::Int(self.value)
    }
}

impl Eval for ExprFloat {
    fn eval(&self) -> NlObject {
        NlObject::Float(self.value)
    }
}

impl Eval for ExprBool {
    fn eval(&self) -> NlObject {
        NlObject::Bool(self.value)
    }
}

impl Eval for ExprString {
    fn eval(&self) -> NlObject {
        NlObject::String(self.value.to_owned())
    }
}

impl Eval for Expr {
    fn eval(&self) -> NlObject {
        match self {
            Expr::Infix(expr) => expr.eval(),
            Expr::Prefix(expr) => expr.eval(),
            Expr::If(expr) => expr.eval(),
            Expr::Int(expr) => expr.eval(),
            Expr::Float(expr) => expr.eval(),
            Expr::Bool(expr) => expr.eval(),
            Expr::String(expr) => expr.eval(),
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
    use crate::parser::parse;
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
                expected,
                parse(input).unwrap().eval(),
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
                expected,
                parse(input).unwrap().eval(),
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
                expected,
                parse(input).unwrap().eval(),
                "eval input: {}",
                input
            );
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
            assert_eq!(
                expected,
                parse(input).unwrap().eval(),
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
                expected,
                parse(input).unwrap().eval(),
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
        ] {
            assert_eq!(
                expected,
                parse(input).unwrap().eval(),
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
                expected,
                parse(input).unwrap().eval(),
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
                expected,
                parse(input).unwrap().eval(),
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
                expected,
                parse(input).unwrap().eval(),
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
                expected,
                parse(input).unwrap().eval(),
                "eval input: {}",
                input
            );
        }
    }

    #[bench]
    #[ignore]
    fn bench_string_addition(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(
                NlObject::String("foo".repeat(10)),
                parse("\"foo\" * 10").unwrap().eval()
            );
        });
    }
}
