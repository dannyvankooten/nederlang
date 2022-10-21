use crate::object::*;
use crate::parser::{Expr, ExprInfix, ExprPrefix, Op, Stmt};

impl ExprInfix {
    fn eval(&self) -> NlObject {
        let left = eval(&self.left);
        let right = eval(&self.right);

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
            _ => todo!(
                "Infix expressions with operator {:?} are not yet implemented.",
                &self.op
            ),
        }
    }
}

impl ExprPrefix {
    fn eval(&self) -> NlObject {
        let right = eval(&self.right);

        match &self.op {
            Op::Negate => !right,
            Op::Subtract => -right,
            _ => todo!(
                "Infix expressions with operator {:?} are not yet implemented.",
                &self.op
            ),
        }
    }
}

pub(crate) fn eval(expr: &Expr) -> NlObject {
    // dbg!(expr);
    match expr {
        Expr::Infix(expr) => expr.eval(),
        Expr::Prefix(expr) => expr.eval(),
        Expr::Integer(v) => NlObject::Int(*v),
        Expr::Float(v) => NlObject::Float(*v),
        Expr::Boolean(v) => NlObject::Bool(*v),
        Expr::String(v) => NlObject::String(v.to_string()),
        _ => todo!(
            "Evaluating expressions of type {:?} is not yet implemented.",
            expr
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_eval() {
        for (input, expected) in [
            ("6+2", NlObject::Int(8)),
            ("6-2", NlObject::Int(4)),
            ("6*2", NlObject::Int(12)),
            ("6/2", NlObject::Int(3)),
            ("6%2", NlObject::Int(0)),
        ] {
            assert_eq!(expected, eval(&parse(input)), "eval input: {}", input);
        }
    }
}
