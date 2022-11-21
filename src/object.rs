use crate::ast::{Expr, ExprFunction};
use crate::eval::Error;
use std::fmt::Display;
use std::ops;
use std::string::String;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub(crate) enum Object {
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Func(*const ExprFunction),
}

impl Object {
    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Object::Bool(v) => *v,
            Object::Int(v) => *v > 0,
            Object::Float(v) => *v > 0.0,
            Object::String(v) => !v.is_empty(),
            _ => unimplemented!(
                "Can not use objects of type {:?} as boolean expression.",
                self
            ),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Bool(v) => f.write_fmt(format_args!("{}", v)),
            Object::Int(v) => f.write_fmt(format_args!("{}", v)),
            Object::Float(v) => f.write_fmt(format_args!("{}", v)),
            Object::String(v) => f.write_fmt(format_args!("{}", v)),
            Object::Null => f.write_str(""),
            Object::Func(..) => f.write_str("function"),
        }
    }
}

impl ops::Add<Object> for Object {
    type Output = Result<Object, Error>;

    fn add(self, rhs: Object) -> Result<Object, Error> {
        let r = match (&self, &rhs) {
            (Object::Int(a), Object::Int(b)) => Object::Int(a + b),
            (Object::Float(a), Object::Float(b)) => Object::Float(a + b),
            (Object::String(a), Object::String(b)) => {
                // Apparently this is the fastest way to concatenate two strings
                // https://github.com/hoodie/concatenation_benchmarks-rs#the-same-results-rearranged-fastest-to-slowest
                let mut s = String::with_capacity(a.len() + b.len());
                s.push_str(a);
                s.push_str(b);
                Object::String(s)
            }
            _ => {
                return Err(Error::TypeError(format!(
                    "unsupported operand types for +: {:?} and {:?}",
                    self, rhs
                )))
            }
        };

        Ok(r)
    }
}

impl ops::Sub<Object> for Object {
    type Output = Result<Object, Error>;

    fn sub(self, rhs: Object) -> Result<Object, Error> {
        let r = match (&self, &rhs) {
            (Object::Int(a), Object::Int(b)) => Object::Int(a - b),
            (Object::Float(a), Object::Float(b)) => Object::Float(a - b),
            _ => {
                return Err(Error::TypeError(format!(
                    "Subtracting objects of type {:?} and {:?} is not supported.",
                    self, rhs
                )))
            }
        };
        Ok(r)
    }
}

impl ops::Mul<Object> for Object {
    type Output = Result<Object, Error>;

    fn mul(self, rhs: Object) -> Result<Object, Error> {
        let r = match (&self, &rhs) {
            (Object::Int(a), Object::Int(b)) => Object::Int(a * b),
            (Object::Float(a), Object::Float(b)) => Object::Float(a * b),
            _ => {
                return Err(Error::TypeError(format!(
                    "Multiplying objects of type {:?} and {:?} is not supported.",
                    self, rhs
                )))
            }
        };

        Ok(r)
    }
}

impl ops::Div<Object> for Object {
    type Output = Result<Object, Error>;

    fn div(self, rhs: Object) -> Result<Object, Error> {
        let r = match (&self, &rhs) {
            (Object::Int(a), Object::Int(b)) => Object::Int(a / b),
            (Object::Float(a), Object::Float(b)) => Object::Float(a / b),
            _ => {
                return Err(Error::TypeError(format!(
                    "Dividing objects of type {:?} and {:?} is not supported.",
                    self, rhs
                )))
            }
        };

        Ok(r)
    }
}

impl ops::Rem<Object> for Object {
    type Output = Result<Object, Error>;

    fn rem(self, rhs: Object) -> Result<Object, Error> {
        let r = match (&self, &rhs) {
            (Object::Int(a), Object::Int(b)) => Object::Int(a % b),
            (Object::Float(a), Object::Float(b)) => Object::Float(a % b),
            _ => {
                return Err(Error::TypeError(format!(
                    "Taking the remainder objects of type {:?} and {:?} is not supported.",
                    self, rhs
                )))
            }
        };
        Ok(r)
    }
}

impl ops::Not for Object {
    type Output = Result<Object, Error>;

    fn not(self) -> Result<Object, Error> {
        let r = match self {
            Object::Bool(v) => Object::Bool(!v),
            _ => {
                return Err(Error::TypeError(format!(
                    "Not on objects of type {:?} is not implemented.",
                    self
                )))
            }
        };

        Ok(r)
    }
}

impl ops::Neg for Object {
    type Output = Result<Object, Error>;

    fn neg(self) -> Result<Object, Error> {
        let r = match self {
            Object::Int(v) => Object::Int(-v),
            Object::Float(v) => Object::Float(-v),
            _ => {
                return Err(Error::TypeError(format!(
                    "Negating objects of type {:?} is not implemented.",
                    self
                )))
            }
        };

        Ok(r)
    }
}
use Object::*;

macro_rules! impl_cmp {
    ($func_name:ident, $op:tt) => {
        pub fn $func_name(&self, other: &Self) -> Result<Object, Error> {

            if std::mem::discriminant(self) != std::mem::discriminant(other) {
                return Err(Error::TypeError(format!(
                    "Comparing objects of type {:?} and {:?} is not supported",
                    self, other
                )));
            }

            Ok(Bool(self $op other))
        }
    };
}

impl Object {
    impl_cmp!(gt, >);
    impl_cmp!(gte, >=);
    impl_cmp!(lt, <);
    impl_cmp!(lte, <=);

    pub fn eq(&self, other: &Self) -> Result<Object, Error> {
        Ok(Bool(self == other))
    }

    pub fn neq(&self, other: &Self) -> Result<Object, Error> {
        Ok(Bool(self != other))
    }
}
