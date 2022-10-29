use crate::ast::BlockStmt;
use crate::parser::ParseError;
use std::fmt::Display;
use std::ops;
use std::string::String;

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
    TypeError(String),
    SyntaxError(ParseError),
    ReferenceError(String),
    IndexError(String),
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub(crate) enum NlObject {
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    CompiledFunction(Vec<u8>, u8),
    String(Box<String>),
    Func(Box<NlFuncObject>),
    Array(Box<Vec<NlObject>>),
    Return(Box<NlObject>),
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub(crate) struct NlFuncObject {
    pub(crate) name: String,
    pub(crate) parameters: Vec<String>,
    pub(crate) body: BlockStmt,
}

impl NlFuncObject {
    pub(crate) fn new(name: String, parameters: Vec<String>, body: BlockStmt) -> NlObject {
        NlObject::Func(Box::new(NlFuncObject {
            name,
            parameters,
            body,
        }))
    }
}

impl NlObject {
    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            NlObject::Bool(v) => *v,
            NlObject::Int(v) => *v > 0,
            NlObject::Float(v) => *v > 0.0,
            NlObject::String(v) => !v.is_empty(),
            _ => unimplemented!(
                "Can not use objects of type {:?} as boolean expression.",
                self
            ),
        }
    }
}

impl Display for NlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NlObject::Bool(v) => f.write_fmt(format_args!("{}", v)),
            NlObject::Int(v) => f.write_fmt(format_args!("{}", v)),
            NlObject::Float(v) => f.write_fmt(format_args!("{}", v)),
            NlObject::String(v) => f.write_fmt(format_args!("{}", v)),
            NlObject::Func(func) => f.write_fmt(format_args!("function {}", func.name)),
            NlObject::Return(value) => return value.fmt(f),
            NlObject::Array(values) => {
                f.write_str("[")?;
                for (i, v) in values.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    v.fmt(f)?;
                }
                f.write_str("]")
            }

            // Write nothing for other types
            _ => Ok(()),
        }
    }
}

impl NlObject {
    pub(crate) fn add(&self, rhs: &NlObject) -> Result<NlObject, Error> {
        let r = match (&self, &rhs) {
            (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a + b),
            (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a + b),
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a + *b as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 + b),
            (NlObject::String(a), NlObject::String(b)) => {
                // Apparently this is the fastest way to concatenate two strings
                // https://github.com/hoodie/concatenation_benchmarks-rs#the-same-results-rearranged-fastest-to-slowest
                let mut s = String::with_capacity(a.len() + b.len());
                s.push_str(a);
                s.push_str(b);
                NlObject::String(Box::new(s))
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

    pub(crate) fn sub(&self, rhs: &NlObject) -> Result<NlObject, Error> {
        let r = match (&self, &rhs) {
            (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a - b),
            (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a - b),
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a - *b as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 - b),
            _ => {
                return Err(Error::TypeError(format!(
                    "Subtracting objects of type {:?} and {:?} is not supported.",
                    self, rhs
                )))
            }
        };
        Ok(r)
    }

    pub(crate) fn mul(&self, rhs: &NlObject) -> Result<NlObject, Error> {
        let r = match (&self, &rhs) {
            (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a * b),
            (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a * b),
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a * (*b) as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 * b),
            (NlObject::String(a), NlObject::Int(b)) => {
                // TODO: Return error if b is negative
                let n = *b as usize;
                let mut s = String::with_capacity(a.len() * n);
                for _ in 0..n {
                    s.push_str(a);
                }
                NlObject::String(Box::new(s))
            }
            _ => {
                return Err(Error::TypeError(format!(
                    "Multiplying objects of type {:?} and {:?} is not supported.",
                    self, rhs
                )))
            }
        };

        Ok(r)
    }

    pub(crate) fn div(&self, rhs: &NlObject) -> Result<NlObject, Error> {
        let r = match (&self, &rhs) {
            (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a / b),
            (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a / b),
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a / *b as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 / b),
            _ => {
                return Err(Error::TypeError(format!(
                    "Dividing objects of type {:?} and {:?} is not supported.",
                    self, rhs
                )))
            }
        };

        Ok(r)
    }

    pub(crate) fn rem(&self, rhs: &NlObject) -> Result<NlObject, Error> {
        let r = match (&self, &rhs) {
            (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a % b),
            (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a % b),
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a % *b as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 % b),
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

impl ops::Not for NlObject {
    type Output = Result<NlObject, Error>;

    fn not(self) -> Result<NlObject, Error> {
        let r = match self {
            NlObject::Bool(v) => NlObject::Bool(!v),
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

impl ops::Neg for NlObject {
    type Output = Result<NlObject, Error>;

    fn neg(self) -> Result<NlObject, Error> {
        let r = match self {
            NlObject::Int(v) => NlObject::Int(-v),
            NlObject::Float(v) => NlObject::Float(-v),
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
macro_rules! impl_cmp {
    ($func_name:ident, $op:tt) => {
        #[inline]
        pub fn $func_name(&self, other: &Self) -> Result<NlObject, Error> {
            match (self, other) {
                (NlObject::String(_), NlObject::String(_)) => Ok(NlObject::Bool(self $op other)),
                (NlObject::Func(_), _)
                | (_, NlObject::Func(_))
                | (NlObject::String(_), _)
                | (_, NlObject::String(_)) => Err(Error::TypeError(format!(
                    "Comparing objects of type {:?} and {:?} is not supported",
                    self, other
                ))),
                _ => Ok(NlObject::Bool(self $op other)),
            }
        }
    };
}

impl NlObject {
    impl_cmp!(gt, >);
    impl_cmp!(gte, >=);
    impl_cmp!(lt, <);
    impl_cmp!(lte, <=);

    #[inline]
    pub fn eq(&self, other: &Self) -> Result<NlObject, Error> {
        Ok(NlObject::Bool(self == other))
    }

    #[inline]
    pub fn neq(&self, other: &Self) -> Result<NlObject, Error> {
        Ok(NlObject::Bool(self != other))
    }
}
