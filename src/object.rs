use std::ops;

pub enum Error {
    TypeError
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum NlObject {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Error(&'static str),
}

impl ops::Add<NlObject> for NlObject {
    type Output = NlObject;

    fn add(self, rhs: NlObject) -> NlObject {
        match (&self, &rhs) {
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
                NlObject::String(s)
            },
            _ => todo!(
                "Adding objects of type {:?} and {:?} is not yet supported.",
                self,
                rhs
            ),
        }
    }
}

impl ops::Sub<NlObject> for NlObject {
    type Output = NlObject;

    fn sub(self, rhs: NlObject) -> NlObject {
        match (&self, &rhs) {
            (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a - b),
            (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a - b),
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a - *b as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 - b),
            _ => todo!(
                "Subtracting objects of type {:?} and {:?} is not yet supported.",
                self,
                rhs
            ),
        }
    }
}

impl ops::Mul<NlObject> for NlObject {
    type Output = NlObject;

    fn mul(self, rhs: NlObject) -> NlObject {
        match (&self, &rhs) {
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
                NlObject::String(s)
            }
            _ => todo!(
                "Multiplying objects of type {:?} and {:?} is not yet supported.",
                self,
                rhs
            ),
        }
    }
}

impl ops::Div<NlObject> for NlObject {
    type Output = NlObject;

    fn div(self, rhs: NlObject) -> NlObject {
        match (&self, &rhs) {
            (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a / b),
            (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a / b),
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a / *b as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 / b),
            _ => todo!(
                "Dividing objects of type {:?} and {:?} is not yet supported.",
                self,
                rhs
            ),
        }
    }
}

impl ops::Rem<NlObject> for NlObject {
    type Output = NlObject;

    fn rem(self, rhs: NlObject) -> NlObject {
        match (&self, &rhs) {
            (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a % b),
            (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a % b),
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a % *b as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 % b),
            _ => todo!(
                "Taking the remainder objects of type {:?} and {:?} is not yet supported.",
                self,
                rhs
            ),
        }
    }
}

impl ops::Not for NlObject {
    type Output = NlObject;

    fn not(self) -> NlObject {
        match self {
            NlObject::Bool(v) => NlObject::Bool(!v),
            _ => todo!(),
        }
    }
}

impl ops::Neg for NlObject {
    type Output = NlObject;

    fn neg(self) -> NlObject {
        match self {
            NlObject::Int(v) => NlObject::Int(-v),
            NlObject::Float(v) => NlObject::Float(-v),
            _ => todo!(),
        }
    }
}
