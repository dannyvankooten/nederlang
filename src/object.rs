use std::ops;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum NlObject {
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
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
            }
            _ => unimplemented!(
                "Adding objects of type {:?} and {:?} is not supported.",
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
            _ => unimplemented!(
                "Subtracting objects of type {:?} and {:?} is not supported.",
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
            _ => unimplemented!(
                "Multiplying objects of type {:?} and {:?} is not supported.",
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
            _ => unimplemented!(
                "Dividing objects of type {:?} and {:?} is not supported.",
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
            _ => unimplemented!(
                "Taking the remainder objects of type {:?} and {:?} is not supported.",
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
            _ => unimplemented!("Not on objects of type {:?} is not implemented.", self),
        }
    }
}

impl ops::Neg for NlObject {
    type Output = NlObject;

    fn neg(self) -> NlObject {
        match self {
            NlObject::Int(v) => NlObject::Int(-v),
            NlObject::Float(v) => NlObject::Float(-v),
            _ => unimplemented!("Negating objects of type {:?} is not implemented.", self),
        }
    }
}
