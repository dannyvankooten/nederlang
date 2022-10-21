use std::ops;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum NlObject {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

impl ops::Add<NlObject> for NlObject {
    type Output = NlObject;

    fn add(self, rhs: NlObject) -> NlObject {
        match (&self, &rhs) {
            (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a + b),
            (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a + b),
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a + *b as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 + b),
            (NlObject::String(a), NlObject::String(b)) => NlObject::String(a.to_owned() + b),
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
            (NlObject::Float(a), NlObject::Int(b)) => NlObject::Float(a * *b as f64),
            (NlObject::Int(a), NlObject::Float(b)) => NlObject::Float(*a as f64 * b),
            (NlObject::String(a), NlObject::Int(b)) => {
                NlObject::String(a.to_owned().repeat(*b as usize))
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
