use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};
use std::fmt::{Display, Write};
use std::ptr::drop_in_place;
use std::string::String as RString;

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeError(RString),
    SyntaxError(RString),
    ReferenceError(RString),
    IndexError(RString),
}

/// A macro for initialising a struct field (without dropping the original default value)
///
/// # Examples
///
//     init!(obj.value => String::new());
macro_rules! init {
    ($field: expr => $value: expr) => {
        unsafe {
            std::ptr::addr_of_mut!($field).write($value);
        }
    };
}

/// The mask to apply to get just the type (tag) from a value object
const TAG_MASK: usize = 0b111;

/// The mask to apply to get just the pointer address from a pointer object
/// This is just the opposite of TAG_MASk
const PTR_MASK: usize = !TAG_MASK;

/// The amount of bits to shift value objects left
/// We use the last 3 bits to store the tag, so the rest can be used for values
const VALUE_SHIFT_BITS: usize = 3;

#[allow(unused)]
/// The max integer value we can store in a value object
const MAX_INT: i64 = std::i64::MAX >> VALUE_SHIFT_BITS;

#[allow(unused)]
/// The minimum integer value we can store in a value object
const MIN_INT: i64 = std::i64::MIN >> VALUE_SHIFT_BITS;

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum Type {
    // The types below are all stored directly inside the pointer
    Null = 0b000,
    Int = 0b001,
    Bool = 0b010,

    // The types below are all heap-allocated
    Float = 0b011,
    String = 0b100,
    Array = 0b101,
    // 0b110
    // 0b111
}

// Object is a wrapper over raw pointers so we can tag them with immediate values (null, bool, float, int)
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct Object(*mut u8);
unsafe impl Sync for Object {}
unsafe impl Send for Object {}

impl Object {
    /// Creates a new object from the value (or address) given with the given type mask applied
    fn with_type(raw: *mut u8, t: Type) -> Self {
        Self((raw as usize | t as usize) as _)
    }

    /// Create a new null value
    pub fn null() -> Self {
        Self::with_type(0 as _, Type::Null)
    }

    /// Create a new boolean value
    pub fn bool(value: bool) -> Self {
        match value {
            true => Self::with_type((1 << VALUE_SHIFT_BITS) as _, Type::Bool),
            false => Self::with_type(0 as _, Type::Bool),
        }
    }

    /// Create a new integer value
    pub fn int(value: i64) -> Self {
        // assert there is no data loss because of the shift
        assert!(((value << VALUE_SHIFT_BITS) >> VALUE_SHIFT_BITS) == value);
        Self::with_type((value << VALUE_SHIFT_BITS) as _, Type::Int)
    }

    /// Returns the type of this object pointer
    pub fn tag(self) -> Type {
        unsafe { std::mem::transmute((self.0 as usize & TAG_MASK) as u8) }
    }

    /// Returns the boolean value of this object pointer
    /// Note that is up to the caller to ensure this pointer is of the correct type
    pub fn as_bool(self) -> bool {
        ((self.0 as u8 >> VALUE_SHIFT_BITS) as u8) != 0
    }

    /// Returns the integer value of this object pointer
    /// Note that is up to the caller to ensure this pointer is of the correct type
    pub fn as_int(self) -> i64 {
        self.0 as i64 >> VALUE_SHIFT_BITS
    }

    /// Returns the float value of this object pointer
    /// The caller should ensure this pointer points to an actual NlFloat type
    pub(crate) unsafe fn as_f64(self) -> f64 {
        Float::read(&self)
    }

    /// Returns the &str value of this object pointer
    /// The caller should ensure this pointer points to an actual NlString type
    pub(crate) unsafe fn as_str(&self) -> &str {
        String::read(&self)
    }

    /// Returns a reference to the Vec<Pointer> value this pointer points to
    /// The caller should ensure this pointer actually points to a NlArray
    pub(crate) unsafe fn as_vec(&self) -> &Vec<Object> {
        unsafe { Array::read(&self) }
    }

    /// Returns a mutable reference to the Vec<Pointer> value this pointer points to
    /// The caller should ensure this pointer actually points to a NlArray
    pub(crate) unsafe fn as_vec_mut(&self) -> &mut Vec<Object> {
        unsafe { &mut self.get_mut::<Array>().value }
    }

    fn as_ptr(self) -> *mut u8 {
        (self.0 as usize & PTR_MASK) as _
    }

    unsafe fn get<'a, T>(self) -> &'a T {
        &*(self.as_ptr() as *const T)
    }

    unsafe fn get_mut<'a, T>(self) -> &'a mut T {
        &mut *(self.as_ptr() as *mut T)
    }

    /// Returns true if this pointer does not contain an immediate value
    /// But points to a heap allocated type (like NlFloat, NlString or NlArray)
    pub fn is_heap_allocated(self) -> bool {
        self.0 as usize & TAG_MASK >= Type::Float as usize
    }
}

impl From<i64> for Object {
    fn from(value: i64) -> Object {
        Object::int(value)
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Object {
        Object::bool(value)
    }
}

impl From<f64> for Object {
    fn from(value: f64) -> Self {
        Float::from_f64(value)
    }
}

impl From<&str> for Object {
    fn from(value: &str) -> Self {
        String::from_str(value)
    }
}

impl From<RString> for Object {
    fn from(value: RString) -> Self {
        String::from_string(value)
    }
}

impl From<&[Object]> for Object {
    fn from(value: &[Object]) -> Self {
        Array::from_slice(&value)
    }
}
impl From<Vec<Object>> for Object {
    fn from(value: Vec<Object>) -> Self {
        Array::from_vec(value)
    }
}

#[derive(Debug)]
#[repr(C)]
struct Header {
    marked: bool,
}

impl Header {
    unsafe fn read(ptr: &Object) -> &mut Header {
        ptr.get_mut::<Self>()
    }
}

#[derive(Debug)]
#[repr(C)]
struct Float {
    header: Header,
    value: f64,
}

impl Float {
    unsafe fn read(ptr: &Object) -> f64 {
        ptr.get::<Self>().value
    }

    unsafe fn destroy(ptr: Object) {
        drop_in_place(ptr.as_ptr() as *mut Self);
        dealloc(ptr.as_ptr(), Layout::new::<Self>());
    }

    fn from_f64(value: f64) -> Object {
        let ptr = Object::with_type(allocate(Layout::new::<Self>()), Type::Float);
        let obj = unsafe { ptr.get_mut::<Self>() };
        obj.header.marked = false;
        init!(obj.value => value );
        ptr
    }
}

#[derive(Debug)]
#[repr(C)]
struct String {
    header: Header,
    value: RString,
}

impl String {
    unsafe fn read(ptr: &Object) -> &str {
        ptr.get::<Self>().value.as_str()
    }

    unsafe fn destroy(ptr: Object) {
        drop_in_place(ptr.as_ptr() as *mut Self);
        dealloc(ptr.as_ptr(), Layout::new::<Self>());
    }

    fn from_string(value: RString) -> Object {
        let ptr = Object::with_type(allocate(Layout::new::<Self>()), Type::String);
        let obj = unsafe { ptr.get_mut::<Self>() };
        obj.header.marked = false;
        init!(obj.value => value);
        ptr
    }

    fn from_str(value: &str) -> Object {
        Self::from_string(value.to_string())
    }
}

#[repr(C)]
struct Array {
    header: Header,
    value: Vec<Object>,
}

impl Array {
    unsafe fn read(ptr: &Object) -> &Vec<Object> {
        ptr.get::<Self>().value.as_ref()
    }

    /// Drops and deallocate this NlArray struct and its value
    unsafe fn destroy(ptr: Object) {
        drop_in_place(ptr.as_ptr() as *mut Self);
        dealloc(ptr.as_ptr(), Layout::new::<Self>());
    }

    fn from_vec(vec: Vec<Object>) -> Object {
        let ptr = Object::with_type(allocate(Layout::new::<Self>()), Type::Array);
        let obj = unsafe { ptr.get_mut::<Self>() };
        obj.header.marked = false;
        init!(obj.value => vec );
        ptr
    }

    /// Creates a new Pointer object pointing to a NlArray
    fn from_slice(slice: &[Object]) -> Object {
        Self::from_vec(slice.to_vec())
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.tag() {
            Type::Null => f.write_str("null")?,
            Type::Bool => f.write_str(if self.as_bool() { "true" } else { "false" })?,
            Type::Float =>
            // Safety: We've checked the pointer tag
            unsafe { f.write_str(&self.as_f64().to_string())? },
            Type::Int => f.write_str(&self.as_int().to_string())?,
            Type::String =>
            // Safety: We've checked the pointer tag
            unsafe { f.write_str(self.as_str())? },
            Type::Array => {
                f.write_char('[')?;
                for (i, p) in unsafe { self.as_vec() }.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    p.fmt(f)?;
                }
                f.write_char(']')?;
            }
        }
        Ok(())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Null => f.write_str("null")?,
            Type::Bool => f.write_str("bool")?,
            Type::Float => f.write_str("float")?,
            Type::Int => f.write_str("int")?,
            Type::String => f.write_str("string")?,
            Type::Array => f.write_str("array")?,
        };

        Ok(())
    }
}

fn allocate(layout: Layout) -> *mut u8 {
    unsafe {
        let ptr = alloc(layout);

        if ptr.is_null() {
            handle_alloc_error(layout);
        } else {
            ptr
        }
    }
}

macro_rules! impl_arith {
    ($func_name:ident, $op:tt) => {
        #[inline]
        pub fn $func_name(self, rhs: Self) -> Result<Object, Error> {
            let result = match (self.tag(), rhs.tag()) {
                (Type::Int, Type::Int) => Object::from(self.as_int() $op rhs.as_int()),
                (Type::Float, Type::Float) => Object::from(unsafe{ self.as_f64() $op rhs.as_f64() }),
                _ => return Err(Error::TypeError(format!("Can not {} objects of type {} and {}", stringify!($op), self.tag(), rhs.tag())))
            };
            Ok(result)
        }
    };
}

macro_rules! impl_logical {
    ($func_name:ident, $op:tt) => {
        #[inline]
        pub fn $func_name(self, rhs: Self) -> Result<Object, Error> {
            let result = match (self.tag(), rhs.tag()) {
                (Type::Bool, Type::Bool) => Object::from(self.as_bool() $op rhs.as_bool()),
                _ => return Err(Error::TypeError(format!("Can not {} objects of type {} and {}", stringify!($op), self.tag(), rhs.tag())))
            };
            Ok(result)
        }
    };
}

// TODO: For heap-allocated objects, this currently does a pointer comparison (instead of checking the actual value)
macro_rules! impl_cmp {
    ($func_name:ident, $op:tt) => {
        #[inline]
        pub fn $func_name(self, rhs: Self) -> Result<Object, Error> {
            if self.tag() != rhs.tag() {
                return Err(Error::TypeError(format!("Can not compare objects of type {} and {}", self.tag(), rhs.tag())));
            }

            let result = match self.tag() {
                // Safety: We've matched on the object type
                Type::Float => unsafe { self.as_f64() $op rhs.as_f64() },
                Type::String => unsafe { self.as_str() $op rhs.as_str() },
                _ => self $op rhs,
            };
            Ok(Object::from(result))
        }
    };
}

impl Object {
    impl_arith!(add, +);
    impl_arith!(sub, -);
    impl_arith!(mul, *);
    impl_arith!(div, /);
    impl_arith!(rem, %);

    impl_cmp!(gt, >);
    impl_cmp!(gte, >=);
    impl_cmp!(lt, <);
    impl_cmp!(lte, <=);
    impl_cmp!(eq, ==);
    impl_cmp!(neq, !=);

    impl_logical!(and, &&);
    impl_logical!(or, ||);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_object_null() {
        assert_eq!(Object::null().tag(), Type::Null);
        assert!(Object::null().is_heap_allocated() == false);
    }

    #[test]
    fn test_object_bool() {
        let t = Object::from(true);
        let f = Object::from(false);

        assert_eq!(t.tag(), Type::Bool);
        assert_eq!(f.tag(), Type::Bool);

        assert_eq!(t.as_bool(), true);
        assert_eq!(f.as_bool(), false);

        assert!(t.is_heap_allocated() == false);
        assert!(f.is_heap_allocated() == false);
    }

    #[test]
    fn test_object_int() {
        let obj = Object::from(1);
        assert_eq!(obj.tag(), Type::Int);
        assert_eq!(obj.as_int(), 1);

        let obj = Object::from(-1);
        assert_eq!(obj.tag(), Type::Int);
        assert_eq!(obj.as_int(), -1);

        let obj = Object::from(MAX_INT);
        assert_eq!(obj.tag(), Type::Int);
        assert_eq!(obj.as_int(), MAX_INT);

        let obj = Object::from(MIN_INT);
        assert_eq!(obj.tag(), Type::Int);
        assert_eq!(obj.as_int(), MIN_INT);
    }

    #[test]
    #[should_panic]
    fn test_object_int_overflow() {
        assert_eq!(Object::int(MAX_INT + 1).tag(), Type::Int);
        assert_eq!(Object::int(MAX_INT + 1).as_int(), MAX_INT + 1);

        assert_eq!(Object::int(MIN_INT - 1).tag(), Type::Int);
        assert_eq!(Object::int(MIN_INT - 1).as_int(), MIN_INT - 1);
    }

    #[test]
    fn test_object_string() {
        let ptr = Object::from("Hello, world!");
        assert_eq!(ptr.tag(), Type::String);
        assert!(ptr.is_heap_allocated());
        unsafe {
            assert_eq!(ptr.as_str(), "Hello, world!");
            String::destroy(ptr);
        }
    }

    #[test]
    fn test_pointer_float() {
        let ptr = Object::from(3.1415);
        assert_eq!(ptr.tag(), Type::Float);
        assert!(ptr.is_heap_allocated());

        unsafe {
            assert_eq!(ptr.as_f64(), 3.1415);
            Float::destroy(ptr);
        }
    }

    #[test]
    fn test_pointer_empty_array() {
        let ptr = Object::from(vec![]);
        assert_eq!(ptr.tag(), Type::Array);
        assert!(ptr.is_heap_allocated());
        unsafe {
            assert_eq!(ptr.as_vec().len(), 0);
            Array::destroy(ptr);
        }
    }

    #[test]
    fn test_pointer_array() {
        let ptr = Object::from(vec![Object::null()]);
        assert_eq!(ptr.tag(), Type::Array);

        unsafe {
            assert_eq!(ptr.as_vec().len(), 1);
            assert_eq!(ptr.as_vec().get(0), Some(&Object::null()));
            Array::destroy(ptr);
        }
    }
}
