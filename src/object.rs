use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};
use std::cmp::Ordering;
use std::fmt::{Display, Write};
use std::ptr::drop_in_place;
use std::string::String as RString;

use crate::gc::GC;

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeError(RString),
    SyntaxError(RString),
    ReferenceError(RString),
    IndexError(RString),
    ArgumentError(RString),
}

/// A macro for initialising a struct field (without dropping the original default value)
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
const PTR_MASK: usize = !TAG_MASK;

/// The amount of bits to shift-left the actual value in value objects (last 3 bits store the type tag)
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

// Object is a wrapper over raw pointers so we can tag them with immediate values (null, bool, int)
#[derive(Copy, Clone, Debug)]
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
        debug_assert_eq!(((value << VALUE_SHIFT_BITS) >> VALUE_SHIFT_BITS), value);

        Self::with_type((value << VALUE_SHIFT_BITS) as _, Type::Int)
    }

    /// Create a new (garbage-collected) String value
    pub fn string(value: &str, gc: &mut GC) -> Self {
        String::from_str(value, gc)
    }

    /// Create a new (garbage-collected) Float value
    pub fn float(value: f64, gc: &mut GC) -> Self {
        Float::from_f64(value, gc)
    }

    /// Create a new (garbage-collected) Array value
    pub fn array(value: &[Object], gc: &mut GC) -> Self {
        Array::from_slice(value, gc)
    }

    /// Returns the type of this object pointer
    pub fn tag(self) -> Type {
        // Safety: self.0 with TAG_MASK applied will always yield a correct Type
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

    /// Returns the f64 value of this object pointer
    /// Panics if object does not point to a Float
    pub fn as_f64(self) -> f64 {
        assert_eq!(self.tag(), Type::Float);
        unsafe { self.as_f64_unchecked() }
    }

    /// Returns the f64 value of this object pointer
    /// The caller should ensure this pointer points to an actual Float type
    pub unsafe fn as_f64_unchecked(self) -> f64 {
        Float::read(&self)
    }

    /// Returns the &str value of this object pointer
    /// Panics if object does not point to a String
    pub fn as_str(&self) -> &str {
        assert_eq!(self.tag(), Type::String);
        unsafe { self.as_str_unchecked() }
    }

    /// Returns the &str value of this object pointer
    /// The caller should ensure this pointer points to an actual String type
    pub unsafe fn as_str_unchecked(&self) -> &str {
        String::read(&self)
    }

    /// Returns a reference to the Vec<Pointer> value this pointer points to
    /// Panics if object does not point to an Array
    pub fn as_vec(&self) -> &Vec<Object> {
        assert_eq!(self.tag(), Type::Array);
        unsafe { self.as_vec_unchecked() }
    }

    /// Returns a reference to the Vec<Pointer> value this pointer points to
    /// The caller should ensure this pointer actually points to an Array
    pub unsafe fn as_vec_unchecked(&self) -> &Vec<Object> {
        Array::read(&self)
    }

    /// Returns a mutable reference to the Vec<Pointer> value this pointer points to
    /// Panics if object does not point to an Array
    pub fn as_vec_mut(&self) -> &mut Vec<Object> {
        assert_eq!(self.tag(), Type::Array);
        unsafe { self.as_vec_unchecked_mut() }
    }

    /// Returns a mutable reference to the Vec<Pointer> value this pointer points to
    /// The caller should ensure this pointer actually points to an Array
    pub unsafe fn as_vec_unchecked_mut(&self) -> &mut Vec<Object> {
        &mut self.get_mut::<Array>().value
    }

    /// Returns the pointer stored in this object
    /// This can return a non-valid address if called on a non-heap allocated object value.
    pub(crate) fn as_ptr(self) -> *mut u8 {
        (self.0 as usize & PTR_MASK) as _
    }

    /// Get a reference to the value this object points to
    /// It is up to the caller to ensure the object is actually heap-allocated and points to a valid memory location.
    unsafe fn get<'a, T>(self) -> &'a T {
        &*(self.as_ptr() as *const T)
    }

    /// Get a mutable reference to the value this object points to
    /// It is up to the caller to ensure the object is actually heap-allocated and points to a valid memory location.
    unsafe fn get_mut<'a, T>(self) -> &'a mut T {
        &mut *(self.as_ptr() as *mut T)
    }

    /// Returns true if this pointer does not contain an immediate value
    /// But points to a heap allocated type (like Float, String or Array)
    pub fn is_heap_allocated(self) -> bool {
        self.0 as usize & TAG_MASK >= Type::Float as usize
    }

    /// Frees the memory address this pointer points to
    pub fn free(self) {
        unsafe {
            match self.tag() {
                Type::Float => Float::destroy(self),
                Type::String => String::destroy(self),
                Type::Array => Array::destroy(self),
                _ => (),
            }
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        if self.tag() != other.tag() {
            return false;
        }

        match self.tag() {
            Type::Null | Type::Bool | Type::Int => self.as_int() == other.as_int(),
            Type::Float => unsafe { self.as_f64_unchecked() == other.as_f64_unchecked() },
            Type::String => unsafe { self.as_str_unchecked() == other.as_str_unchecked() },
            Type::Array => {
                unimplemented!("Can not yet compare objects of type array")
            }
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        debug_assert_eq!(self.tag(), other.tag());

        match self.tag() {
            Type::Null | Type::Bool | Type::Int => self.as_int().partial_cmp(&other.as_int()),
            Type::Float => unsafe { self.as_f64_unchecked().partial_cmp(&other.as_f64()) },
            Type::String => unsafe { self.as_str_unchecked().partial_cmp(other.as_str()) },
            Type::Array => {
                unimplemented!("Can not yet order objects of type array")
            }
        }
    }
}

#[repr(C)]
pub(crate) struct Header {
    pub(crate) marked: bool,
}

impl Header {
    pub(crate) unsafe fn read(obj: &Object) -> &mut Header {
        obj.get_mut::<Self>()
    }
}

#[repr(C)]
struct Float {
    header: Header,
    value: f64,
}

impl Float {
    unsafe fn read(obj: &Object) -> f64 {
        obj.get::<Self>().value
    }

    unsafe fn destroy(obj: Object) {
        drop_in_place(obj.as_ptr() as *mut Self);
        dealloc(obj.as_ptr(), Layout::new::<Self>());
    }

    fn from_f64(value: f64, gc: &mut GC) -> Object {
        let ptr = Object::with_type(allocate(Layout::new::<Self>()), Type::Float);
        let obj = unsafe { ptr.get_mut::<Self>() };
        obj.header.marked = false;
        init!(obj.value => value );

        // Add allocated object to GC tracer
        gc.trace(ptr);

        ptr
    }
}

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

    fn from_string(value: RString, gc: &mut GC) -> Object {
        let ptr = Object::with_type(allocate(Layout::new::<Self>()), Type::String);
        let obj = unsafe { ptr.get_mut::<Self>() };
        obj.header.marked = false;
        init!(obj.value => value);

        // Add allocated object to GC tracer
        gc.trace(ptr);

        ptr
    }

    fn from_str(value: &str, gc: &mut GC) -> Object {
        Self::from_string(value.to_string(), gc)
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

    fn from_vec(vec: Vec<Object>, gc: &mut GC) -> Object {
        let ptr = Object::with_type(allocate(Layout::new::<Self>()), Type::Array);
        let obj = unsafe { ptr.get_mut::<Self>() };
        obj.header.marked = false;
        init!(obj.value => vec);

        // Add allocated object to GC tracer
        gc.trace(ptr);

        ptr
    }

    /// Creates a new Pointer object pointing to Array
    fn from_slice(slice: &[Object], gc: &mut GC) -> Object {
        Self::from_vec(slice.to_vec(), gc)
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.tag() {
            Type::Null => (),
            Type::Bool => f.write_str(if self.as_bool() { "true" } else { "false" })?,
            Type::Float => unsafe { f.write_str(&self.as_f64_unchecked().to_string())? },
            Type::Int => f.write_str(&self.as_int().to_string())?,
            Type::String => unsafe { f.write_str(self.as_str_unchecked())? },
            Type::Array => {
                let values = unsafe { self.as_vec_unchecked() };
                f.write_char('[')?;
                for (i, p) in values.iter().enumerate() {
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

/// Allocate a chunk of memory with the given layout
fn allocate(layout: Layout) -> *mut u8 {
    let ptr = unsafe { alloc(layout) };

    if ptr.is_null() {
        handle_alloc_error(layout);
    } else {
        ptr
    }
}

macro_rules! impl_arith {
    ($func_name:ident, $op:tt) => {
        #[inline]
        pub fn $func_name(self, rhs: Self, gc: &mut GC) -> Result<Object, Error> {

            if self.tag() != rhs.tag() {
                return Err(Error::TypeError(format!("Can not {} objects of different type {} and {}", stringify!($op), self.tag(), rhs.tag())))
            }

            let result = match self.tag() {
                Type::Int => Object::int(self.as_int() $op rhs.as_int()),

                // Safety: We've already asserted the object type
                Type::Float => unsafe {
                    Object::float(self.as_f64_unchecked() $op rhs.as_f64_unchecked(), gc)
                }
                _ => return Err(Error::TypeError(format!("Can not {} on objects of type {}", stringify!($op), self.tag()))),
            };

            Ok(result)
        }
    };
}

macro_rules! impl_logical {
    ($func_name:ident, $op:tt) => {
        #[inline]
        pub fn $func_name(self, rhs: Self, _gc: &mut GC) -> Result<Object, Error> {
            let result = match (self.tag(), rhs.tag()) {
                (Type::Bool, Type::Bool) => Object::bool(self.as_bool() $op rhs.as_bool()),
                _ => return Err(Error::TypeError(format!("Can not {} objects of type {} and {}", stringify!($op), self.tag(), rhs.tag())))
            };
            Ok(result)
        }
    };
}

macro_rules! impl_cmp {
    ($func_name:ident, $op:tt) => {
        #[inline]
        pub fn $func_name(self, rhs: Self, _gc: &mut GC) -> Result<Object, Error> {
            if self.tag() != rhs.tag() {
                return Err(Error::TypeError(format!("Can not compare objects of type {} and {}", self.tag(), rhs.tag())));
            }

            // Delegate actual comparison to PartialOrd/PartialEq implementation
            Ok(Object::bool(self $op rhs,))
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
        let t = Object::bool(true);
        let f = Object::bool(false);

        assert_eq!(t.tag(), Type::Bool);
        assert_eq!(f.tag(), Type::Bool);

        assert_eq!(t.as_bool(), true);
        assert_eq!(f.as_bool(), false);

        assert!(t.is_heap_allocated() == false);
        assert!(f.is_heap_allocated() == false);
    }

    #[test]
    fn test_object_int() {
        let obj = Object::int(1);
        assert_eq!(obj.tag(), Type::Int);
        assert_eq!(obj.as_int(), 1);

        let obj = Object::int(-1);
        assert_eq!(obj.tag(), Type::Int);
        assert_eq!(obj.as_int(), -1);

        let obj = Object::int(MAX_INT);
        assert_eq!(obj.tag(), Type::Int);
        assert_eq!(obj.as_int(), MAX_INT);

        let obj = Object::int(MIN_INT);
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
        let mut gc = GC::new();
        let obj = Object::string("Hello, world!", &mut gc);
        assert_eq!(obj.tag(), Type::String);
        assert!(obj.is_heap_allocated());
        assert_eq!(obj.as_str(), "Hello, world!");
    }

    #[test]
    fn test_pointer_float() {
        let mut gc = GC::new();
        let obj = Object::float(3.1415, &mut gc);
        assert_eq!(obj.tag(), Type::Float);
        assert!(obj.is_heap_allocated());
        assert_eq!(obj.as_f64(), 3.1415);
    }

    #[test]
    fn test_pointer_empty_array() {
        let mut gc = GC::new();
        let ptr = Array::from_slice(&[], &mut gc);
        assert_eq!(ptr.tag(), Type::Array);
        assert!(ptr.is_heap_allocated());
        assert_eq!(ptr.as_vec().len(), 0);
    }

    #[test]
    fn test_pointer_array() {
        let mut gc = GC::new();
        let ptr = Array::from_slice(&[Object::null()], &mut gc);
        assert_eq!(ptr.tag(), Type::Array);
        assert_eq!(ptr.as_vec().len(), 1);
        assert_eq!(ptr.as_vec().get(0), Some(&Object::null()));
    }

    // TODO: Test PartialEq & PartialOrd implementations
}
