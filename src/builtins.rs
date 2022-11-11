use crate::{
    gc::GC,
    object::{Object, Type},
};

#[repr(u8)]
pub(crate) enum Builtin {
    Print,
    Type,
    Bool,
    Float,
    Int,
    String,
}

pub(crate) fn resolve(name: &str) -> Option<Builtin> {
    match name {
        "print" => Some(Builtin::Print),
        "type" => Some(Builtin::Type),
        "int" => Some(Builtin::Int),
        "float" => Some(Builtin::Float),
        "bool" => Some(Builtin::Bool),
        "string" => Some(Builtin::String),
        _ => None,
    }
}

pub(crate) fn call(builtin: Builtin, args: &[Object], gc: &mut GC) -> Object {
    match builtin {
        Builtin::Print => call_print(args),
        Builtin::Type => call_type(args, gc),
        Builtin::String => call_string(args, gc),
        Builtin::Bool => call_bool(args),
        Builtin::Float => call_float(args, gc),
        Builtin::Int => call_int(args),
    }
}

// Prints all the given arguments
fn call_print(args: &[Object]) -> Object {
    for arg in args {
        print!("{}", arg);
    }
    println!();

    Object::null()
}

// Returns the given type of an object as a string object
fn call_type(args: &[Object], gc: &mut GC) -> Object {
    // TODO: return error here
    assert!(args.len() == 1);

    let t = match args[0].tag() {
        Type::Null => "null",
        Type::Bool => "bool",
        Type::Float => "float",
        Type::Int => "int",
        Type::String => "string",
        Type::Array => "array",
    };
    Object::string(t, gc)
}

/// Casts the given object to a string object
fn call_string(args: &[Object], gc: &mut GC) -> Object {
    // TODO: return error here
    assert!(args.len() == 1);

    let t = match args[0].tag() {
        Type::Null => String::from(""),
        Type::Bool => args[0].as_bool().to_string(),
        Type::Float => unsafe { args[0].as_f64_unchecked().to_string() },
        Type::Int => args[0].as_int().to_string(),
        Type::String => return args[0],
        Type::Array => unimplemented!("Kan array nog niet omzetten naar een string"),
    };
    Object::string(&t, gc)
}

/// Casts the given object to a bool object
fn call_bool(args: &[Object]) -> Object {
    // TODO: return error here
    assert!(args.len() == 1);

    match args[0].tag() {
        Type::Null => Object::bool(false),
        Type::Bool => args[0],
        Type::Float => unsafe { Object::bool(args[0].as_f64_unchecked() > 0.00) },
        Type::Int => Object::bool(args[0].as_int() > 0),
        Type::String => unsafe { Object::bool(args[0].as_str_unchecked().len() > 0) },
        Type::Array => unsafe { Object::bool(args[0].as_vec_unchecked().len() > 0) },
    }
}

/// Casts the given object to an object of type int
fn call_int(args: &[Object]) -> Object {
    // TODO: return error here
    assert!(args.len() == 1);

    match args[0].tag() {
        Type::Null => Object::int(0),
        Type::Bool => {
            if args[0].as_bool() {
                Object::int(1)
            } else {
                Object::int(0)
            }
        }
        Type::Float => unsafe { Object::int(args[0].as_f64_unchecked() as i64) },
        Type::Int => args[0],
        Type::String => unsafe { Object::int(args[0].as_str_unchecked().parse().unwrap()) },
        Type::Array => panic!("Kan geen integer maken van een array."),
    }
}

/// Casts the given object to an object of type float
fn call_float(args: &[Object], gc: &mut GC) -> Object {
    // TODO: return error here
    assert!(args.len() == 1);

    match args[0].tag() {
        Type::Null => Object::float(0.0, gc),
        Type::Bool => {
            if args[0].as_bool() {
                Object::float(1.0, gc)
            } else {
                Object::float(0.0, gc)
            }
        }
        Type::Float => args[0],
        Type::Int => Object::float(args[0].as_int() as f64, gc),
        Type::String => unsafe { Object::float(args[0].as_str_unchecked().parse().unwrap(), gc) },
        Type::Array => panic!("Kan geen float maken van een array."),
    }
}
