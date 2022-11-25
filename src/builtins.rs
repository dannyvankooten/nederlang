use crate::{
    gc::GC,
    object::{Error, FromString, Object, Type},
};

#[repr(u8)]
pub(crate) enum Builtin {
    Print,
    Type,
    Bool,
    Float,
    Int,
    String,
    Length,
}

pub(crate) fn resolve(name: &str) -> Option<Builtin> {
    match name {
        "print" => Some(Builtin::Print),
        "type" => Some(Builtin::Type),
        "int" => Some(Builtin::Int),
        "float" => Some(Builtin::Float),
        "bool" => Some(Builtin::Bool),
        "string" => Some(Builtin::String),
        "lengte" => Some(Builtin::Length),
        _ => None,
    }
}

#[inline]
pub(crate) fn call(builtin: Builtin, args: &[Object], gc: &mut GC) -> Result<Object, Error> {
    match builtin {
        Builtin::Print => call_print(args),
        Builtin::Type => call_type(args, gc),
        Builtin::String => call_string(args, gc),
        Builtin::Bool => call_bool(args),
        Builtin::Float => call_float(args, gc),
        Builtin::Int => call_int(args),
        Builtin::Length => call_length(args),
    }
}

/// Prints all the given arguments using a very simple format scheme
/// Example:
///     print("Hallo {}!", "wereld") => prints "Hallo wereld" to stdout
fn call_print(args: &[Object]) -> Result<Object, Error> {
    if !args.is_empty() {
        let mut args = args.iter();
        let mut format_str = args.next().unwrap().to_string();

        for replacement in args {
            format_str = format_str.replacen("{}", &replacement.to_string(), 1);
        }

        print!("{format_str}");
    }

    println!();
    Ok(Object::null())
}

// Returns the given type of an object as a string object
fn call_type(args: &[Object], gc: &mut GC) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error::ArgumentError(format!(
            "type() verwacht 1 argument, maar kreeg er {}",
            args.len()
        )));
    }

    Ok(Object::string(args[0].tag().to_string(), gc))
}

/// Casts the given object to a string object
fn call_string(args: &[Object], gc: &mut GC) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error::ArgumentError(format!(
            "string() verwacht 1 argument, maar kreeg er {}",
            args.len()
        )));
    }

    let t = match args[0].tag() {
        Type::Null => String::from(""),
        Type::Bool => args[0].as_bool().to_string(),
        Type::Float => unsafe { args[0].as_f64_unchecked().to_string() },
        Type::Int => args[0].as_int().to_string(),
        Type::String => return Ok(args[0]),
        Type::Array | Type::Function => {
            return Err(Error::ArgumentError(format!(
                "kan geen string maken van een {}",
                args[0].tag()
            )))
        }
    };
    Ok(Object::string(t.as_str(), gc))
}

/// Casts the given object to a bool object
fn call_bool(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error::ArgumentError(format!(
            "bool() verwacht 1 argument, maar kreeg er {}",
            args.len()
        )));
    }

    let result = match args[0].tag() {
        Type::Null => false,
        Type::Bool => return Ok(args[0]),
        Type::Float => unsafe { args[0].as_f64_unchecked() > 0.00 },
        Type::Int => args[0].as_int() > 0,
        Type::String => unsafe { !args[0].as_str_unchecked().is_empty() },
        Type::Array => unsafe { !args[0].as_vec_unchecked().is_empty() },
        Type::Function => {
            return Err(Error::ArgumentError(format!(
                "kan geen bool maken van een {}",
                args[0].tag()
            )))
        }
    };
    Ok(Object::bool(result))
}

/// Casts the given object to an object of type int
fn call_int(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error::ArgumentError(format!(
            "int() verwacht 1 argument, maar kreeg er {}",
            args.len()
        )));
    }

    let result = match args[0].tag() {
        Type::Null => 0,
        Type::Bool => {
            if args[0].as_bool() {
                1
            } else {
                0
            }
        }
        Type::Float => unsafe { args[0].as_f64_unchecked() as isize },
        Type::Int => return Ok(args[0]),
        Type::String => unsafe {
            match args[0].as_str_unchecked().trim().parse() {
                Ok(val) => val,
                Err(_) => {
                    return Err(Error::ArgumentError(format!(
                        "kan {:?} niet converteren naar een integer",
                        args[0].as_str_unchecked()
                    )))
                }
            }
        },
        Type::Array | Type::Function => {
            return Err(Error::ArgumentError(format!(
                "kan geen int maken van een {}",
                args[0].tag()
            )))
        }
    };

    Ok(Object::int(result))
}

/// Casts the given object to an object of type float
fn call_float(args: &[Object], gc: &mut GC) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error::ArgumentError(format!(
            "float() verwacht 1 argument, maar kreeg er {}",
            args.len()
        )));
    }

    let result = match args[0].tag() {
        Type::Null => 0.0,
        Type::Bool => {
            if args[0].as_bool() {
                1.0
            } else {
                0.0
            }
        }
        Type::Float => return Ok(args[0]),
        Type::Int => args[0].as_int() as f64,
        Type::String => unsafe {
            match args[0].as_str_unchecked().trim().parse() {
                Ok(val) => val,
                Err(_) => {
                    return Err(Error::ArgumentError(format!(
                        "kan {:?} niet converteren naar een float",
                        args[0].as_str_unchecked()
                    )))
                }
            }
        },
        Type::Array | Type::Function => {
            return Err(Error::ArgumentError(format!(
                "kan geen float maken van een {}",
                args[0].tag()
            )))
        }
    };

    Ok(Object::float(result, gc))
}

fn call_length(args: &[Object]) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(Error::ArgumentError(format!(
            "lengte() verwacht 1 argument, maar kreeg er {}",
            args.len()
        )));
    }

    let length = match args[0].tag() {
        Type::String => args[0].as_str().chars().count(),
        Type::Array => args[0].as_vec().len(),
        _ => {
            return Err(Error::TypeError(format!(
                "kan geen lengte opvragen van object met type {}",
                args[0].tag()
            )))
        }
    };
    Ok(Object::int(length as isize))
}
