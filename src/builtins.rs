use crate::{eval::Error, object::NlObject};

pub(crate) fn len(arg: NlObject) -> Result<NlObject, Error> {
    let result = match arg {
        NlObject::Array(values) => NlObject::Int(values.len() as i64),
        NlObject::String(value) => NlObject::Int(value.len() as i64),
        _ => NlObject::Null,
    };

    Ok(result)
}

pub(crate) fn print(args: &[NlObject]) -> Result<NlObject, Error> {
    if args.len() != 1 {
        return Err(Error::TypeError(format!(
            "print() accepts only one argument, {} given",
            args.len()
        )));
    }

    println!("{}", args[0]);
    Ok(NlObject::Null)
}
