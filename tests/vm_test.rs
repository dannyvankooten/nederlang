extern crate nederlang;

use nederlang::object::{Error, Object, Type};
use nederlang::vm::run_str;
use std::ffi::OsStr;
use std::fs;
use std::mem::size_of;
use std::thread;

#[test]
fn test_object_size() {
    assert!(size_of::<Object>() <= 8);
}

#[test]
fn test_examples() {
    let mut threads = Vec::new();

    for entry in fs::read_dir("./examples").unwrap() {
        let path = entry.unwrap().path();
        if path.extension() != Some(OsStr::new("nl")) || path.starts_with(".") {
            continue;
        }

        let handle = thread::spawn(move || {
            let program = fs::read_to_string(&path).unwrap();
            let result = run_str(&program);
            assert!(
                result.is_ok(),
                "sample program {} returned an error: {:?}",
                path.display(),
                result.unwrap_err()
            );
        });
        threads.push(handle);
    }

    for t in threads {
        if let Err(e) = t.join() {
            panic!("{:?}", e);
        }
    }
}

#[test]
fn test_int_expression() {
    assert_eq!(run_str("1"), Ok(Object::from(1)));
    assert_eq!(run_str("1; 2"), Ok(Object::from(2)));
}

#[test]
fn test_infix_expression() {
    assert_eq!(run_str("4 + 2"), Ok(Object::from(6)));
    assert_eq!(run_str("4 - 2"), Ok(Object::from(2)));
    assert_eq!(run_str("4 * 2"), Ok(Object::from(8)));
    assert_eq!(run_str("4 / 4"), Ok(Object::from(1)));
    assert_eq!(run_str("4 == 4"), Ok(Object::from(true)));
    assert_eq!(run_str("4 != 4"), Ok(Object::from(false)));
    assert_eq!(run_str("4 > 4"), Ok(Object::from(false)));
    assert_eq!(run_str("4 >= 4"), Ok(Object::from(true)));
    assert_eq!(run_str("4 < 4"), Ok(Object::from(false)));
    assert_eq!(run_str("4 <= 4"), Ok(Object::from(true)));
}

#[test]
fn test_logical_andor() {
    assert_eq!(run_str("ja && ja"), Ok(Object::from(true)));
    assert_eq!(run_str("ja && nee"), Ok(Object::from(false)));
    assert_eq!(run_str("nee && nee"), Ok(Object::from(false)));
    assert_eq!(run_str("nee || nee"), Ok(Object::from(false)));
    assert_eq!(run_str("nee || ja"), Ok(Object::from(true)));
    assert_eq!(run_str("1 > 0 || 0 > 1"), Ok(Object::from(true)));
}

#[test]
fn test_negating_values() {
    assert_eq!(run_str("-1"), Ok(Object::from(-1)));

    let result = run_str("-1.00");
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_eq!(result.tag(), Type::Float);
    unsafe {
        assert_eq!(result.as_f64(), -1.00);
        nederlang::vm::GC.run(&[]);
    }
}

#[test]
fn test_not_values() {
    assert_eq!(run_str("!ja"), Ok(Object::from(false)));
    assert_eq!(run_str("!nee"), Ok(Object::from(true)));
    assert_eq!(run_str("!!nee"), Ok(Object::from(false)));
}

#[test]
fn test_empty_block_statement() {
    assert_eq!(run_str("{}"), Ok(Object::null()));
}

#[test]
fn test_empty_while() {
    assert_eq!(run_str("zolang nee {}"), Ok(Object::null()));
}

#[test]
fn test_if_expression() {
    assert_eq!(run_str("als ja { 1 }"), Ok(Object::from(1)));
    assert_eq!(run_str("als nee { 1 }"), Ok(Object::null()));
}

#[test]
fn test_if_expression_with_else() {
    assert_eq!(run_str("als ja { 1 } anders { 2 }"), Ok(Object::from(1)));
    assert_eq!(run_str("als nee { 1 } anders { 2 }"), Ok(Object::from(2)));
    assert_eq!(
        run_str("als nee { 1 } anders als nee { 2 } anders { 3 + 3 }"),
        Ok(Object::from(6))
    );
}

#[test]
fn test_if_expression_with_empy_body() {
    assert_eq!(run_str("als ja { }"), Ok(Object::null()));
    assert_eq!(run_str("als nee { } anders { 1 }"), Ok(Object::from(1)));
}

#[test]
fn test_if_expression_with_empty_else() {
    assert_eq!(run_str("als ja { 1 } anders {  }"), Ok(Object::from(1)));
    assert_eq!(run_str("als nee { 1 } anders {  }"), Ok(Object::null()));
}

#[test]
fn test_function_expression_calls() {
    assert_eq!(run_str("functie() { 1 }()"), Ok(Object::from(1)));
    assert_eq!(
        run_str("functie() { 1 }() + functie() { 2 }()"),
        Ok(Object::from(3))
    );
    assert_eq!(
        run_str("functie() { functie() { 1 }() }()"),
        Ok(Object::from(1))
    );
    assert_eq!(
        run_str("functie() { functie() { 1 }() }() + functie() { 2 }()"),
        Ok(Object::from(3))
    );
}

#[test]
fn test_nested_local_scopes() {
    assert_eq!(
        run_str(
            "1 + functie() { 1 + functie() { 1 }() }() + functie() { functie() { 1 }() + 1 }()"
        ),
        Ok(Object::from(5))
    );
}

#[test]
fn test_variables() {
    for (program, expected_result) in [
        ("stel a = 1; a", Object::from(1)),
        ("stel a = 1; stel b = 2; a", Object::from(1)),
        ("stel a = 1; stel b = 2; b", Object::from(2)),
        ("stel a = 1; stel b = 2; stel c = a + b; c", Object::from(3)),
    ] {
        let result = run_str(program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_result, "test program: {program}")
    }
}

#[test]
fn test_assignments() {
    for (program, expected_result) in [
        ("stel a = 1; a = 2; a", Object::from(2)),
        ("stel a = 1; a = 2; a = 3; a", Object::from(3)),
    ] {
        let result = run_str(program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_result, "test program: {program}")
    }
}

#[test]
fn test_op_assign() {
    assert_eq!(run_str("stel a = 0; a += 5"), Ok(Object::from(5)));
}

#[test]
fn test_referencing_undeclared_vars() {
    for program in [
        "a",
        "{ stel a = 1; } a",
        "{ { stel a = 1; } a }",
        "functie() { stel a = 1; }() a",
    ] {
        assert_eq!(
            run_str(program),
            Err(Error::ReferenceError("a is not defined".to_string()))
        )
    }
}

#[test]

fn test_scoped_variables() {
    for (program, expected_result) in [
        ("functie(a) { a + 1 }(1)", Object::from(2)),
        // Shadow declaration in same scope:
        // ("stel a = 1; stel a = 2; a", Object::from(2)),
        // This is valid, because the scopes differ:
        ("stel a = 1; { stel a = 2; } a", Object::from(1)),
        ("stel a = 1; { stel b = 2; } stel c = 3; c", Object::from(3)),
        (
            "stel a = 1; { stel b = a; { stel c = b; c } }",
            Object::from(1),
        ),
    ] {
        let result = run_str(program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_result, "test program: {program}")
    }
}

#[test]
fn test_function_vars() {
    assert_eq!(
        run_str("stel a = 1; functie() { stel a = 2; } a"),
        Ok(Object::from(1))
    );
    assert_eq!(
        run_str("stel a = 1; functie(a) { antwoord a; }(2)"),
        Ok(Object::from(2))
    );
    assert_eq!(
        run_str("stel a = 1; functie(a, b) { a * 2 + b }(a, 1)"),
        Ok(Object::from(3))
    );
}

#[test]
fn test_named_functions() {
    assert_eq!(
        run_str("functie optellen(a, b) { a + b }; optellen(10, 20)"),
        Ok(Object::from(30))
    );
    assert_eq!(
        run_str("functie optellen(a, b) { a + b }; functie aftrekken(a, b) { a - b } aftrekken(optellen(10, 20), 30)"),
        Ok(Object::from(0))
    );

    assert_eq!(
        run_str("stel opt1 = functie opt2(a, b) { a + b }; opt1(1, 2)"),
        Ok(Object::from(3))
    );
    assert_eq!(
        run_str("stel opt1 = functie opt2(a, b) { a + b }; opt2(1, 2)"),
        Ok(Object::from(3))
    );
}

#[test]
fn test_functions_as_argument() {
    assert_eq!(
        run_str("(functie (a) { a() })(functie() { 100 });"),
        Ok(Object::from(100))
    );
}

#[test]
fn test_function_accessing_global() {
    assert_eq!(
        run_str("stel a = 1; functie() { a }();"),
        Ok(Object::from(1))
    );
    assert_eq!(
        run_str("functie a() { 1 }; functie b() { a() }; b()"),
        Ok(Object::from(1))
    );
}

#[test]
fn test_fib_recursion() {
    assert_eq!(
        run_str(
            "stel fib = functie(n) { als n < 2 { antwoord n; } fib(n - 1 ) + fib(n - 2) }; fib(6);"
        ),
        Ok(Object::from(8))
    );
}

#[test]
fn test_fib_loop() {
    assert_eq!(
        run_str(include_str!("../examples/fib-loop.nl")),
        Ok(Object::from(9227465))
    );
}

#[test]
fn test_break_statement() {
    assert_eq!(
        run_str("stel a = 0; zolang a < 10 { a = a + 1; als a == 5 { stop } } a"),
        Ok(Object::from(5))
    );

    assert_eq!(
        run_str(
            "
        stel x = 0;
        stel y = 0;
        zolang x < 100 {
            x += 1

            als x == 2 {
                volgende;
            }

            als x == 4 {
                volgende;
            }

            als x == 6 {
                stop;
            }

            stel z = 0;
            zolang z < 3 {
                als z == 2 {
                    stop;
                }

                y += z;
                z += 1;               
            }            
        }
        y
        "
        ),
        Ok(Object::from(3))
    );
}

#[test]
fn test_continue_statement() {
    assert_eq!(run_str("stel i = 0; stel a = 2; zolang i < 10 { i = i + 1; als i >= 5 { volgende; } a = a * 2; } a"), Ok(Object::from(32)));
}

#[test]
fn test_garbage_collection() {
    for (test, expected) in [("3.14 + 3.15", 3.14 + 3.15)] {
        let result = run_str(test);
        assert!(result.is_ok());

        unsafe {
            assert_eq!(result.unwrap().as_f64(), expected);
        }
    }

    let result = run_str(r#""Hello, world!""#);
    assert!(result.is_ok());
    unsafe {
        assert_eq!(result.unwrap().as_str(), "Hello, world!");
        nederlang::vm::GC.run(&[]);
    }
}
