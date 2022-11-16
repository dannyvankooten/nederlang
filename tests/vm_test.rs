extern crate nederlang;

use nederlang::object::{Error, Object};
use nederlang::vm::run_str;
use std::ffi::OsStr;
use std::fs;
use std::mem::size_of;
use std::thread;

#[test]
fn test_object_size() {
    // Assert our object representation is still 8 bytes or less
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
    assert_eq!(run_str("1"), Ok(Object::int(1)));
    assert_eq!(run_str("1; 2"), Ok(Object::int(2)));
}

#[test]
fn test_infix_expression() {
    assert_eq!(run_str("4 + 2"), Ok(Object::int(6)));
    assert_eq!(run_str("4 - 2"), Ok(Object::int(2)));
    assert_eq!(run_str("4 * 2"), Ok(Object::int(8)));
    assert_eq!(run_str("4 / 4"), Ok(Object::int(1)));
    assert_eq!(run_str("4 == 4"), Ok(Object::bool(true)));
    assert_eq!(run_str("4 != 4"), Ok(Object::bool(false)));
    assert_eq!(run_str("4 > 4"), Ok(Object::bool(false)));
    assert_eq!(run_str("4 >= 4"), Ok(Object::bool(true)));
    assert_eq!(run_str("4 < 4"), Ok(Object::bool(false)));
    assert_eq!(run_str("4 <= 4"), Ok(Object::bool(true)));
}

#[test]
fn test_const_local_cmp() {
    assert_eq!(run_str("stel n = 100; n == 100"), Ok(Object::bool(true)));
    assert_eq!(run_str("stel n = 100; n != 100"), Ok(Object::bool(false)));
    assert_eq!(run_str("stel n = 100; n > 100"), Ok(Object::bool(false)));
    assert_eq!(run_str("stel n = 100; n >= 100"), Ok(Object::bool(true)));
    assert_eq!(run_str("stel n = 100; n < 100"), Ok(Object::bool(false)));
    assert_eq!(run_str("stel n = 100; n <= 100"), Ok(Object::bool(true)));
    assert_eq!(run_str("stel n = 100; n <= 100"), Ok(Object::bool(true)));
    assert_eq!(run_str("stel n = 100; n + 2"), Ok(Object::int(102)));
    assert_eq!(run_str("stel n = 100; n - 2"), Ok(Object::int(98)));
    assert_eq!(run_str("stel n = 100; n / 2"), Ok(Object::int(50)));
    assert_eq!(run_str("stel n = 100; n * 2"), Ok(Object::int(200)));
    assert_eq!(run_str("stel n = 100; n % 2"), Ok(Object::int(0)));
}

#[test]
fn test_logical_andor() {
    assert_eq!(run_str("ja && ja"), Ok(Object::bool(true)));
    assert_eq!(run_str("ja && nee"), Ok(Object::bool(false)));
    assert_eq!(run_str("nee && nee"), Ok(Object::bool(false)));
    assert_eq!(run_str("nee || nee"), Ok(Object::bool(false)));
    assert_eq!(run_str("nee || ja"), Ok(Object::bool(true)));
    assert_eq!(run_str("1 > 0 || 0 > 1"), Ok(Object::bool(true)));
}

#[test]
fn test_negate_int() {
    assert_eq!(run_str("-1"), Ok(Object::int(-1)));
}

#[test]
fn test_not_values() {
    assert_eq!(run_str("!ja"), Ok(Object::bool(false)));
    assert_eq!(run_str("!nee"), Ok(Object::bool(true)));
    assert_eq!(run_str("!!nee"), Ok(Object::bool(false)));
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
    assert_eq!(run_str("als ja { 1 }"), Ok(Object::int(1)));
    assert_eq!(run_str("als nee { 1 }"), Ok(Object::null()));
}

#[test]
fn test_if_expression_with_else() {
    assert_eq!(run_str("als ja { 1 } anders { 2 }"), Ok(Object::int(1)));
    assert_eq!(run_str("als nee { 1 } anders { 2 }"), Ok(Object::int(2)));
    assert_eq!(
        run_str("als nee { 1 } anders als nee { 2 } anders { 3 + 3 }"),
        Ok(Object::int(6))
    );
}

#[test]
fn test_if_expression_with_empy_body() {
    assert_eq!(run_str("als ja { }"), Ok(Object::null()));
    assert_eq!(run_str("als nee { } anders { 1 }"), Ok(Object::int(1)));
}

#[test]
fn test_if_expression_with_empty_else() {
    assert_eq!(run_str("als ja { 1 } anders {  }"), Ok(Object::int(1)));
    assert_eq!(run_str("als nee { 1 } anders {  }"), Ok(Object::null()));
}

#[test]
fn test_function_expression_calls() {
    assert_eq!(run_str("functie() { 1 }()"), Ok(Object::int(1)));
    assert_eq!(
        run_str("functie() { 1 }() + functie() { 2 }()"),
        Ok(Object::int(3))
    );
    assert_eq!(
        run_str("functie() { functie() { 1 }() }()"),
        Ok(Object::int(1))
    );
    assert_eq!(
        run_str("functie() { functie() { 1 }() }() + functie() { 2 }()"),
        Ok(Object::int(3))
    );
}

#[test]
fn test_nested_local_scopes() {
    assert_eq!(
        run_str(
            "1 + functie() { 1 + functie() { 1 }() }() + functie() { functie() { 1 }() + 1 }()"
        ),
        Ok(Object::int(5))
    );
}

#[test]
fn test_variables() {
    for (program, expected_result) in [
        ("stel a = 1; a", Object::int(1)),
        ("stel a = 1; stel b = 2; a", Object::int(1)),
        ("stel a = 1; stel b = 2; b", Object::int(2)),
        ("stel a = 1; stel b = 2; stel c = a + b; c", Object::int(3)),
    ] {
        let result = run_str(program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_result, "test program: {program}")
    }
}

#[test]
fn test_assignments() {
    for (program, expected_result) in [
        ("stel a = 1; a = 2; a", Object::int(2)),
        ("stel a = 1; a = 2; a = 3; a", Object::int(3)),
    ] {
        let result = run_str(program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_result, "test program: {program}")
    }
}

#[test]
fn test_op_assign() {
    assert_eq!(run_str("stel a = 0; a += 5"), Ok(Object::int(5)));
}

#[test]
fn test_referencing_undeclared_vars() {
    for program in [
        "a",
        "{ stel a = 1; } a",
        "{ { stel a = 1; } a }",
        "functie() { stel a = 1; }() a",
    ] {
        // assert error is of type ReferenceError
        let result = matches!(run_str(program).unwrap_err(), Error::ReferenceError(_));
        assert!(result)
    }
}

#[test]

fn test_scoped_variables() {
    for (program, expected_result) in [
        ("functie(a) { a + 1 }(1)", Object::int(2)),
        // Shadow declaration in same scope:
        // ("stel a = 1; stel a = 2; a", Object::int(2)),
        // This is valid, because the scopes differ:
        ("stel a = 1; { stel a = 2; } a", Object::int(1)),
        ("stel a = 1; { stel b = 2; } stel c = 3; c", Object::int(3)),
        (
            "stel a = 1; { stel b = a; { stel c = b; c } }",
            Object::int(1),
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
        Ok(Object::int(1))
    );
    assert_eq!(
        run_str("stel a = 1; functie(a) { antwoord a; }(2)"),
        Ok(Object::int(2))
    );
    assert_eq!(
        run_str("stel a = 1; functie(a, b) { a * 2 + b }(a, 1)"),
        Ok(Object::int(3))
    );
}

#[test]
fn test_named_functions() {
    assert!(10 as u32 as i64 as u32 == 10);
    assert!(run_str("stel a = 100; a();").is_err());
    assert!(run_str("stel a = functie() { 1 }; a();").is_ok());

    assert_eq!(
        run_str("functie optellen(a, b) { a + b }; optellen(10, 20)"),
        Ok(Object::int(30))
    );
    assert_eq!(
        run_str("functie optellen(a, b) { a + b }; functie aftrekken(a, b) { a - b } aftrekken(optellen(10, 20), 30)"),
        Ok(Object::int(0))
    );

    assert_eq!(
        run_str("stel opt1 = functie opt2(a, b) { a + b }; opt1(1, 2)"),
        Ok(Object::int(3))
    );
    assert_eq!(
        run_str("stel opt1 = functie opt2(a, b) { a + b }; opt2(1, 2)"),
        Ok(Object::int(3))
    );
}

#[test]
fn test_functions_as_argument() {
    assert_eq!(
        run_str("(functie (a) { a() })(functie() { 100 });"),
        Ok(Object::int(100))
    );
}

#[test]
fn test_function_accessing_global() {
    assert_eq!(
        run_str("stel a = 1; functie() { a }();"),
        Ok(Object::int(1))
    );
    assert_eq!(
        run_str("functie a() { 1 }; functie b() { a() }; b()"),
        Ok(Object::int(1))
    );
}

#[test]
fn test_fib_recursion() {
    assert_eq!(
        run_str(
            "stel fib = functie(n) { als n < 2 { antwoord n; } fib(n - 1 ) + fib(n - 2) }; fib(6);"
        ),
        Ok(Object::int(8))
    );
}

#[test]
fn test_fib_loop() {
    assert_eq!(
        run_str(include_str!("../examples/fib-loop.nl")),
        Ok(Object::int(9227465))
    );
}

#[test]
fn test_break_statement() {
    assert_eq!(
        run_str("stel a = 0; zolang a < 10 { a = a + 1; als a == 5 { stop } } a"),
        Ok(Object::int(5))
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
        Ok(Object::int(3))
    );
}

#[test]
fn test_continue_statement() {
    assert_eq!(run_str("stel i = 0; stel a = 2; zolang i < 10 { i = i + 1; als i >= 5 { volgende; } a = a * 2; } a"), Ok(Object::int(32)));
}

#[test]
fn test_gc_negate_float() {
    let result = run_str("-1.00");
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_eq!(result.as_f64(), -1.00);
    result.free();
}

#[test]
fn test_gc_float() {
    for (test, expected) in [("3.14 + 3.15", 3.14 + 3.15)] {
        let result = run_str(test);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.as_f64(), expected);
        result.free();
    }
}

#[test]
fn test_gc_str() {
    let result = run_str(r#""Hello, world!""#);
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_eq!(result.as_str(), "Hello, world!");
    result.free();
}

#[test]
fn test_cast_int() {
    for t in ["int(1)", "int(\"1\")", "int(1.00)", "int(ja)"] {
        assert_eq!(run_str(t), Ok(Object::int(1)));
    }

    assert_eq!(run_str("int(nee)"), Ok(Object::int(0)));
}

#[test]
fn test_cast_bool() {
    for t in [
        "bool(1)",
        "bool(\"1\")",
        "bool(1.00)",
        "bool(ja)",
        "bool(\"0\")",
    ] {
        assert_eq!(run_str(t), Ok(Object::bool(true)));
    }

    for t in [
        "bool(0)",
        "bool(0.00)",
        "bool(nee)",
        "bool(-1)",
        "bool(-1.00)",
    ] {
        assert_eq!(run_str(t), Ok(Object::bool(false)));
    }
}

#[test]
fn test_cast_string() {
    for (t, e) in [
        ("string(1)", "1"),
        ("string(\"1\")", "1"),
        ("string(1.00)", "1"),
        ("string(ja)", "true"),
        ("string(nee)", "false"),
        ("string(\"0\")", "0"),
    ] {
        let result = run_str(t);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.as_str(), e);
        result.free();
    }
}

#[test]
fn test_cast_float() {
    for t in ["float(1)", "float(\"1\")", "float(1.00)", "float(ja)"] {
        let result = run_str(t);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.as_f64(), 1.00);
        result.free();
    }
}

#[test]
fn test_array() {
    for (t, e) in [
        ("[]", vec![]),
        ("[1]", vec![Object::int(1)]),
        ("[1, 2]", vec![Object::int(1), Object::int(2)]),
    ] {
        let result = run_str(t);
        assert!(result.is_ok());

        let result = result.unwrap();
        let vec = result.as_vec();
        assert_eq!(vec.len(), e.len());
        for (e, v) in e.iter().zip(vec) {
            assert_eq!(e, v);
        }

        result.free_recursive();
    }
}
