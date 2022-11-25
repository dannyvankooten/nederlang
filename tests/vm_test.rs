extern crate nederlang;

use nederlang::compiler::Compiler;
use nederlang::eval;
use nederlang::object::{Error, Object};
use nederlang::parser::parse;
use nederlang::vm::VM;
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
            let result = eval(&program);
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
    assert_eq!(eval("1"), Ok(Object::int(1)));
    assert_eq!(eval("1; 2"), Ok(Object::int(2)));
}

#[test]
fn test_infix_expression() {
    assert_eq!(eval("4 + 2"), Ok(Object::int(6)));
    assert_eq!(eval("4 - 2"), Ok(Object::int(2)));
    assert_eq!(eval("4 * 2"), Ok(Object::int(8)));
    assert_eq!(eval("4 / 4"), Ok(Object::int(1)));
    assert_eq!(eval("4 == 4"), Ok(Object::bool(true)));
    assert_eq!(eval("4 != 4"), Ok(Object::bool(false)));
    assert_eq!(eval("4 > 4"), Ok(Object::bool(false)));
    assert_eq!(eval("4 >= 4"), Ok(Object::bool(true)));
    assert_eq!(eval("4 < 4"), Ok(Object::bool(false)));
    assert_eq!(eval("4 <= 4"), Ok(Object::bool(true)));
}

#[test]
fn test_const_local_cmp() {
    assert_eq!(eval("stel n = 100; n == 100"), Ok(Object::bool(true)));
    assert_eq!(eval("stel n = 100; n != 100"), Ok(Object::bool(false)));
    assert_eq!(eval("stel n = 100; n > 100"), Ok(Object::bool(false)));
    assert_eq!(eval("stel n = 100; n >= 100"), Ok(Object::bool(true)));
    assert_eq!(eval("stel n = 100; n < 100"), Ok(Object::bool(false)));
    assert_eq!(eval("stel n = 100; n <= 100"), Ok(Object::bool(true)));
    assert_eq!(eval("stel n = 100; n <= 100"), Ok(Object::bool(true)));
    assert_eq!(eval("stel n = 100; n + 2"), Ok(Object::int(102)));
    assert_eq!(eval("stel n = 100; n - 2"), Ok(Object::int(98)));
    assert_eq!(eval("stel n = 100; n / 2"), Ok(Object::int(50)));
    assert_eq!(eval("stel n = 100; n * 2"), Ok(Object::int(200)));
    assert_eq!(eval("stel n = 100; n % 2"), Ok(Object::int(0)));
}

#[test]
fn test_logical_andor() {
    assert_eq!(eval("ja && ja"), Ok(Object::bool(true)));
    assert_eq!(eval("ja && nee"), Ok(Object::bool(false)));
    assert_eq!(eval("nee && nee"), Ok(Object::bool(false)));
    assert_eq!(eval("nee || nee"), Ok(Object::bool(false)));
    assert_eq!(eval("nee || ja"), Ok(Object::bool(true)));
    assert_eq!(eval("1 > 0 || 0 > 1"), Ok(Object::bool(true)));
}

#[test]
fn test_negate_int() {
    assert_eq!(eval("-1"), Ok(Object::int(-1)));
}

#[test]
fn test_not_values() {
    assert_eq!(eval("!ja"), Ok(Object::bool(false)));
    assert_eq!(eval("!nee"), Ok(Object::bool(true)));
    assert_eq!(eval("!!nee"), Ok(Object::bool(false)));
}

#[test]
fn test_empty_block_statement() {
    assert_eq!(eval("{}"), Ok(Object::null()));
}

#[test]
fn test_empty_while() {
    assert_eq!(eval("zolang nee {}"), Ok(Object::null()));
}

#[test]
fn test_if_expression() {
    assert_eq!(eval("als ja { 1 }"), Ok(Object::int(1)));
    assert_eq!(eval("als nee { 1 }"), Ok(Object::null()));
}

#[test]
fn test_if_expression_with_else() {
    assert_eq!(eval("als ja { 1 } anders { 2 }"), Ok(Object::int(1)));
    assert_eq!(eval("als nee { 1 } anders { 2 }"), Ok(Object::int(2)));
    assert_eq!(
        eval("als nee { 1 } anders als nee { 2 } anders { 3 + 3 }"),
        Ok(Object::int(6))
    );
}

#[test]
fn test_if_expression_with_empy_body() {
    assert_eq!(eval("als ja { }"), Ok(Object::null()));
    assert_eq!(eval("als nee { } anders { 1 }"), Ok(Object::int(1)));
}

#[test]
fn test_if_expression_with_empty_else() {
    assert_eq!(eval("als ja { 1 } anders {  }"), Ok(Object::int(1)));
    assert_eq!(eval("als nee { 1 } anders {  }"), Ok(Object::null()));
}

#[test]
fn test_function_expression_calls() {
    assert_eq!(eval("functie() { 1 }()"), Ok(Object::int(1)));
    assert_eq!(
        eval("functie() { 1 }() + functie() { 2 }()"),
        Ok(Object::int(3))
    );
    assert_eq!(
        eval("functie() { functie() { 1 }() }()"),
        Ok(Object::int(1))
    );
    assert_eq!(
        eval("functie() { functie() { 1 }() }() + functie() { 2 }()"),
        Ok(Object::int(3))
    );
}

#[test]
fn test_nested_local_scopes() {
    assert_eq!(
        eval("1 + functie() { 1 + functie() { 1 }() }() + functie() { functie() { 1 }() + 1 }()"),
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
        let result = eval(program);
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
        let result = eval(program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_result, "test program: {program}")
    }
}

#[test]
fn test_op_assign() {
    assert_eq!(eval("stel a = 0; a += 5"), Ok(Object::int(5)));
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
        let result = matches!(eval(program).unwrap_err(), Error::ReferenceError(_));
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
        let result = eval(program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_result, "test program: {program}")
    }
}

#[test]
fn test_function_vars() {
    assert_eq!(
        eval("stel a = 1; functie() { stel a = 2; } a"),
        Ok(Object::int(1))
    );
    assert_eq!(
        eval("stel a = 1; functie(a) { antwoord a; }(2)"),
        Ok(Object::int(2))
    );
    assert_eq!(
        eval("stel a = 1; functie(a, b) { a * 2 + b }(a, 1)"),
        Ok(Object::int(3))
    );
}

#[test]
fn test_named_functions() {
    assert!(10 as u32 as i64 as u32 == 10);
    assert!(eval("stel a = 100; a();").is_err());
    assert!(eval("stel a = functie() { 1 }; a();").is_ok());

    assert_eq!(
        eval("functie optellen(a, b) { a + b }; optellen(10, 20)"),
        Ok(Object::int(30))
    );
    assert_eq!(
        eval("functie optellen(a, b) { a + b }; functie aftrekken(a, b) { a - b } aftrekken(optellen(10, 20), 30)"),
        Ok(Object::int(0))
    );

    assert_eq!(
        eval("stel opt1 = functie opt2(a, b) { a + b }; opt1(1, 2)"),
        Ok(Object::int(3))
    );
    assert_eq!(
        eval("stel opt1 = functie opt2(a, b) { a + b }; opt2(1, 2)"),
        Ok(Object::int(3))
    );
}

#[test]
fn test_functions_as_argument() {
    assert_eq!(
        eval("(functie (a) { a() })(functie() { 100 });"),
        Ok(Object::int(100))
    );
}

#[test]
fn test_function_accessing_global() {
    assert_eq!(eval("stel a = 1; functie() { a }();"), Ok(Object::int(1)));
    assert_eq!(
        eval("functie a() { 1 }; functie b() { a() }; b()"),
        Ok(Object::int(1))
    );
}

#[test]
fn test_fib_recursion() {
    assert_eq!(
        eval(
            "stel fib = functie(n) { als n < 2 { antwoord n; } fib(n - 1 ) + fib(n - 2) }; fib(6);"
        ),
        Ok(Object::int(8))
    );
}

#[test]
fn test_fib_loop() {
    assert_eq!(
        eval(include_str!("../examples/fib-loop.nl")),
        Ok(Object::int(9227465))
    );
}

#[test]
fn test_break_statement() {
    assert_eq!(
        eval("stel a = 0; zolang a < 10 { a = a + 1; als a == 5 { stop } } a"),
        Ok(Object::int(5))
    );

    assert_eq!(
        eval(
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
    assert_eq!(eval("stel i = 0; stel a = 2; zolang i < 10 { i = i + 1; als i >= 5 { volgende; } a = a * 2; } a"), Ok(Object::int(32)));
}

#[test]
fn test_gc_negate_float() {
    let result = eval("-1.00");
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_eq!(result.as_f64(), -1.00);
    result.free();
}

#[test]
fn test_gc_float() {
    for (test, expected) in [("3.14 + 3.15", 3.14 + 3.15)] {
        let result = eval(test);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.as_f64(), expected);
        result.free();
    }
}

#[test]
fn test_gc_str() {
    let result = eval(r#""Hello, world!""#);
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_eq!(result.as_str(), "Hello, world!");
    result.free();
}

#[test]
fn test_cast_int() {
    for t in ["int(1)", "int(\"1\")", "int(1.00)", "int(ja)"] {
        assert_eq!(eval(t), Ok(Object::int(1)));
    }

    assert_eq!(eval("int(nee)"), Ok(Object::int(0)));
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
        assert_eq!(eval(t), Ok(Object::bool(true)));
    }

    for t in [
        "bool(0)",
        "bool(0.00)",
        "bool(nee)",
        "bool(-1)",
        "bool(-1.00)",
    ] {
        assert_eq!(eval(t), Ok(Object::bool(false)));
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
        let result = eval(t);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.as_str(), e);
        result.free();
    }
}

#[test]
fn test_cast_float() {
    for t in ["float(1)", "float(\"1\")", "float(1.00)", "float(ja)"] {
        let result = eval(t);
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
        let result = eval(t);
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

fn assert_string_eq(program: &str, expected_str: &str) {
    let result = eval(program);
    assert!(result.is_ok());

    let result = result.unwrap();
    assert_eq!(result.as_str(), expected_str);
    result.free();
}

#[test]
fn test_string_with_quotes() {
    assert_string_eq(r#""Ik heet \"Danny\"""#, r#"Ik heet "Danny""#);
}

#[test]
fn test_string_with_escape_chars() {
    assert_string_eq(r#""5 \\ 5""#, r#"5 \ 5"#);
}

#[test]
fn test_string_with_newline() {
    assert_string_eq(r#""\n""#, "\n");
}

#[test]
fn test_array_indexing() {
    for (t, e) in [
        ("[1][0]", Object::int(1)),
        ("[1][-1]", Object::int(1)),
        ("[1, 2, 3][1+1-1]", Object::int(2)),
        ("[1, 2, 3][2]", Object::int(3)),
        ("[1, 2, 3][-1]", Object::int(3)),
        ("[1, 2, 3][-2]", Object::int(2)),
    ] {
        let result = eval(t);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result, e);
    }
}

#[test]
fn test_array_indexing_out_of_bounds() {
    for t in ["[][1]", "[][-1]", "[1, 2, 3][3]"] {
        let result = eval(t);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), Error::IndexError(_)));
    }
}

#[test]
fn test_array_indexing_invalid_index() {
    for t in ["[1][1.0]", "[1][ja]", "[1][\"foobar\"]"] {
        let result = eval(t);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), Error::TypeError(_)));
    }
}

#[test]
fn test_indexing_on_non_array() {
    for t in ["1[0]", "ja[0]"] {
        let result = eval(t);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), Error::TypeError(_)));
    }
}

#[test]
fn test_array_index_assignment() {
    for (t, e) in [
        ("[1][0] = 2", Object::int(2)),
        ("stel a = [1]; a[0] = 2; a[0]", Object::int(2)),
    ] {
        let result = eval(t);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result, e);
    }
}

#[test]
fn test_array_index_assignment_out_of_bounds() {
    for t in ["[][1] = 1", "[][-1] = 1", "[1, 2, 3][3] = 1"] {
        let result = eval(t);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), Error::IndexError(_)));
    }
}

#[test]
fn test_array_index_assignment_invalid_index() {
    for t in ["[1][1.0] = 1", "[1][ja] = 1", "[1][\"foobar\"] = 1"] {
        let result = eval(t);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), Error::TypeError(_)));
    }
}

#[test]
fn test_string_indexing() {
    for (t, e) in [
        (r#""foobar"[0]"#, "f"),
        (r#""foobar"[1]"#, "o"),
        (r#""foobar"[-1]"#, "r"),
    ] {
        let result = eval(t);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.as_str(), e);
        result.free();
    }
}

#[test]
fn test_string_index_assignment() {
    for (t, e) in [
        (r#"stel a = "foobar"; a[0] = "d"; a"#, "doobar"),
        (r#"stel a = "foobar"; a[1] = "d"; a"#, "fdobar"),
        (r#"stel a = "foobar"; a[-1] = "d"; a"#, "foobad"),
    ] {
        let result = eval(t);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.as_str(), e);
        result.free();
    }
}

#[test]
fn test_string_index_out_of_bounds() {
    for t in [(r#""foo"[4]"#), (r#""foo"[-4]"#)] {
        let result = eval(t);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), Error::IndexError(_)));
    }
}

#[test]
fn test_string_index_assignment_invalid_index() {
    for t in [
        r#""foobar"[1.0] = 1"#,
        r#""foobar"[ja] = 1"#,
        r#""foobar"["foobar"] = 1"#,
    ] {
        let result = eval(t);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, Error::TypeError(_)), "got {:?}", err);
    }
}

#[test]
fn test_string_index_assignment_invalid_value() {
    for t in [
        r#""foobar"[0] = 1"#,
        r#""foobar"[0] = ja"#,
        r#""foobar"[0] = 1.0"#,
        r#""foobar"[0] = [1]"#,
    ] {
        let result = eval(t);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, Error::TypeError(_)), "got {:?}", err);
    }
}

#[test]
fn test_retained_state() {
    let mut compiler = Compiler::new();
    let mut vm = VM::new();

    let ast = parse("stel a = 100").unwrap();
    let code = compiler.compile_ast(&ast).unwrap();
    let result = vm.run(code);
    assert!(result.is_ok());

    let ast = parse("a").unwrap();
    let code = compiler.compile_ast(&ast).unwrap();
    let result = vm.run(code);
    assert_eq!(result, Ok(Object::int(100)));
}

// TODO: This test currently fails because we are compiling functions into the instructions array
// And while the function object survives, the IP will point to a non-existing index into a different instructions array
// So if we want functions to survive in between REPL evaluations, we need to re-think this approach
#[test]
#[ignore]
fn test_retained_functions() {
    let mut compiler = Compiler::new();
    let mut vm = VM::new();

    let ast = parse("functie a() { 100 }").unwrap();
    let code = compiler.compile_ast(&ast).unwrap();
    let result = vm.run(code);
    assert!(result.is_ok());

    let ast = parse("a()").unwrap();
    let code = compiler.compile_ast(&ast).unwrap();
    let result = vm.run(code);
    assert_eq!(result, Ok(Object::int(100)));
}
