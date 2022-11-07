extern crate nederlang;

use nederlang::object::{Error, NlObject};
use nederlang::vm::run_str;
use std::ffi::OsStr;
use std::fs;
use std::thread;

#[test]
fn test_examples() {
    let mut threads = Vec::new();

    for entry in fs::read_dir("./examples").unwrap() {
        let path = entry.unwrap().path();
        if path.extension() != Some(OsStr::new("nl")) {
            continue;
        }

        let handle = thread::spawn(move || {
            let program = fs::read_to_string(&path).unwrap();
            assert!(
                run_str(&program).is_ok(),
                "sample program {} returned an error",
                path.display()
            );
        });
        threads.push(handle);
    }

    for t in threads {
        let _ = t.join();
    }
}

#[test]
fn test_int_expression() {
    assert_eq!(run_str("1"), Ok(NlObject::Int(1)));
    assert_eq!(run_str("1; 2"), Ok(NlObject::Int(2)));
}

#[test]
fn test_infix_expression() {
    assert_eq!(run_str("4 + 2"), Ok(NlObject::Int(6)));
    assert_eq!(run_str("4 - 2"), Ok(NlObject::Int(2)));
    assert_eq!(run_str("4 * 2"), Ok(NlObject::Int(8)));
    assert_eq!(run_str("4 / 4"), Ok(NlObject::Int(1)));
    assert_eq!(run_str("4 == 4"), Ok(NlObject::Bool(true)));
    assert_eq!(run_str("4 != 4"), Ok(NlObject::Bool(false)));
    assert_eq!(run_str("4 > 4"), Ok(NlObject::Bool(false)));
    assert_eq!(run_str("4 >= 4"), Ok(NlObject::Bool(true)));
    assert_eq!(run_str("4 < 4"), Ok(NlObject::Bool(false)));
    assert_eq!(run_str("4 <= 4"), Ok(NlObject::Bool(true)));
}

#[test]
fn test_logical_andor() {
    assert_eq!(run_str("ja en ja"), Ok(NlObject::Bool(true)));
    assert_eq!(run_str("ja en nee"), Ok(NlObject::Bool(false)));
    assert_eq!(run_str("nee en nee"), Ok(NlObject::Bool(false)));
    assert_eq!(run_str("nee of nee"), Ok(NlObject::Bool(false)));
    assert_eq!(run_str("nee of ja"), Ok(NlObject::Bool(true)));
    assert_eq!(run_str("1 > 0 of 0 > 1"), Ok(NlObject::Bool(true)));
}

#[test]
fn test_negating_values() {
    assert_eq!(run_str("-1"), Ok(NlObject::Int(-1)));
    assert_eq!(run_str("-1.00"), Ok(NlObject::Float(-1.00)));
}

#[test]
fn test_not_values() {
    assert_eq!(run_str("!ja"), Ok(NlObject::Bool(false)));
    assert_eq!(run_str("!nee"), Ok(NlObject::Bool(true)));
    assert_eq!(run_str("!!nee"), Ok(NlObject::Bool(false)));
}

#[test]
fn test_if_expression() {
    assert_eq!(run_str("als ja { 1 }"), Ok(NlObject::Int(1)));
    assert_eq!(run_str("als nee { 1 }"), Ok(NlObject::Null));
}

#[test]
fn test_if_expression_with_else() {
    assert_eq!(run_str("als ja { 1 } anders { 2 }"), Ok(NlObject::Int(1)));
    assert_eq!(run_str("als nee { 1 } anders { 2 }"), Ok(NlObject::Int(2)));
    assert_eq!(
        run_str("als nee { 1 } anders als nee { 2 } anders { 3 + 3 }"),
        Ok(NlObject::Int(6))
    );
}

#[test]
fn test_if_expression_with_empty_else() {
    assert_eq!(run_str("als ja { 1 } anders {  }"), Ok(NlObject::Int(1)));
    // The following currently panics because nothing is on the stack
    // Compiler should probably delete that (dce)
    //assert_eq!(run_str("als nee { 1 } anders {  }"), Ok(NlObject::Int(1)));
}

#[test]
fn test_function_expression_calls() {
    assert_eq!(run_str("functie() { 1 }()"), Ok(NlObject::Int(1)));
    assert_eq!(
        run_str("functie() { 1 }() + functie() { 2 }()"),
        Ok(NlObject::Int(3))
    );
    assert_eq!(
        run_str("functie() { functie() { 1 }() }()"),
        Ok(NlObject::Int(1))
    );
    assert_eq!(
        run_str("functie() { functie() { 1 }() }() + functie() { 2 }()"),
        Ok(NlObject::Int(3))
    );
}

#[test]
fn test_nested_local_scopes() {
    assert_eq!(
        run_str(
            "1 + functie() { 1 + functie() { 1 }() }() + functie() { functie() { 1 }() + 1 }()"
        ),
        Ok(NlObject::Int(5))
    );
}

#[test]
fn test_variables() {
    for (program, expected_result) in [
        ("stel a = 1; a", NlObject::Int(1)),
        ("stel a = 1; stel b = 2; a", NlObject::Int(1)),
        ("stel a = 1; stel b = 2; b", NlObject::Int(2)),
        (
            "stel a = 1; stel b = 2; stel c = a + b; c",
            NlObject::Int(3),
        ),
    ] {
        let result = run_str(program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_result, "test program: {program}")
    }
}

#[test]
fn test_assignments() {
    for (program, expected_result) in [
        ("stel a = 1; a = 2; a", NlObject::Int(2)),
        ("stel a = 1; a = 2; a = 3; a", NlObject::Int(3)),
    ] {
        let result = run_str(program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_result, "test program: {program}")
    }
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
        ("functie(a) { a + 1 }(1)", NlObject::Int(2)),
        // Shadow declaration in same scope:
        // ("stel a = 1; stel a = 2; a", NlObject::Int(2)),
        // This is valid, because the scopes differ:
        ("stel a = 1; { stel a = 2; } a", NlObject::Int(1)),
        (
            "stel a = 1; { stel b = 2; } stel c = 3; c",
            NlObject::Int(3),
        ),
        (
            "stel a = 1; { stel b = a; { stel c = b; c } }",
            NlObject::Int(1),
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
        Ok(NlObject::Int(1))
    );
    assert_eq!(
        run_str("stel a = 1; functie(a) { antwoord a; }(2)"),
        Ok(NlObject::Int(2))
    );
    assert_eq!(
        run_str("stel a = 1; functie(a, b) { a * 2 + b }(a, 1)"),
        Ok(NlObject::Int(3))
    );
}

#[test]
fn test_named_functions() {
    assert_eq!(
        run_str("functie optellen(a, b) { a + b }; optellen(10, 20)"),
        Ok(NlObject::Int(30))
    );
    assert_eq!(
        run_str("functie optellen(a, b) { a + b }; functie aftrekken(a, b) { a - b } aftrekken(optellen(10, 20), 30)"),
        Ok(NlObject::Int(0))
    );

    assert_eq!(
        run_str("stel opt1 = functie opt2(a, b) { a + b }; opt1(1, 2)"),
        Ok(NlObject::Int(3))
    );
    assert_eq!(
        run_str("stel opt1 = functie opt2(a, b) { a + b }; opt2(1, 2)"),
        Ok(NlObject::Int(3))
    );
}

#[test]
fn test_functions_as_argument() {
    assert_eq!(
        run_str("(functie (a) { a() })(functie() { 100 });"),
        Ok(NlObject::Int(100))
    );
}

#[test]
fn test_function_accessing_global() {
    assert_eq!(
        run_str("stel a = 1; functie() { a }();"),
        Ok(NlObject::Int(1))
    );
    assert_eq!(
        run_str("functie a() { 1 }; functie b() { a() }; b()"),
        Ok(NlObject::Int(1))
    );
}

#[test]
fn test_fib_recursion() {
    assert_eq!(
        run_str(
            "stel fib = functie(n) { als n < 2 { antwoord n; } fib(n - 1 ) + fib(n - 2) }; fib(6);"
        ),
        Ok(NlObject::Int(8))
    );
}

#[test]
fn test_fib_loop() {
    assert_eq!(
        run_str(include_str!("../examples/fib-loop.nl")),
        Ok(NlObject::Int(9227465))
    );
}

#[test]
fn test_break_statement() {
    assert_eq!(
        run_str("stel a = 0; zolang a < 10 { a = a + 1; als a == 5 { stop } } a"),
        Ok(NlObject::Int(5))
    );
}

#[test]
fn test_continue_statement() {
    assert_eq!(run_str("stel i = 0; stel a = 2; zolang i < 10 { i = i + 1; als i >= 5 { volgende; } a = a * 2; } a"), Ok(NlObject::Int(32)));
}
