use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use nederlang::{self, object::NlObject};

#[derive(Copy, Clone)]
pub enum Type {
    Bool,
    Int,
    Float,
    String,
}
pub struct Object<'a> {
    t: Type,
    value: Value<'a>,
}
pub union Value<'a> {
    int: i64,
    float: f64,
    _bool: bool,
    _string: &'a str,
}

fn add_ints<'a>(a: &Object<'_>, b: &Object<'_>) -> Object<'a> {
    Object {
        t: Type::Int,
        value: Value {
            int: unsafe { a.value.int + b.value.int },
        },
    }
}

fn add_floats<'a>(a: &Object<'_>, b: &Object<'_>) -> Object<'a> {
    Object {
        t: Type::Float,
        value: Value {
            float: unsafe { a.value.float + b.value.float },
        },
    }
}

fn add_objects_unknown_types<'a>(a: &Object<'_>, b: &Object<'_>) -> Object<'a> {
    match (a.t, b.t) {
        (Type::Int, Type::Int) => add_ints(a, b),
        (Type::Float, Type::Float) => add_floats(a, b),
        _ => panic!("Not implemented."),
    }
}

fn add_objects(a: &NlObject, b: &NlObject) -> NlObject {
    match (a, b) {
        (NlObject::Int(a), NlObject::Int(b)) => NlObject::Int(a + b),
        (NlObject::Float(a), NlObject::Float(b)) => NlObject::Float(a + b),
        _ => panic!("Not implemented."),
    }
}

fn bench_objects(c: &mut Criterion) {
    let mut group = c.benchmark_group("Objects");

    for i in [20i64].iter() {
        let obj_a = Object {
            t: Type::Int,
            value: Value { int: *i },
        };
        let obj_b = Object {
            t: Type::Int,
            value: Value { int: *i + 1 },
        };
        group.bench_function(BenchmarkId::new("Union (known type)", i), |b| {
            b.iter(|| add_ints(&obj_a, &obj_b))
        });

        group.bench_function(BenchmarkId::new("Union (unknown type)", i), |b| {
            b.iter(|| add_objects_unknown_types(&obj_a, &obj_b))
        });

        let obj_a = NlObject::Int(*i);
        let obj_b = NlObject::Int(*i + 1);
        group.bench_function(BenchmarkId::new("Enum", i), |b| {
            b.iter(|| add_objects(&obj_a, &obj_b))
        });
    }
    group.finish();
}

criterion_group!(benches, bench_objects);
criterion_main!(benches);
