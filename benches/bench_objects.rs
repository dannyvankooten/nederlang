use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nederlang::object::NlObject;

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

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("objects");
    let obj_a = Object {
        t: Type::Int,
        value: Value { int: 200 },
    };
    let obj_b = Object {
        t: Type::Int,
        value: Value { int: 800 },
    };
    group.bench_function("union/knowntype", |b| {
        b.iter(|| add_ints(black_box(&obj_a), &obj_b))
    });

    group.bench_function("union/unknowntype", |b| {
        b.iter(|| add_objects_unknown_types(black_box(&obj_a), &obj_b))
    });

    let obj_a = NlObject::Int(200);
    let obj_b = NlObject::Int(800);
    group.bench_function("enum", |b| {
        b.iter(|| add_objects(black_box(&obj_a), &obj_b))
    });
    group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(1000);
    targets = criterion_benchmark
}
criterion_main!(benches);
