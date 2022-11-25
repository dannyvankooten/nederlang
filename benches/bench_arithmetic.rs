extern crate nederlang;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nederlang::eval;

pub fn criterion_benchmark(c: &mut Criterion) {
    let input = include_str!("../tests/data/benchmark_1.nl");
    c.bench_function("arithmetic", |b| {
        b.iter(|| assert!(eval(black_box(input)).is_ok()))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(1000);
    targets = criterion_benchmark
}
criterion_main!(benches);
