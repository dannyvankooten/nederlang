extern crate nederlang;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nederlang::vm::run_str;

pub fn criterion_benchmark(c: &mut Criterion) {
    let input = include_str!("../examples/benchmark_1.nl");
    c.bench_function("arithmetic", |b| {
        b.iter(|| assert!(run_str(black_box(input)).is_ok()))
    });
}

criterion_group!{
    name = benches;
    config = Criterion::default().sample_size(1000);
    targets = criterion_benchmark
}
criterion_main!(benches);
