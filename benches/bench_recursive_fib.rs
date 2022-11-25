extern crate nederlang;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nederlang::eval;

pub fn criterion_benchmark(c: &mut Criterion) {
    let code = "
    stel fib = functie(n) {
        als n < 2 {
            antwoord n
        } 
        
        fib(n - 1) + fib(n - 2)
    }
    
    fib(24)
    ";
    c.bench_function("fib(24)", |b| b.iter(|| eval(black_box(code))));
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(1000);
    targets = criterion_benchmark
}
criterion_main!(benches);
