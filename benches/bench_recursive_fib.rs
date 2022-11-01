extern crate nederlang;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nederlang::vm::run_str;

pub fn criterion_benchmark(c: &mut Criterion) {
    let code = "
    stel fib = functie(n) {
        als n < 2 {
            antwoord n
        } 
        
        fib(n - 1) + fib(n - 2)
    }
    
    fib(22)
    ";
    c.bench_function("fib(22) in vm", |b| b.iter(|| run_str(black_box(code))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
