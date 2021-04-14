use criterion::{black_box, criterion_group, criterion_main, Criterion}; 
use yatima::parse::term::parse;

pub fn factorial(c: &mut Criterion) {
    let input = String::from("
    let zero: Type = (λ s z => z);
    let succ: Type = (λ n s z => s (n s z));
    let one: Type = (λ s z => s z);
    let mult: Type = (λ n m s => m (n s));
    let pred: Type = (λ n f x => n (λ g h => h (g f))(λ u => x)(λ u => u));
    let minus: Type = (λn m => m pred n);
    let true: Type = (λ t f => t);
    let false: Type = (λ t f => f);
    let not: Type = (λ p => p false true);
    let and: Type = (λ p q => p q p);
    let is_zero: Type = (λn => n (λx => false) true);
    let leq: Type = (λ n m => is_zero (minus n m));
    let Z: Type = (λ f => (λ x => (x x) λ x => f (λ y => x x y)));
    let factorial: Type = (Z (λ f n => ((leq n zero)(succ zero)(λ y => (mult n (f (pred n))) y))));
    factorial one"
);
  //c.bench_with_input(BenchmarkId::new("fact one", &input), &input, |b, s| b.iter(|| parse(&s)));
    c.bench_function("fact one", |b| b.iter(|| parse(black_box(&input))));

}

criterion_group!(benches, factorial);
criterion_main!(benches);
