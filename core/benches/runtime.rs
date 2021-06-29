#![feature(test)]

extern crate test;
extern crate yatima_core;

use core::sync::atomic::Ordering;
use nom_locate::LocatedSpan;
use test::{
  Bencher,
};
use yatima_core::{
  defs::Defs,
  parse::term::input_cid,
  runtime::UPCOPY_COUNT,
};

fn bench_fact(main: &str) {
  let s = "def fact (x: #Nat): #Nat = #Nat.mul x (fact (#Nat.pre x))";
  let (_, (defs, _)) = yatima_core::parse::package::parse_defs(
    input_cid(s),
    Defs::new(),
  )(LocatedSpan::from(s))
  .unwrap();
  let mut dag = yatima_core::dag::DAG::from_term(
    &yatima_core::parse::term::parse(main, defs.clone()).unwrap().1,
  );
  dag.norm(&defs);
  println!("{} upcopy", UPCOPY_COUNT.load(Ordering::SeqCst));
}

#[bench]
fn fact_two(b: &mut Bencher) {
  b.iter(|| {
    bench_fact("fact 2");
  })
}
