#![feature(test)]

extern crate test;
extern crate yatima_core;

use core::sync::atomic::Ordering;
use nom_locate::LocatedSpan;
use test::Bencher;
use yatima_core::{
  defs::Defs,
  parse::term::input_cid,
  upcopy::UPCOPY_COUNT,
};

fn bench_fact(main: &str, b: &mut Bencher) {
  let s = "def fact (x: #Nat): #Nat = (case x) (λ _ => #Nat) 1 (λ x' => \
           #Nat.mul x (fact x'))";
  let (_, (defs, _)) = yatima_core::parse::package::parse_defs(
    input_cid(s),
    Defs::new(),
  )(LocatedSpan::from(s))
  .unwrap();
  let dag = yatima_core::dag::DAG::from_term(
    &yatima_core::parse::term::parse(main, defs.clone()).unwrap().1,
  );
  dag.clone().norm(&defs.clone(), true);
  println!("{} upcopies", UPCOPY_COUNT.swap(0, Ordering::SeqCst));
  b.iter(|| {
    dag.clone().norm(&defs.clone(), false);
  });
}

#[bench]
fn fact2(b: &mut Bencher) { bench_fact("fact 2", b); }

#[bench]
fn fact3(b: &mut Bencher) { bench_fact("fact 3", b); }

#[bench]
fn fact4(b: &mut Bencher) { bench_fact("fact 4", b); }

#[bench]
fn fact5(b: &mut Bencher) { bench_fact("fact 5", b); }
