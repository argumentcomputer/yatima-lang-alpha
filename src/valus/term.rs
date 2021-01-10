use std::fmt;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Term {
  Var(String, u64),
  Lam(String, Box<Term>),
  App(Box<(Term, Term)>),
}

impl Term {}

#[macro_export]
macro_rules! var {
  ($n:literal,$i:literal) => {
    Term::Var(String::from($n), $i)
  };
}

#[macro_export]
macro_rules! lam {
  ($n:literal,$b:expr) => {
    Term::Lam(String::from($n), Box::new($b))
  };
}

#[macro_export]
macro_rules! app {
  ($i:expr, $j:expr) => {
    Term::App(Box::new(($i, $j)));
  };
}
