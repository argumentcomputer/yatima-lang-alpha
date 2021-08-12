use crate::{
  literal::Literal,
  term::Term,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct IoOp {
  pub fun_symbol: fn() -> String,
  pub fun_type_of: fn() -> Term,
  pub fun_arity: fn() -> u64,
  pub fun_apply0: fn() -> Option<Literal>,
  pub fun_apply1: fn(Literal) -> Option<Literal>,
  pub fun_apply2: fn(Literal, Literal) -> Option<Literal>,
}

impl IoOp {
  pub fn symbol(self) -> String {
    let f = self.fun_symbol;
    f()
  }

  pub fn type_of(self) -> Term {
    let f = self.fun_type_of;
    f()
  }

  pub fn arity(self) -> u64 {
    let f = self.fun_arity;
    f()
  }

  pub fn apply0(self) -> Option<Literal> {
    let f = self.fun_apply0;
    f()
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    let f = self.fun_apply1;
    f(x.clone())
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    let f = self.fun_apply2;
    f(x.clone(), y.clone())
  }
}
