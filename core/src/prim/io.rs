use crate::{
  literal::Literal,
  term::Term,
};
use core::fmt;

use alloc::{
  string::String,
  rc::Rc,
};

#[derive(Clone)]
pub struct IoOp {
  pub fun_symbol: fn() -> String,
  pub fun_type_of: fn() -> Term,
  pub fun_arity: fn() -> u64,
  pub fun_apply0: fn() -> Option<Literal>,
  pub fun_apply1: Rc<dyn Fn(Literal) -> Option<Literal>>,
  pub fun_apply2: fn(Literal, Literal) -> Option<Literal>,
}

impl PartialEq for IoOp {
  fn eq(&self, rhs: &Self) -> bool {
    self.fun_symbol == rhs.fun_symbol
  }
}

impl Eq for IoOp {}

impl fmt::Debug for IoOp {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    let f = self.fun_symbol;
    fmt.write_fmt(format_args!("IoOp {}", f()))
  }
}

impl IoOp {
  pub fn symbol(&self) -> String {
    let f = self.fun_symbol;
    f()
  }

  pub fn type_of(&self) -> Term {
    let f = self.fun_type_of;
    f()
  }

  pub fn arity(&self) -> u64 {
    let f = self.fun_arity;
    f()
  }

  pub fn apply0(&self) -> Option<Literal> {
    let f = self.fun_apply0;
    f()
  }

  pub fn apply1(&self, x: &Literal) -> Option<Literal> {
    let f = self.fun_apply1.clone();
    f(x.clone())
  }

  pub fn apply2(&self, x: &Literal, y: &Literal) -> Option<Literal> {
    let f = self.fun_apply2;
    f(x.clone(), y.clone())
  }
}
