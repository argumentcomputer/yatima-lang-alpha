use hashexpr::{
  position::Pos,
  Bits,
  Expr,
  Expr::{
    Atom,
    Cons,
  },
  Symbol,
};

use crate::decode_error::{
  DecodeError,
  Expected,
};

/// The computationally irrelevant naming metadata of a term in a lambda-like
/// language
#[derive(Debug, Clone, PartialEq)]
pub enum MetaTerm {
  Ctor(Option<Pos>, Vec<MetaTerm>),
  Bind(String, Box<MetaTerm>),
  Link(String),
  Leaf,
}

impl MetaTerm {
  pub fn encode(self) -> Expr {
    match self {
      Self::Ctor(pos, xs) => {
        let mut ys = Vec::new();
        for x in xs {
          ys.push(x.encode());
        }
        Expr::Cons(pos, ys)
      }
      Self::Bind(n, x) => cons!(None, atom!(symb!(n)), x.encode()),
      Self::Link(n) => cons!(None, atom!(symb!(n))),
      Self::Leaf => atom!(bits!(vec![])),
    }
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(name))] => Ok(Self::Link(name.to_owned())),
        [Atom(_, Symbol(name)), bound] => {
          let bound = MetaTerm::decode(bound.to_owned())?;
          Ok(Self::Bind(name.to_owned(), Box::new(bound)))
        }
        [xs @ ..] => {
          let mut ys = Vec::new();
          for x in xs {
            let y = MetaTerm::decode(x.to_owned())?;
            ys.push(y);
          }
          Ok(Self::Ctor(pos, ys))
        }
      },
      Atom(_, Bits(..)) => Ok(Self::Leaf),
      _ => Err(DecodeError::new(expr.position(), vec![Expected::MetaTerm])),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::{
    MetaTerm,
    MetaTerm::*,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;

  use crate::term::tests::arbitrary_name;

  impl Arbitrary for MetaTerm {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let x: u32 = g.gen_range(0, 45);
      match x {
        0 => {
          let n: u32 = g.gen_range(0, 10);
          let mut xs = Vec::new();
          for _ in 0..n {
            xs.push(Arbitrary::arbitrary(g))
          }
          Ctor(None, xs)
        }
        1 => Bind(arbitrary_name(g), Arbitrary::arbitrary(g)),
        2 => Link(arbitrary_name(g)),
        _ => Leaf,
      }
    }
  }

  #[quickcheck]
  fn meta_term_encode_decode(x: MetaTerm) -> bool {
    match MetaTerm::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
