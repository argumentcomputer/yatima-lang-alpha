use hashexpr::{
  Bits,
  Expr,
  Expr::{
    Atom,
    Cons,
  },
  Link,
  Nat,
  Symbol,
};
use std::convert::TryInto;

use crate::decode_error::{
  DecodeError,
  Expected,
};

/// The computationally irrelevant naming metadata of a term in a lambda-like
/// language
#[derive(Debug, Clone, PartialEq)]
pub enum NameMeta {
  Ctor(Vec<NameMeta>),
  Bind(String, Box<NameMeta>),
  Link(String, Link),
  Leaf,
}

impl NameMeta {
  pub fn encode(self) -> Expr {
    match self {
      Self::Ctor(xs) => {
        let mut ys = Vec::new();
        for x in xs {
          ys.push(x.encode());
        }
        Expr::Cons(None, ys)
      }
      Self::Bind(n, x) => cons!(None, atom!(symb!(n)), x.encode()),
      Self::Link(n, link) => cons!(None, atom!(symb!(n)), atom!(link!(link))),
      Self::Leaf => atom!(bits!(vec![])),
    }
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(_, xs) => match xs.as_slice() {
        [Atom(_, Symbol(name)), Atom(_, Link(link))] => {
          Ok(Self::Link(name.to_owned(), *link))
        }
        [Atom(_, Symbol(name)), bound] => {
          let bound = NameMeta::decode(bound.to_owned())?;
          Ok(Self::Bind(name.to_owned(), Box::new(bound)))
        }
        [xs @ ..] => {
          let mut ys = Vec::new();
          for x in xs {
            let y = NameMeta::decode(x.to_owned())?;
            ys.push(y);
          }
          Ok(Self::Ctor(ys))
        }
      },
      Atom(_, Bits(..)) => Ok(Self::Leaf),
      _ => Err(DecodeError::new(expr.position(), vec![Expected::NameMeta])),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::{
    NameMeta,
    NameMeta::*,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;

  use crate::term::tests::arbitrary_name;

  pub fn arbitrary_link<G: Gen>(g: &mut G) -> hashexpr::Link {
    let bytes: [u8; 32] = [
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
      Arbitrary::arbitrary(g),
    ];
    hashexpr::Link::from(bytes)
  }

  impl Arbitrary for NameMeta {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let x: u32 = g.gen_range(0, 45);
      match x {
        0 => {
          let n: u32 = g.gen_range(0, 10);
          let mut xs = Vec::new();
          for _ in 0..n {
            xs.push(Arbitrary::arbitrary(g))
          }
          Ctor(xs)
        }
        1 => Bind(arbitrary_name(g), Arbitrary::arbitrary(g)),
        2 => Link(arbitrary_name(g), arbitrary_link(g)),
        _ => Leaf,
      }
    }
  }

  #[quickcheck]
  fn name_meta_encode_decode(x: NameMeta) -> bool {
    match NameMeta::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
