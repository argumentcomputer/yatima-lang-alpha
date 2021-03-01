use hashexpr::{
  atom,
  atom::Atom::*,
  position::Pos,
  Expr,
  Expr::{
    Atom,
    Cons,
  },
  Link,
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
  Link(String, Link),
  Leaf,
}

impl MetaTerm {
  pub fn encode(&self) -> Expr {
    match self {
      Self::Ctor(pos, xs) => {
        let mut ys = Vec::new();
        for x in xs {
          ys.push(x.encode());
        }
        Expr::Cons(*pos, ys)
      }
      Self::Bind(n, x) => cons!(None, text!(n.clone()), x.encode()),
      Self::Link(n, l) => {
        cons!(None, text!(n.clone()), link!(*l))
      }
      Self::Leaf => bits!(vec![]),
    }
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Text(name)), Atom(_, Link(l))] => {
          Ok(Self::Link(name.to_owned(), *l))
        }
        [Atom(_, Text(name)), bound] => {
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

  use crate::term::tests::{
    arbitrary_link,
    arbitrary_name,
  };

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
        2 => Link(arbitrary_name(g), arbitrary_link(g)),
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
