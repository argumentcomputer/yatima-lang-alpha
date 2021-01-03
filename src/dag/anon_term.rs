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

/// The anonymous abstract syntax tree of a term in a lambda-like language
#[derive(Debug, Clone, PartialEq)]
pub enum AnonTerm {
  Ctor(String, Vec<AnonTerm>),
  Bind(Box<AnonTerm>),
  Vari(u64),
  Link(Link),
  Data(Vec<u8>),
}

impl AnonTerm {
  pub fn encode(self) -> Expr {
    match self {
      Self::Ctor(ctor, xs) => {
        let mut ys = Vec::new();
        ys.push(atom!(symb!(ctor)));
        for x in xs {
          ys.push(x.encode());
        }
        Expr::Cons(None, ys)
      }
      Self::Bind(x) => cons!(None, x.encode()),
      Self::Vari(x) => atom!(Nat(x.into(), Some(64))),
      Self::Link(link) => atom!(link!(link)),
      Self::Data(data) => atom!(bits!(data)),
    }
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(ctor)), tail @ ..] => {
          let mut xs = Vec::new();
          for y in tail {
            let x = AnonTerm::decode(y.to_owned())?;
            xs.push(x);
          }
          Ok(Self::Ctor(ctor.to_owned(), xs))
        }
        [bound] => {
          let x = AnonTerm::decode(bound.to_owned())?;
          Ok(Self::Bind(Box::new(x.to_owned())))
        }
        _ => Err(DecodeError::new(pos, vec![Expected::AnonTermCons])),
      },
      Atom(pos, Nat(x, Some(64))) => {
        let idx: u64 = x.try_into().map_err(|_| {
          DecodeError::new(pos, vec![Expected::AnonTermVariU64])
        })?;
        Ok(Self::Vari(idx))
      }
      Atom(_, Link(link)) => Ok(Self::Link(link)),
      Atom(_, Bits(data, None)) => Ok(Self::Data(data)),
      _ => Err(DecodeError::new(expr.position(), vec![Expected::AnonTermAtom])),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::{
    AnonTerm,
    AnonTerm::*,
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

  pub fn arbitrary_anon_term<G: Gen>(g: &mut G, ctx: u64) -> AnonTerm {
    let x: u32 = g.gen_range(0, 6);
    match x {
      0 => {
        let n: u32 = g.gen_range(0, 10);
        let mut xs = Vec::new();
        for _ in 0..n {
          xs.push(arbitrary_anon_term(g, ctx))
        }
        Ctor(arbitrary_name(g), xs)
      }
      1 => Bind(Box::new(arbitrary_anon_term(g, ctx + 1))),
      2 => Vari(g.gen_range(0, ctx + 1)),
      3 => Link(arbitrary_link(g)),
      _ => Data(Arbitrary::arbitrary(g)),
    }
  }

  impl Arbitrary for AnonTerm {
    fn arbitrary<G: Gen>(g: &mut G) -> Self { arbitrary_anon_term(g, 0) }
  }
  #[quickcheck]
  fn anon_term_encode_decode(x: AnonTerm) -> bool {
    match AnonTerm::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
