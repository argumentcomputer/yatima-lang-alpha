use hashexpr::{
  atom,
  atom::Atom::*,
  Expr,
  Expr::{
    Atom,
    Cons,
  },
  Link,
};
use std::{
  convert::TryInto,
  fmt,
};

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
        ys.push(text!(ctor));
        for x in xs {
          ys.push(x.encode());
        }
        Expr::Cons(None, ys)
      }
      Self::Bind(x) => cons!(None, x.encode()),
      Self::Vari(x) => nat!(x.into()),
      Self::Link(link) => link!(link),
      Self::Data(data) => bits!(data),
    }
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Text(ctor)), tail @ ..] => {
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
      Atom(pos, Nat(x)) => {
        let idx: u64 = x.try_into().map_err(|_| {
          DecodeError::new(pos, vec![Expected::AnonTermVariU64])
        })?;
        Ok(Self::Vari(idx))
      }
      Atom(_, Link(link)) => Ok(Self::Link(link)),
      Atom(_, Bits(data)) => Ok(Self::Data(data)),
      _ => Err(DecodeError::new(expr.position(), vec![Expected::AnonTermAtom])),
    }
  }
}

impl fmt::Display for AnonTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use AnonTerm::*;
    match self {
      Vari(i) => write!(f, "{}", i),
      Bind(x) => write!(f, "(bind ({}))", x),
      Link(l) => write!(f, "{}", l),
      Data(d) => write!(f, "{}", bits!(d.to_owned())),
      Ctor(n, xs) => {
        let mut res = String::new();
        for x in xs {
          res.push_str(" ");
          res.push_str(&format!("{}", x));
        }
        write!(f, "(ctor \"{}\"{})", n, res)
      }
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

  use crate::term::tests::{
    arbitrary_link,
    arbitrary_name,
    frequency
  };

  fn arbitrary_ctor(ctx: u64) -> Box<dyn Fn(&mut Gen) -> AnonTerm> {
    Box::new(move |g: &mut Gen| {
      let mut rng = rand::thread_rng();
      let n: u32 = rng.gen_range(0..10);
      let mut xs = Vec::new();
      for _ in 0..n {
        xs.push(arbitrary_anon_term(g, ctx))
      }
      Ctor(arbitrary_name(g), xs)
    })
  }

  fn arbitrary_vari(ctx: u64) -> Box<dyn Fn(&mut Gen) -> AnonTerm> {
    Box::new(move |g: &mut Gen| {
      let mut rng = rand::thread_rng();
      Vari(rng.gen_range(0..ctx + 1))
    })
  }


  pub fn arbitrary_anon_term(g: &mut Gen, ctx: u64) -> AnonTerm {
    frequency(g, vec![
      //(1, arbitrary_ctor(ctx)),
      (1, arbitrary_vari(ctx)),
      (1, Box::new(|g| {
        let mut rng = rand::thread_rng();
        let n: u32 = rng.gen_range(0..10);
        let mut xs = Vec::new();
        for _ in 0..n {
          xs.push(arbitrary_anon_term(g, ctx))
        }
        Ctor(arbitrary_name(g), xs)
      })),
      (1, Box::new(|g| Bind(Box::new(arbitrary_anon_term(g, ctx + 1))))),
      (1, Box::new(|g| Link(arbitrary_link(g)))),
      (1, Box::new(|g| Data(Arbitrary::arbitrary(g))))
    ])
  }

  impl Arbitrary for AnonTerm {
    fn arbitrary(g: &mut Gen) -> Self { arbitrary_anon_term(g, 0) }
  }
  #[quickcheck]
  fn anon_term_encode_decode(x: AnonTerm) -> bool {
    match AnonTerm::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[test]
  fn test_cases() {
    let f = Ctor(format!("lam"), vec![Bind(Box::new(Vari(0)))]);
    assert_eq!(String::from(r##"(ctor "lam" (bind (0)))"##), format!("{}", f))
  }
}
