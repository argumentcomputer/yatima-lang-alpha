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

use std::fmt;

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

impl fmt::Display for MetaTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use MetaTerm::*;
    match self {
      Leaf => write!(f, "leaf"),
      Link(n, l) => write!(f, "(link \"{}\" {})", n, l),
      Bind(n, x) => write!(f, "(bind \"{}\" ({}))", n, x),
      Ctor(p, xs) => {
        let mut res = String::new();
        for x in xs {
          res.push_str(" ");
          res.push_str(&format!("{}", x));
        }
        match p {
          Some(p) => write!(f, "(ctor ({}){})", p, res),
          None => write!(f, "(ctor (){})", res),
        }
      }
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::{
    MetaTerm,
    MetaTerm::*,
    Pos,
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

  fn arbitrary_metaterm_ctor() -> Box<dyn Fn(&mut Gen) -> MetaTerm> {
    Box::new(move |g: &mut Gen| {
      let mut rng = rand::thread_rng();
      let n: u32 = rng.gen_range(0..10);
      //let n: u32 = g.gen_range(0, 10);
      let mut xs = Vec::new();
      for _ in 0..n {
        xs.push(Arbitrary::arbitrary(g))
      }
      Ctor(None, xs)
    })
  }
  impl Arbitrary for MetaTerm {
    fn arbitrary(g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let x: u32 = rng.gen_range(0..45);
      //let x: u32 = g.gen_range(0, 45);
      frequency(g, vec![
        (1, arbitrary_metaterm_ctor()),
        (1, Box::new(|g| Bind(arbitrary_name(g), Arbitrary::arbitrary(g)))),
        (1, Box::new(|g| Link(arbitrary_name(g), arbitrary_link(g)))),
        (1, Box::new(|_| Leaf))
        ])
    }
  }

  #[quickcheck]
  fn meta_term_encode_decode(x: MetaTerm) -> bool {
    match MetaTerm::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[test]
  fn test_cases() {
    let f = Ctor(None, vec![Bind(format!("x"), Box::new(Leaf))]);
    assert_eq!(
      String::from(r##"(ctor () (bind "x" (leaf)))"##),
      format!("{}", f)
    );
    let p = Pos {
      from_offset: 0,
      from_line: 1,
      from_column: 1,
      upto_offset: 10,
      upto_line: 2,
      upto_column: 1,
    };
    let f = Ctor(Some(p), vec![Bind(format!("x"), Box::new(Leaf))]);
    assert_eq!(
      String::from(r##"(ctor (1:1-2:1) (bind "x" (leaf)))"##),
      format!("{}", f)
    )
  }
}
