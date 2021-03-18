use crate::decode_error::{
  DecodeError,
  Expected,
};

use hashexpr::{
  atom,
  atom::Atom::*,
  Expr,
};
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Uses {
  None,
  Affi,
  Once,
  Many,
}

impl Uses {
  pub fn add(x: Self, y: Self) -> Self {
    match (x, y) {
      (Self::None, y) => y,
      (x, Self::None) => x,
      (..) => Self::Many,
    }
  }

  pub fn mul(x: Self, y: Self) -> Self {
    match (x, y) {
      (Self::None, _) => Self::None,
      (_, Self::None) => Self::None,
      (Self::Many, _) => Self::Many,
      (_, Self::Many) => Self::Many,
      (Self::Affi, _) => Self::Affi,
      (Self::Once, x) => x,
    }
  }

  pub fn lte(x: Self, y: Self) -> bool {
    match (x, y) {
      (Self::None, Self::Once) => false,
      (Self::None, _) => true,
      (Self::Affi, Self::None) => false,
      (Self::Affi, Self::Once) => false,
      (Self::Affi, _) => true,
      (Self::Once, Self::None) => false,
      (Self::Once, _) => true,
      (Self::Many, Self::Many) => true,
      (Self::Many, _) => false,
    }
  }

  pub fn gth(x: Self, y: Self) -> bool { !Self::lte(x, y) }

  pub fn encode(self) -> Expr {
    match self {
      Self::None => text!("0"),
      Self::Affi => text!("&"),
      Self::Once => text!("1"),
      Self::Many => text!("ω"),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Expr::Atom(_, Text(n)) if *n == String::from("0") => Ok(Self::None),
      Expr::Atom(_, Text(n)) if *n == String::from("&") => Ok(Self::Affi),
      Expr::Atom(_, Text(n)) if *n == String::from("1") => Ok(Self::Once),
      Expr::Atom(_, Text(n)) if *n == String::from("ω") => Ok(Self::Many),
      x => Err(DecodeError::new(x.position(), vec![Expected::Uses])),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;
  use crate::term::tests::{
    frequency
  };

  fn arbitrary_none() -> Box<dyn Fn(&mut Gen) -> Uses> {
    Box::new(move |g: &mut Gen| {
      Uses::None
    })
  }

  impl Arbitrary for Uses {
    fn arbitrary(g: &mut Gen) -> Self {
      frequency(g, vec![
        (1, arbitrary_none()),
        (1, Box::new(|_| Uses::Affi)),
        (1, Box::new(|_| Uses::Once)),
        (1, Box::new(|_| Uses::Many)),
      ])
    }
  }
}
