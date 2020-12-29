use hashexpr::{
  atom::{
    Atom,
    Atom::*,
    Link,
  },
  Expr,
};
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Uses {
  None,
  Affi,
  Once,
  Many,
}

impl Uses {
  pub fn add(x: Uses, y: Uses) -> Uses {
    match (x, y) {
      (Self::None, y) => y,
      (x, Self::None) => x,
      (..) => Self::Many,
    }
  }

  pub fn mul(x: Uses, y: Uses) -> Uses {
    match (x, y) {
      (Self::None, _) => Self::None,
      (_, Self::None) => Self::None,
      (Self::Many, _) => Self::Many,
      (_, Self::Many) => Self::Many,
      (Self::Affi, _) => Self::Affi,
      (Self::Once, x) => x,
    }
  }

  pub fn lte(x: Uses, y: Uses) -> bool {
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

  pub fn gth(x: Uses, y: Uses) -> bool { !Self::lte(x, y) }

  pub fn encode(self) -> Expr {
    match self {
      Self::None => atom!(None, symb!("0")),
      Self::Affi => atom!(None, symb!("&")),
      Self::Once => atom!(None, symb!("1")),
      Self::Many => atom!(None, symb!("w")),
    }
  }

  pub fn decode(x: Expr) -> Option<Self> {
    match x {
      Expr::Atom(_, Symbol(n)) if *n == String::from("0") => Some(Self::None),
      Expr::Atom(_, Symbol(n)) if *n == String::from("&") => Some(Self::Affi),
      Expr::Atom(_, Symbol(n)) if *n == String::from("1") => Some(Self::Once),
      Expr::Atom(_, Symbol(n)) if *n == String::from("w") => Some(Self::Many),
      _ => None,
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
    StdThreadGen,
  };
  use rand::{
    prelude::IteratorRandom,
    Rng,
  };

  impl Arbitrary for Uses {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let x: u32 = g.gen_range(0, 3);
      match x {
        0 => Uses::None,
        1 => Uses::Affi,
        2 => Uses::Once,
        _ => Uses::Many,
      }
    }
  }
}
