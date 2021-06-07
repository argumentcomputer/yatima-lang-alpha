use crate::ipld_error::IpldError;
use libipld::ipld::Ipld;
use std::fmt;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Uses {
  None,
  Affi,
  Once,
  Many,
}

impl std::ops::Mul for Uses {
  type Output = Self;

  fn mul(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Self::None, _) | (_, Self::None) => Self::None,
      (Self::Many, _) | (_, Self::Many) => Self::Many,
      (Self::Affi, _) => Self::Affi,
      (Self::Once, x) => x,
    }
  }
}

impl std::ops::Add for Uses {
  type Output = Self;

  fn add(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Self::None, y) => y,
      (x, Self::None) => x,
      (..) => Self::Many,
    }
  }
}

impl std::ops::Sub for Uses {
  type Output = Option<Self>;

  fn sub(self, rhs: Self) -> Option<Self> {
    match (self, rhs) {
      (x, Self::None) => Some(x),
      (Self::None, _) => None,
      (Self::Once, Self::Once) => Some(Self::None),
      (Self::Once, _) => None,
      (Self::Affi, Self::Many) => None,
      (Self::Affi, _) => Some(Self::None),
      (Self::Many, _) => Some(Self::Many),
    }
  }
}

// Division-remainder for multiplicities is as follows: if x and y are
// multiplicities, then x/y and x%y are such that x = y*(x/y) + x%y in such a
// way that x/y and x%y are maximal, with x/y taking precedence.
impl std::ops::Div for Uses {
  type Output = Self;

  fn div(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Self::Many, _) => Self::Many,
      (_, Self::Many) => Self::None,
      (_, Self::None) => Self::Many,
      (Self::None, _) => Self::None,
      (x, Self::Once) => x,
      (Self::Affi, Self::Affi) => Self::Once,
      (Self::Once, Self::Affi) => Self::None,
    }
  }
}

impl std::ops::Rem for Uses {
  type Output = Self;

  fn rem(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Self::Many, _) => Self::Many,
      (x, Self::Many) => x,
      (x, Self::None) => x,
      (Self::None, _) => Self::None,
      (_, Self::Once) => Self::None,
      (Self::Affi, Self::Affi) => Self::None,
      (Self::Once, Self::Affi) => Self::Once,
    }
  }
}

impl fmt::Display for Uses {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::None => write!(f, "0"),
      Self::Affi => write!(f, "&"),
      Self::Once => write!(f, "1"),
      Self::Many => write!(f, "ω"),
    }
  }
}

impl Uses {
  /// A preorder relation, comparing two multiplicities. We can't use
  /// PartialOrd, because this relation is neither symmetric nor antisymmetric
  pub fn lte(self, y: Self) -> bool {
    match (self, y) {
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

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::None => Ipld::Integer(0),
      Self::Affi => Ipld::Integer(1),
      Self::Once => Ipld::Integer(2),
      Self::Many => Ipld::Integer(3),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::None),
      Ipld::Integer(1) => Ok(Self::Affi),
      Ipld::Integer(2) => Ok(Self::Once),
      Ipld::Integer(3) => Ok(Self::Many),
      x => Err(IpldError::Uses(x.to_owned())),
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

  use crate::tests::frequency;

  impl Arbitrary for Uses {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Uses>)> = vec![
        (1, Box::new(|_| Uses::None)),
        (1, Box::new(|_| Uses::Affi)),
        (1, Box::new(|_| Uses::Once)),
        (1, Box::new(|_| Uses::Many)),
      ];
      frequency(g, input)
    }
  }

  #[quickcheck]
  fn uses_ipld(x: Uses) -> bool {
    match Uses::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
