use crate::ipld_error::IpldError;
use libipld::ipld::Ipld;

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

  pub fn gth(self, y: Self) -> bool { !self.lte(y) }

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
