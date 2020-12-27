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
}
