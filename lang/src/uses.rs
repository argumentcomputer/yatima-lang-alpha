use sp_std::fmt;
use yatima_core::{
  name::Name,
  uses::Uses as Core,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Uses {
  None,
  Affi,
  Once,
  Many,
  Meta(Name),
}

impl Uses {
  pub fn from_core(core: Core) -> Self {
    match core {
      Core::None => Self::None,
      Core::Affi => Self::Affi,
      Core::Once => Self::Once,
      Core::Many => Self::Many,
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
      Self::Meta(name) => write!(f, "?{}", name),
    }
  }
}
