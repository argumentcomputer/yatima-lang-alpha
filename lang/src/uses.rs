use sp_std::fmt;
use yatima_core::name::Name;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Uses {
  None,
  Affi,
  Once,
  Many,
  Meta(Name),
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
