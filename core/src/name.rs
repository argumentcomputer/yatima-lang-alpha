use sp_std::{
  borrow::Borrow,
  fmt,
  ops::Deref,
  rc::Rc,
};

use alloc::string::{
  String,
  ToString,
};

/// The name for packages, defs, and expressions
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Name {
  inner: Rc<str>,
}
impl fmt::Debug for Name {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "\"{}\"", self.inner)
  }
}

impl AsRef<str> for Name {
  fn as_ref(&self) -> &str { self.inner.as_ref() }
}

impl Borrow<str> for Name {
  fn borrow(&self) -> &str { self.inner.borrow() }
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.inner.to_string())
  }
}

impl Deref for Name {
  type Target = str;

  fn deref(&self) -> &str { self.inner.deref() }
}

impl<'a> From<&'a str> for Name {
  fn from(v: &str) -> Name { Self { inner: Rc::from(v) } }
}

impl From<String> for Name {
  fn from(v: String) -> Name { Self { inner: Rc::from(v) } }
}
