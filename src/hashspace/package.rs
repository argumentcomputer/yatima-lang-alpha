use hashexpr::{
  atom::Atom::*,
  link::Link,
  Expr,
  Expr::*,
};

use std::collections::HashMap;

use crate::{
  decode_error::{
    DecodeError,
    Expected,
  },
  defs::Defs,
  imports::Imports,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Package {
  name: String,
  docs: String,
  source: Link,
  imports: Imports,
  index: Index,
}

pub struct Imports {
  imports: Vec<(Link, String)>,
}
pub struct Index {
  entries: HashMap<String, (Link, Link, Link)>,
}
