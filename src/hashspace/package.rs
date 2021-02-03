use hashexpr::{
  atom::Atom::*,
  Expr,
  Expr::*,
};

use crate::term::Link;

use im::HashMap;

#[derive(PartialEq, Clone, Debug)]
pub struct Package {
  pub name: String,
  pub docs: String,
  pub source: Link,
  pub imports: Imports,
  pub index: Index,
}
#[derive(PartialEq, Clone, Debug)]
pub struct Imports {
  pub imports: Vec<(Link, String)>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Entry {
  pub def_link: Link,
  pub ast_link: Link,
  pub type_posi: Link,
  pub term_posi: Link,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Index {
  pub entries: HashMap<String, Entry>,
}
