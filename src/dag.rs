use hashexpr::atom::Link;
use std::collections::HashMap;

pub enum Anon {
  Ctor(String, Vec<Anon>),
  Bind(Box<Anon>),
  Link(Link),
  Data(Vec<u8>),
}

pub enum Meta {
  Ctor(Vec<Meta>),
  Bind(String, Box<Meta>),
  Link(String, Link),
  Leaf,
}

pub struct Def {
  name: String,
  docs: String,
  term_ast: Link,
  type_ast: Link,
  term_meta: Meta,
  type_meta: Meta,
}

pub struct Imports {
  imports: Vec<(Link, String)>,
}

pub struct Index {
  entries: HashMap<String, (Link, Link)>,
}

pub struct Source {
  name: String,
  text: String,
}

pub struct Package {
  name: String,
  docs: String,
  source: Link,
  imports: Imports,
  index: Index,
}
