use hashexpr::{
  atom::{
    Atom::*,
    Link,
  },
  position::Pos,
  Expr,
};

use crate::term::Term;

pub struct Def {
  pos: Pos,
  name: String,
  doc: String,
  term: Term,
  typ_: Term,
}

pub type Defs = HashMap<Link, Def>;
