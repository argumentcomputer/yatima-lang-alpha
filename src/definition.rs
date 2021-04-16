use crate::{
  decode_error::{
    DecodeError,
    Expected,
  },
  meta_term::MetaTerm,
};

use hashexpr::{
  atom,
  atom::Atom::*,
  position::Pos,
  Expr,
  Expr::{
    Atom,
    Cons,
  },
  Link,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Definition {
  pub name: String,
  pub pos: Option<Pos>,
  pub docs: String,
  pub type_anon: Link,
  pub term_anon: Link,
  pub type_meta: MetaTerm,
  pub term_meta: MetaTerm,
}

impl Definition {
  #[must_use]
  pub const fn new(
    name: String,
    pos: Option<Pos>,
    docs: String,
    type_anon: Link,
    term_anon: Link,
    type_meta: MetaTerm,
    term_meta: MetaTerm,
  ) -> Self {
    Self { name, pos, docs, type_anon, term_anon, type_meta, term_meta }
  }

  #[must_use]
  pub fn encode(self) -> Expr {
    cons!(
      self.pos,
      text!("def"),
      text!(self.name),
      text!(self.docs),
      link!(self.type_anon),
      link!(self.term_anon),
      self.type_meta.encode(),
      self.term_meta.encode()
    )
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    if let Cons(pos, xs) = expr {
      match xs.as_slice() {
        [Atom(_, Text(c)), tail @ ..] if *c == "def" => {
          match tail {
            [Atom(_, Text(n)), Atom(_, Text(d)), Atom(_, Link(t)), Atom(_, Link(x)), tm, xm] =>
            {
              let type_meta = MetaTerm::decode(tm.clone())?;
              let term_meta = MetaTerm::decode(xm.clone())?;
              Ok(Self {
                name: n.clone(),
                pos,
                docs: d.clone(),
                type_anon: *t,
                term_anon: *x,
                type_meta,
                term_meta,
              })
            }
            _ => Err(DecodeError::new(pos, vec![Expected::DefinitionContents])),
          }
        }
        _ => Err(DecodeError::new(pos, vec![Expected::Definition])),
    } } else {
        println!("foo");
        Err(DecodeError::new(expr.position(), vec![Expected::Definition]))
    }
} }
