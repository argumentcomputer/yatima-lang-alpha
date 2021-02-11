use crate::{
  decode_error::{
    DecodeError,
    Expected,
  },
  meta_term::MetaTerm,
};

use hashexpr::{
  position::Pos,
  AVal,
  AVal::*,
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
  pub fn new(
    name: String,
    pos: Option<Pos>,
    docs: String,
    type_anon: Link,
    term_anon: Link,
    type_meta: MetaTerm,
    term_meta: MetaTerm,
  ) -> Self {
    Definition { name, pos, docs, type_anon, term_anon, type_meta, term_meta }
  }

  pub fn encode(self) -> Expr {
    cons!(
      self.pos,
      symb!("def"),
      symb!(self.name),
      text!(self.docs),
      link!(self.type_anon),
      link!(self.term_anon),
      self.type_meta.encode(),
      self.term_meta.encode()
    )
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(c)), tail @ ..] if *c == String::from("def") => {
          match tail {
            [Atom(_, Symbol(n)), Atom(_, Text(d)), Atom(_, Link(t)), Atom(_, Link(x)), tm, xm] =>
            {
              let type_meta = MetaTerm::decode(tm.to_owned())?;
              let term_meta = MetaTerm::decode(xm.to_owned())?;
              Ok(Definition {
                name: n.to_owned(),
                pos,
                docs: d.to_owned(),
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
      },
      _ => {
        println!("foo");
        Err(DecodeError::new(expr.position(), vec![Expected::Definition]))
      }
    }
  }
}
