use crate::{
  anon::Anon,
  embed_error::EmbedError,
  package::Definition,
  position::Pos,
  term::Term,
};

use cid::Cid;

use im::HashMap;

#[derive(Clone, Debug)]
pub struct Def {
  pub pos: Pos,
  pub typ_: Term,
  pub term: Term,
}

impl PartialEq for Def {
  fn eq(&self, other: &Def) -> bool {
    self.typ_ == other.typ_ && self.term == other.term
  }
}

/// A map of names to pairs of links. The first link points to the
/// Definition, the second to the AnonTerm
pub type Refs = HashMap<String, (Cid, Cid)>;

/// A map of content-ids to defs
pub type Defs = HashMap<String, Def>;

impl Def {
  pub fn embed(&self) -> (Definition, Anon, Anon) {
    let (type_anon, type_meta) = self.typ_.embed();
    let (term_anon, term_meta) = self.term.embed();
    let d = Definition {
      pos: self.pos,
      term_anon: term_anon.cid(),
      type_anon: type_anon.cid(),
      term_meta,
      type_meta,
    };
    (d, type_anon, term_anon)
  }

  pub fn unembed(
    def: Definition,
    type_anon: Anon,
    term_anon: Anon,
  ) -> Result<Self, EmbedError> {
    let typ_ = Term::unembed(&type_anon, &def.type_meta)?;
    let term = Term::unembed(&term_anon, &def.term_meta)?;
    Ok(Def { pos: def.pos, typ_, term })
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::term::tests::{
    arbitrary_term,
    test_refs,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  impl Arbitrary for Def {
    fn arbitrary(g: &mut Gen) -> Self {
      Def {
        pos: Pos::None,
        typ_: Arbitrary::arbitrary(g),
        term: arbitrary_term(g, true, test_refs(), im::Vector::new()),
      }
    }
  }

  #[quickcheck]
  fn def_embed_unembed(x: Def) -> bool {
    let (d, ta, xa) = x.clone().embed();
    match Def::unembed(d, ta, xa) {
      Ok(y) => {
        if x == y {
          true
        }
        else {
          //          println!("x: {:?}", x);
          //          println!("y: {:?}", y);
          false
        }
      }
      _e => {
        //        println!("x: {:?}", x);
        //        println!("a: {:?}", a);
        //        println!("m: {:?}", m);
        //        println!("e: {:?}", e);
        false
      }
    }
  }
}
