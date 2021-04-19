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
  pub def_cid: Cid,
  pub ast_cid: Cid,
  pub typ_: Term,
  pub term: Term,
}

impl PartialEq for Def {
  fn eq(&self, other: &Def) -> bool {
    self.def_cid == other.def_cid
      && self.ast_cid == other.ast_cid
      && self.typ_ == other.typ_
      && self.term == other.term
  }
}

/// A map of content-ids to defs, with content ids for the def
pub type Defs = HashMap<String, Def>;

impl Def {
  pub fn make(pos: Pos, typ_: Term, term: Term) -> (Self, Definition) {
    let (type_anon, type_meta) = typ_.embed();
    let (term_anon, term_meta) = term.embed();
    let ast_cid = term_anon.cid();
    let defn = Definition {
      pos,
      type_anon: type_anon.cid(),
      type_meta,
      term_anon: ast_cid,
      term_meta,
    };
    let def = Def { pos, def_cid: defn.cid(), ast_cid, typ_, term };
    (def, defn)
  }

  pub fn embed(&self) -> (Definition, Anon, Anon) {
    let (type_anon, type_meta) = self.typ_.embed();
    let (term_anon, term_meta) = self.term.embed();
    let d = Definition {
      pos: self.pos,
      type_anon: type_anon.cid(),
      term_anon: self.ast_cid,
      type_meta,
      term_meta,
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
    Ok(Def {
      pos: def.pos,
      def_cid: def.cid(),
      ast_cid: def.term_anon,
      typ_,
      term,
    })
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::term::tests::{
    arbitrary_term,
    test_defs,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  pub fn arbitrary_def(g: &mut Gen) -> (Def, Definition) {
    let typ_: Term = Arbitrary::arbitrary(g);
    let term = arbitrary_term(g, true, test_defs(), im::Vector::new());
    Def::make(Pos::None, typ_, term)
  }

  impl Arbitrary for Def {
    fn arbitrary(g: &mut Gen) -> Self { arbitrary_def(g).0 }
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
