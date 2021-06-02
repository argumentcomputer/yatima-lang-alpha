use crate::{
  anon::Anon,
  embed_error::EmbedError,
  name::Name,
  package::{
    import_alias,
    Entry,
    Import,
  },
  position::Pos,
  term::Term,
};

use cid::Cid;

use std::collections::HashMap;

use std::fmt;

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
#[derive(PartialEq, Clone, Debug)]
pub struct Defs {
  pub defs: HashMap<Cid, Def>,
  pub names: HashMap<Name, Cid>,
}

impl Def {
  pub fn make(pos: Pos, typ_: Term, term: Term) -> (Self, Entry) {
    let (type_anon, type_meta) = typ_.embed();
    let (term_anon, term_meta) = term.embed();
    let ast_cid = term_anon.cid();
    let defn = Entry {
      pos,
      type_anon: type_anon.cid(),
      type_meta,
      term_anon: ast_cid,
      term_meta,
    };
    let def = Def { pos, def_cid: defn.cid(), ast_cid, typ_, term };
    (def, defn)
  }

  pub fn embed(&self) -> (Entry, Anon, Anon) {
    let (type_anon, type_meta) = self.typ_.embed();
    let (term_anon, term_meta) = self.term.embed();
    let d = Entry {
      pos: self.pos,
      type_anon: type_anon.cid(),
      term_anon: self.ast_cid,
      type_meta,
      term_meta,
    };
    (d, type_anon, term_anon)
  }

  pub fn unembed(
    def: Entry,
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

  pub fn pretty(&self, name: String) -> String {
    format!(
      "def {} : {} = {}",
      name,
      self.typ_.pretty(Some(&name)),
      self.term.pretty(Some(&name))
    )
  }
}

impl Defs {
  pub fn new() -> Self { Defs { defs: HashMap::new(), names: HashMap::new() } }

  pub fn names(&self) -> Vec<Name> {
    let mut res = Vec::new();
    for (n, _) in &self.names {
      res.push(n.clone())
    }
    res
  }

  pub fn named_defs(&self) -> Vec<(Name, Def)> {
    let mut res = Vec::new();
    for (n, cid) in &self.names {
      res.push((n.clone(), self.defs.get(cid).unwrap().clone()))
    }
    res
  }

  pub fn insert(&mut self, name: Name, def: Def) -> Option<Def> {
    self.names.insert(name, def.def_cid);
    self.defs.insert(def.def_cid, def)
  }

  pub fn get(&self, name: Name) -> Option<&Def> {
    let def_cid = self.names.get(&name)?;
    self.defs.get(&def_cid)
  }

  pub fn merge(self, other: Defs, import: &Import) -> Self {
    let mut defs = self.defs;
    for (k, v) in other.defs {
      defs.insert(k, v);
    }
    let mut names = self.names;
    for (k, v) in other.names {
      names.insert(import_alias(k, import), v);
    }
    Defs { defs, names }
  }
}

impl Default for Defs {
  fn default() -> Self { Self::new() }
}

impl fmt::Display for Def {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.pretty("#^".to_string()))
  }
}
impl fmt::Display for Defs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (k, v) in &self.names {
      let def = self.defs.get(v).unwrap();
      writeln!(f, "{}:", def.def_cid)?;
      writeln!(
        f,
        "def {} : {} = {}",
        k.clone(),
        def.typ_.pretty(Some(&k.to_string())),
        def.term.pretty(Some(&k.to_string())),
      )?;
      writeln!(f)?;
    }
    Ok(())
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

  pub fn arbitrary_def(g: &mut Gen) -> (Def, Entry) {
    let typ_: Term = Arbitrary::arbitrary(g);
    let term = arbitrary_term(g, true, test_defs(), std::collections::VecDeque::new());
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
