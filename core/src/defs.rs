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

use sp_cid::Cid;

use alloc::{
  collections::btree_map::BTreeMap,
  rc::Rc,
  vec::Vec,
  string::{
    String,
    ToString,
  },
};

use core::fmt;

/// A type-annotated definition with content ids for the def and its
/// data, represented as an AST
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

/// A map of content ids to defs, with content ids for the def
#[derive(PartialEq, Clone, Debug)]
pub struct Defs {
  pub defs: BTreeMap<Cid, Def>,
  pub names: BTreeMap<Name, Cid>,
}

impl Def {
  /// Creates a def and a corresponding package entry
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

  /// Embeds the def's data and type into a package entry
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

  /// Retrieves a def from a package and anonymous data
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

  /// Formats the def for pretty-printing
  pub fn pretty(&self, name: String, ind: bool) -> String {
    format!(
      "def {} : {} = {}",
      name,
      self.typ_.pretty(Some(&name), ind),
      self.term.pretty(Some(&name), ind)
    )
  }
}

impl Defs {
  /// Creates a new map of content ids to defs and names to content ids
  pub fn new() -> Self {
    Defs { defs: BTreeMap::new(), names: BTreeMap::new() }
  }

  /// Gets a list of the name keys in sorted order
  pub fn names(&self) -> Vec<Name> {
    self.names.keys().cloned().collect()
  }

  /// Gets a list of the named defs
  pub fn named_defs(&self) -> Vec<(Name, Def)> {
    let mut res = Vec::new();
    for (n, cid) in &self.names {
      res.push((n.clone(), self.defs.get(cid).unwrap().clone()))
    }
    res
  }

  /// Adds a def and its name to the defs
  pub fn insert(&mut self, name: Name, def: Def) -> Option<Def> {
    self.names.insert(name, def.def_cid);
    self.defs.insert(def.def_cid, def)
  }

  /// Gets a def from the defs
  pub fn get(&self, name: &Name) -> Option<&Def> {
    let def_cid = self.names.get(name)?;
    self.defs.get(def_cid)
  }

  /// Merges Defs from an Import
  pub fn merge(self, other: Defs, import: &Import) -> Self {
    let mut defs = self.defs;
    for (k, v) in other.defs {
      defs.insert(k, v);
    }
    let mut names = self.names;
    for k in import.with.iter() {
      let k = k.clone();
      let v = other.names.get(&k).unwrap();
      names.insert(import_alias(k, import), *v);
    }
    Defs { defs, names }
  }

  /// Merges Defs mutably at the same level like in a REPL env
  pub fn flat_merge_mut(&mut self, other: Rc<Defs>) {
    for (k, v) in other.defs.iter() {
      self.defs.insert(*k, v.clone());
    }
    for (k, v) in other.names.iter() {
      self.names.insert(k.clone(), *v);
    }
  }

  /// Merges Defs at the same level like in a REPL env
  pub fn flat_merge(self, other: Defs) -> Self {
    let mut defs = self.defs;
    for (k, v) in other.defs {
      defs.insert(k, v);
    }
    let mut names = self.names;
    for (k, v) in other.names.iter() {
      names.insert(k.clone(), *v);
    }
    Defs { defs, names }
  }
}

impl Default for Defs {
  fn default() -> Self { Self::new() }
}

impl fmt::Display for Def {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.pretty("#^".to_string(), false))
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
        def.typ_.pretty(Some(&k.to_string()), false),
        def.term.pretty(Some(&k.to_string()), false),
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
  use sp_im::Vector;

  pub fn arbitrary_def(g: &mut Gen) -> (Def, Entry) {
    let typ_: Term = Arbitrary::arbitrary(g);
    let term = arbitrary_term(g, true, test_defs(), Vector::new());
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
