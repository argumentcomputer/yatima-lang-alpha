use core::fmt;

use crate::{
  ipld_error::IpldError,
  meta::Meta,
  name::Name,
  position::Pos,
};

use sp_cid::Cid;
use sp_ipld::{
  dag_cbor::cid,
  Ipld,
};

use sp_std::{
  borrow::ToOwned,
  vec::Vec,
};

use alloc::string::{
  String,
  ToString,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Package {
  pub pos: Pos,
  pub name: Name,
  pub imports: Vec<Import>,
  pub index: Index,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Import {
  pub cid: Cid,
  pub name: Name,
  pub alias: Name,
  pub with: Vec<Name>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Index(pub Vec<(Name, Cid)>);

#[derive(PartialEq, Clone, Debug)]
pub struct Entry {
  pub pos: Pos,
  pub type_anon: Cid,
  pub term_anon: Cid,
  pub type_meta: Meta,
  pub term_meta: Meta,
}

impl Entry {
  pub fn to_ipld(&self) -> Ipld {
    Ipld::List(vec![
      self.pos.to_ipld(),
      Ipld::Link(self.type_anon),
      Ipld::Link(self.term_anon),
      self.type_meta.to_ipld(),
      self.term_meta.to_ipld(),
    ])
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        #[rustfmt::skip]
        [ pos,
          Ipld::Link(type_anon),
          Ipld::Link(term_anon),
          type_meta,
          term_meta,
        ] => {
          let pos = Pos::from_ipld(pos)?;
          let type_meta = Meta::from_ipld(type_meta)?;
          let term_meta = Meta::from_ipld(term_meta)?;
          Ok(Entry {
            pos,
            type_anon: *type_anon,
            term_anon: *term_anon,
            type_meta,
            term_meta
            })
        }
        xs => Err(IpldError::Entry(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Entry(xs.to_owned())),
    }
  }

  pub fn cid(&self) -> Cid { cid(&self.to_ipld()) }
}

impl fmt::Display for Entry {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "Entry")?;
    writeln!(f, "  Type ({}):", self.type_anon)?;
    writeln!(f, "  {}", self.type_meta)?;
    writeln!(f, "  Term ({}):", self.term_anon)?;
    writeln!(f, "  {}", self.term_meta)?;
    Ok(())
  }
}

impl Index {
  pub fn to_ipld(&self) -> Ipld {
    Ipld::List(
      self
        .0
        .iter()
        .map(|(k, v)| {
          Ipld::List(vec![Ipld::String(k.to_string()), Ipld::Link(*v)])
        })
        .collect(),
    )
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => {
        let mut res: Vec<(Name, Cid)> = Vec::new();
        for x in xs {
          match x {
            Ipld::List(xs) => match xs.as_slice() {
              [Ipld::String(n), Ipld::Link(cid)] => {
                res.push((Name::from(n.clone()), *cid));
              }
              xs => {
                return Err(IpldError::IndexEntry(Ipld::List(xs.to_owned())));
              }
            },
            x => {
              return Err(IpldError::IndexEntry(x.to_owned()));
            }
          }
        }
        Ok(Index(res))
      }
      xs => Err(IpldError::Index(xs.to_owned())),
    }
  }

  pub fn keys(&self) -> Vec<Name> {
    let mut res = Vec::new();
    for (n, _) in &self.0 {
      res.push(n.clone())
    }
    res
  }
}

impl fmt::Display for Index {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "Exporting")?;
    for (n, cid) in self.0.clone() {
      writeln!(f, " {} ({})", n, cid)?;
    }
    Ok(())
  }
}

impl Import {
  pub fn to_ipld(&self) -> Ipld {
    Ipld::List(vec![
      Ipld::Link(self.cid),
      Ipld::String(self.name.to_string()),
      Ipld::String(self.alias.to_string()),
      Ipld::List(
        self.with.iter().map(|x| Ipld::String(x.to_string())).collect(),
      ),
    ])
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Link(cid), Ipld::String(name), Ipld::String(alias), Ipld::List(with)] =>
        {
          let mut res: Vec<String> = Vec::new();
          for w in with {
            match w {
              Ipld::String(w) => {
                res.push(w.clone());
              }
              w => return Err(IpldError::ImportEntry(w.to_owned())),
            }
          }
          Ok(Self {
            cid: *cid,
            name: Name::from(name.clone()),
            alias: Name::from(alias.clone()),
            with: res.iter().cloned().map(Name::from).collect(),
          })
        }
        xs => Err(IpldError::Import(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Import(xs.to_owned())),
    }
  }
}

impl fmt::Display for Import {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.alias.is_empty() {
      writeln!(f, "Importing from {} ({})", self.name, self.cid)?;
    }
    else {
      writeln!(
        f,
        "Importing from {} as {} ({})",
        self.name, self.alias, self.cid
      )?;
    }
    for with_ident in self.with.clone() {
      writeln!(f, "  {}", with_ident)?;
    }
    Ok(())
  }
}

pub fn import_alias(name: Name, import: &Import) -> Name {
  if import.with.iter().any(|x| *x == name) {
    if import.alias.is_empty() {
      name
    }
    else {
      Name::from(format!("{}.{}", import.alias, name))
    }
  }
  else {
    Name::from(format!("{}.{}", import.name, name))
  }
}

impl Package {
  pub fn to_ipld(&self) -> Ipld {
    Ipld::List(vec![
      self.pos.to_ipld(),
      Ipld::String(self.name.to_string()),
      Ipld::List(self.imports.iter().map(Import::to_ipld).collect()),
      self.index.to_ipld(),
    ])
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [pos, Ipld::String(name), Ipld::List(is), index] => {
          let pos: Pos = Pos::from_ipld(pos)?;
          let mut imports: Vec<Import> = Vec::new();
          for i in is {
            let i = Import::from_ipld(i)?;
            imports.push(i);
          }
          let index = Index::from_ipld(index)?;
          Ok(Package { pos, name: Name::from(name.clone()), imports, index })
        }
        xs => Err(IpldError::Package(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Package(xs.to_owned())),
    }
  }

  pub fn cid(&self) -> Cid { cid(&self.to_ipld()) }
}

impl fmt::Display for Package {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "Package \"{}\"", self.name)?;
    for i in self.imports.clone() {
      writeln!(f, "{}", i)?;
    }
    writeln!(f, "{}", self.index)?;
    Ok(())
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use crate::{
    defs::tests::arbitrary_def,
    term::tests::arbitrary_name,
    tests::arbitrary_cid,
  };

  impl Arbitrary for Entry {
    fn arbitrary(g: &mut Gen) -> Self { arbitrary_def(g).1 }
  }

  impl Arbitrary for Index {
    fn arbitrary(g: &mut Gen) -> Self {
      let vec: Vec<()> = Arbitrary::arbitrary(g);
      Index(
        vec
          .into_iter()
          .map(|_| (arbitrary_name(g), arbitrary_cid(g)))
          .collect(),
      )
    }
  }

  impl Arbitrary for Import {
    fn arbitrary(g: &mut Gen) -> Self {
      let vec: Vec<()> = Arbitrary::arbitrary(g);
      let vec: Vec<Name> = vec.into_iter().map(|_| arbitrary_name(g)).collect();
      Self {
        name: Name::from("Test"),
        cid: arbitrary_cid(g),
        alias: arbitrary_name(g),
        with: vec,
      }
    }
  }

  impl Arbitrary for Package {
    fn arbitrary(g: &mut Gen) -> Self {
      Package {
        pos: Pos::None,
        name: arbitrary_name(g),
        imports: Arbitrary::arbitrary(g),
        index: Arbitrary::arbitrary(g),
      }
    }
  }

  #[quickcheck]
  fn entry_ipld(x: Entry) -> bool {
    match Entry::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
  #[quickcheck]
  fn index_ipld(x: Index) -> bool {
    match Index::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
  #[quickcheck]
  fn import_ipld(x: Import) -> bool {
    match Import::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
  #[quickcheck]
  fn package_ipld(x: Package) -> bool {
    match Package::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
