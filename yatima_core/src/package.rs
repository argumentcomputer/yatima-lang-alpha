use crate::{
  ipld_error::IpldError,
  meta::Meta,
  position::Pos,
};

use cid::Cid;
use libipld::{
  cbor::DagCborCodec,
  codec::Codec,
  ipld::Ipld,
};
use multihash::{
  Code,
  MultihashDigest,
};
use std::collections::BTreeMap;

#[derive(PartialEq, Clone, Debug)]
pub struct Package {
  pub pos: Pos,
  pub name: String,
  pub imports: Vec<Import>,
  pub index: Index,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Import {
  All { cid: Cid, alias: String },
  Some { cid: Cid, alias: String, with: Vec<String> },
}

#[derive(PartialEq, Clone, Debug)]
pub struct Index(pub BTreeMap<String, Cid>);

#[derive(PartialEq, Clone, Debug)]
pub struct Definition {
  pub pos: Pos,
  pub type_anon: Cid,
  pub term_anon: Cid,
  pub type_meta: Meta,
  pub term_meta: Meta,
}

impl Definition {
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
          Ok(Definition {
            pos,
            type_anon: *type_anon,
            term_anon: *term_anon,
            type_meta,
            term_meta
            })
        }
        xs => Err(IpldError::Definition(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Definition(xs.to_owned())),
    }
  }

  pub fn cid(&self) -> Cid {
    Cid::new_v1(
      0x71,
      Code::Blake2b256.digest(&DagCborCodec.encode(&self.to_ipld()).unwrap()),
    )
  }
}

impl Index {
  pub fn to_ipld(&self) -> Ipld {
    Ipld::StringMap(
      self.0.iter().map(|(k, v)| (k.clone(), Ipld::Link(*v))).collect(),
    )
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::StringMap(xs) => {
        let mut res: BTreeMap<String, Cid> = BTreeMap::new();
        for (k, v) in xs {
          match v {
            Ipld::Link(cid) => {
              res.insert(k.clone(), *cid);
            }
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
}

impl Import {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::All { cid, alias } => Ipld::List(vec![
        Ipld::Link(*cid),
        Ipld::String(alias.clone()),
        Ipld::Null,
      ]),
      Self::Some { cid, alias, with } => Ipld::List(vec![
        Ipld::Link(*cid),
        Ipld::String(alias.clone()),
        Ipld::List(with.iter().map(|x| Ipld::String(x.clone())).collect()),
      ]),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Link(cid), Ipld::String(alias), Ipld::Null] => {
          Ok(Self::All { cid: *cid, alias: alias.clone() })
        }
        [Ipld::Link(cid), Ipld::String(alias), Ipld::List(with)] => {
          let mut res: Vec<String> = Vec::new();
          for w in with {
            match w {
              Ipld::String(w) => {
                res.push(w.clone());
              }
              w => return Err(IpldError::ImportEntry(w.to_owned())),
            }
          }
          Ok(Self::Some { cid: *cid, alias: alias.clone(), with: res })
        }
        xs => Err(IpldError::Import(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Import(xs.to_owned())),
    }
  }
}
impl Package {
  pub fn to_ipld(&self) -> Ipld {
    Ipld::List(vec![
      self.pos.to_ipld(),
      Ipld::String(self.name.clone()),
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
          Ok(Package { pos, name: name.clone(), imports, index })
        }
        xs => Err(IpldError::Package(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Package(xs.to_owned())),
    }
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

  impl Arbitrary for Definition {
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
      let b: bool = Arbitrary::arbitrary(g);
      if b {
        Self::All { cid: arbitrary_cid(g), alias: arbitrary_name(g) }
      }
      else {
        let vec: Vec<()> = Arbitrary::arbitrary(g);
        let vec: Vec<String> =
          vec.into_iter().map(|_| arbitrary_name(g)).collect();
        Self::Some {
          cid: arbitrary_cid(g),
          alias: arbitrary_name(g),
          with: vec,
        }
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
  fn definition_ipld(x: Definition) -> bool {
    match Definition::from_ipld(&x.to_ipld()) {
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
