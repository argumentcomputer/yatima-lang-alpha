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
  pub docs: String,
  pub index: Index,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Index(BTreeMap<String, Cid>);

#[derive(PartialEq, Clone, Debug)]
pub struct Definition {
  pub pos: Pos,
  pub type_anon: Cid,
  pub term_anon: Cid,
  pub type_meta: Meta,
  pub term_meta: Meta,
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
}
