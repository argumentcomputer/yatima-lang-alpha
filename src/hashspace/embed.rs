// hashspace,
// hashspace::{
//   anon_term::AnonTerm,
//   cache::Cache,
//   meta_term::MetaTerm,
// },
// imports::Imports,
// term,
// };

// use im::{
// HashMap,
// OrdMap,
// Vector,
// };

// impl Index {
//  pub fn encode(self) -> Expr {
//  }
//
//  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
//    let mut defs = OrdMap::new();
//    match expr {
//    }
//  }
//}

// pub fn embed_defs(defs: Defs, cache: Cache) -> (Index, Cache) {
//  let mut ds = OrdMap::new();
//  let mut cache = HashMap::new();
//  for d in defs.defs {
//    let (d, ta, xa) = embed_def(d);
//    ds.push(d);
//    let ta = ta.encode();
//    cache.insert(ta.link(), ta);
//    let xa = xa.encode();
//    cache.insert(xa.link(), xa);
//  }
//  (Index { defs: ds }, cache)
//}
// pub fn unembed_defs(
//  defs: hashspace::embed::Defs,
//  cache: Cache,
//) -> Result<defs::Defs, EmbeddingError> {
//  let mut ds = Vec::new();
//  for d in defs.defs {
//    let type_anon = cache
//      .get(&d.type_anon)
//      .ok_or(EmbeddingError::UnknownLink(d.type_anon))
//      .and_then(|x| {
//        AnonTerm::decode(x.to_owned())
//          .map_err(|e| EmbeddingError::DecodeError(e))
//      })?;
//    let term_anon = cache
//      .get(&d.term_anon)
//      .ok_or(EmbeddingError::UnknownLink(d.type_anon))
//      .and_then(|x| {
//        AnonTerm::decode(x.to_owned())
//          .map_err(|e| EmbeddingError::DecodeError(e))
//      })?;
//    let def = unembed_def(d, type_anon, term_anon)?;
//    ds.push(def);
//  }
//  Ok(defs::Defs { defs: ds })
//}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use crate::term::tests::{
    arbitrary_def,
    arbitrary_name,
    test_defs,
  };

  impl Arbitrary for Def {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let name = arbitrary_name(g);
      embed_def(arbitrary_def(g, test_defs(), name)).0
    }
  }

  impl Arbitrary for Defs {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      embed_defs(Arbitrary::arbitrary(g)).0
    }
  }

  #[quickcheck]
  fn def_encode_decode(x: Def) -> bool {
    match Def::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn defs_encode_decode(x: Defs) -> bool {
    match Defs::decode(x.clone().encode()) {
      Ok(y) => x == y,
      e => {
        // println!("x: {:?}", x);
        // println!("xe: {:?}", x.encode());
        // println!("e: {:?}", e);
        false
      }
    }
  }

  #[quickcheck]
  fn defs_embed_unembed(x: defs::Defs) -> bool {
    let (ds, c) = embed_defs(x.clone());
    match unembed_defs(ds, c) {
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
      e => {
        //        println!("x: {:?}", x);
        //        println!("a: {:?}", a);
        //        println!("m: {:?}", m);
        //        println!("e: {:?}", e);
        false
      }
    }
  }
}
