use sp_std::{
  self,
  collections::btree_map::BTreeMap,
  vec::Vec,
};

use sp_cid::Cid;

#[derive(Clone, PartialEq)]
pub enum Ipld {
  /// Represents the absence of a value or the value undefined.
  Null,
  /// Represents a boolean value.
  Bool(bool),
  /// Represents an integer.
  Integer(i128),
  /// Represents a floating point value.
  Float(f64),
  /// Represents an UTF-8 string.
  String(String),
  /// Represents a sequence of bytes.
  Bytes(Vec<u8>),
  /// Represents a list.
  List(Vec<Ipld>),
  /// Represents a map of strings.
  StringMap(BTreeMap<String, Ipld>),
  /// Represents a link to an Ipld node.
  Link(Cid),
}

impl sp_std::fmt::Debug for Ipld {
  fn fmt(&self, f: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
    use Ipld::*;
    match self {
      Null => write!(f, "null"),
      Bool(b) => write!(f, "{:?}", b),
      Integer(i) => write!(f, "{:?}", i),
      Float(i) => write!(f, "{:?}", i),
      String(s) => write!(f, "{:?}", s),
      Bytes(b) => write!(f, "{:?}", b),
      List(l) => write!(f, "{:?}", l),
      StringMap(m) => write!(f, "{:?}", m),
      Link(cid) => write!(f, "{}", cid),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::rand::Rng;
  use sp_multihash::{
    Code,
    MultihashDigest,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  pub fn arbitrary_cid(g: &mut Gen) -> Cid {
    let mut bytes: [u8; 32] = [0; 32];
    for x in bytes.iter_mut() {
      *x = Arbitrary::arbitrary(g);
    }
    Cid::new_v1(0x55, Code::Blake2b256.digest(&bytes))
  }

  pub fn frequency<T, F: Fn(&mut Gen) -> T>(
    g: &mut Gen,
    gens: Vec<(i64, F)>,
  ) -> T {
    if gens.iter().any(|(v, _)| *v < 0) {
      panic!("Negative weight");
    }
    let sum: i64 = gens.iter().map(|x| x.0).sum();
    let mut rng = rand::thread_rng();
    let mut weight: i64 = rng.gen_range(1..=sum);
    // let mut weight: i64 = g.rng.gen_range(1, sum);
    for gen in gens {
      if weight - gen.0 <= 0 {
        return gen.1(g);
      }
      else {
        weight -= gen.0;
      }
    }
    panic!("Calculation error for weight = {}", weight);
  }

  fn arbitrary_null() -> Box<dyn Fn(&mut Gen) -> Ipld> {
    Box::new(move |_: &mut Gen| Ipld::Null)
  }

  fn arbitrary_bool() -> Box<dyn Fn(&mut Gen) -> Ipld> {
    Box::new(move |g: &mut Gen| Ipld::Bool(Arbitrary::arbitrary(g)))
  }

  fn arbitrary_link() -> Box<dyn Fn(&mut Gen) -> Ipld> {
    Box::new(move |g: &mut Gen| Ipld::Link(arbitrary_cid(g)))
  }

  pub fn arbitrary_i128() -> Box<dyn Fn(&mut Gen) -> i128> {
    Box::new(move |g: &mut Gen| {
      let sgn: bool = Arbitrary::arbitrary(g);
      if sgn {
        let x: u64 = Arbitrary::arbitrary(g);
        x as i128
      }
      else {
        let x: i64 = Arbitrary::arbitrary(g);
        if x.is_positive() { -x as i128 } else { x as i128 }
      }
    })
  }

  pub fn arbitrary_integer() -> Box<dyn Fn(&mut Gen) -> Ipld> {
    Box::new(move |g: &mut Gen| Ipld::Integer(arbitrary_i128()(g)))
  }

  fn arbitrary_string() -> Box<dyn Fn(&mut Gen) -> Ipld> {
    Box::new(move |g: &mut Gen| Ipld::String(Arbitrary::arbitrary(g)))
  }

  fn arbitrary_bytes() -> Box<dyn Fn(&mut Gen) -> Ipld> {
    Box::new(move |g: &mut Gen| Ipld::Bytes(Arbitrary::arbitrary(g)))
  }

  fn arbitrary_float() -> Box<dyn Fn(&mut Gen) -> Ipld> {
    Box::new(move |g: &mut Gen| Ipld::Float(Arbitrary::arbitrary(g)))
  }

  pub fn arbitrary_list() -> Box<dyn Fn(&mut Gen) -> Ipld> {
    Box::new(move |g: &mut Gen| {
      let mut rng = rand::thread_rng();
      let size = rng.gen_range(0..5);
      Ipld::List((0..size).map(|_| Arbitrary::arbitrary(g)).collect())
    })
  }

  pub fn arbitrary_stringmap() -> Box<dyn Fn(&mut Gen) -> Ipld> {
    Box::new(move |g: &mut Gen| {
      let mut rng = rand::thread_rng();
      let size = rng.gen_range(0..5);
      Ipld::StringMap((0..size).map(|_| Arbitrary::arbitrary(g)).collect())
    })
  }

  impl Arbitrary for Ipld {
    fn arbitrary(g: &mut Gen) -> Self {
      frequency(g, vec![
        (100, arbitrary_null()),
        (100, arbitrary_bool()),
        (100, arbitrary_link()),
        (100, arbitrary_integer()),
        (100, arbitrary_string()),
        (100, arbitrary_bytes()),
        (30, arbitrary_list()),
        (30, arbitrary_stringmap()),
      ])
    }
  }
}
