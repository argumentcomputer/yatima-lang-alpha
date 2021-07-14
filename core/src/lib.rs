#![cfg_attr(not(any(feature = "std", test)), no_std)]

#[macro_use]
extern crate alloc;

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
extern crate rand;

pub mod anon;
pub mod check;
pub mod dag;
pub mod defs;
pub mod dll;
pub mod embed_error;
pub mod eval;
pub mod ipld_error;
pub mod literal;
pub mod meta;
pub mod name;
pub mod package;
pub mod typedef;
#[macro_use]
pub mod parse;
pub mod position;
pub mod prim;
pub mod runtime;
pub mod term;
pub mod upcopy;
pub mod uses;

#[cfg(test)]
pub mod tests {
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;
  use sp_cid::Cid;
  use sp_multihash::{
    Code,
    MultihashDigest,
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
    gens: sp_std::vec::Vec<(i64, F)>,
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
}
