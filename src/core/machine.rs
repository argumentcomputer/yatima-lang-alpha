// A G-machine like graph reducer using closures instead of supercombinators, reference counting,
// and a bottom-up, hash-consed, construction of graphs 

use std::convert::TryInto;
use std::rc::Rc;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

// The size of pointers, which depends on the architecture
const PTR_SIZE: usize = 8;
// The maximum byte size of the environment
const ENV_SIZE: usize = 4;
// The maximum byte size of the closure mapping size
const MAP_SIZE: usize = 4;

type CODE = u8;
// Build a closure
const MK_FUN: CODE = 1;
// Build an application node
const MK_APP: CODE = 2;
// Build a free variable node
const MK_VAR: CODE = 3;
// Reference to the argument of the function
const REF_ARG: CODE = 4;
// Reference to a value in the environment
const REF_ENV: CODE = 5;
// Evaluate last node
const EVAL: CODE = 6;
// End of code
const END: CODE = 0;

pub enum Graph {
  // Hash, a  and array of bound variable values
  Fun(u64, usize, Vec<Rc<Graph>>),
  // Hash, function node, argument node
  App(u64, Rc<Graph>, Rc<Graph>),
  // Hash
  Var(u64),
}

#[inline]
pub fn hash(x: u64) -> u64 {
  let mut hasher = DefaultHasher::new();
  x.hash(&mut hasher);
  hasher.finish()
}

#[inline]
pub fn get_hash(term: &Graph) -> u64 {
  match term {
    Graph::Fun(hash, _, _) => *hash,
    Graph::Var(hash) => *hash,
    Graph::App(hash, _, _) => *hash,
  }
}

// little endian encoding of bytes
#[inline]
pub fn bytes_to_usize(bytes: &[u8], len: usize) -> usize {
  let mut result: usize = 0;
  for i in 0..len {
    result = result + ((bytes[i] as usize) << (8*i));
  }
  result
}

#[inline]
pub fn build_graph(code: Vec<CODE>, bind: Vec<Rc<Graph>>, argm: Rc<Graph>) -> Rc<Graph> {
  let mut pc = 0;
  let mut args: Vec<Rc<Graph>> = vec![];
  loop {
    match code[pc] {
      MK_APP => {
        let arg1 = args.pop().unwrap();
        let arg2 = args.pop().unwrap();
        let hash1 = get_hash(&*arg1);
        let hash2 = get_hash(&*arg2);
        let app_hash =
          hash(MK_APP as u64 + hash1 + hash2);
        args.push(Rc::new(
          Graph::App(app_hash, arg1, arg2)
        ));
      },
      MK_FUN => {
        panic!("MK_FUN TODO")
      }
      REF_ARG => {
        args.push(argm.clone());
      },
      REF_ENV => {
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let index = bytes_to_usize(bytes, ENV_SIZE);
        args.push(bind[index].clone());
        pc = pc+ENV_SIZE;
      },
      MK_VAR => {
        let bytes = &code[pc+1..pc+9];
        let hash = u64::from_be_bytes(bytes.try_into().unwrap());
        args.push(Rc::new(
          Graph::Var(hash)
        ));
      },
      EVAL => {
        panic!("EVAL TODO")
      }
      END => break,
      _ => panic!("Operation does not exist"),
    }
    pc = pc+1;
  }
  if args.len() != 1 {
    panic!("Invalid graph")
  }
  drop(bind);
  drop(argm);
  args.pop().unwrap()
}
