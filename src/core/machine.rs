// A G-machine like graph reducer using closures instead of supercombinators, reference counting,
// and a bottom-up, hash-consed, construction of graphs 

// In this initial draft, each function will take exactly one argument. The elements of the stack
// will be copied as needed to the environment of closures.

use std::convert::TryInto;
use std::rc::Rc;
use std::cell::RefCell;
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
// Update the root node
const UPDATE: CODE = 7;
// End of code
const END: CODE = 0;

type Link<T> = Rc<RefCell<T>>;

pub enum Graph {
  // Hash, index of function code and environment
  Fun(u64, usize, Vec<Link<Graph>>),
  // Hash, function node, argument node
  App(u64, Link<Graph>, Link<Graph>),
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
pub fn build_graph(code: &Vec<CODE>, env: &Vec<Link<Graph>>, arg: Link<Graph>, redex: Link<Graph>) -> Link<Graph> {
  let mut pc = 0;
  let mut args: Vec<Link<Graph>> = vec![];
  loop {
    match code[pc] {
      MK_APP => {
        let arg1 = args.pop().unwrap();
        let arg2 = args.pop().unwrap();
        let hash1 = get_hash(&arg1.borrow());
        let hash2 = get_hash(&arg2.borrow());
        let app_hash =
          hash(MK_APP as u64 + hash1 + hash2);
        args.push(Rc::new(RefCell::new(
          Graph::App(app_hash, arg1, arg2)
        )));
      },
      MK_FUN => {
        panic!("MK_FUN TODO")
      }
      REF_ARG => {
        args.push(arg.clone());
      },
      REF_ENV => {
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let index = bytes_to_usize(bytes, ENV_SIZE);
        args.push(env[index].clone());
        pc = pc+ENV_SIZE;
      },
      MK_VAR => {
        let bytes = &code[pc+1..pc+9];
        let hash = u64::from_be_bytes(bytes.try_into().unwrap());
        args.push(Rc::new(RefCell::new(
          Graph::Var(hash)
        )));
      },
      EVAL => {
        panic!("EVAL TODO")
      }
      UPDATE => {
        panic!("UPDATE TODO")
      }
      END => break,
      _ => panic!("Operation does not exist"),
    }
    pc = pc+1;
  }
  if args.len() != 1 {
    panic!("Invalid graph")
  }
  args.pop().unwrap()
}

pub fn reduce(fun_defs: &Vec<Vec<CODE>>, top_node: Rc<RefCell<Graph>>) -> Rc<RefCell<Graph>> {
  let mut node = top_node;
  let mut trail = vec![];
  loop {
    let next_node = match &*node.borrow() {
      Graph::App(_, fun, _) => {
        trail.push(node.clone());
        fun.clone()
      }
      Graph::Fun(_, idx, env) => {
        if let Some(redex) = trail.pop() {
          // Since we know these are app nodes, can't we extract the field without matching
          // i.e., without double checking the tag?
          let arg = match &*redex.borrow() { 
            Graph::App(_,_,arg) => {
              arg.clone()
            }
            _ => panic!("Implementation of reduce incorrect")
          };
          build_graph(&fun_defs[*idx], env, arg, redex)
        }
        else {
          break
        }
      },
      _ => break,
    };
    node = next_node;
  }
  if trail.is_empty() {
    node
  }
  else {
    // This is only correct if we are correctly updating the redex nodes
    // Can we extract without cloning, since we know that `trail` will be collected afterwards?
    trail[0].clone()
  }
}
