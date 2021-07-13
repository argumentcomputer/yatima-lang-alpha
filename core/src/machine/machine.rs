// A G-machine like graph reducer using closures instead of supercombinators, reference counting,
// and a bottom-up, hash-consed, construction of graphs

// In this initial draft, each function will take exactly one argument. The elements of the stack
// will be copied as needed to the environment of closures.

use crate::name::Name;

use alloc::string::String;

use sp_std::{
  vec::Vec,
  convert::TryInto,
  rc::Rc,
  cell::RefCell,
  hash::{Hash, Hasher},
};

// The maximum byte size of the environment
pub const ENV_SIZE: usize = 4;
// The maximum byte size of the closure mapping size
pub const MAP_SIZE: usize = 4;

pub type CODE = u8;
// Build a closure
pub const MK_FUN: CODE = 1;
// Build an application node
pub const MK_APP: CODE = 2;
// Build a free variable node
pub const MK_VAR: CODE = 3;
// Reference to the argument of the function
pub const REF_ARG: CODE = 4;
// Reference to a value in the environment
pub const REF_ENV: CODE = 5;
// Evaluate last node
pub const EVAL: CODE = 6;
// End of code
pub const END: CODE = 0;

pub type Link<T> = Rc<RefCell<T>>;

#[derive(Clone)]
pub enum Graph {
  // Hash, index of function code and environment
  Fun(u64, usize, Vec<Link<Graph>>),
  // Hash, function node, argument node
  App(u64, Link<Graph>, Link<Graph>),
  // Hash, name
  Var(u64, Name),
}

pub struct FunCell {
  pub arg_name: Name,
  pub code: Vec<CODE>,
}

#[inline]
pub fn make_hash<T: Hash>(x: &T) -> u64 {
  // let mut hasher = DefaultHasher::new();
  // x.hash(&mut hasher);
  // hasher.finish()
  0
}

#[inline]
pub fn get_hash(term: &Graph) -> u64 {
  match term {
    Graph::Fun(hash, _, _) => *hash,
    Graph::Var(hash, _) => *hash,
    Graph::App(hash, _, _) => *hash,
  }
}

// little endian encoding of bytes
#[inline]
pub fn bytes_to_usize(bytes: &[u8]) -> usize {
  let mut result: usize = 0;
  let mut i: usize = 0;
  for byte in bytes.iter() {
    result = result + ((*byte as usize) << (8*i));
    i = i+1;
  }
  result
}

#[inline]
pub fn usize_to_bytes<const N: usize>(num: usize) -> [u8; N] {
  let mut result: [u8; N] = [0; N];
  for i in 0..N {
    result[i] = (num >> (N*i)) as u8;
  }
  result
}

#[inline]
pub fn build_graph(fun_defs: &Vec<FunCell>, idx: usize, env: &Vec<Link<Graph>>, arg: Link<Graph>, redex: Link<Graph>) -> Link<Graph> {
  let code = &fun_defs[idx].code;
  let mut pc = 0;
  let mut args: Vec<Link<Graph>> = vec![];
  loop {
    match code[pc] {
      MK_APP => {
        // Function comes last, so it is popped first
        let fun = args.pop().unwrap();
        let arg = args.pop().unwrap();
        let hash_fun = get_hash(&fun.borrow());
        let hash_arg = get_hash(&arg.borrow());
        let mut app_hash = (MK_APP as u64)
          .wrapping_add(hash_fun)
          .wrapping_add(hash_arg);
        app_hash = make_hash(&app_hash);
        args.push(Rc::new(RefCell::new(
          Graph::App(app_hash, fun, arg)
        )));
      },
      MK_FUN => {
        let bytes = &code[pc+1..pc+1+MAP_SIZE];
        let fun_index = bytes_to_usize(bytes);
        pc = pc+MAP_SIZE;
        let mut hash = make_hash(&fun_defs[fun_index].code);
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let env_length = bytes_to_usize(bytes);
        pc = pc+ENV_SIZE;
        let mut fun_env = vec![];
        for _ in 0..env_length {
          let bytes = &code[pc+1..pc+1+ENV_SIZE];
          let index = bytes_to_usize(bytes);
          // Index 0 is a reference to the argument of the function, other indices are
          // references to the environment. We encode the difference by a trick: 0 is
          // argument, otherwise n = pos+1, where pos is the position of the variable
          // in the environment
          if index == 0 {
            fun_env.push(arg.clone());
            hash = hash
              .wrapping_add(get_hash(&arg.borrow()));
          }
          else {
            fun_env.push(env[index-1].clone());
            hash = hash
              .wrapping_add(get_hash(&env[index-1].borrow()));
          }
          hash = make_hash(&hash);
          pc = pc+ENV_SIZE;
        }
        args.push(Rc::new(RefCell::new(
          Graph::Fun(hash, fun_index, fun_env)
        )));
      }
      REF_ARG => {
        args.push(arg.clone());
      },
      REF_ENV => {
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let index = bytes_to_usize(bytes);
        args.push(env[index].clone());
        pc = pc+ENV_SIZE;
      },
      MK_VAR => {
        let bytes = &code[pc+1..pc+9];
        let hash = u64::from_be_bytes(bytes.try_into().unwrap());
        // TODO: add proper names
        args.push(Rc::new(RefCell::new(
          Graph::Var(hash, Name::from("x"))
        )));
      },
      EVAL => {
        // EVAL should be used for projection functions, i.e., functions where the body is a single variable node
        if let Some(arg) = args.last() {
          reduce(fun_defs, arg.clone());
        }
        else {
          panic!("Stack underflow")
        }
      }
      END => break,
      _ => panic!("Operation does not exist"),
    }
    pc = pc+1;
  }
  // Optimization TODO: build the argument on top of the redex instead of copying over the redex
  let arg = args.pop().unwrap();
  {
    let mut mut_ref = (*redex).borrow_mut();
    *mut_ref = (*arg.borrow()).clone();
  }
  redex
}

#[inline]
pub fn reduce(fun_defs: &Vec<FunCell>, top_node: Link<Graph>) -> Link<Graph> {
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
            _ => unreachable!()
          };
          build_graph(fun_defs, *idx, env, arg, redex)
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

#[inline]
pub fn stringify_graph(fun_defs: &Vec<FunCell>, graph: Link<Graph>) -> String {
  match &*graph.borrow() {
      Graph::App(_, fun, arg) => {
        let fun = stringify_graph(fun_defs, fun.clone());
        let arg = stringify_graph(fun_defs, arg.clone());
        format!("(App {} {})", fun, arg)
      }
      Graph::Fun(_, idx, env) => {
        let mut env_str = vec![];
        for graph in env {
          env_str.push(stringify_graph(fun_defs, graph.clone()));
        }
        stringify_code(fun_defs, *idx, &env_str)
      }
      Graph::Var(name, _) => {
        format!("(Var {})", name)
      }
  }
}

pub fn stringify_code(fun_defs: &Vec<FunCell>, idx: usize, env: &Vec<String>) -> String {
  let code = &fun_defs[idx].code;
  let arg_name = &fun_defs[idx].arg_name;
  let mut pc = 0;
  let mut args = vec![];
  loop {
    match code[pc] {
      MK_APP => {
        // Function comes last, so it is popped first
        let fun = args.pop().unwrap();
        let arg = args.pop().unwrap();
        args.push(format!("(App {} {})", fun, arg));
      },
      MK_FUN => {
        let bytes = &code[pc+1..pc+1+MAP_SIZE];
        let fun_index = bytes_to_usize(bytes);
        pc = pc+MAP_SIZE;
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let env_length = bytes_to_usize(bytes);
        pc = pc+ENV_SIZE;
        let mut fun_env = vec![];
        for _ in 0..env_length {
          let bytes = &code[pc+1..pc+1+ENV_SIZE];
          let index = bytes_to_usize(bytes);
          if index == 0 {
            fun_env.push(format!("(Var {})", arg_name));
          }
          else {
            fun_env.push(env[index-1].clone());
          }
          pc = pc+ENV_SIZE;
        }
        args.push(stringify_code(fun_defs, fun_index, &fun_env));
      }
      REF_ARG => {
        args.push(format!("(Var {})", arg_name));
      },
      REF_ENV => {
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let index = bytes_to_usize(bytes);
        args.push(env[index].clone());
        pc = pc+ENV_SIZE;
      },
      MK_VAR => {
        todo!()
      },
      EVAL => {
      }
      END => break,
      _ => panic!("Operation does not exist"),
    }
    pc = pc+1;
  }
  format!("(Lam {} {})", arg_name, args.pop().unwrap())
}
