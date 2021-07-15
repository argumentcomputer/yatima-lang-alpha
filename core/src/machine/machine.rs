// A G-machine like graph reducer using closures instead of supercombinators, reference counting,
// and a bottom-up, hash-consed, construction of graphs

// In this initial draft, each function will take exactly one argument. The elements of the stack
// will be copied as needed to the environment of closures.

// As an optimization we might add Thunks as well, to allow lazy building of graphs. It will also
// be possible to skip the building of the spine altogether. However, App nodes are still necessary
// for typechecking

use crate::name::Name;

use alloc::string::String;
use sp_std::{
  vec::Vec,
  rc::Rc,
  cell::RefCell,
};

use sp_multihash::{
  U32,
  StatefulHasher,
  Blake3Hasher,
  Blake3Digest,
};

// The maximum byte size of array of globals
pub const GBL_SIZE: usize = 4;
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
// Reference a global definition
pub const REF_GBL: CODE = 4;
// Reference the argument of the function
pub const REF_ARG: CODE = 5;
// Reference a value in the environment
pub const REF_ENV: CODE = 6;
// Evaluate last node
pub const EVAL: CODE = 7;
// End of code
pub const END: CODE = 0;

pub type Link<T> = Rc<RefCell<T>>;
pub type Hash = Blake3Digest<U32>;

#[derive(Clone)]
pub enum Graph {
  // Hash, index of function code and environment
  Fun(Hash, usize, Vec<Link<Graph>>),
  // Hash, function node, argument node
  App(Hash, Link<Graph>, Link<Graph>),
  // Hash, name
  Var(Hash, Name),
  // Hash, reference
  Ref(Hash, usize),

  // All(Pos, Uses, Name, Box<(Term, Term)>),
  // Slf(Pos, Name, Box<Term>),
  // Dat(Pos, Box<Term>),
  // Cse(Pos, Box<Term>),
  // Ref(Pos, Name, Cid, Cid),
  // Let(Pos, bool, Uses, Name, Box<(Term, Term, Term)>),
  // Typ(Pos),
  // Ann(Pos, Box<(Term, Term)>),
  // Lit(Pos, Literal),
  // LTy(Pos, LitType),
  // Opr(Pos, Op),
  // Rec(Pos),
}

pub struct DefCell {
  pub name: Name,
  pub term: Link<Graph>,
}

pub struct FunCell {
  pub arg_name: Name,
  pub code: Vec<CODE>,
  pub hash: Hash,
}

#[inline]
pub fn update_hasher(hasher: &mut Blake3Hasher<U32>, x: usize) {
  let bytes = usize_to_bytes::<8>(x);
  hasher.update(&bytes);
}

#[inline]
pub fn get_hash<'a>(term: &'a Graph) -> &'a [u8] {
  match term {
    Graph::Fun(hash, _, _) => hash.as_ref(),
    Graph::Var(hash, _) => hash.as_ref(),
    Graph::App(hash, _, _) => hash.as_ref(),
    Graph::Ref(hash, _) => hash.as_ref(),
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
pub fn build_graph(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  idx: usize,
  env: &Vec<Link<Graph>>,
  arg: Link<Graph>,
  redex: Link<Graph>
) -> Link<Graph> {
  let code = &fun_defs[idx].code;
  let mut pc = 0;
  let mut args: Vec<Link<Graph>> = vec![];
  let mut hasher: Blake3Hasher<U32> = Blake3Hasher::default();
  loop {
    match code[pc] {
      MK_APP => {
        // Function comes last, so it is popped first
        let fun = args.pop().unwrap();
        let arg = args.pop().unwrap();
        hasher.reset();
        hasher.update(get_hash(&arg.borrow()));
        hasher.update(get_hash(&fun.borrow()));
        update_hasher(&mut hasher, MK_APP as usize);
        let hash = hasher.finalize();
        args.push(Rc::new(RefCell::new(
          Graph::App(hash, fun, arg)
        )));
      },
      MK_FUN => {
        let bytes = &code[pc+1..pc+1+MAP_SIZE];
        let fun_index = bytes_to_usize(bytes);
        pc = pc+MAP_SIZE;
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let env_length = bytes_to_usize(bytes);
        pc = pc+ENV_SIZE;
        let mut fun_env = vec![];
        hasher.reset();
        update_hasher(&mut hasher, MK_FUN as usize);
        hasher.update(fun_defs[fun_index].hash.as_ref());
        update_hasher(&mut hasher, env_length);
        for _ in 0..env_length {
          let bytes = &code[pc+1..pc+1+ENV_SIZE];
          let index = bytes_to_usize(bytes);
          // Index 0 is a reference to the argument of the function, other indices are
          // references to the environment. We encode the difference by a trick: 0 is
          // argument, otherwise n = pos+1, where pos is the position of the variable
          // in the environment
          if index == 0 {
            fun_env.push(arg.clone());
            hasher.update(get_hash(&arg.borrow()));
          }
          else {
            fun_env.push(env[index-1].clone());
            hasher.update(get_hash(&env[index-1].borrow()));
          }
          pc = pc+ENV_SIZE;
        }
        let hash = hasher.finalize();
        args.push(Rc::new(RefCell::new(
          Graph::Fun(hash, fun_index, fun_env)
        )));
      }
      REF_GBL => {
        let bytes = &code[pc+1..pc+1+GBL_SIZE];
        let index = bytes_to_usize(bytes);
        args.push(globals[index].term.clone());
        pc = pc+ENV_SIZE;
      },
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
        hasher.reset();
        update_hasher(&mut hasher, MK_VAR as usize);
        hasher.update(bytes);
        let hash = hasher.finalize();
        // TODO: add proper names
        let name = Name::from("x");
        args.push(Rc::new(RefCell::new(
          Graph::Var(hash, name)
        )));
        pc = pc+8;
      },
      EVAL => {
        // EVAL should be used for projection functions, i.e., functions where the body is a single variable node
        if let Some(arg) = args.last() {
          reduce(globals, fun_defs, arg.clone());
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
pub fn reduce(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  top_node: Link<Graph>
) -> Link<Graph> {
  let mut node = top_node;
  let mut trail = vec![];
  loop {
    let mut is_ref = false;
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
          build_graph(globals, fun_defs, *idx, env, arg, redex)
        }
        else {
          break
        }
      },
      Graph::Ref(_, idx) => {
        is_ref = true;
        reduce(globals, fun_defs, globals[*idx].term.clone())
      },
      _ => break,
    };
    if is_ref {
      let mut mut_ref = (*node).borrow_mut();
      *mut_ref = (*next_node.borrow()).clone();
    }
    else {
      node = next_node;
    }
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
pub fn stringify_graph(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  graph: Link<Graph>
) -> String {
  match &*graph.borrow() {
    Graph::App(_, fun, arg) => {
      let fun = stringify_graph(globals, fun_defs, fun.clone());
      let arg = stringify_graph(globals, fun_defs, arg.clone());
      format!("(App {} {})", fun, arg)
    }
    Graph::Fun(_, idx, env) => {
      let mut env_str = vec![];
      for graph in env {
        env_str.push(stringify_graph(globals, fun_defs, graph.clone()));
      }
      stringify_code(globals, fun_defs, *idx, &env_str)
    }
    Graph::Var(_, nam) => {
      format!("(Var {})", nam)
    }
    Graph::Ref(_, idx) => {
      format!("(Ref {})", globals[*idx].name)
    },
  }
}

pub fn stringify_code(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  idx: usize,
  env: &Vec<String>
) -> String {
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
        args.push(stringify_code(globals, fun_defs, fun_index, &fun_env));
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
        let bytes = &code[pc+1..pc+9];
        // TODO: add proper names
        args.push(format!("(Var {})", bytes_to_usize(bytes)));
        pc = pc+8;
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
