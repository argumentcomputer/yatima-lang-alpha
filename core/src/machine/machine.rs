// A G-machine like graph reducer using closures instead of supercombinators, reference counting,
// and a bottom-up, hash-consed, construction of graphs

// In this initial draft, each function will take exactly one argument. The elements of the stack
// will be copied as needed to the environment of closures.

// As an optimization we might add Thunks as well, to allow lazy building of graphs. It will also
// be possible to skip the building of the spine altogether. However, App nodes are still necessary
// for typechecking

use crate::{
  name::Name,
  uses::Uses,
};

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
// Reference a global definition
pub const REF_GBL: CODE = 1;
// Reference the argument of the function
pub const REF_ARG: CODE = 2;
// Reference a value in the environment
pub const REF_ENV: CODE = 3;
// Build a lambda node
pub const MK_LAM: CODE = 4;
// Build an application node
pub const MK_APP: CODE = 5;
// Build a free variable node
pub const MK_VAR: CODE = 6;
// Build a type node
pub const MK_TYP: CODE = 7;
// Build a self type node
pub const MK_ALL: CODE = 8;
// Build a self type node
pub const MK_SLF: CODE = 9;
// Build a data node
pub const MK_DAT: CODE = 10;
// Build a case node
pub const MK_CSE: CODE = 11;
// Build an annotation node
pub const MK_ANN: CODE = 12;
// Evaluate last node
pub const EVAL: CODE = 13;
// End of code
pub const END: CODE = 0;

pub type Link<T> = Rc<RefCell<T>>;
pub type Hash = Blake3Digest<U32>;

#[derive(Clone)]
pub enum Graph {
  Lam(Hash, Closure),
  App(Hash, Link<Graph>, Link<Graph>),
  Var(Hash, Name),
  Ref(Hash, usize),
  Typ(Hash),
  All(Hash, Uses, Link<Graph>, Closure),
  Slf(Hash, Closure),
  Dat(Hash, Link<Graph>),
  Cse(Hash, Link<Graph>),
  Ann(Hash, Link<Graph>, Link<Graph>),
  // Let(Pos, bool, Uses, Name, Box<(Term, Term, Term)>),
  // Lit(Pos, Literal),
  // LTy(Pos, LitType),
  // Opr(Pos, Op),
}

#[derive(Clone)]
pub struct Closure {
  pub idx: usize,
  pub env: Vec<Link<Graph>>,
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
    Graph::Lam(hash, _) => hash.as_ref(),
    Graph::Var(hash, _) => hash.as_ref(),
    Graph::App(hash, _, _) => hash.as_ref(),
    Graph::Ref(hash, _) => hash.as_ref(),
    Graph::Typ(hash) => hash.as_ref(),
    Graph::All(hash, _, _, _) => hash.as_ref(),
    Graph::Slf(hash, _) => hash.as_ref(),
    Graph::Dat(hash, _) => hash.as_ref(),
    Graph::Cse(hash, _) => hash.as_ref(),
    Graph::Ann(hash, _, _) => hash.as_ref(),
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
pub fn code_to_uses(uses: CODE) -> Uses {
  match uses {
    0 => Uses::None,
    1 => Uses::Once,
    2 => Uses::Affi,
    _ => Uses::Many,
  }
}

#[inline]
pub fn build_closure(
  code: &Vec<CODE>,
  pc: &mut usize,
  hasher: &mut Blake3Hasher<U32>,
  fun_defs: &Vec<FunCell>,
  arg: Link<Graph>,
  env: &Vec<Link<Graph>>,
) -> Closure {
  let bytes = &code[*pc+1..*pc+1+MAP_SIZE];
  let fun_index = bytes_to_usize(bytes);
  *pc = *pc+MAP_SIZE;
  let bytes = &code[*pc+1..*pc+1+ENV_SIZE];
  let env_length = bytes_to_usize(bytes);
  *pc = *pc+ENV_SIZE;
  let mut fun_env = vec![];
  hasher.update(fun_defs[fun_index].hash.as_ref());
  update_hasher(hasher, env_length);
  for _ in 0..env_length {
    let bytes = &code[*pc+1..*pc+1+ENV_SIZE];
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
    *pc = *pc+ENV_SIZE;
  }
  let clos = Closure {
    idx: fun_index,
    env: fun_env,
  };
  clos
}

// Builds a graph from a closure. The `eval` flag determines whether we are evaluating, i.e.,
// reducing a graph. If not, we only build the graph "as is". Currently, this only matters in
// two cases: projection closures, which reduces nodes before copying for performance reasons,
// and annotations, which are dropped when reducing. Later, when we introduce thunks, `eval = false`
// will also allow us to construct the spine instead of directly adding the arguments to the stack.
// This is needed for typechecking reasons.
pub fn build_graph(
  eval: bool,
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
        hasher.update(get_hash(&arg.borrow()));
        hasher.update(get_hash(&fun.borrow()));
        update_hasher(&mut hasher, MK_APP as usize);
        let hash = hasher.finalize();
        hasher.reset();
        args.push(Rc::new(RefCell::new(
          Graph::App(hash, fun, arg)
        )));
      },
      MK_LAM => {
        let clos = build_closure(
          code,
          &mut pc,
          &mut hasher,
          fun_defs,
          arg.clone(),
          env,
        );
        update_hasher(&mut hasher, MK_LAM as usize);
        let hash = hasher.finalize();
        hasher.reset();
        args.push(Rc::new(RefCell::new(
          Graph::Lam(hash, clos)
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
        update_hasher(&mut hasher, MK_VAR as usize);
        hasher.update(bytes);
        let hash = hasher.finalize();
        hasher.reset();
        // TODO: add proper names
        let name = Name::from("x");
        args.push(Rc::new(RefCell::new(
          Graph::Var(hash, name)
        )));
        pc = pc+8;
      },
      MK_TYP => {
        update_hasher(&mut hasher, MK_TYP as usize);
        let hash = hasher.finalize();
        hasher.reset();
        args.push(Rc::new(RefCell::new(
          Graph::Typ(hash)
        )));
      },
      MK_ALL => {
        let uses = code[pc+1];
        pc = pc+1;
        let clos = build_closure(
          code,
          &mut pc,
          &mut hasher,
          fun_defs,
          arg.clone(),
          env,
        );
        let dom = args.pop().unwrap();
        hasher.update(get_hash(&dom.borrow()));
        update_hasher(&mut hasher, code[pc+1] as usize);
        update_hasher(&mut hasher, MK_ALL as usize);
        let hash = hasher.finalize();
        hasher.reset();
        args.push(Rc::new(RefCell::new(
          Graph::All(hash, code_to_uses(uses), dom, clos)
        )));
      },
      MK_SLF => {
        let clos = build_closure(
          code,
          &mut pc,
          &mut hasher,
          fun_defs,
          arg.clone(),
          env,
        );
        update_hasher(&mut hasher, MK_SLF as usize);
        let hash = hasher.finalize();
        hasher.reset();
        args.push(Rc::new(RefCell::new(
          Graph::Slf(hash, clos)
        )));
      },
      MK_DAT => {
        let bod = args.pop().unwrap();
        hasher.update(get_hash(&bod.borrow()));
        update_hasher(&mut hasher, MK_DAT as usize);
        let hash = hasher.finalize();
        hasher.reset();
        args.push(Rc::new(RefCell::new(
          Graph::Dat(hash, bod)
        )));
      },
      MK_CSE => {
        let bod = args.pop().unwrap();
        hasher.update(get_hash(&bod.borrow()));
        update_hasher(&mut hasher, MK_CSE as usize);
        let hash = hasher.finalize();
        hasher.reset();
        args.push(Rc::new(RefCell::new(
          Graph::Cse(hash, bod)
        )));
      },
      MK_ANN => {
        if eval {
          // Can we skip building the type altogether?
          let _ = args.pop().unwrap();
        }
        else {
          let typ = args.pop().unwrap();
          let exp = args.pop().unwrap();
          hasher.update(get_hash(&exp.borrow()));
          hasher.update(get_hash(&typ.borrow()));
          update_hasher(&mut hasher, MK_ANN as usize);
          let hash = hasher.finalize();
          hasher.reset();
          args.push(Rc::new(RefCell::new(
            Graph::Ann(hash, typ, exp)
          )));
        }
      },
      EVAL => {
        // EVAL should be used for projection functions, i.e., functions where the body is a single variable node
        if let Some(arg) = args.last() {
          if eval {
            reduce(globals, fun_defs, arg.clone());
          }
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

pub fn reduce(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  top_node: Link<Graph>
) -> Link<Graph> {
  let mut node = top_node;
  let mut trail = vec![];
  loop {
    let next_node = {
      let borrow = &mut *(node.borrow_mut());
      match borrow {
        Graph::App(_, fun, _) => {
          trail.push(node.clone());
          fun.clone()
        }
        Graph::Lam(_, clos) => {
          let Closure { idx, env } = clos;
          let len = trail.len();
          if len > 0 {
            let redex = trail[len-1].clone();
            let arg = match &*redex.borrow() {
              Graph::App(_,_,arg) => {
                arg.clone()
              }
              // Must be a Cse node, in which case we are in whnf
              _ => break
            };
            trail.pop();
            build_graph(true, globals, fun_defs, *idx, env, arg, redex)
          }
          else {
            break
          }
        },
        Graph::Ref(_, idx) => {
          let reduced_node = reduce(globals, fun_defs, globals[*idx].term.clone());
          *borrow = (*reduced_node.borrow()).clone();
          node.clone()
        },
        Graph::Cse(_, bod) => {
          trail.push(node.clone());
          bod.clone()
        }
        Graph::Dat(_, bod) => {
          let len = trail.len();
          if len > 0 {
            let redex = trail[len-1].clone();
            match &*redex.borrow() {
              Graph::Cse(_,_) => (),
              // Must be an App node, in which case we are in whnf
              _ => break,
            };
            trail.pop();
            // The reason we reduce bod before copying its node over the redex is that
            // we don't want to duplicate possible redexes
            let reduced_node = reduce(globals, fun_defs, bod.clone());
            {
              let mut mut_ref = (*redex).borrow_mut();
              *mut_ref = (*reduced_node.borrow()).clone();
            }
            redex
          }
          else {
            break
          }
        }
        Graph::Ann(_, _, exp) => {
          let reduced_node = reduce(globals, fun_defs, exp.clone());
          *borrow = (*reduced_node.borrow()).clone();
          node.clone()
        }
        _ => break,
      }
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
    Graph::Lam(_, clos) => {
      let Closure { idx, env } = clos;
      let mut env_str = vec![];
      for graph in env {
        env_str.push(stringify_graph(globals, fun_defs, graph.clone()));
      }
      let code_str = stringify_code(globals, fun_defs, *idx, &env_str);
      let arg_name = &fun_defs[*idx].arg_name;
      format!("(Lam {} {})", arg_name, code_str)
    }
    Graph::Var(_, nam) => {
      format!("(Var {})", nam)
    }
    Graph::Ref(_, idx) => {
      format!("(Ref {})", globals[*idx].name)
    },
    Graph::Typ(_) => {
      format!("Typ")
    },
    Graph::All(_, uses, dom, clos) => {
      let dom = stringify_graph(globals, fun_defs, dom.clone());
      let Closure { idx, env } = clos;
      let mut env_str = vec![];
      for graph in env {
        env_str.push(stringify_graph(globals, fun_defs, graph.clone()));
      }
      let code_str = stringify_code(globals, fun_defs, *idx, &env_str);
      let arg_name = &fun_defs[*idx].arg_name;
      format!("(All {} {} {} {})", uses, arg_name, dom, code_str)
    },
    Graph::Slf(_, clos) => {
      let Closure { idx, env } = clos;
      let mut env_str = vec![];
      for graph in env {
        env_str.push(stringify_graph(globals, fun_defs, graph.clone()));
      }
      let code_str = stringify_code(globals, fun_defs, *idx, &env_str);
      let arg_name = &fun_defs[*idx].arg_name;
      format!("(Slf {} {})", arg_name, code_str)
    },
    Graph::Dat(_, bod) => {
      let bod = stringify_graph(globals, fun_defs, bod.clone());
      format!("(Dat {})", bod)
    },
    Graph::Cse(_, bod) => {
      let bod = stringify_graph(globals, fun_defs, bod.clone());
      format!("(Dat {})", bod)
    },
    Graph::Ann(_, typ, exp) => {
      let typ = stringify_graph(globals, fun_defs, typ.clone());
      let exp = stringify_graph(globals, fun_defs, exp.clone());
      format!("(Ann {} {})", typ, exp)
    },
  }
}

#[inline]
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
      MK_LAM => {
        let (fun_index, fun_env) = stringify_env(code, &mut pc, arg_name, &env);
        let code_str = stringify_code(globals, fun_defs, fun_index, &fun_env);
        let arg_name = &fun_defs[fun_index].arg_name;
        args.push(format!("(Lam {} {})", arg_name, code_str));
      }
      REF_GBL => {
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let index = bytes_to_usize(bytes);
        args.push(format!("(Ref {})", globals[index].name));
        pc = pc+ENV_SIZE;
      },
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
        let index = bytes_to_usize(bytes);
        args.push(format!("(Var {})", index));
        pc = pc+8;
      },
      MK_TYP => {
        args.push(format!("Typ"));
      },
      MK_ALL => {
        let uses = code[pc+1];
        pc = pc+1;
        let (fun_index, fun_env) = stringify_env(code, &mut pc, arg_name, &env);
        let dom = args.pop().unwrap();
        let code_str = stringify_code(globals, fun_defs, fun_index, &fun_env);
        let arg_name = &fun_defs[fun_index].arg_name;
        args.push(format!("(All {} {} {} {})", code_to_uses(uses), arg_name, dom, code_str));
      },
      MK_SLF => {
        let (fun_index, fun_env) = stringify_env(code, &mut pc, arg_name, &env);
        let code_str = stringify_code(globals, fun_defs, fun_index, &fun_env);
        let arg_name = &fun_defs[fun_index].arg_name;
        args.push(format!("(Slf {} {})", arg_name, code_str));
      },
      MK_DAT => {
        let bod = args.pop().unwrap();
        args.push(format!("(Dat {})", bod));
      },
      MK_CSE => {
        let bod = args.pop().unwrap();
        args.push(format!("(Cse {})", bod));
      },
      MK_ANN => {
        let typ = args.pop().unwrap();
        let exp = args.pop().unwrap();
        args.push(format!("(App {} {})", typ, exp));
      },
      EVAL => {
      }
      END => break,
      _ => panic!("Operation does not exist"),
    }
    pc = pc+1;
  }
  format!("{}", args.pop().unwrap())
}

#[inline]
pub fn stringify_env(
  code: &Vec<CODE>,
  pc: &mut usize,
  arg_name: &Name,
  env: &Vec<String>,
) -> (usize, Vec<String>) {
  let bytes = &code[*pc+1..*pc+1+MAP_SIZE];
  let fun_index = bytes_to_usize(bytes);
  *pc = *pc+MAP_SIZE;
  let bytes = &code[*pc+1..*pc+1+ENV_SIZE];
  let env_length = bytes_to_usize(bytes);
  *pc = *pc+ENV_SIZE;
  let mut fun_env = vec![];
  for _ in 0..env_length {
    let bytes = &code[*pc+1..*pc+1+ENV_SIZE];
    let index = bytes_to_usize(bytes);
    if index == 0 {
      fun_env.push(format!("(Var {})", arg_name));
    }
    else {
      fun_env.push(env[index-1].clone());
    }
    *pc = *pc+ENV_SIZE;
  }
  (fun_index, fun_env)
}
