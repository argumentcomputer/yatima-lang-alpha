// A G-machine like graph reducer using closures instead of supercombinators, reference counting,
// and a bottom-up construction of graphs

// In this initial draft, each function will take exactly one argument. The elements of the stack
// will be copied as needed to the environment of closures.

// As an optimization we might add Thunks as well, to allow lazy building of graphs. It will also
// be possible to skip the building of the spine altogether. However, App nodes are still necessary
// for typechecking

use crate::{
  name::Name,
  uses::Uses,
  literal::{
    LitType,
    Literal,
  },
  prim::Op,
  machine::ir,
};

use alloc::string::String;
use sp_std::{
  mem,
  vec::Vec,
  rc::Rc,
  cell::RefCell,
  collections::{
    btree_map::BTreeMap,
  },
};

use num_bigint::{
  BigUint,
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
// Build a let node
pub const MK_LET: CODE = 13;
// Build a fix node
pub const MK_FIX: CODE = 14;
// Build a fix node
pub const MK_LIT: CODE = 15;
// Build a fix node
pub const MK_LTY: CODE = 16;
// Build a fix node
pub const MK_OPR: CODE = 17;
// Evaluate last node
pub const EVAL: CODE = 18;
// End of code
pub const END: CODE = 0;

pub type Link<T> = Rc<RefCell<T>>;

#[inline]
pub fn new_link<T>(value: T) -> Link<T> {
  Rc::new(RefCell::new(value))
}

#[derive(Clone, Debug)]
pub enum Graph {
  Lam(Closure),
  App(Link<Graph>, Link<Graph>),
  Var(usize, Name),
  Ref(usize),
  Typ,
  All(Uses, Link<Graph>, Closure),
  Slf(Closure),
  Dat(Link<Graph>),
  Cse(Link<Graph>),
  Ann(Link<Graph>, Link<Graph>),
  Let(Uses, Link<Graph>, Link<Graph>, Closure),
  Fix(Closure),
  Lit(Literal),
  LTy(LitType),
  Opr(Op),
  // Only used for typechecking Fix
  Unr(usize, Link<Graph>),
}

#[derive(Clone, Debug)]
pub struct Closure {
  pub idx: usize,
  pub env: Vec<Link<Graph>>,
}

#[derive(Clone, Debug)]
pub struct DefCell {
  pub name: Name,
  pub term: Link<Graph>,
  pub typ_: Link<Graph>,
}

#[derive(Clone, Debug)]
pub struct FunCell {
  pub arg_name: Name,
  pub code: Vec<CODE>,
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
    result[i] = (num >> (8*i)) as u8;
  }
  result
}

#[inline]
pub fn uses_to_code(uses: Uses) -> CODE {
  match uses {
    Uses::None => 0,
    Uses::Once => 1,
    Uses::Affi => 2,
    Uses::Many => 3,
  }
}

#[inline]
pub fn code_to_uses(uses: CODE) -> Uses {
  match uses {
    0 => Uses::None,
    1 => Uses::Once,
    2 => Uses::Affi,
    3 => Uses::Many,
    _ => unreachable!(),
  }
}

#[inline]
pub fn opr_to_code(opr: Op) -> (CODE, CODE) {
  match opr {
    Op::Nat(x) => (0, x as u8),
    Op::Int(x) => (1, x as u8),
    Op::Bits(x) => (2, x as u8),
    Op::Bytes(x) => (3, x as u8),
    Op::Text(x) => (4, x as u8),
    Op::Char(x) => (5, x as u8),
    Op::Bool(x) => (6, x as u8),
    Op::U8(x) => (7, x as u8),
    Op::U16(x) => (8, x as u8),
    Op::U32(x) => (9, x as u8),
    Op::U64(x) => (10, x as u8),
    Op::I8(x) => (11, x as u8),
    Op::I16(x) => (12, x as u8),
    Op::I32(x) => (13, x as u8),
    Op::I64(x) => (14, x as u8),
  }
}

#[inline]
pub fn code_to_opr(pair: (CODE, CODE)) -> Op {
  unsafe {
    match pair {
      (0, x) => Op::Nat(mem::transmute(x)),
      (1, x) => Op::Int(mem::transmute(x)),
      (2, x) => Op::Bits(mem::transmute(x)),
      (3, x) => Op::Bytes(mem::transmute(x)),
      (4, x) => Op::Text(mem::transmute(x)),
      (5, x) => Op::Char(mem::transmute(x)),
      (6, x) => Op::Bool(mem::transmute(x)),
      (7, x) => Op::U8(mem::transmute(x)),
      (8, x) => Op::U16(mem::transmute(x)),
      (9, x) => Op::U32(mem::transmute(x)),
      (10, x) => Op::U64(mem::transmute(x)),
      (11, x) => Op::I8(mem::transmute(x)),
      (12, x) => Op::I16(mem::transmute(x)),
      (13, x) => Op::I32(mem::transmute(x)),
      (14, x) => Op::I64(mem::transmute(x)),
      _ => unreachable!(),
    }
  }
}

pub fn full_clone(node: Link<Graph>) -> Link<Graph> {
  fn go(node: Link<Graph>, map: &mut BTreeMap<*mut Graph, Link<Graph>>) -> Link<Graph> {
    if let Some(copy) = map.get(&node.as_ptr()) {
      return copy.clone()
    }
    let copy = match &*node.borrow() {
      Graph::Lam(Closure { idx, env }) => {
        let env = env.iter().map(|node| go(node.clone(), map)).collect();
        let new_node = Graph::Lam(Closure { idx: *idx, env });
        new_link(new_node)
      }
      Graph::App(fun, arg) => {
        let fun = go(fun.clone(), map);
        let arg = go(arg.clone(), map);
        let new_node = Graph::App(fun, arg);
        new_link(new_node)
      }
      Graph::All(uses, dom, Closure { idx, env }) => {
        let dom = go(dom.clone(), map);
        let env = env.iter().map(|node| go(node.clone(), map)).collect();
        let new_node = Graph::All(*uses, dom, Closure { idx: *idx, env });
        new_link(new_node)
      }
      Graph::Slf(Closure { idx, env}) => {
        let env = env.iter().map(|node| go(node.clone(), map)).collect();
        let new_node = Graph::Slf(Closure { idx: *idx, env });
        new_link(new_node)
      }
      Graph::Dat(bod) => {
        let bod = go(bod.clone(), map);
        let new_node = Graph::Dat(bod);
        new_link(new_node)
      }
      Graph::Cse(bod) => {
        let bod = go(bod.clone(), map);
        let new_node = Graph::Cse(bod);
        new_link(new_node)
      }
      Graph::Ann(typ, exp) => {
        let typ = go(typ.clone(), map);
        let exp = go(exp.clone(), map);
        let new_node = Graph::Ann(typ, exp);
        new_link(new_node)
      }
      Graph::Let(uses, typ, exp, Closure { idx, env }) => {
        let typ = go(typ.clone(), map);
        let exp = go(exp.clone(), map);
        let env = env.iter().map(|node| go(node.clone(), map)).collect();
        let new_node = Graph::Let(*uses, typ, exp, Closure { idx: *idx, env });
        new_link(new_node)
      }
      Graph::Fix(Closure { idx, env }) => {
        let env = env.iter().map(|node| go(node.clone(), map)).collect();
        let new_node = Graph::Fix(Closure { idx: *idx, env });
        new_link(new_node)
      }
      Graph::Unr(idx, exp) => {
        let exp = go(exp.clone(), map);
        let new_node = Graph::Unr(*idx, exp);
        new_link(new_node)
      }
      node => new_link(node.clone())
    };
    map.insert(node.as_ptr(), copy.clone());
    copy
  }
  let mut map = BTreeMap::new();
  go(node, &mut map)
}

pub fn build_closure(
  code: &Vec<CODE>,
  pc: &mut usize,
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
  for _ in 0..env_length {
    let bytes = &code[*pc+1..*pc+1+ENV_SIZE];
    let index = bytes_to_usize(bytes);
    // Index 0 is a reference to the argument of the function, other indices are
    // references to the environment. We encode the difference by a trick: 0 is
    // argument, otherwise n = pos+1, where pos is the position of the variable
    // in the environment
    if index == 0 {
      fun_env.push(arg.clone());
    }
    else {
      fun_env.push(env[index-1].clone());
    }
    *pc = *pc+ENV_SIZE;
  }
  Closure {
    idx: fun_index,
    env: fun_env,
  }
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
) -> Link<Graph> {
  let code = &fun_defs[idx].code;
  let mut pc = 0;
  let mut args: Vec<Link<Graph>> = vec![];
  loop {
    match code[pc] {
      MK_APP => {
        // Function comes last, so it is popped first
        let fun = args.pop().unwrap();
        let arg = args.pop().unwrap();
        args.push(new_link(
          Graph::App(fun, arg)
        ));
      },
      MK_LAM => {
        let clos = build_closure(
          code,
          &mut pc,
          arg.clone(),
          env,
        );
        args.push(new_link(
          Graph::Lam(clos)
        ));
      }
      REF_GBL => {
        let bytes = &code[pc+1..pc+1+GBL_SIZE];
        let index = bytes_to_usize(bytes);
        pc = pc+ENV_SIZE;
        if eval {
          args.push(globals[index].term.clone());
        }
        else {
          args.push(new_link(
            Graph::Ref(index)
          ))
        }
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
        let index = bytes_to_usize(bytes);
        // TODO: add proper names
        let name = Name::from("x");
        args.push(new_link(
          Graph::Var(index, name)
        ));
        pc = pc+8;
      },
      MK_TYP => {
        args.push(new_link(
          Graph::Typ
        ));
      },
      MK_ALL => {
        let uses = code[pc+1];
        pc = pc+1;
        let dom = args.pop().unwrap();
        let clos = build_closure(
          code,
          &mut pc,
          arg.clone(),
          env,
        );
        args.push(new_link(
          Graph::All(code_to_uses(uses), dom, clos)
        ));
      },
      MK_SLF => {
        let clos = build_closure(
          code,
          &mut pc,
          arg.clone(),
          env,
        );
        args.push(new_link(
          Graph::Slf(clos)
        ));
      },
      MK_DAT => {
        let bod = args.pop().unwrap();
        args.push(new_link(
          Graph::Dat(bod)
        ));
      },
      MK_CSE => {
        let bod = args.pop().unwrap();
        args.push(new_link(
          Graph::Cse(bod)
        ));
      },
      MK_ANN => {
        if eval {
          // Can we skip building the type altogether?
          let _ = args.pop().unwrap();
        }
        else {
          let typ = args.pop().unwrap();
          let exp = args.pop().unwrap();
          args.push(new_link(
            Graph::Ann(typ, exp)
          ));
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
      MK_LET => {
        let uses = code[pc+1];
        pc = pc+1;
        let typ = args.pop().unwrap();
        let exp = args.pop().unwrap();
        let clos = build_closure(
          code,
          &mut pc,
          arg.clone(),
          env,
        );
        args.push(new_link(
          Graph::Let(code_to_uses(uses), typ, exp, clos)
        ));
      },
      MK_FIX => {
        let clos = build_closure(
          code,
          &mut pc,
          arg.clone(),
          env,
        );
        args.push(new_link(
          Graph::Fix(clos)
        ));
      },
      MK_LIT => {
        todo!()
      }
      MK_LTY => {
        let lty = code[pc+1];
        pc = pc+1;
        let lty = unsafe { mem::transmute(lty) };
        args.push(new_link(
          Graph::LTy(lty)
        ));
      }
      MK_OPR => {
        let typ_code = code[pc+1];
        let opr_code = code[pc+2];
        pc = pc+2;
        let opr = code_to_opr((typ_code, opr_code));
        args.push(new_link(
          Graph::Opr(opr)
        ));
      }
      END => break,
      _ => panic!("Operation does not exist"),
    }
    pc = pc+1;
  }
  args.pop().unwrap()
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
      let borrow = node.borrow();
      match &*borrow {
        Graph::App(fun, _) => {
          trail.push(node.clone());
          fun.clone()
        }
        Graph::Lam(Closure { idx, env }) => {
          let len = trail.len();
          if len > 0 {
            let redex = trail[len-1].clone();
            let arg = match &*redex.borrow() {
              Graph::App(_, arg) => {
                arg.clone()
              }
              // Must be a Cse node, in which case we are in whnf
              _ => break
            };
            trail.pop();
            // Optimization TODO: build the reduced node on top of the redex instead of copying
            let reduced_node = build_graph(true, globals, fun_defs, *idx, env, arg);
            {
              let mut mut_ref = (*redex).borrow_mut();
              *mut_ref = (*reduced_node.borrow()).clone();
            }
            redex
          }
          else {
            break
          }
        },
        Graph::Ref(idx) => {
          let reduced_node = reduce(globals, fun_defs, globals[*idx].term.clone());
          drop(borrow);
          let mut borrow = node.borrow_mut();
          *borrow = (*reduced_node.borrow()).clone();
          node.clone()
        },
        Graph::Cse(bod) => {
          trail.push(node.clone());
          bod.clone()
        }
        Graph::Dat(bod) => {
          let len = trail.len();
          if len > 0 {
            let redex = trail[len-1].clone();
            match &*redex.borrow() {
              Graph::Cse(_) => (),
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
        Graph::Ann(_, exp) => {
          let reduced_node = reduce(globals, fun_defs, exp.clone());
          drop(borrow);
          let mut borrow = node.borrow_mut();
          *borrow = (*reduced_node.borrow()).clone();
          node.clone()
        }
        Graph::Let(_, _, exp, Closure { idx, env }) => {
          let reduced_node = build_graph(true, globals, fun_defs, *idx, env, exp.clone());
          drop(borrow);
          let mut borrow = node.borrow_mut();
          *borrow = (*reduced_node.borrow()).clone();
          node.clone()
        }
        Graph::Fix(_) => {
          // We have to clone the contents of `node` and drop its borrow only to
          // borrow and match it again, otherwise the borrow checker complains
          let arg = new_link(borrow.clone());
          drop(borrow);
          let mut borrow = node.borrow_mut();
          match &mut *borrow {
            Graph::Fix(Closure { idx, env }) => {
              let reduced_node = build_graph(true, globals, fun_defs, *idx, env, arg);
              *borrow = (*reduced_node.borrow()).clone();
            }
            _ => unreachable!(),
          }
          node.clone()
        }
        Graph::Lit(lit) => {
          let len = trail.len();
          if len > 0 {
            let redex = trail[len-1].clone();
            match &*redex.borrow() {
              Graph::Cse(_) => (),
              _ => break,
            };
            trail.pop();
            match expand_lit(lit) {
              Some(expand) => {
                let mut mut_ref = (*redex).borrow_mut();
                *mut_ref = expand;
              },
              None => break,
            };
            redex
          }
          else {
            break
          }
        }
        Graph::Opr(opr) => {
          let len = trail.len();
          if len == 0 && opr.arity() == 0 {
            let res = opr.apply0();
            if let Some(res) = res {
              new_link(Graph::Lit(res))
            }
            else {
              break;
            }
          }
          else if len >= 1 && opr.arity() == 1 {
            let app = trail[len-1].clone();
            let arg = match &*app.borrow() {
              Graph::App(_, arg) => arg.clone(),
              _ => break,
            };
            let arg = reduce(globals, fun_defs, arg);
            let borrow = arg.borrow();
            match &*borrow {
              Graph::Lit(x) => {
                let res = opr.apply1(x);
                if let Some(res) = res {
                  let top = trail.pop().unwrap();
                  {
                    let mut mut_ref = (*top).borrow_mut();
                    *mut_ref = Graph::Lit(res);
                  }
                  top
                }
                else {
                  break;
                }
              }
              _ => break,
            }
          }
          else if len >= 2 && opr.arity() == 2 {
            let app = trail[len-1].clone();
            let arg1 = match &*app.borrow() {
              Graph::App(_, arg) => arg.clone(),
              _ => break,
            };
            let app = trail[len-2].clone();
            let arg2 = match &*app.borrow() {
              Graph::App(_, arg) => arg.clone(),
              _ => break,
            };
            let arg1 = reduce(globals, fun_defs, arg1);
            let arg2 = reduce(globals, fun_defs, arg2);
            let borrow1 = arg1.borrow();
            let borrow2 = arg2.borrow();
            match (&*borrow1, &*borrow2) {
              (Graph::Lit(x), Graph::Lit(y)) => {
                let res = opr.apply2(x, y);
                if let Some(res) = res {
                  trail.pop();
                  let top = trail.pop().unwrap();
                  {
                    let mut mut_ref = (*top).borrow_mut();
                    *mut_ref = Graph::Lit(res);
                  }
                  top
                }
                else {
                  break;
                }
              }
              _ => break,
            }
          }
          else if len >= 3 && opr.arity() == 3 {
            let app = trail[len-1].clone();
            let arg1 = match &*app.borrow() {
              Graph::App(_, arg) => arg.clone(),
              _ => break,
            };
            let app = trail[len-2].clone();
            let arg2 = match &*app.borrow() {
              Graph::App(_, arg) => arg.clone(),
              _ => break,
            };
            let app = trail[len-3].clone();
            let arg3 = match &*app.borrow() {
              Graph::App(_, arg) => arg.clone(),
              _ => break,
            };
            let arg1 = reduce(globals, fun_defs, arg1);
            let arg2 = reduce(globals, fun_defs, arg2);
            let arg3 = reduce(globals, fun_defs, arg3);
            let borrow1 = arg1.borrow();
            let borrow2 = arg2.borrow();
            let borrow3 = arg3.borrow();
            match (&*borrow1, &*borrow2, &*borrow3) {
              (Graph::Lit(x), Graph::Lit(y), Graph::Lit(z)) => {
                let res = opr.apply3(x, y, z);
                if let Some(res) = res {
                  trail.pop();
                  trail.pop();
                  let top = trail.pop().unwrap();
                  {
                    let mut mut_ref = (*top).borrow_mut();
                    *mut_ref = Graph::Lit(res);
                  }
                  top
                }
                else {
                  break;
                }
              }
              _ => break,
            }
          }
          else {
            break;
          }
        }
        Graph::Unr(_, exp) => {
          let reduced_node = reduce(globals, fun_defs, exp.clone());
          drop(borrow);
          let mut borrow = node.borrow_mut();
          *borrow = (*reduced_node.borrow()).clone();
          node.clone()
        }
        _ => break,
      }
    };
    node = next_node;
  }
  // This is only correct because we are updating the redex nodes.
  node
}

pub fn expand_lit(lit: &Literal) -> Option<Graph> {
  let idx = match lit {
    Literal::Nat(n) => {
      if *n == BigUint::from(0u64) {
        ir::ZERO_EXPANSION
      }
      else {
        todo!()
        // Some(yatima!(
        //   "λ P z s => s #$0",
        //   Term::Lit(Pos::None, Literal::Nat(n - BigUint::from(1u64)))
        // ))
      }
    }
    Literal::Int(_) => panic!("Integer expansion not yet implemented"),
    Literal::Bits(t) => {
      let c = t.last();
      match c {
        None => {
          ir::NIL_EXPANSION
        }
        Some(_) => {
          todo!()
        //   Some(yatima!(
        //   "λ P n c => c #$0 #$1",
        //   Term::Lit(Pos::None, Literal::Bool(c)),
        //   Term::Lit(Pos::None, Literal::Bits(t))
        // )),
        }
      }
    }
    Literal::Bytes(t) => {
      let c = t.last();
      match c {
        None => {
          ir::NIL_EXPANSION
        },
        Some(_) => {
          todo!()
        //   Some(yatima!(
        //   "λ P n c => c #$0 #$1",
        //   Term::Lit(Pos::None, Literal::U8(c)),
        //   Term::Lit(Pos::None, Literal::Bytes(t))
        // )),
        }
      }
    }
    Literal::Text(t) => {
      let len = t.len_chars();
      if len == 0 {
        ir::NIL_EXPANSION
      }
      else {
          todo!()
      // Some(yatima!(
      //   "λ P n c => c #$0 #$1",
      //   Term::Lit(Pos::None, Literal::Char(c)),
      //   Term::Lit(Pos::None, Literal::Text(t))
      // )),
      }
    },
    Literal::Bool(true) => {
      ir::TRUE_EXPANSION
    },
    Literal::Bool(false) => {
      ir::FALSE_EXPANSION
    },
    _ => return None,
  };
  Some(Graph::Ref(idx))
}

pub fn stringify_graph(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  graph: Link<Graph>
) -> String {
  match &*graph.borrow() {
    Graph::App(fun, arg) => {
      let fun = stringify_graph(globals, fun_defs, fun.clone());
      let arg = stringify_graph(globals, fun_defs, arg.clone());
      format!("(App {} {})", fun, arg)
    }
    Graph::Lam(Closure { idx, env }) => {
      let code_str = stringify_code(globals, fun_defs, *idx, env);
      let arg_name = &fun_defs[*idx].arg_name;
      format!("(Lam {} {})", arg_name, code_str)
    }
    Graph::Var(_, nam) => {
      format!("(Var {})", nam)
    }
    Graph::Ref(idx) => {
      format!("(Ref {})", globals[*idx].name)
    },
    Graph::Typ => {
      format!("Typ")
    },
    Graph::All(uses, dom, Closure { idx, env }) => {
      let dom = stringify_graph(globals, fun_defs, dom.clone());
      let code_str = stringify_code(globals, fun_defs, *idx, env);
      let arg_name = &fun_defs[*idx].arg_name;
      format!("(All {} {} {} {})", uses, arg_name, dom, code_str)
    },
    Graph::Slf(Closure { idx, env }) => {
      let code_str = stringify_code(globals, fun_defs, *idx, env);
      let arg_name = &fun_defs[*idx].arg_name;
      format!("(Slf {} {})", arg_name, code_str)
    },
    Graph::Dat(bod) => {
      let bod = stringify_graph(globals, fun_defs, bod.clone());
      format!("(Dat {})", bod)
    },
    Graph::Cse(bod) => {
      let bod = stringify_graph(globals, fun_defs, bod.clone());
      format!("(Dat {})", bod)
    },
    Graph::Ann(typ, exp) => {
      let typ = stringify_graph(globals, fun_defs, typ.clone());
      let exp = stringify_graph(globals, fun_defs, exp.clone());
      format!("(Ann {} {})", typ, exp)
    },
    Graph::Let(uses, typ, exp, Closure { idx, env }) => {
      let typ = stringify_graph(globals, fun_defs, typ.clone());
      let exp = stringify_graph(globals, fun_defs, exp.clone());
      let code_str = stringify_code(globals, fun_defs, *idx, env);
      let arg_name = &fun_defs[*idx].arg_name;
      format!("(Let {} {} {} {} {})", uses, arg_name, typ, exp, code_str)
    },
    Graph::Fix(Closure { idx, env }) => {
      let code_str = stringify_code(globals, fun_defs, *idx, env);
      let arg_name = &fun_defs[*idx].arg_name;
      format!("(Fix {} {})", arg_name, code_str)
    },
    Graph::Lit(lit) => {
      format!("(Lit {})", lit)
    },
    Graph::LTy(lty) => {
      format!("(LTy {})", lty)
    },
    Graph::Opr(opr) => {
      format!("(Opr {})", opr)
    },
    Graph::Unr(_, exp) => {
      let exp = stringify_graph(globals, fun_defs, exp.clone());
      format!("{}", exp)
    },
  }
}

pub fn stringify_code(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  idx: usize,
  env: &Vec<Link<Graph>>
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
        let (fun_index, fun_env) = extract_env(code, &mut pc, arg_name, &env);
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
        let hash = stringify_graph(globals, fun_defs, env[index].clone());
        args.push(hash);
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
        let (fun_index, fun_env) = extract_env(code, &mut pc, arg_name, &env);
        let dom = args.pop().unwrap();
        let code_str = stringify_code(globals, fun_defs, fun_index, &fun_env);
        let arg_name = &fun_defs[fun_index].arg_name;
        args.push(format!("(All {} {} {} {})", code_to_uses(uses), arg_name, dom, code_str));
      },
      MK_SLF => {
        let (fun_index, fun_env) = extract_env(code, &mut pc, arg_name, &env);
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
        args.push(format!("(Ann {} {})", typ, exp));
      },
      MK_LET => {
        let uses = code[pc+1];
        pc = pc+1;
        let (fun_index, fun_env) = extract_env(code, &mut pc, arg_name, &env);
        let typ = args.pop().unwrap();
        let exp = args.pop().unwrap();
        let code_str = stringify_code(globals, fun_defs, fun_index, &fun_env);
        let arg_name = &fun_defs[fun_index].arg_name;
        args.push(format!("(Let {} {} {} {} {})", code_to_uses(uses), arg_name, typ, exp, code_str));
      },
      MK_FIX => {
        let (fun_index, fun_env) = extract_env(code, &mut pc, arg_name, &env);
        let code_str = stringify_code(globals, fun_defs, fun_index, &fun_env);
        let arg_name = &fun_defs[fun_index].arg_name;
        args.push(format!("(Fix {} {})", arg_name, code_str));
      },
      MK_LIT => {
        todo!()
      }
      MK_LTY => {
        let lty = code[pc+1];
        pc = pc+1;
        let lty: LitType = unsafe { mem::transmute(lty) };
        args.push(format!("(LTy {})", lty));
      }
      MK_OPR => {
        let typ_code = code[pc+1];
        let opr_code = code[pc+2];
        pc = pc+2;
        let opr = code_to_opr((typ_code, opr_code));
        args.push(format!("(Opr {})", opr));
      }
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
pub fn extract_env(
  code: &Vec<CODE>,
  pc: &mut usize,
  arg_name: &Name,
  env: &Vec<Link<Graph>>,
) -> (usize, Vec<Link<Graph>>) {
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
      fun_env.push(new_link(Graph::Var(0, arg_name.clone())));
    }
    else {
      fun_env.push(env[index-1].clone());
    }
    *pc = *pc+ENV_SIZE;
  }
  (fun_index, fun_env)
}
