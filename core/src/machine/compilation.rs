use crate::machine::{
  freevars::FreeVars,
  ir::*,
  machine::*,
};
use crate::name::Name;

use sp_std::{
  rc::Rc,
  vec::Vec,
  cell::RefCell,
  collections::btree_set::BTreeSet,
};

use sp_multihash::{
  U32,
  StatefulHasher,
  Blake3Hasher,
};

pub fn defs_to_globals(defs: &Vec<(Name, IR)>, fun_defs: &mut Vec<FunCell>) -> (Vec<DefCell>, Option<usize>) {
  let mut done = BTreeSet::new();
  let mut globals = vec![];
  let mut main_idx = None;
  for i in 0..defs.len() {
    if done.insert(i) {
      let new_global = ir_to_graph(&defs[i].1, fun_defs);
      globals.push(DefCell {
        name: defs[i].0.clone(),
        term: new_global,
      });
      if &*defs[i].0 == "main" {
        main_idx = Some(i);
      }
    }
  }
  (globals, main_idx)
}

pub fn ir_to_graph(ir: &IR, fun_defs: &mut Vec<FunCell>) -> Link<Graph> {
  match ir {
    IR::Var(name, idx) => {
      let mut hasher = Blake3Hasher::default();
      update_hasher(&mut hasher, MK_VAR as usize);
      update_hasher(&mut hasher, *idx as usize);
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Var(hash, name.clone())
      ))
    },
    IR::Lam(name, _, bod) => {
      // It is assumed that bod has no free variables, since this function
      // will be called on top-level terms. It will be possible in the future,
      // however, to add a non-empty environment of only top-level terms, which
      // will give a better sharing of graphs.
      let (hash, pos) = compile_ir(name.clone(), &bod, &FreeVars::new(), fun_defs);
      Rc::new(RefCell::new(
        Graph::Fun(hash, pos, vec![])
      ))
    },
    IR::App(fun, arg) => {
      let fun = ir_to_graph(fun, fun_defs);
      let arg = ir_to_graph(arg, fun_defs);
      let mut hasher = Blake3Hasher::default();
      hasher.update(get_hash(&arg.borrow()));
      hasher.update(get_hash(&fun.borrow()));
      update_hasher(&mut hasher, MK_APP as usize);
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::App(hash, fun, arg)
      ))
    },
    IR::Ref(idx) => {
      let mut hasher = Blake3Hasher::default();
      update_hasher(&mut hasher, REF_GBL as usize);
      update_hasher(&mut hasher, *idx as usize);
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Ref(hash, *idx)
      ))
    },
  }
}

// TODO: change `fun_defs` into a hashmap as to not duplicate lambdas
pub fn compile_ir(name: Name, ir: &IR, env: &FreeVars, fun_defs: &mut Vec<FunCell>) -> (Hash, usize) {
  fn go(is_proj: bool, hasher: &mut Blake3Hasher<U32>, ir: &IR, code: &mut Vec<CODE>, env: &FreeVars, fun_defs: &mut Vec<FunCell>) {
    match ir {
      IR::Var(_, idx) => {
        if *idx == 0 {
          code.push(REF_ARG);
          update_hasher(hasher, REF_ARG as usize);
        }
        else {
          // The environment of free variables we get is one depth less than the depth of
          // the node we get, which is why we must correct the indices
          if let Some(pos) = env.get(idx-1){
            let bytes = usize_to_bytes::<ENV_SIZE>(pos);
            code.push(REF_ENV);
            code.extend_from_slice(&bytes);
            update_hasher(hasher, REF_ENV as usize);
            update_hasher(hasher, (*idx-1) as usize);
          }
          else {
            // Are we ever going to need functions that purposefully build free variables?
            // If so, then we should build a variable node here. For now, it is panicking
            panic!("Unbound variable")
          }
        }
        // Projection functions should evaluate the node it is going to substitute for
        // the variable before substitution, as to not duplicate possible redexes
        if is_proj {
          code.push(EVAL);
        }
      },
      IR::Lam(name, free, bod) => {
        let (hash, pos) = compile_ir(name.clone(), bod, &free, fun_defs);
        code.push(MK_FUN);
        let bytes = usize_to_bytes::<MAP_SIZE>(pos);
        code.extend_from_slice(&bytes);
        let bytes = usize_to_bytes::<ENV_SIZE>(free.len());
        code.extend_from_slice(&bytes);
        update_hasher(hasher, MK_FUN as usize);
        hasher.update(hash.as_ref());
        for pos in 0..free.len() {
          let idx = free.peek()[pos];
          // Index 0 is a reference to the argument of the function, other indices are
          // references to the environment. We encode the difference by a trick: 0 is
          // argument, otherwise n = pos+1, where pos is the position of the variable
          // in the environment
          if idx == 0 {
            let bytes = usize_to_bytes::<ENV_SIZE>(0);
            code.extend_from_slice(&bytes);
            update_hasher(hasher, REF_ARG as usize);
          }
          else {
            let bytes = usize_to_bytes::<ENV_SIZE>(pos+1);
            code.extend_from_slice(&bytes);
            update_hasher(hasher, REF_ENV as usize);
            update_hasher(hasher, (idx-1) as usize);
          }
        }
      },
      IR::App(fun, arg) => {
        // Argument first, function second
        go(false, hasher, arg, code, env, fun_defs);
        go(false, hasher, fun, code, env, fun_defs);
        code.push(MK_APP);
        update_hasher(hasher, MK_APP as usize);
      },
      IR::Ref(idx) => {
        let bytes = usize_to_bytes::<GBL_SIZE>(*idx);
        code.push(REF_GBL);
        code.extend_from_slice(&bytes);
        update_hasher(hasher, REF_GBL as usize);
        update_hasher(hasher, *idx as usize);
      }
    }
  }

  let mut hasher = Blake3Hasher::default();
  let mut code = vec![];
  go(true, &mut hasher, ir, &mut code, env, fun_defs);
  code.push(END);
  let hash = hasher.finalize();
  fun_defs.push(FunCell {
    arg_name: name.clone(),
    code,
    hash,
  });
  (hash, fun_defs.len()-1)
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{
    defs::Defs,
    parse::term::parse,
  };

  #[test]
  fn compilation_test() {
    let (_, x) = parse("λ x y z => y (λ w => w x z) z", Defs::new()).unwrap();
    let string = "(Lam x (Lam y (Lam z (App (App (Var y) (Lam w (App (App (Var w) (Var x)) (Var z)))) (Var z)))))";
    let x = term_to_ir(&x);
    let mut fun_defs = vec![];
    let graph = ir_to_graph(&x, &mut fun_defs);
    assert_eq!(string, stringify_graph(&fun_defs, graph));
    let (_, x) = parse("λ x y => y (λ z w => w x z y) x", Defs::new()).unwrap();
    let string = "(Lam x (Lam y (App (App (Var y) (Lam z (Lam w (App (App (App (Var w) (Var x)) (Var z)) (Var y))))) (Var x))))";
    let x = term_to_ir(&x);
    let mut fun_defs = vec![];
    let graph = ir_to_graph(&x, &mut fun_defs);
    assert_eq!(string, stringify_graph(&fun_defs, graph));
  }
}
