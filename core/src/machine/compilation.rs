use crate::{
  name::Name,
  machine::freevars::FreeVars,
  machine::ir::*,
  machine::machine::*,
};

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

pub fn defs_to_globals(
  defs: &Vec<(Name, IR, IR)>,
  fun_defs: &mut Vec<FunCell>
) -> (Vec<DefCell>, Option<usize>) {
  let mut done = BTreeSet::new();
  let mut globals = vec![];
  let mut main_idx = None;
  for i in 0..defs.len() {
    if done.insert(i) {
      let (name, term_ir, typ_ir) = &defs[i];
      let term = ir_to_graph(term_ir, fun_defs);
      let typ_ = ir_to_graph(typ_ir, fun_defs);
      globals.push(DefCell {
        name: name.clone(),
        term,
        typ_,
      });
      if &*defs[i].0 == "main" {
        main_idx = Some(i);
      }
    }
  }
  (globals, main_idx)
}

pub fn ir_to_graph(
  ir: &IR,
  fun_defs: &mut Vec<FunCell>
) -> Link<Graph> {
  match ir {
    IR::Var(name, idx) => {
      let mut hasher = Blake3Hasher::default();
      update_hasher(&mut hasher, *idx as usize);
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Var(hash, *idx as usize, name.clone())
      ))
    },
    IR::Lam(name, _, bod) => {
      // It is assumed that bod has no free variables, since this function
      // will be called on top-level terms. It will be possible in the future,
      // however, to add a non-empty environment of only top-level terms, which
      // will give a better sharing of graphs.
      let mut hasher = Blake3Hasher::default();
      update_hasher(&mut hasher, MK_LAM as usize);
      let pos = compile_ir(&mut hasher, name.clone(), &bod, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Lam(hash, clos)
      ))
    },
    IR::App(fun, arg) => {
      let fun = ir_to_graph(fun, fun_defs);
      let arg = ir_to_graph(arg, fun_defs);
      let mut hasher = Blake3Hasher::default();
      hasher.update(get_u8_hash(&arg.borrow()));
      hasher.update(get_u8_hash(&fun.borrow()));
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
    IR::Typ => {
      let mut hasher = Blake3Hasher::default();
      update_hasher(&mut hasher, MK_TYP as usize);
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Typ(hash)
      ))
    },
    IR::All(uses, name, dom, _, img) => {
      let mut hasher = Blake3Hasher::default();
      let dom = ir_to_graph(dom, fun_defs);
      hasher.update(get_u8_hash(&dom.borrow()));
      update_hasher(&mut hasher, MK_ALL as usize);
      update_hasher(&mut hasher, *uses as usize);
      let pos = compile_ir(&mut hasher, name.clone(), &img, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::All(hash, *uses, dom, clos)
      ))
    },
    IR::Slf(name, _, bod) => {
      let mut hasher = Blake3Hasher::default();
      update_hasher(&mut hasher, MK_SLF as usize);
      let pos = compile_ir(&mut hasher, name.clone(), &bod, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Slf(hash, clos)
      ))
    },
    IR::Dat(bod) => {
      let bod = ir_to_graph(bod, fun_defs);
      let mut hasher = Blake3Hasher::default();
      hasher.update(get_u8_hash(&bod.borrow()));
      update_hasher(&mut hasher, MK_DAT as usize);
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Dat(hash, bod)
      ))
    },
    IR::Cse(bod) => {
      let bod = ir_to_graph(bod, fun_defs);
      let mut hasher = Blake3Hasher::default();
      hasher.update(get_u8_hash(&bod.borrow()));
      update_hasher(&mut hasher, MK_CSE as usize);
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Cse(hash, bod)
      ))
    },
    IR::Ann(typ, exp) => {
      let typ = ir_to_graph(typ, fun_defs);
      let exp = ir_to_graph(exp, fun_defs);
      let mut hasher = Blake3Hasher::default();
      hasher.update(get_u8_hash(&exp.borrow()));
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Ann(hash, typ, exp)
      ))
    },
    IR::Let(uses, name, typ, exp, _, bod) => {
      let mut hasher = Blake3Hasher::default();
      let typ = ir_to_graph(typ, fun_defs);
      let exp = ir_to_graph(exp, fun_defs);
      // Use and type annotations do not matter
      hasher.update(get_u8_hash(&exp.borrow()));
      update_hasher(&mut hasher, MK_LET as usize);
      let pos = compile_ir(&mut hasher, name.clone(), &bod, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Let(hash, *uses, typ, exp, clos)
      ))
    },
    IR::Fix(name, _, bod) => {
      let mut hasher = Blake3Hasher::default();
      update_hasher(&mut hasher, MK_FIX as usize);
      let pos = compile_ir(&mut hasher, name.clone(), &bod, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      let hash = hasher.finalize();
      Rc::new(RefCell::new(
        Graph::Fix(hash, clos)
      ))
    }
  }
}

// TODO: change `fun_defs` into a hashmap as to not duplicate lambdas
pub fn compile_ir(
  hasher: &mut Blake3Hasher<U32>,
  name: Name,
  ir: &IR,
  env: &FreeVars,
  fun_defs: &mut Vec<FunCell>
) -> usize {

  fn go(
    is_proj: bool,
    hasher: &mut Blake3Hasher<U32>,
    ir: &IR,
    code: &mut Vec<CODE>,
    env: &FreeVars,
    fun_defs: &mut Vec<FunCell>
  ) {
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
        code.push(MK_LAM);
        compile_closure(name.clone(), free, hasher, bod, code, fun_defs);
        update_hasher(hasher, MK_LAM as usize);
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
      IR::Typ => {
        code.push(MK_TYP)
      },
      IR::All(uses, name, dom, free, img) => {
        go(false, hasher, dom, code, env, fun_defs);
        let uses = uses_to_code(*uses);
        code.push(MK_ALL);
        code.push(uses);
        update_hasher(hasher, MK_ALL as usize);
        update_hasher(hasher, uses as usize);
        compile_closure(name.clone(), free, hasher, img, code, fun_defs);
      },
      IR::Slf(name, free, bod) => {
        code.push(MK_SLF);
        compile_closure(name.clone(), free, hasher, bod, code, fun_defs);
        update_hasher(hasher, MK_SLF as usize);
      },
      IR::Dat(bod) => {
        go(false, hasher, bod, code, env, fun_defs);
        code.push(MK_DAT);
        update_hasher(hasher, MK_DAT as usize);
      },
      IR::Cse(bod) => {
        go(false, hasher, bod, code, env, fun_defs);
        code.push(MK_CSE);
        update_hasher(hasher, MK_CSE as usize);
      },
      IR::Ann(typ, exp) => {
        // Ann ignores typ when evaluating, thus we pass on `is_proj` to exp alone
        // Also annotations are not hashed
        go(is_proj, hasher, exp, code, env, fun_defs);
        go(false, &mut Blake3Hasher::default(), typ, code, env, fun_defs);
        code.push(MK_ANN);
      },
      IR::Let(uses, name, typ, exp, free, bod) => {
        // Let ignores typ similarly to Ann
        go(is_proj, hasher, exp, code, env, fun_defs);
        go(false, &mut Blake3Hasher::default(), typ, code, env, fun_defs);
        let uses = uses_to_code(*uses);
        code.push(MK_LET);
        code.push(uses);
        update_hasher(hasher, MK_LET as usize);
        compile_closure(name.clone(), free, hasher, bod, code, fun_defs);
      },
      IR::Fix(name, free, bod) => {
        code.push(MK_FIX);
        compile_closure(name.clone(), free, hasher, bod, code, fun_defs);
        update_hasher(hasher, MK_FIX as usize);
      }
    }
  }

  let mut code = vec![];
  go(true, hasher, ir, &mut code, env, fun_defs);
  code.push(END);
  let hash = hasher.finalize();
  fun_defs.push(FunCell {
    arg_name: name.clone(),
    code,
    hash,
  });
  fun_defs.len()-1
}

#[inline]
pub fn compile_closure(
  name: Name,
  free: &FreeVars,
  hasher: &mut Blake3Hasher<U32>,
  bod: &IR,
  code: &mut Vec<CODE>,
  fun_defs: &mut Vec<FunCell>
) {
  let mut new_hasher = Blake3Hasher::default();
  let pos = compile_ir(&mut new_hasher, name, bod, free, fun_defs);
  let hash = new_hasher.finalize();
  let bytes = usize_to_bytes::<MAP_SIZE>(pos);
  code.extend_from_slice(&bytes);
  let bytes = usize_to_bytes::<ENV_SIZE>(free.len());
  code.extend_from_slice(&bytes);
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
