use crate::{
  name::Name,
  machine::freevars::FreeVars,
  machine::ir::*,
  machine::machine::*,
};

use sp_std::{
  vec::Vec,
  collections::btree_set::BTreeSet,
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
      new_link(Graph::Var(*idx as usize, name.clone()))
    },
    IR::Lam(name, _, bod) => {
      // It is assumed that bod has no free variables, since this function
      // will be called on top-level terms. It will be possible in the future,
      // however, to add a non-empty environment of only top-level terms, which
      // will give a better sharing of graphs.
      let pos = compile_ir(name.clone(), &bod, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      new_link(Graph::Lam(clos))
    },
    IR::App(fun, arg) => {
      let fun = ir_to_graph(fun, fun_defs);
      let arg = ir_to_graph(arg, fun_defs);
      new_link(Graph::App(fun, arg))
    },
    IR::Ref(idx) => {
      new_link(Graph::Ref(*idx))
    },
    IR::Typ => {
      new_link(Graph::Typ)
    },
    IR::All(uses, name, dom, _, img) => {
      let dom = ir_to_graph(dom, fun_defs);
      let pos = compile_ir(name.clone(), &img, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      new_link(Graph::All(*uses, dom, clos))
    },
    IR::Slf(name, _, bod) => {
      let pos = compile_ir(name.clone(), &bod, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      new_link(Graph::Slf(clos))
    },
    IR::Dat(bod) => {
      let bod = ir_to_graph(bod, fun_defs);
      new_link(Graph::Dat(bod))
    },
    IR::Cse(bod) => {
      let bod = ir_to_graph(bod, fun_defs);
      new_link(Graph::Cse(bod))
    },
    IR::Ann(typ, exp) => {
      let typ = ir_to_graph(typ, fun_defs);
      let exp = ir_to_graph(exp, fun_defs);
      new_link(Graph::Ann(typ, exp))
    },
    IR::Let(uses, name, typ, exp, _, bod) => {
      let typ = ir_to_graph(typ, fun_defs);
      let exp = ir_to_graph(exp, fun_defs);
      // Use and type annotations do not matter
      let pos = compile_ir(name.clone(), &bod, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      new_link(Graph::Let(*uses, typ, exp, clos))
    },
    IR::Fix(name, _, bod) => {
      let pos = compile_ir(name.clone(), &bod, &FreeVars::new(), fun_defs);
      let clos = Closure {
        idx: pos,
        env: vec![],
      };
      new_link(Graph::Fix(clos))
    }
    IR::Lit(lit) => {
      new_link(Graph::Lit(lit.clone()))
    }
    IR::LTy(lty) => {
      new_link(Graph::LTy(*lty))
    }
    IR::Opr(opr) => {
      new_link(Graph::Opr(*opr))
    }
  }
}

// TODO: change `fun_defs` into a hashmap as to not duplicate lambdas
pub fn compile_ir(
  name: Name,
  ir: &IR,
  env: &FreeVars,
  fun_defs: &mut Vec<FunCell>
) -> usize {
  fn go(
    is_proj: bool,
    ir: &IR,
    code: &mut Vec<CODE>,
    env: &FreeVars,
    fun_defs: &mut Vec<FunCell>
  ) {
    match ir {
      IR::Var(_, idx) => {
        if *idx == 0 {
          code.push(REF_ARG);
        }
        else {
          // The environment of free variables we get is one depth less than the depth of
          // the node we get, which is why we must correct the indices
          if let Some(pos) = env.search(idx-1){
            let bytes = usize_to_bytes::<ENV_SIZE>(pos);
            code.push(REF_ENV);
            code.extend_from_slice(&bytes);
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
        compile_closure(name.clone(), free, env, bod, code, fun_defs);
      },
      IR::App(fun, arg) => {
        // Argument first, function second
        go(false, arg, code, env, fun_defs);
        go(false, fun, code, env, fun_defs);
        code.push(MK_APP);
      },
      IR::Ref(idx) => {
        let bytes = usize_to_bytes::<GBL_SIZE>(*idx);
        code.push(REF_GBL);
        code.extend_from_slice(&bytes);
      }
      IR::Typ => {
        code.push(MK_TYP)
      },
      IR::All(uses, name, dom, free, img) => {
        go(false, dom, code, env, fun_defs);
        let uses = uses_to_code(*uses);
        code.push(MK_ALL);
        code.push(uses);
        compile_closure(name.clone(), free, env, img, code, fun_defs);
      },
      IR::Slf(name, free, bod) => {
        code.push(MK_SLF);
        compile_closure(name.clone(), free, env, bod, code, fun_defs);
      },
      IR::Dat(bod) => {
        go(false, bod, code, env, fun_defs);
        code.push(MK_DAT);
      },
      IR::Cse(bod) => {
        go(false, bod, code, env, fun_defs);
        code.push(MK_CSE);
      },
      IR::Ann(typ, exp) => {
        // Ann ignores typ when evaluating, thus we pass on `is_proj` to exp alone
        go(is_proj, exp, code, env, fun_defs);
        go(false, typ, code, env, fun_defs);
        code.push(MK_ANN);
      },
      IR::Let(uses, name, typ, exp, free, bod) => {
        // Let ignores typ similarly to Ann
        go(is_proj, exp, code, env, fun_defs);
        go(false, typ, code, env, fun_defs);
        let uses = uses_to_code(*uses);
        code.push(MK_LET);
        code.push(uses);
        compile_closure(name.clone(), free, env, bod, code, fun_defs);
      },
      IR::Fix(name, free, bod) => {
        code.push(MK_FIX);
        compile_closure(name.clone(), free, env, bod, code, fun_defs);
      }
      IR::Lit(lit) => {
        code.push(MK_LIT);
        let (lit_type, lit) = lit_to_code(lit);
        code.push(lit_type);
        code.extend_from_slice(&lit);
      }
      IR::LTy(lty) => {
        code.push(MK_LTY);
        code.push(*lty as u8);
      }
      IR::Opr(opr) => {
        let (typ_code, opr_code) = opr_to_code(*opr);
        code.push(MK_OPR);
        code.push(typ_code as u8);
        code.push(opr_code as u8);
      }
    }
  }

  let mut code = vec![];
  go(true, ir, &mut code, env, fun_defs);
  code.push(END);
  fun_defs.push(FunCell {
    arg_name: name.clone(),
    code,
  });
  fun_defs.len()-1
}

#[inline]
pub fn compile_closure(
  name: Name,
  free: &FreeVars,
  env: &FreeVars,
  bod: &IR,
  code: &mut Vec<CODE>,
  fun_defs: &mut Vec<FunCell>
) {
  let pos = compile_ir(name, bod, free, fun_defs);
  let bytes = usize_to_bytes::<MAP_SIZE>(pos);
  code.extend_from_slice(&bytes);
  let bytes = usize_to_bytes::<ENV_SIZE>(free.len());
  code.extend_from_slice(&bytes);
  for idx in free.peek() {
    // Index 0 is a reference to the argument of the function, other indices are
    // references to the environment. We encode the difference by a trick: 0 is
    // argument, otherwise n = pos+1, where pos is the position of the variable
    // in the environment
    if *idx == 0 {
      let bytes = usize_to_bytes::<ENV_SIZE>(0);
      code.extend_from_slice(&bytes);
    }
    else {
      if let Some(pos) = env.search(*idx-1){
        let bytes = usize_to_bytes::<ENV_SIZE>(pos+1);
        code.extend_from_slice(&bytes);
      }
      else {
        panic!("Unbound variable {} at {:?}", idx-1, free.peek())
      }
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
