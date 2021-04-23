use crate::machine::freevars::FreeVars;
use crate::machine::ir::*;
use crate::machine::machine::*;

use std::rc::Rc;
use std::cell::RefCell;

pub fn ir_to_graph(ir: &IR, fun_defs: &mut Vec<Vec<CODE>>) -> Link<Graph> {
  match ir {
    IR::Var(name, idx) => Rc::new(RefCell::new(
      Graph::Var(*idx, name.clone())
    )),
    IR::Lam(_, _, bod) => {
      // It is assumed that bod has no free variables, since this function
      // will be called on top-level terms. It will be possible in the future,
      // however, to add a non-empty environment of only top-level terms, which
      // will give a better sharing of graphs.
      let (hash, pos) = compile_ir(&bod, &FreeVars::new(), fun_defs);
      Rc::new(RefCell::new(
        Graph::Fun(hash, pos, vec![])
      ))
    },
    IR::App(fun, arg) => {
      let fun = ir_to_graph(fun, fun_defs);
      let arg = ir_to_graph(arg, fun_defs);
      let hash =
        make_hash(&(MK_APP as u64 + get_hash(&fun.borrow()) + get_hash(&arg.borrow())));
      Rc::new(RefCell::new(
        Graph::App(hash, fun, arg)
      ))
    },
  }
}

// TODO: change `fun_defs` into a hashmap as to not duplicate lambdas
pub fn compile_ir(ir: &IR, env: &FreeVars, fun_defs: &mut Vec<Vec<CODE>>) -> (u64, usize) {
  fn go(is_proj: bool, ir: &IR, code: &mut Vec<CODE>, env: &FreeVars, fun_defs: &mut Vec<Vec<CODE>>) {
    match ir {
      IR::Var(_, idx) => {
        if *idx == 0 {
          code.push(REF_ARG);
        }
        else {
          // The environment of free variables we get is one depth less than the depth of
          // the node we get, which is why we must correct the indices
          if let Some(pos) = env.get(idx-1){
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
      IR::Lam(_, free, bod) => {
        let (_, pos) = compile_ir(bod, &free, fun_defs);
        code.push(MK_FUN);
        let bytes = usize_to_bytes::<MAP_SIZE>(pos);
        code.extend_from_slice(&bytes);
        let bytes = usize_to_bytes::<ENV_SIZE>(free.len());
        code.extend_from_slice(&bytes);
        for pos in 0..free.len() {
          let idx = free.peek()[pos];
          // Index 0 is a reference to the argument of the function, other indices are
          // references to the environment. We encode the difference by a trick: 0 is
          // argument, otherwise n = pos+1, where pos is the position of the variable
          // in the environment
          if idx == 0 {
            let bytes = usize_to_bytes::<ENV_SIZE>(0);
            code.extend_from_slice(&bytes);
          }
          else {
            let bytes = usize_to_bytes::<ENV_SIZE>(pos+1);
            code.extend_from_slice(&bytes);
          }
        }
      },
      IR::App(fun, arg) => {
        go(false, arg, code, env, fun_defs);
        go(false, fun, code, env, fun_defs);
        code.push(MK_APP);
      },
    }
  }

  let mut code = vec![];
  go(true, ir, &mut code, env, fun_defs);
  code.push(END);
  let hash = make_hash(&code);
  fun_defs.push(code);
  (hash, fun_defs.len())
}
