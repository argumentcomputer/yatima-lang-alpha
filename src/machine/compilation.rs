use crate::machine::freevars::FreeVars;
use crate::machine::ir::*;
use crate::machine::machine::*;

// TODO: change `fun_defs` into a hashmap as to not duplicate lambdas
pub fn compile_ir(ir: &IR, env: &FreeVars, fun_defs: &mut Vec<Vec<CODE>>) -> (u64, usize) {
  fn go(ir: &IR, code: &mut Vec<CODE>, env: &FreeVars, fun_defs: &mut Vec<Vec<CODE>>) -> u64 {
    match ir {
      IR::Var(_, idx) => {
        if *idx == 0 {
          code.push(REF_ARG);
          0
        }
        else {
          if let Some(pos) = env.get(idx-1){
            let bytes = usize_to_bytes::<ENV_SIZE>(pos);
            code.push(REF_ENV);
            code.extend_from_slice(&bytes);
            pos as u64
          }
          else {
            // Are we ever going to need functions that purposefully build free variables?
            // If so, then we should build a variable node here. For now, it is panicking
            panic!("Unbound variable")
          }
        }
      },
      IR::Lam(_, free, bod) => {
        todo!()
      },
      IR::App(fun, arg) => {
        let hash_arg = go(arg, code, env, fun_defs);
        let hash_fun = go(fun, code, env, fun_defs);
        code.push(MK_APP);
        make_hash(MK_APP as u64 + hash_fun + hash_arg)
      },
    }
  }

  let mut code = vec![];
  let hash = go(ir, &mut code, env, fun_defs);
  code.push(END);
  fun_defs.push(code);
  (hash, fun_defs.len())
}
