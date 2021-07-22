use crate::{
  uses::Uses,
  machine::machine::*,
  machine::equality::*,
};

use alloc::string::String;
use alloc::string::ToString;
use sp_std::{
  vec::Vec,
  rc::Rc,
  cell::RefCell,
};

#[derive(Debug)]
pub enum CheckError {
  GenericError(String),
}

pub type Ctx = Vec<(String, Uses, Link<Graph>)>;

#[inline]
pub fn add_mul_ctx(uses: Uses, use_ctx: &mut Ctx, use_ctx2: Ctx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i].1 = (uses * use_ctx[i].1) + use_ctx2[i].1
  }
}

#[inline]
pub fn add_ctx(use_ctx: &mut Ctx, use_ctx2: Ctx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i].1 = use_ctx[i].1 + use_ctx2[i].1
  }
}

#[inline]
pub fn mul_ctx(uses: Uses, use_ctx: &mut Ctx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i].1 = use_ctx[i].1 * uses
  }
}

#[inline]
pub fn div_ctx(uses: Uses, use_ctx: &mut Ctx) -> Ctx {
  let mut rest = use_ctx.clone();
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    rest[i].1 = use_ctx[i].1 % uses;
    use_ctx[i].1 = use_ctx[i].1 / uses
  }
  rest
}

// The core typechecking function. It assumes typ is in whnf for performance,
// so make sure to call reduce on the type before checking
pub fn check(
  globals: &Vec<DefCell>,
  mut_globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  ctx: &mut Ctx,
  uses: Uses,
  term: Link<Graph>,
  typ: Link<Graph>,
) -> Result<(), CheckError> {
  let term_borrow = term.borrow();
  match &*term_borrow {
    Graph::Lam(_, bod) => {
      match &*typ.borrow() {
        Graph::All(_, lam_uses, dom, img) => {
          // Build the graphs for the lambda body and the image
          let lam_name = &fun_defs[bod.idx].arg_name;
          let all_name = &fun_defs[img.idx].arg_name;
          let bod = build_graph(false, globals, fun_defs, bod.idx, &bod.env, new_var(ctx.len(), lam_name.clone()));
          let img = build_graph(true, mut_globals, fun_defs, img.idx, &img.env, new_var(ctx.len(), all_name.clone()));
          // Adjust the context multiplicity, add the argument to the context and check the body
          let rest_ctx = div_ctx(uses, ctx);
          ctx.push((all_name.to_string(), *lam_uses, dom.clone()));
          check(globals, mut_globals, fun_defs, ctx, Uses::Once, bod, img)?;
          // Check whether the rest 'contains' zero (i.e., zero is less than or
          // equal to the rest), otherwise the variable was not used enough
          let (_, rest, _) = ctx.last().unwrap();
          if !Uses::lte(Uses::None, *rest) {
            Err(CheckError::GenericError(format!("Variable not used enough")))
          }
          else {
            // Remove the argument from the context, readjust the context
            // multiplicity
            ctx.pop();
            add_mul_ctx(uses, ctx, rest_ctx);
            Ok(())
          }
        }
        _ => {
          Err(CheckError::GenericError(format!("Lambda not a function")))
        }
      }
    }
    Graph::Dat(_, bod) => {
      match &*typ.borrow() {
        Graph::Slf(_, slf_bod) => {
          // The type of the body of the data must be the body of the self with term
          // substituted for its variable.
          let arg = Rc::new(RefCell::new(term_borrow.clone()));
          let unrolled_slf = build_graph(true, mut_globals, fun_defs, slf_bod.idx, &slf_bod.env, arg);
          check(globals, mut_globals, fun_defs, ctx, uses, bod.clone(), unrolled_slf)?;
          Ok(())
        }
        _ => {
          Err(CheckError::GenericError(format!("Data not a self type")))
        }
      }
    }
    Graph::Let(_, _, _, exp, Closure { idx, env }) => {
      // Since we don't have a good function for substituting free variables
      // we are going to be expanding let expressions
      let bod = build_graph(false, globals, fun_defs, *idx, env, exp.clone());
      check(globals, mut_globals, fun_defs, ctx, uses, bod, typ)
    }
    _ => {
      let depth = ctx.len();
      let detected_typ = infer(globals, mut_globals, fun_defs, ctx, uses, term.clone())?;
      if equal(mut_globals, fun_defs, depth, typ, detected_typ) {
        Ok(())
      }
      else {
        Err(CheckError::GenericError(format!("Wrong types")))
      }
    }
  }
}

pub fn infer(
  globals: &Vec<DefCell>,
  mut_globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  ctx: &mut Ctx,
  uses: Uses,
  term: Link<Graph>,
) -> Result<Link<Graph>, CheckError> {
  match &*term.borrow() {
    Graph::Var(_, dep, _) => {
      let (_, ctx_uses, typ) = ctx.get_mut(*dep).ok_or_else(|| {
        CheckError::GenericError(format!("Unbound variable"))
      })?;
      let subtract_use = (*ctx_uses - uses).ok_or_else(|| {
        CheckError::GenericError(format!("Variable used too much"))
      })?;
      *ctx_uses = subtract_use;
      Ok(typ.clone())
    },
    Graph::Ref(_, idx) => {
      Ok(mut_globals[*idx].typ_.clone())
    },
    Graph::App(_, fun, arg) => {
      let fun_typ = infer(globals, mut_globals, fun_defs, ctx, uses, fun.clone())?;
      match &*reduce(mut_globals, fun_defs, fun_typ).borrow() {
        Graph::All(_, lam_uses, dom, Closure { idx, env }) => {
          let dom = reduce(mut_globals, fun_defs, dom.clone());
          check(globals, mut_globals, fun_defs, ctx, *lam_uses * uses, arg.clone(), dom.clone())?;
          // We need to fully clone arg so that it does not get reduced
          let app_typ = build_graph(true, mut_globals, fun_defs, *idx, env, full_clone(arg.clone()));
          Ok(app_typ)
        }
        _ => Err(CheckError::GenericError(
          format!("Tried to apply an expression which is not a function")
        )),
      }
    },
    Graph::Cse(_, exp) => {
      let exp_typ = infer(globals, mut_globals, fun_defs, ctx, uses, exp.clone())?;
      match &*reduce(mut_globals, fun_defs, exp_typ.clone()).borrow() {
        Graph::Slf(_, Closure { idx, env }) => {
          // We need to fully clone exp so that it does not get reduced
          let cse_typ = build_graph(true, mut_globals, fun_defs, *idx, env, full_clone(exp.clone()));
          Ok(cse_typ)
        }
        _ => Err(CheckError::GenericError(format!(
          "Tried to case match on an expression which is not an inductive datatype"
        ))),
      }
    }
    Graph::All(_, _, dom, Closure { idx, env }) => {
      let typ = new_typ();
      check(globals, mut_globals, fun_defs, ctx, Uses::None, dom.clone(), typ.clone())?;
      let name = &fun_defs[*idx].arg_name;
      let img = build_graph(false, globals, fun_defs, *idx, env, new_var(ctx.len(), name.clone()));
      ctx.push((name.to_string(), Uses::None, full_clone(dom.clone())));
      check(globals, mut_globals, fun_defs, ctx, Uses::None, img, typ.clone())?;
      ctx.pop();
      Ok(typ)
  }
    Graph::Slf(_, Closure { idx, env }) => {
      let typ = new_typ();
      let name = &fun_defs[*idx].arg_name;
      let bod = build_graph(false, globals, fun_defs, *idx, env, new_var(ctx.len(), name.clone()));
      ctx.push((name.to_string(), Uses::None, full_clone(term.clone())));
      check(globals, mut_globals, fun_defs, ctx, Uses::None, bod, typ.clone())?;
      ctx.pop();
      Ok(typ)
  }
    Graph::Let(_, _, _, exp, Closure { idx, env }) => {
      // See remark at the Let case in `check`
      let bod = build_graph(false, globals, fun_defs, *idx, env, exp.clone());
      infer(globals, mut_globals, fun_defs, ctx, uses, bod)
      // let typ = reduce(mut_globals, fun_defs, full_clone(typ.clone()));
      // check(globals, mut_globals, fun_defs, ctx, *exp_uses * uses, exp.clone(), typ.clone())?;
      // let rest_ctx = div_ctx(uses, ctx);
      // let name = &fun_defs[*idx].arg_name;
      // let bod = build_graph(false, globals, fun_defs, *idx, env, new_var(ctx.len(), name.clone()));
      // ctx.push((name.to_string(), *exp_uses, typ.clone()));
      // let bod_typ = infer(globals, mut_globals, fun_defs, ctx, Uses::Once, bod)?;
      // let (_, rest, _) = ctx.last().unwrap();
      // // Have to check whether the rest 'contains' zero (i.e., zero is less than or
      // // equal to the rest), otherwise the variable was not used enough
      // if !Uses::lte(Uses::None, *rest) {
      //   Err(CheckError::GenericError(format!("Variable not used enough")))
      // }
      // else {
      //   ctx.pop();
      //   add_mul_ctx(uses, ctx, rest_ctx);
      //   // let bod_typ = subst(bod_typ, ctx.len(), full_clone(exp.clone()));
      //   Ok(bod_typ)
      // }
    }
    Graph::Typ(_) => {
      // The reason term can be cloned is that Typ nodes are never mutably borrowed
      Ok(term.clone())
    }
    Graph::Ann(_, typ, exp) => {
      let typ = reduce(mut_globals, fun_defs, full_clone(typ.clone()));
      check(globals, mut_globals, fun_defs, ctx, uses, exp.clone(), typ.clone())?;
      Ok(typ)
    }
    Graph::Lam(..) => Err(CheckError::GenericError(format!("Untyped lambda"))),
    Graph::Dat(..) => Err(CheckError::GenericError(format!("Untyped data"))),
    _ => Err(CheckError::GenericError(format!("TODO"))),
  }
}
