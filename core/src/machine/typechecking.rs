use crate::{
  uses::Uses,
  defs::Defs,
  literal::{
    LitType,
    Literal,
  },
  machine::machine::*,
  machine::ir::*,
  machine::compilation::*,
  machine::equality::*,
};

use alloc::string::String;
use alloc::string::ToString;
use sp_std::{
  vec::Vec,
  rc::Rc,
  cell::RefCell,
  collections::{
    btree_map::BTreeMap,
  },
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
  fun_defs: &mut Vec<FunCell>,
  ctx: &mut Ctx,
  uses: Uses,
  term: Link<Graph>,
  typ: Link<Graph>,
) -> Result<(), CheckError> {
  let term_borrow = term.borrow();
  match &*term_borrow {
    Graph::Lam(bod) => {
      match &*typ.borrow() {
        Graph::All(lam_uses, dom, img) => {
          // Build the graphs for the lambda body and the image
          let lam_name = &fun_defs[bod.idx].arg_name;
          let all_name = &fun_defs[img.idx].arg_name;
          let bod = build_graph(false, globals, fun_defs, bod.idx, &bod.env, new_link(Graph::Var(ctx.len(), lam_name.clone())));
          let img = build_graph(true, mut_globals, fun_defs, img.idx, &img.env, new_link(Graph::Var(ctx.len(), all_name.clone())));
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
    Graph::Dat(bod) => {
      match &*typ.borrow() {
        Graph::Slf(slf_bod) => {
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
    Graph::Let(_, exp_typ, exp, Closure { idx, env }) => {
      // Since we don't have a good function for substituting free variables
      // we are going to be expanding let expressions
      let ann_exp = new_link(Graph::Ann(exp_typ.clone(), exp.clone()));
      let bod = build_graph(false, globals, fun_defs, *idx, env, ann_exp);
      check(globals, mut_globals, fun_defs, ctx, uses, bod, typ)
    }
    Graph::Fix(_) => {
      todo!()
      // let hex = hash_to_hex(hash);
      // if let Some(detected_typ) = unrolled_terms.get(&hex) {
      //   let depth = ctx.len();
      //   if equal(mut_globals, fun_defs, depth, typ, detected_typ.clone()) {
      //     Ok(())
      //   }
      //   else {
      //     Err(CheckError::GenericError(format!("Wrong types")))
      //   }
      // }
      // else {
      //   unrolled_terms.insert(hex, typ.clone());
      //   let arg = Rc::new(RefCell::new(Graph::Fix(*hash, Closure { idx: *idx, env: env.clone() })));
      //   let bod = build_graph(false, globals, fun_defs, *idx, env, arg);
      //   // This will remove all linear and affine variables, which cannot be used inside a fix
      //   let rest_ctx = div_ctx(Uses::Many, ctx);
      //   check(unrolled_terms, globals, mut_globals, fun_defs, ctx, uses, bod, typ)?; // TODO better error message
      //   add_ctx(ctx, rest_ctx);
      //   Ok(())
      // }
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
  fun_defs: &mut Vec<FunCell>,
  ctx: &mut Ctx,
  uses: Uses,
  term: Link<Graph>,
) -> Result<Link<Graph>, CheckError> {
  match &*term.borrow() {
    Graph::Var(dep, _) => {
      let (_, ctx_uses, typ) = ctx.get_mut(*dep).ok_or_else(|| {
        CheckError::GenericError(format!("Unbound variable"))
      })?;
      let subtract_use = (*ctx_uses - uses).ok_or_else(|| {
        CheckError::GenericError(format!("Variable used too much"))
      })?;
      *ctx_uses = subtract_use;
      Ok(typ.clone())
    },
    Graph::Ref(idx) => {
      Ok(mut_globals[*idx].typ_.clone())
    },
    Graph::App(fun, arg) => {
      let fun_typ = infer(globals, mut_globals, fun_defs, ctx, uses, fun.clone())?;
      match &*reduce(mut_globals, fun_defs, fun_typ).borrow() {
        Graph::All(lam_uses, dom, Closure { idx, env }) => {
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
    Graph::Cse(exp) => {
      let exp_typ = infer(globals, mut_globals, fun_defs, ctx, uses, exp.clone())?;
      match &*reduce(mut_globals, fun_defs, exp_typ.clone()).borrow() {
        Graph::Slf(Closure { idx, env }) => {
          // We need to fully clone exp so that it does not get reduced
          let cse_typ = build_graph(true, mut_globals, fun_defs, *idx, env, full_clone(exp.clone()));
          Ok(cse_typ)
        }
        _ => Err(CheckError::GenericError(format!(
          "Tried to case match on an expression which is not an inductive datatype"
        ))),
      }
    }
    Graph::All(_, dom, Closure { idx, env }) => {
      let typ = new_link(Graph::Typ);
      check(globals, mut_globals, fun_defs, ctx, Uses::None, dom.clone(), typ.clone())?;
      let name = &fun_defs[*idx].arg_name;
      let img = build_graph(false, globals, fun_defs, *idx, env, new_link(Graph::Var(ctx.len(), name.clone())));
      ctx.push((name.to_string(), Uses::None, full_clone(dom.clone())));
      check(globals, mut_globals, fun_defs, ctx, Uses::None, img, typ.clone())?;
      ctx.pop();
      Ok(typ)
    }
    Graph::Slf(Closure { idx, env }) => {
      let typ = new_link(Graph::Typ);
      let name = &fun_defs[*idx].arg_name;
      let bod = build_graph(false, globals, fun_defs, *idx, env, new_link(Graph::Var(ctx.len(), name.clone())));
      ctx.push((name.to_string(), Uses::None, full_clone(term.clone())));
      check(globals, mut_globals, fun_defs, ctx, Uses::None, bod, typ.clone())?;
      ctx.pop();
      Ok(typ)
    }
    Graph::Let(_, exp_typ, exp, Closure { idx, env }) => {
      // See remark at the Let case in `check`
      let ann_exp = new_link(Graph::Ann(exp_typ.clone(), exp.clone()));
      let bod = build_graph(false, globals, fun_defs, *idx, env, ann_exp);
      infer(globals, mut_globals, fun_defs, ctx, uses, bod)
      // let typ = reduce(mut_globals, fun_defs, full_clone(typ.clone()));
      // check(unrolled_terms, globals, mut_globals, fun_defs, ctx, *exp_uses * uses, exp.clone(), typ.clone())?;
      // let rest_ctx = div_ctx(uses, ctx);
      // let name = &fun_defs[*idx].arg_name;
      // let bod = build_graph(false, globals, fun_defs, *idx, env, new_var(ctx.len(), name.clone()));
      // ctx.push((name.to_string(), *exp_uses, typ.clone()));
      // let bod_typ = infer(unrolled_terms, globals, mut_globals, fun_defs, ctx, Uses::Once, bod)?;
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
    Graph::Typ => {
      // The reason term can be cloned is that Typ nodes are never mutably borrowed
      Ok(term.clone())
    }
    Graph::Ann(typ, exp) => {
      let typ = reduce(mut_globals, fun_defs, full_clone(typ.clone()));
      check(globals, mut_globals, fun_defs, ctx, uses, exp.clone(), typ.clone())?;
      Ok(typ)
    }
    Graph::Lam(..) => Err(CheckError::GenericError(format!("Untyped lambda"))),
    Graph::Dat(..) => Err(CheckError::GenericError(format!("Untyped data"))),
    Graph::Fix(_) => {
      todo!()
      // let hex = hash_to_hex(hash);
      // if let Some(typ) = unrolled_terms.get(&hex) {
      //   Ok(typ.clone())
      // }
      // else {
      //   Err(CheckError::GenericError(format!("Untyped fix")))
      // }
    }
    Graph::Lit(lit) => {
      let lty = infer_lit(lit);
      Ok(new_link(Graph::LTy(lty)))
    },
    Graph::LTy(_) => {
      let typ = new_link(Graph::Typ);
      Ok(typ)
    },
    Graph::Opr(opr) => {
      // TODO: Write something more efficient than compiling terms
      // perhaps constructing the graphs directly
      let typ = opr.type_of();
      // The types of opr contain no references, thus we can start with an empty Defs and Map
      let (ir, _) = term_to_ir(&Defs::new(), &mut BTreeMap::new(), &mut vec![], 0, &typ);
      let typ = ir_to_graph(&ir, fun_defs);
      Ok(typ)
    },
  }
}

pub fn infer_lit(lit: &Literal) -> LitType {
  match lit {
    Literal::Nat(_) => LitType::Nat,
    Literal::Int(_) => LitType::Int,
    Literal::Bytes(_) => LitType::Bytes,
    Literal::Bits(_) => LitType::Bits,
    Literal::Text(_) => LitType::Text,
    Literal::Char(_) => LitType::Char,
    Literal::Bool(_) => LitType::Bool,
    Literal::U8(_) => LitType::U8,
    Literal::U16(_) => LitType::U16,
    Literal::U32(_) => LitType::U32,
    Literal::U64(_) => LitType::U64,
    Literal::U128(_) => LitType::U128,
    Literal::I8(_) => LitType::I8,
    Literal::I16(_) => LitType::I16,
    Literal::I32(_) => LitType::I32,
    Literal::I64(_) => LitType::I64,
    Literal::I128(_) => LitType::I128,
  }
}
