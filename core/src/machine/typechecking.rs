use crate::{
  name::Name,
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
  collections::{
    btree_set::BTreeSet,
  },
};

use sp_multihash::{
  U32,
  StatefulHasher,
  Blake3Hasher,
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
  let typ_borrow = typ.borrow();
  match &*term_borrow {
    Graph::Lam(_, bod) => {
      match &*typ_borrow {
        Graph::All(_, lam_uses, dom, img) => {
          // Build the graphs for the lambda body and the image
          let lam_name = &fun_defs[bod.idx].arg_name;
          let all_name = &fun_defs[img.idx].arg_name;
          let bod = build_graph(false, globals, fun_defs, bod.idx, &bod.env, new_var(ctx.len(), lam_name.clone()));
          let img = build_graph(true, globals, fun_defs, img.idx, &img.env, new_var(ctx.len(), all_name.clone()));
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
      match &*(typ.clone().borrow()) {
        Graph::Slf(_, slf_bod) => {
          // The type of the body of the data must be the body of the self with term
          // substituted for its variable.
          let arg = Rc::new(RefCell::new(term_borrow.clone()));
          let unrolled_slf = build_graph(true, globals, fun_defs, slf_bod.idx, &slf_bod.env, arg);
          check(globals, mut_globals, fun_defs, ctx, uses, bod.clone(), unrolled_slf)?;
          Ok(())
        }
        _ => {
          Err(CheckError::GenericError(format!("Data not a self type")))
        }
      }
    }
    _ => {
      let depth = ctx.len();
      let detected_typ = infer(globals, mut_globals, fun_defs, ctx, uses, term.clone())?;
      drop(typ_borrow);
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
    // Graph::App(_, fun_arg) => infer_app(defs, ctx, uses, pos, &fun_arg.0, &fun_arg.1),
    // Graph::Cse(_, exp) => infer_cse(defs, ctx, uses, pos, exp),
    // Graph::All(_, _, nam, dom_img) => infer_all(defs, ctx, nam, &dom_img.0, &dom_img.1),
    // Graph::Slf(_, nam, bod) => infer_slf(defs, ctx, term, nam, bod),
    // Graph::Let(_, false, exp_uses, nam, triple) => {
    //   infer_let(defs, ctx, uses, pos, *exp_uses, nam, &triple.0, &triple.1, &triple.2)
    // }
    Graph::Typ(_) => {
      // The reason term can be cloned is that Typ nodes are never mutably borrowed
      Ok(term.clone())
    }
    Graph::Ann(_, typ, exp) => {
      // Type annotations might mutate but this is not a problem since it does not
      // ever appear in term position. This might change in the future though, and
      // we will have to do a full copy of its graph
      check(globals, mut_globals, fun_defs, ctx, uses, exp.clone(), typ.clone())?;
      Ok(typ.clone())
    }
    Graph::Lam(..) => Err(CheckError::GenericError(format!("Untyped lambda"))),
    Graph::Dat(..) => Err(CheckError::GenericError(format!("Untyped data"))),
    _ => Err(CheckError::GenericError(format!("TODO"))),
  }
}

// #[inline]
// pub fn infer_app(
//   defs: &Defs,
//   ctx: &mut Ctx,
//   uses: Uses,
//   pos: &Pos,
//   fun: &Term,
//   arg: &Term,
// ) -> Result<DAG, CheckError> {
//   let mut fun_typ = infer(defs, ctx, uses, fun)?;
//   fun_typ.whnf(defs);
//   match fun_typ.head {
//     Graph::All(link) => {
//       let All { uses: lam_uses, dom, img, .. } = unsafe { &mut *link.as_ptr() };
//       let Lam { var, bod: img, .. } = unsafe { &mut *img.as_ptr() };
//       check(defs, ctx, *lam_uses * uses, arg, &mut DAG::new(*dom))?;
//       let mut map = BTreeMap::new();
//       if var.parents.is_some() {
//         map.insert(
//           Graph::Var(NonNull::new(var).unwrap()),
//           DAG::from_term_inner(arg, ctx.len() as u64, BTreeMap::new(), None),
//         );
//       }
//       let root = alloc_val(DLL::singleton(ParentPtr::Root));
//       let new_img = DAG::from_subdag(*img, &mut map, Some(root));
//       fun_typ.free();
//       Ok(DAG::new(new_img))
//     }
//     _ => Err(CheckError::AppFunMismatch(
//       *pos,
//       error_context(&ctx),
//       fun.clone(),
//       fun_typ.to_term(false),
//     )),
//   }
// }

// #[inline]
// pub fn infer_cse(
//   defs: &Defs,
//   ctx: &mut Ctx,
//   uses: Uses,
//   pos: &Pos,
//   exp: &Term,
// ) -> Result<DAG, CheckError> {
//   let mut exp_typ = infer(defs, ctx, uses, exp)?;
//   exp_typ.whnf(defs);
//   match exp_typ.head {
//     Graph::Slf(link) => {
//       let Slf { var, bod, .. } = unsafe { &mut *link.as_ptr() };
//       let mut map = BTreeMap::new();
//       if var.parents.is_some() {
//         map.insert(
//           Graph::Var(NonNull::new(var).unwrap()),
//           DAG::from_term_inner(exp, ctx.len() as u64, BTreeMap::new(), None),
//         );
//       }
//       let root = alloc_val(DLL::singleton(ParentPtr::Root));
//       let new_bod = DAG::from_subdag(*bod, &mut map, Some(root));
//       exp_typ.free();
//       Ok(DAG::new(new_bod))
//     }
//     _ => Err(CheckError::CseDatMismatch(
//       *pos,
//       error_context(&ctx),
//       exp.clone(),
//       exp_typ.to_term(false),
//     )),
//   }
// }

// #[inline]
// pub fn infer_all(
//   defs: &Defs,
//   ctx: &mut Ctx,
//   nam: &Name,
//   dom: &Term,
//   img: &Term,
// ) -> Result<DAG, CheckError> {
//   let mut typ = DAG::from_term(&Graph::Typ(Pos::None));
//   check(defs, ctx, Uses::None, dom, &mut typ)?;
//   let mut dom_dag =
//     DAG::from_term_inner(dom, ctx.len() as u64, BTreeMap::new(), None);
//   ctx.push((nam.to_string(), Uses::None, &mut dom_dag));
//   check(defs, ctx, Uses::None, img, &mut typ)?;
//   ctx.pop();
//   free_dead_node(dom_dag);
//   Ok(typ)
// }

// #[inline]
// pub fn infer_slf(
//   defs: &Defs,
//   ctx: &mut Ctx,
//   term: &Term,
//   nam: &Name,
//   bod: &Term,
// ) -> Result<DAG, CheckError> {
//   let mut typ = DAG::from_term(&Graph::Typ(Pos::None));
//   let mut term_dag =
//     DAG::from_term_inner(term, ctx.len() as u64, BTreeMap::new(), None);
//   ctx.push((nam.to_string(), Uses::None, &mut term_dag));
//   check(defs, ctx, Uses::None, bod, &mut typ)?;
//   ctx.pop();
//   free_dead_node(term_dag);
//   Ok(typ)
// }

// #[inline]
// pub fn infer_let(
//   defs: &Defs,
//   ctx: &mut Ctx,
//   uses: Uses,
//   pos: &Pos,
//   exp_uses: Uses,
//   nam: &Name,
//   exp_typ: &Term,
//   exp: &Term,
//   bod: &Term,
// ) -> Result<DAG, CheckError> {
//   let exp_dag =
//     &mut DAG::new(DAG::from_term_inner(exp, ctx.len() as u64, BTreeMap::new(), None));
//   let root = alloc_val(DLL::singleton(ParentPtr::Root));
//   let exp_typ_dag = &mut DAG::new(DAG::from_term_inner(
//     exp_typ,
//     ctx.len() as u64,
//     BTreeMap::new(),
//     Some(root),
//   ));
//   check(defs, ctx, exp_uses * uses, exp, exp_typ_dag)?;
//   let rest_ctx = div_ctx(uses, ctx);
//   ctx.push((nam.to_string(), exp_uses, &mut exp_typ_dag.head));
//   let mut bod_typ = infer(defs, ctx, Uses::Once, bod)?;
//   let (_, rest, _) = ctx.last().unwrap();
//   // Have to check whether the rest 'contains' zero (i.e., zero is less than or
//   // equal to the rest), otherwise the variable was not used enough
//   if !Uses::lte(Uses::None, *rest) {
//     Err(CheckError::QuantityTooLittle(*pos, error_context(ctx), nam.to_string(), exp_uses, *rest))
//   }
//   else {
//     ctx.pop();
//     DAG::new(exp_typ_dag.head).free();
//     add_mul_ctx(uses, ctx, rest_ctx);
//     bod_typ.subst(ctx.len() as u64, exp_dag.head);
//     Ok(bod_typ)
//   }
// }
