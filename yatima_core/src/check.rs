pub mod ctx;
pub mod error;

use ctx::*;
use error::CheckError;

use crate::{
  dag::*,
  defs::Defs,
  dll::*,
  literal::{
    LitType,
    Literal,
  },
  position::Pos,
  term::Term,
  uses::*,
};

use im::{
  HashMap,
  Vector,
};

use libipld::Cid;

use core::ptr::NonNull;
use std::collections::HashSet;

pub fn hash(dag: DAGPtr, dep: u64) -> Cid {
  let mut map = HashMap::new();
  DAG::dag_ptr_to_term(&dag, &mut map, dep, true).embed().0.cid()
}

pub fn equal(defs: &Defs, a: &mut DAG, b: &mut DAG, dep: u64) -> bool {
  a.whnf(defs);
  b.whnf(defs);
  let mut triples = vec![(a.head, b.head, dep)];
  let mut set: HashSet<(Cid, Cid)> = HashSet::new();
  while let Some((a, b, dep)) = triples.pop() {
    let mut a = DAG::new(a);
    let mut b = DAG::new(b);
    a.whnf(defs);
    b.whnf(defs);
    let hash_a = hash(a.head, dep);
    let hash_b = hash(b.head, dep);
    let eq = hash_a == hash_b
      || set.contains(&(hash_a, hash_b))
      || set.contains(&(hash_b, hash_a));
    set.insert((hash_a, hash_b));
    if !eq {
      match (a.head, b.head) {
        (DAGPtr::Lam(a_link), DAGPtr::Lam(b_link)) => unsafe {
          let Lam { bod: a_bod, .. } = *a_link.as_ptr();
          let Lam { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep + 1));
        },
        (DAGPtr::Slf(a_link), DAGPtr::Slf(b_link)) => unsafe {
          let Slf { bod: a_bod, .. } = *a_link.as_ptr();
          let Slf { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep + 1));
        },
        (DAGPtr::Cse(a_link), DAGPtr::Cse(b_link)) => unsafe {
          let Cse { bod: a_bod, .. } = *a_link.as_ptr();
          let Cse { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep));
        },
        (DAGPtr::Dat(a_link), DAGPtr::Dat(b_link)) => unsafe {
          let Dat { bod: a_bod, .. } = *a_link.as_ptr();
          let Dat { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep));
        },
        (DAGPtr::All(a_link), DAGPtr::All(b_link)) => unsafe {
          let All { uses: a_uses, dom: a_dom, img: a_img, .. } =
            *a_link.as_ptr();
          let All { uses: b_uses, dom: b_dom, img: b_img, .. } =
            *b_link.as_ptr();
          let a_img = DAGPtr::Lam(a_img);
          let b_img = DAGPtr::Lam(b_img);
          if a_uses != b_uses {
            return false;
          }
          triples.push((a_dom, b_dom, dep));
          triples.push((a_img, b_img, dep + 1));
        },
        (DAGPtr::App(a_link), DAGPtr::App(b_link)) => unsafe {
          let App { fun: a_fun, arg: a_arg, .. } = *a_link.as_ptr();
          let App { fun: b_fun, arg: b_arg, .. } = *b_link.as_ptr();
          triples.push((a_fun, b_fun, dep));
          triples.push((a_arg, b_arg, dep));
        },
        _ => return false,
      }
    }
  }
  true
}

pub fn check(
  defs: &Defs,
  mut ctx: Ctx,
  uses: Uses,
  term: &mut DAG,
  typ: &mut DAG,
) -> Result<UseCtx, CheckError> {
  match term.head {
    DAGPtr::Lam(lam_link) => {
      typ.whnf(defs);
      let Lam { bod: lam_bod, var: lam_var, pos: lam_pos, .. } =
        unsafe { &mut *lam_link.as_ptr() };
      match typ.head {
        DAGPtr::All(all_link) => {
          let All { uses: lam_uses, dom, img, .. } =
            unsafe { &mut *all_link.as_ptr() };
          let Lam { var: all_var, bod: img, .. } =
            unsafe { &mut *img.as_ptr() };
          // Annotate the depth of the node that binds each variable
          (*all_var).dep = ctx.len() as u64;
          (*lam_var).dep = ctx.len() as u64;
          // Add the domain of the function to the ctxcontext
          let mut lam_bod = DAG::new(*lam_bod);
          let mut img = DAG::new(*img);
          ctx.push_back((all_var.nam.to_string(), dom));
          let mut use_ctx =
            check(defs, ctx.clone(), Uses::Once, &mut lam_bod, &mut img)?;
          let infer_uses = use_ctx.pop_back().unwrap();
          if Uses::gth(infer_uses, *lam_uses) {
            Err(CheckError::QuantityMismatch(
              *lam_pos,
              error_context(&ctx),
              *lam_uses,
              infer_uses,
            ))
          }
          else {
            mul_use_ctx(uses, &mut use_ctx);
            Ok(use_ctx)
          }
        }
        _ => {
          let checked = term.to_term(false);
          let against = typ.to_term(false);
          Err(CheckError::LamAllMismatch(
            *lam_pos,
            error_context(&ctx),
            checked,
            against,
          ))
        }
      }
    }
    DAGPtr::Dat(dat_link) => {
      typ.whnf(defs);
      let Dat { bod: dat_bod, pos: dat_pos, .. } =
        unsafe { &mut *dat_link.as_ptr() };
      match typ.head {
        DAGPtr::Slf(slf_link) => {
          let Slf { var, bod: slf_bod, .. } =
            unsafe { &mut *slf_link.as_ptr() };
          let mut map = if var.parents.is_some() {
            HashMap::unit(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              DAG::from_subdag(term.head, &mut HashMap::new(), None),
            )
          }
          else {
            HashMap::new()
          };
          let root = alloc_val(DLL::singleton(ParentPtr::Root));
          let mut new_bod =
            DAG::new(DAG::from_subdag(*slf_bod, &mut map, Some(root)));
          let mut dat_bod = DAG::new(*dat_bod);
          let use_ctx = check(defs, ctx, uses, &mut dat_bod, &mut new_bod)?;
          new_bod.free();
          Ok(use_ctx)
        }
        _ => {
          let checked = term.to_term(false);
          let against = typ.to_term(false);
          Err(CheckError::DatSlfMismatch(
            *dat_pos,
            error_context(&ctx),
            checked,
            against,
          ))
        }
      }
    }
    DAGPtr::Let(let_link) => {
      let Let { pos: let_pos, .. } = unsafe { &mut *let_link.as_ptr() };
      Err(CheckError::GenericError(
        *let_pos,
        error_context(&ctx),
        "TODO: Let typecheck".to_string(),
      ))
    }
    _ => {
      let depth = ctx.len();
      let (use_ctx, mut infer_typ) = infer(defs, ctx.clone(), uses, term)?;
      let eq = equal(defs, typ, &mut infer_typ, depth as u64);
      if eq {
        infer_typ.free();
        Ok(use_ctx)
      }
      else {
        let expected = typ.to_term(false);
        let detected = infer_typ.to_term(false);
        infer_typ.free();
        Err(CheckError::TypeMismatch(
          DAG::pos(&term.head),
          error_context(&ctx),
          expected,
          detected,
        ))
      }
    }
  }
}

pub fn infer(
  defs: &Defs,
  mut ctx: Ctx,
  uses: Uses,
  term: &mut DAG,
) -> Result<(UseCtx, DAG), CheckError> {
  match term.head {
    DAGPtr::Var(mut link) => {
      let Var { dep, pos, nam, .. } = unsafe { link.as_mut() };
      let mut use_ctx: UseCtx = vec![Uses::None; ctx.len()].into();
      use_ctx[*dep as usize] = uses;
      let bind = ctx.get(*dep as usize).ok_or_else(|| {
        CheckError::UnboundVariable(
          *pos,
          error_context(&ctx),
          nam.clone(),
          *dep,
        )
      })?;
      let typ = unsafe {
        let dag = DAG::new(*bind.1);
        #[allow(clippy::redundant_clone)]
        dag.clone()
      };
      Ok((use_ctx, typ))
    }
    DAGPtr::Ref(mut link) => {
      let Ref { nam, exp: def_link, pos, .. } = unsafe { link.as_mut() };
      let def = defs
        .defs
        .get(def_link)
        .ok_or_else(|| CheckError::UndefinedReference(*pos, nam.clone()))?;
      let use_ctx = vec![Uses::None; ctx.len()].into();
      let typ = DAG::from_term(&def.typ_);
      Ok((use_ctx, typ))
    }
    DAGPtr::App(mut link) => {
      let App { fun, arg, .. } = unsafe { link.as_mut() };
      let mut fun = DAG::new(*fun);
      let mut arg = DAG::new(*arg);
      let (mut fun_use_ctx, mut fun_typ) =
        infer(defs, ctx.clone(), uses, &mut fun)?;
      fun_typ.whnf(defs);
      match fun_typ.head {
        DAGPtr::All(link) => {
          let All { uses: lam_uses, dom, img, .. } =
            unsafe { &mut *link.as_ptr() };
          let Lam { var, bod: img, .. } = unsafe { &mut *img.as_ptr() };
          let arg_use_ctx =
            check(defs, ctx, *lam_uses * uses, &mut arg, &mut DAG::new(*dom))?;
          add_use_ctx(&mut fun_use_ctx, arg_use_ctx);
          let mut map = if var.parents.is_some() {
            HashMap::unit(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              DAG::from_subdag(arg.head, &mut HashMap::new(), None),
            )
          }
          else {
            HashMap::new()
          };
          let root = alloc_val(DLL::singleton(ParentPtr::Root));
          let new_img = DAG::from_subdag(*img, &mut map, Some(root));
          fun_typ.free();
          Ok((fun_use_ctx, DAG::new(new_img)))
        }
        node => Err(CheckError::AppFunMismatch(
          DAG::pos(&node),
          error_context(&ctx),
          fun.to_term(false),
          fun_typ.to_term(false),
        )),
      }
    }
    DAGPtr::Cse(mut link) => {
      let Cse { bod: exp, .. } = unsafe { link.as_mut() };
      let mut exp = DAG::new(*exp);
      let (exp_use_ctx, mut exp_typ) =
        infer(defs, ctx.clone(), uses, &mut exp)?;
      exp_typ.whnf(defs);
      match exp_typ.head {
        DAGPtr::Slf(link) => {
          let Slf { var, bod, .. } = unsafe { &mut *link.as_ptr() };
          let mut map = if var.parents.is_some() {
            HashMap::unit(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              #[allow(clippy::redundant_clone)]
              DAG::from_subdag(exp.head, &mut HashMap::new(), None),
            )
          }
          else {
            HashMap::new()
          };
          let root = alloc_val(DLL::singleton(ParentPtr::Root));
          let new_bod = DAG::from_subdag(*bod, &mut map, Some(root));
          exp_typ.free();
          Ok((exp_use_ctx, DAG::new(new_bod)))
        }
        DAGPtr::LTy(link) => {
          let LTy { lty, .. } = unsafe { &mut *link.as_ptr() };
          let root = alloc_val(DLL::singleton(ParentPtr::Root));
          let induction = DAG::from_term_inner(
            &lty.induction(exp.to_term(true)),
            0,
            Vector::new(),
            Some(root),
            None,
          );
          Ok((exp_use_ctx, DAG::new(induction)))
        }
        node => Err(CheckError::CseDatMismatch(
          DAG::pos(&node),
          error_context(&ctx),
          exp.to_term(false),
          exp_typ.to_term(false),
        )),
      }
    }
    DAGPtr::All(link) => {
      let All { dom, img, pos: all_pos, .. } = unsafe { &mut *link.as_ptr() };
      let Lam { var, bod: img, .. } = unsafe { &mut *img.as_ptr() };
      let mut typ = DAG::from_term(&Term::Typ(*all_pos));
      let _ =
        check(defs, ctx.clone(), Uses::None, &mut DAG::new(*dom), &mut typ)?;
      (*var).dep = ctx.len() as u64;
      ctx.push_back((var.nam.to_string(), dom));
      let mut img = DAG::new(*img);
      let mut use_ctx = check(defs, ctx, Uses::None, &mut img, &mut typ)?;
      use_ctx.pop_back();
      Ok((use_ctx, typ))
    }
    DAGPtr::Slf(mut link) => {
      let Slf { var, bod, pos: slf_pos, .. } = unsafe { link.as_mut() };
      let mut typ = DAG::from_term(&Term::Typ(*slf_pos));
      (*var).dep = ctx.len() as u64;
      ctx.push_back((var.nam.to_string(), &mut term.head));
      let mut bod = DAG::new(*bod);
      let mut use_ctx = check(defs, ctx, Uses::None, &mut bod, &mut typ)?;
      use_ctx.pop_back();
      Ok((use_ctx, typ))
    }
    DAGPtr::Typ(link) => {
      let Typ { pos, .. } = unsafe { link.as_ref() };
      let use_ctx = vec![Uses::None; ctx.len()].into();
      let typ = DAG::from_term(&Term::Typ(*pos));
      Ok((use_ctx, typ))
    }
    DAGPtr::Ann(mut link) => {
      let Ann { typ, exp, .. } = unsafe { link.as_mut() };
      #[allow(clippy::redundant_clone)]
      let mut typ_clone = DAG::new(*typ).clone();
      let mut exp = DAG::new(*exp);
      let use_ctx = check(defs, ctx, uses, &mut exp, &mut typ_clone)?;
      Ok((use_ctx, typ_clone))
    }
    DAGPtr::Let(_) => {
      panic!("TODO: Let inference")
    }
    DAGPtr::Lit(mut link) => {
      let Lit { lit, .. } = unsafe { link.as_mut() };
      let use_ctx = vec![Uses::None; ctx.len()].into();
      Ok((use_ctx, DAG::from_term(&infer_lit(lit.to_owned()))))
    }
    DAGPtr::LTy(mut link) => {
      let LTy { lty, .. } = unsafe { link.as_mut() };
      let use_ctx = vec![Uses::None; ctx.len()].into();
      Ok((use_ctx, DAG::from_term(&infer_lty(*lty))))
    }
    DAGPtr::Opr(mut link) => {
      let Opr { opr, .. } = unsafe { link.as_mut() };
      let use_ctx = vec![Uses::None; ctx.len()].into();
      Ok((use_ctx, DAG::from_term(&opr.type_of())))
    }
    DAGPtr::Lam(_) => {
      Err(CheckError::UntypedLambda(DAG::pos(&term.head), error_context(&ctx)))
    }
    node => Err(CheckError::GenericError(
      DAG::pos(&node),
      error_context(&ctx),
      "TODO".to_string(),
    )),
  }
}

pub fn infer_lit(lit: Literal) -> Term {
  match lit {
    Literal::Nat(_) => Term::LTy(Pos::None, LitType::Nat),
    Literal::Int(_) => Term::LTy(Pos::None, LitType::Int),
    Literal::Bits(_) => Term::LTy(Pos::None, LitType::Bits),
    Literal::Bytes(_) => Term::LTy(Pos::None, LitType::Bytes),
    Literal::Text(_) => Term::LTy(Pos::None, LitType::Text),
    Literal::Char(_) => Term::LTy(Pos::None, LitType::Char),
    Literal::Bool(_) => Term::LTy(Pos::None, LitType::Bool),
    Literal::U8(_) => Term::LTy(Pos::None, LitType::U8),
    Literal::U16(_) => Term::LTy(Pos::None, LitType::U16),
    Literal::U32(_) => Term::LTy(Pos::None, LitType::U32),
    Literal::U64(_) => Term::LTy(Pos::None, LitType::U64),
    Literal::U128(_) => Term::LTy(Pos::None, LitType::U128),
    Literal::I8(_) => Term::LTy(Pos::None, LitType::I8),
    Literal::I16(_) => Term::LTy(Pos::None, LitType::I16),
    Literal::I32(_) => Term::LTy(Pos::None, LitType::I32),
    Literal::I64(_) => Term::LTy(Pos::None, LitType::I64),
    Literal::I128(_) => Term::LTy(Pos::None, LitType::I128),
  }
}

#[allow(clippy::match_single_binding)]
pub fn infer_lty(lty: LitType) -> Term {
  match lty {
    _ => Term::Typ(Pos::None),
  }
}

pub fn infer_term(defs: &Defs, term: Term) -> Result<Term, CheckError> {
  let mut dag = DAG::from_term(&term);
  let (_, typ_dag) = infer(&defs, vec![].into(), Uses::Once, &mut dag)?;
  let typ = DAG::to_term(&typ_dag, true);
  typ_dag.free();
  dag.free();
  Ok(typ)
}

pub fn check_def(defs: &Defs, name: &str) -> Result<Term, CheckError> {
  let def = defs.get(&name.to_string()).ok_or_else(|| {
    CheckError::UndefinedReference(Pos::None, name.to_owned())
  })?;
  let root = Some(alloc_val(DLL::singleton(ParentPtr::Root)));
  let mut trm = DAG::new(DAG::from_ref(
    &def,
    name.to_string(),
    def.def_cid,
    def.ast_cid,
    root,
  ));
  let mut typ = DAG::from_term(&def.typ_);
  check(&defs, vec![].into(), Uses::Once, &mut trm, &mut typ)?;
  trm.free();
  typ.free();
  Ok(def.typ_.clone())
}
