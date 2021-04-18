use std::{
  error::Error,
  fmt,
};

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

use im::HashMap;

use core::ptr::NonNull;
use std::collections::HashSet;

#[derive(Debug)]
pub enum CheckError {
  GenericError(String),
}

impl fmt::Display for CheckError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      CheckError::GenericError(msg) => write!(f, "{}", msg),
    }
  }
}

impl Error for CheckError {
  fn description(&self) -> &str {
    match self {
      CheckError::GenericError(msg) => &msg,
    }
  }
}

// TODO: quickly return the values of nodes that were ctxviously computed
pub fn stringify(dag: DAGPtr, ini: u64, dep: u64) -> String {
  match dag {
    DAGPtr::Var(link) => unsafe {
      let var_dep = link.as_ref().dep;
      if var_dep < ini {
        format!("#{}", var_dep)
      }
      else {
        format!("^{}", dep - 1 - var_dep)
      }
    },
    DAGPtr::Typ(_) => format!("*"),
    DAGPtr::Lit(link) => unsafe { format!("I<{}>", link.as_ref().lit) },
    DAGPtr::LTy(link) => unsafe { format!("T<{}>", link.as_ref().lty) },
    DAGPtr::Opr(link) => unsafe { format!("O<{}>", link.as_ref().opr) },
    DAGPtr::Ref(link) => unsafe { format!("R<{}>", link.as_ref().nam) },
    DAGPtr::Lam(link) => unsafe {
      let Lam { var, bod, .. } = &mut *link.as_ptr();
      var.dep = dep;
      format!("λ{}", stringify(*bod, ini, dep + 1))
    },
    DAGPtr::Slf(link) => unsafe {
      let Slf { var, bod, .. } = &mut *link.as_ptr();
      var.dep = dep;
      format!("${}", stringify(*bod, ini, dep + 1))
    },
    DAGPtr::Cse(link) => unsafe {
      let Cse { bod, .. } = link.as_ref();
      format!("C{}", stringify(*bod, ini, dep))
    },
    DAGPtr::Dat(link) => unsafe {
      let Dat { bod, .. } = link.as_ref();
      format!("D{}", stringify(*bod, ini, dep))
    },
    DAGPtr::All(link) => unsafe {
      let All { var, uses, dom, img, .. } = &mut *link.as_ptr();
      var.dep = dep;
      let ctxfix = match uses {
        Uses::None => "∀0",
        Uses::Once => "∀1",
        Uses::Affi => "∀&",
        Uses::Many => "∀ω",
      };
      format!(
        "{}{}{}",
        ctxfix,
        stringify(*dom, ini, dep),
        stringify(*img, ini, dep + 1)
      )
    },
    DAGPtr::App(link) => unsafe {
      let App { fun, arg, .. } = &mut *link.as_ptr();
      format!("@{}{}", stringify(*fun, ini, dep), stringify(*arg, ini, dep))
    },
    DAGPtr::Let(link) => unsafe {
      let Let { var, exp, bod, .. } = &mut *link.as_ptr();
      var.dep = dep;
      format!("L{}{}", stringify(*exp, ini, dep), stringify(*bod, ini, dep + 1))
    },
    DAGPtr::Ann(link) => unsafe { stringify((*link.as_ptr()).exp, ini, dep) },
  }
}

pub fn equal(defs: &Defs, a: &mut DAG, b: &mut DAG, dep: u64) -> bool {
  a.whnf(defs);
  b.whnf(defs);
  let mut triples = vec![(a.head, b.head, dep)];
  let mut set = HashSet::new();
  while let Some((a, b, dep)) = triples.pop() {
    let mut a = DAG::new(a);
    let mut b = DAG::new(b);
    a.whnf(defs);
    b.whnf(defs);
    let hash_a = stringify(a.head, dep, dep);
    let hash_b = stringify(b.head, dep, dep);
    let hash_ab = format!("{}{}", hash_a, hash_b);
    let eq = hash_a == hash_b || set.contains(&hash_ab);
    set.insert(hash_ab);
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

// Optimization: use of `*mut DAGPtr` instead of `DAG`. The context does not
// need to hold full, "autonomous" DAGs. Instead it can keep references to parts
// of the terms in the typechecker. The reason it must be a pointer of a DAG
// pointer is because the DAG pointer itself might change during typechecking.
// This double pointer is actually a pointer to the parent's field that
// references the node. It does not ever change since it is already in whnf by
// the time we add the child node to the context.
type Ctx = Vec<(String, *mut DAGPtr)>;

type UseCtx = Vec<Uses>;

#[inline]
pub fn add_use_ctx(use_ctx: &mut UseCtx, use_ctx2: UseCtx) {
  for i in 0..use_ctx.len() {
    use_ctx[i] = use_ctx[i] + use_ctx2[i]
  }
}

#[inline]
pub fn mul_use_ctx(uses: Uses, use_ctx: &mut UseCtx) {
  for i in 0..use_ctx.len() {
    use_ctx[i] = use_ctx[i] * uses
  }
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
      match typ.head {
        DAGPtr::All(all_link) => {
          let Lam { bod: lam_bod, var: lam_var, .. } =
            unsafe { &mut *lam_link.as_ptr() };
          let All { uses: lam_uses, var: all_var, dom, img, .. } =
            unsafe { &mut *all_link.as_ptr() };
          // Annotate the depth of the node that binds each variable
          (*all_var).dep = ctx.len() as u64;
          (*lam_var).dep = ctx.len() as u64;
          // Add the domain of the function to the ctxcontext
          let mut lam_bod = DAG::new(*lam_bod);
          let mut img = DAG::new(*img);
          ctx.push((all_var.nam.to_string(), dom));
          let mut use_ctx =
            check(defs, ctx, Uses::Once, &mut lam_bod, &mut img)?;
          let infer_uses = use_ctx.pop().unwrap();
          if Uses::covers(infer_uses, *lam_uses) {
            Err(CheckError::GenericError(format!("Quantity mismatch.")))
          }
          else {
            mul_use_ctx(uses, &mut use_ctx);
            Ok(use_ctx)
          }
        }
        _ => Err(CheckError::GenericError(format!(
          "The type of a lambda must be a forall."
        ))),
      }
    }
    DAGPtr::Dat(dat_link) => {
      typ.whnf(defs);
      match typ.head {
        DAGPtr::Slf(slf_link) => {
          let Dat { bod: dat_bod, .. } = unsafe { &mut *dat_link.as_ptr() };
          let Slf { var, bod: slf_bod, .. } =
            unsafe { &mut *slf_link.as_ptr() };
          let mut map = if var.parents.is_some() {
            HashMap::unit(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              term.clone().head,
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
        _ => Err(CheckError::GenericError(format!(
          "The type of data must be a self."
        ))),
      }
    }
    DAGPtr::Let(_) => {
      Err(CheckError::GenericError(format!("TODO: Let typecheck")))
    }
    _ => {
      let depth = ctx.len();
      let (use_ctx, mut infer_typ) = infer(defs, ctx, uses, term)?;
      let eq = equal(defs, typ, &mut infer_typ, depth as u64);
      infer_typ.free();
      if eq {
        Ok(use_ctx)
      }
      else {
        Err(CheckError::GenericError(format!("Type mismatch.")))
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
      let Var { dep, .. } = unsafe { link.as_mut() };
      let mut use_ctx = vec![Uses::None; ctx.len()];
      use_ctx[*dep as usize] = uses;
      let bind = ctx
        .get(*dep as usize)
        .ok_or(CheckError::GenericError(format!("Unbound variable.")))?;
      let typ = unsafe {
        let dag = DAG::new(*bind.1);
        dag.clone()
      };
      Ok((use_ctx, typ))
    }
    DAGPtr::Ref(mut link) => {
      let Ref { nam, exp: def_link, .. } = unsafe { link.as_mut() };
      let def = defs.get(nam).ok_or(CheckError::GenericError(format!(
        "Undefined reference: {}, {}",
        nam, def_link
      )))?;
      let use_ctx = vec![Uses::None; ctx.len()];
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
          let All { var, uses: lam_uses, dom, img, .. } =
            unsafe { &mut *link.as_ptr() };
          let arg_use_ctx =
            check(defs, ctx, *lam_uses * uses, &mut arg, &mut DAG::new(*dom))?;
          add_use_ctx(&mut fun_use_ctx, arg_use_ctx);
          let mut map = if var.parents.is_some() {
            HashMap::unit(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              arg.clone().head,
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
        _ => Err(CheckError::GenericError(format!(
          "Tried to apply something that isn't a lambda."
        ))),
      }
    }
    DAGPtr::Cse(mut link) => {
      let Cse { bod: exp, .. } = unsafe { link.as_mut() };
      let mut exp = DAG::new(*exp);
      let (exp_use_ctx, mut exp_typ) = infer(defs, ctx, uses, &mut exp)?;
      exp_typ.whnf(defs);
      match exp_typ.head {
        DAGPtr::Slf(link) => {
          let Slf { var, bod, .. } = unsafe { &mut *link.as_ptr() };
          let mut map = if var.parents.is_some() {
            HashMap::unit(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              exp.clone().head,
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
        _ => Err(CheckError::GenericError(format!(
          "Tried to case on something that isn't a datatype."
        ))),
      }
    }
    DAGPtr::All(link) => {
      let All { var, dom, img, .. } = unsafe { &mut *link.as_ptr() };
      let mut typ = DAG::from_term(&Term::Typ(Pos::None));
      let _ =
        check(defs, ctx.clone(), Uses::None, &mut DAG::new(*dom), &mut typ)?;
      (*var).dep = ctx.len() as u64;
      ctx.push((var.nam.to_string(), dom));
      let mut img = DAG::new(*img);
      let use_ctx = check(defs, ctx, Uses::None, &mut img, &mut typ)?;
      Ok((use_ctx, typ))
    }
    DAGPtr::Slf(mut link) => {
      let Slf { var, bod, .. } = unsafe { link.as_mut() };
      let mut typ = DAG::from_term(&Term::Typ(Pos::None));
      (*var).dep = ctx.len() as u64;
      ctx.push((var.nam.to_string(), &mut term.head));
      let mut bod = DAG::new(*bod);
      let use_ctx = check(defs, ctx, Uses::None, &mut bod, &mut typ)?;
      Ok((use_ctx, typ))
    }
    DAGPtr::Typ(_) => {
      let use_ctx = vec![Uses::None; ctx.len()];
      let typ = DAG::from_term(&Term::Typ(Pos::None));
      Ok((use_ctx, typ))
    }
    DAGPtr::Ann(mut link) => {
      let Ann { typ, exp, .. } = unsafe { link.as_mut() };
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
      let use_ctx = vec![Uses::None; ctx.len()];
      Ok((use_ctx, DAG::from_term(&infer_lit(lit.to_owned()))))
    }
    DAGPtr::LTy(mut link) => {
      let LTy { lty, .. } = unsafe { link.as_mut() };
      let use_ctx = vec![Uses::None; ctx.len()];
      Ok((use_ctx, DAG::from_term(&infer_lty(*lty))))
    }
    DAGPtr::Opr(_link) => {
      Err(CheckError::GenericError(format!("TODO")))
      // let Opr { opr, .. } = unsafe { &*link.as_ptr() };
      // let use_ctx = vec![Uses::None; ctx.len()];
      // Ok((use_ctx, DAG::from_term(&infer_opr(*opr))))
    }
    DAGPtr::Lam(_) => Err(CheckError::GenericError(format!("Untyped lambda."))),
    _ => Err(CheckError::GenericError(format!("TODO"))),
  }
}

pub fn infer_lit(lit: Literal) -> Term {
  match lit {
    Literal::Nat(_) => Term::LTy(Pos::None, LitType::Nat),
    Literal::Int(_) => Term::LTy(Pos::None, LitType::Int),
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

pub fn infer_lty(lty: LitType) -> Term {
  match lty {
    _ => Term::Typ(Pos::None),
  }
}

// pub fn infer_opr(lit: PrimOp) -> Term {}

pub fn check_def(defs: &Defs, name: &String) -> Result<(), CheckError> {
  let def = defs
    .get(name)
    .ok_or(CheckError::GenericError(format!("Undefined reference.")))?;
  let mut trm =
    DAG::new(DAG::from_ref(&def, name.clone(), def.def_cid, def.ast_cid));
  let mut typ = DAG::from_term(&def.typ_);
  check(&defs, vec![], Uses::Once, &mut trm, &mut typ)?;
  trm.free();
  typ.free();
  Ok(())
}
