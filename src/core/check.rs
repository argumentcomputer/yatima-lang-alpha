use std::{
  error::Error,
  fmt,
};

use crate::{
  core::{
    dag::*,
    literal::{
      LitType,
      Literal,
    },
    primop::PrimOp,
    uses::*,
  },
  term::{
    Defs,
    Refs,
    Term,
  },
};

use core::ptr::NonNull;
use std::collections::HashSet;

type PreCtx = Vec<(String, DAG)>;
type Ctx = Vec<(String, Uses, DAG)>;

pub fn to_ctx(pre: PreCtx) -> Ctx {
  let mut ctx = vec![];
  for (nam, typ) in pre {
    ctx.push((nam, Uses::None, typ));
  }
  ctx
}

#[inline]
pub fn add_ctx(ctx: &mut Ctx, ctx2: Ctx) {
  for i in 0..ctx.len() {
    ctx[i].1 = Uses::add(ctx[i].1, ctx2[i].1)
  }
}

#[inline]
pub fn mul_ctx(uses: Uses, ctx: &mut Ctx) {
  for mut bind in ctx {
    bind.1 = Uses::mul(bind.1, uses)
  }
}

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

// TODO: quickly return the values of nodes that were previously computed
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
    DAGPtr::Lam(mut link) => unsafe {
      let Lam { var, bod, .. } = link.as_mut();
      var.dep = dep;
      format!("λ{}", stringify(*bod, ini, dep + 1))
    },
    DAGPtr::Slf(mut link) => unsafe {
      let Slf { var, bod, .. } = link.as_mut();
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
    DAGPtr::All(mut link) => unsafe {
      let All { var, uses, dom, img, .. } = link.as_mut();
      var.dep = dep;
      let prefix = match uses {
        Uses::None => "∀0",
        Uses::Once => "∀1",
        Uses::Affi => "∀&",
        Uses::Many => "∀ω",
      };
      format!(
        "{}{}{}",
        prefix,
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

pub fn check(
  defs: &Defs,
  mut pre: PreCtx,
  uses: Uses,
  term: &DAG,
  typ: &mut DAG,
) -> Result<Ctx, CheckError> {
  match term.head {
    DAGPtr::Lam(mut link) => {
      let Lam { bod: term_bod, var: lam_var, .. } = unsafe { link.as_mut() };
      // A potential problem is that `term` might also mutate, which is an
      // unwanted side-effect, since `term` and `typ` might be coupled. To
      // deal with this we will need to make copies of `term` in the self
      // rule
      typ.whnf(defs);
      match typ.head {
        DAGPtr::All(mut link) => {
          let All { uses: lam_uses, var, dom, img, .. } =
            unsafe { link.as_mut() };
          // Annotate the depth of the node that binds each variable
          (*var).dep = pre.len() as u64;
          (*lam_var).dep = pre.len() as u64;
          // Add the domain of the function to the precontext
          let dom = DAG::new(*dom);
          let term_bod = DAG::new(*term_bod);
          let mut img = DAG::new(*img);
          pre.push((var.nam.to_string(), dom.clone()));
          let mut ctx = check(defs, pre, Uses::Once, &term_bod, &mut img)?;
          let (_, infer_uses, infer_typ) = ctx
            .pop()
            .ok_or(CheckError::GenericError(format!("Empty context.")))?;
          // Discard the popped type
          infer_typ.free();
          if Uses::gth(infer_uses, *lam_uses) {
            Err(CheckError::GenericError(format!("Quantity mismatch.")))
          }
          else {
            mul_ctx(uses, &mut ctx);
            Ok(ctx)
          }
        }
        _ => Err(CheckError::GenericError(format!(
          "The type of a lambda must be a forall."
        ))),
      }
    }
    DAGPtr::Dat(mut link) => {
      let Dat { bod: dat_bod, .. } = unsafe { link.as_mut() };
      typ.whnf(defs);
      match typ.head {
        DAGPtr::Slf(_) => {
          // Copy the self type
          let mut new_link = match typ.clone().head {
            DAGPtr::Slf(link) => link,
            _ => panic!("Error in the clone implementation"),
          };
          let Slf { var: new_var, bod: new_bod, .. } =
            unsafe { new_link.as_mut() };
          // Substitute the term for its variable
          if new_var.parents.is_some() {
            // Create the term DAG
            let term_copy = term.clone();
            replace_child(
              DAGPtr::Var(NonNull::new(new_var).unwrap()),
              term_copy.head,
            );
          }
          // Replace the copied self type with its body and free the dead nodes
          replace_child(DAGPtr::Slf(new_link), *new_bod);
          // Check against `new_bod` and free it later
          let dat_bod = DAG::new(*dat_bod);
          let mut new_bod = DAG::new(*new_bod);
          let ctx = check(defs, pre, uses, &dat_bod, &mut new_bod)?;
          free_dead_node(DAGPtr::Slf(new_link));
          new_bod.free();
          Ok(ctx)
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
      let depth = pre.len();
      let (ctx, mut infer_typ) = infer(defs, pre, uses, term)?;
      let eq = equal(defs, typ, &mut infer_typ, depth as u64);
      // infer_typ.free();
      if eq {
        Ok(ctx)
      }
      else {
        Err(CheckError::GenericError(format!("Type mismatch.")))
      }
    }
  }
}

pub fn infer(
  defs: &Defs,
  mut pre: PreCtx,
  uses: Uses,
  term: &DAG,
) -> Result<(Ctx, DAG), CheckError> {
  match term.head {
    DAGPtr::Var(mut link) => {
      let Var { dep, .. } = unsafe { link.as_mut() };
      let bind = pre
        .get(*dep as usize)
        .ok_or(CheckError::GenericError(format!("Unbound variable.")))?;
      let typ = bind.1.clone();
      let mut ctx = to_ctx(pre);
      ctx[*dep as usize].1 = uses;
      Ok((ctx, typ))
    }
    DAGPtr::Ref(mut link) => {
      let Ref { nam, exp: def_link, .. } = unsafe { link.as_mut() };
      let def = defs.get(def_link).ok_or(CheckError::GenericError(format!(
        "Undefined reference: {}, {}",
        nam, def_link
      )))?;
      let ctx = to_ctx(pre);
      let typ = DAG::from_term(&def.typ_);
      Ok((ctx, typ))
    }
    DAGPtr::App(mut link) => {
      let App { fun, arg, .. } = unsafe { link.as_mut() };
      let mut fun = DAG::new(*fun);
      let arg = DAG::new(*arg);
      let (mut fun_ctx, mut fun_typ) =
        infer(defs, pre.clone(), uses, &mut fun)?;
      fun_typ.whnf(defs);
      match fun_typ.head {
        DAGPtr::All(mut link) => {
          let All { uses: lam_uses, dom, .. } = unsafe { link.as_mut() };
          let mut dom = DAG::new(*dom);
          let arg_ctx =
            check(defs, pre, Uses::mul(*lam_uses, uses), &arg, &mut dom)?;
          add_ctx(&mut fun_ctx, arg_ctx);
          // Copy the All type (in a efficient implementation, only image is
          // copied)
          let mut new_link = match fun_typ.clone().head {
            DAGPtr::All(link) => link,
            _ => panic!("Error in the clone implementation"),
          };
          let All { var: new_var, img: new_img, .. } =
            unsafe { new_link.as_mut() };
          // Substitute the argument for the image's variable
          if new_var.parents.is_some() {
            // Create the argument DAG
            replace_child(
              DAGPtr::Var(NonNull::new(new_var).unwrap()),
              arg.clone().head,
            );
          }
          // Replace the copied type with the new image and free the dead nodes
          replace_child(DAGPtr::All(new_link), *new_img);
          let new_img = DAG::new(*new_img);
          free_dead_node(DAGPtr::All(new_link));
          Ok((fun_ctx, new_img))
        }
        _ => Err(CheckError::GenericError(format!(
          "Tried to apply something that isn't a lambda."
        ))),
      }
    }
    DAGPtr::Cse(mut link) => {
      let Cse { bod: exp, .. } = unsafe { link.as_mut() };
      let exp = DAG::new(*exp);
      let (exp_ctx, mut exp_typ) = infer(defs, pre, uses, &exp)?;
      exp_typ.whnf(defs);
      match exp_typ.head {
        DAGPtr::Slf(_) => {
          let new_link = match exp_typ.clone().head {
            DAGPtr::Slf(link) => link,
            _ => panic!("Error in the clone implementation"),
          };
          let Slf { var: new_var, bod: new_bod, .. } =
            unsafe { &mut *new_link.as_ptr() };
          if new_var.parents.is_some() {
            let exp_clone = exp.clone();
            replace_child(
              DAGPtr::Var(NonNull::new(new_var).unwrap()),
              exp_clone.head,
            );
          }
          replace_child(DAGPtr::Slf(new_link), *new_bod);
          let new_bod = DAG::new(*new_bod);
          free_dead_node(DAGPtr::Slf(new_link));
          Ok((exp_ctx, new_bod))
        }
        _ => Err(CheckError::GenericError(format!(
          "Tried to case on something that isn't a datatype."
        ))),
      }
    }
    DAGPtr::All(link) => {
      let All { var, dom, img, .. } = unsafe { &mut *link.as_ptr() };
      let mut typ = DAG::from_term(&Term::Typ(None));
      let dom = DAG::new(*dom);
      let _ = check(defs, pre.clone(), Uses::None, &dom, &mut typ)?;
      let dom_clone = dom.clone();
      (*var).dep = pre.len() as u64;
      pre.push((var.nam.to_string(), dom_clone));
      let img = DAG::new(*img);
      let ctx = check(defs, pre, Uses::None, &img, &mut typ)?;
      Ok((ctx, typ))
    }
    DAGPtr::Slf(mut link) => {
      let Slf { var, bod, .. } = unsafe { link.as_mut() };
      let mut typ = DAG::from_term(&Term::Typ(None));
      let term_clone = term.clone();
      (*var).dep = pre.len() as u64;
      pre.push((var.nam.to_string(), term_clone));
      let bod = DAG::new(*bod);
      let ctx = check(defs, pre, Uses::None, &bod, &mut typ)?;
      Ok((ctx, typ))
    }
    DAGPtr::Typ(_) => Ok((to_ctx(pre), term.clone())),
    DAGPtr::Ann(mut link) => {
      let Ann { typ, exp, .. } = unsafe { link.as_mut() };
      let mut typ_clone = DAG::new(*typ).clone();
      let exp = DAG::new(*exp);
      let ctx = check(defs, pre, uses, &exp, &mut typ_clone)?;
      Ok((ctx, typ_clone))
    }
    DAGPtr::Let(_) => {
      panic!("TODO: Let inference")
    }
    DAGPtr::Lit(mut link) => {
      let Lit { lit, .. } = unsafe { link.as_mut() };
      Ok((to_ctx(pre), DAG::from_term(&infer_lit(lit.to_owned()))))
    }
    DAGPtr::LTy(mut link) => {
      let LTy { lty, .. } = unsafe { link.as_mut() };
      Ok((to_ctx(pre), DAG::from_term(&infer_lty(*lty))))
    }
    DAGPtr::Opr(_link) => {
      Err(CheckError::GenericError(format!("TODO")))
      // let Opr { opr, .. } = unsafe { &*link.as_ptr() };
      // Ok((to_ctx(pre), DAG::from_term(&infer_opr(*opr))))
    }
    DAGPtr::Lam(_) => Err(CheckError::GenericError(format!("Untyped lambda."))),
    _ => Err(CheckError::GenericError(format!("TODO"))),
  }
}

pub fn infer_lit(lit: Literal) -> Term {
  match lit {
    Literal::Natural(_) => Term::LTy(None, LitType::Natural),
    Literal::Integer(_) => Term::LTy(None, LitType::Integer),
    Literal::BitString(_) => Term::LTy(None, LitType::BitString),
    Literal::Text(_) => Term::LTy(None, LitType::Text),
    Literal::Char(_) => Term::LTy(None, LitType::Char),
    Literal::Bool(_) => Term::LTy(None, LitType::Bool),
    Literal::U8(_) => Term::LTy(None, LitType::U8),
    Literal::U16(_) => Term::LTy(None, LitType::U16),
    Literal::U32(_) => Term::LTy(None, LitType::U32),
    Literal::U64(_) => Term::LTy(None, LitType::U64),
    Literal::I8(_) => Term::LTy(None, LitType::I8),
    Literal::I16(_) => Term::LTy(None, LitType::I16),
    Literal::I32(_) => Term::LTy(None, LitType::I32),
    Literal::I64(_) => Term::LTy(None, LitType::I64),
  }
}

pub fn infer_lty(lty: LitType) -> Term {
  match lty {
    _ => Term::Typ(None),
  }
}

// pub fn infer_opr(lit: PrimOp) -> Term {}

pub fn check_def(
  defs: &Defs,
  refs: &Refs,
  name: &String,
) -> Result<(), CheckError> {
  let (def_link, ast_link) = refs
    .get(name)
    .ok_or(CheckError::GenericError(format!("Undefined reference.")))?;
  let def = defs
    .get(def_link)
    .ok_or(CheckError::GenericError(format!("Undefined reference.")))?;
  let trm = DAG::from_def(&def.term, (def.name.clone(), *def_link, *ast_link));
  let mut typ = DAG::from_term(&def.typ_);
  let ctx = check(&defs, vec![], Uses::Once, &trm, &mut typ)?;
  for (_, _, dag) in ctx {
    dag.free()
  }
  typ.free();
  Ok(())
}
