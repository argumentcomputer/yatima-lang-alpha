use std::error::Error;
use std::fmt;

use crate::{
  term::{
    Term,
    Defs,
    Refs,
  },
  core::{
    dag::*,
    uses::*
  },
};

use core::ptr::NonNull;
use std::collections::HashSet;

type PreCtx = Vec<(String, DAG)>;
type Ctx    = Vec<(String, Uses, DAG)>;

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
      CheckError::GenericError(msg) => write!(f,"{}",msg),
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
      let var_dep = (*link.as_ptr()).dep;
      if var_dep < ini {
        format!("#{}", var_dep)
      }
      else {
        format!("^{}", dep-1-var_dep)
      }
    },
    DAGPtr::Typ(_) => format!("*"),
    DAGPtr::Lit(link) => unsafe {
      format!("I<{}>", (*link.as_ptr()).lit)
    }
    DAGPtr::LTy(link) => unsafe {
      format!("T<{}>", (*link.as_ptr()).lty)
    }
    DAGPtr::Opr(link) => unsafe {
      format!("O<{}>", (*link.as_ptr()).opr)
    }
    DAGPtr::Ref(link) => unsafe {
      format!("R<{}>", (*link.as_ptr()).nam)
    },
    DAGPtr::Lam(link) => unsafe {
      let Lam { var, bod, .. } = &mut *link.as_ptr();
      var.dep = dep;
      format!("λ{}", stringify(*bod, ini, dep+1))
    },
    DAGPtr::Slf(link) => unsafe {
      let Slf { var, bod, .. } = &mut *link.as_ptr();
      var.dep = dep;
      format!("${}", stringify(*bod, ini, dep+1))
    },
    DAGPtr::Fix(link) => unsafe {
      let Fix { var, bod, .. } = &mut *link.as_ptr();
      var.dep = dep;
      format!("μ{}", stringify(*bod, ini, dep+1))
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
      let prefix = match uses {
        Uses::None => "∀0",
        Uses::Once => "∀1",
        Uses::Affi => "∀&",
        Uses::Many => "∀ω",
      };
      format!("{}{}{}", prefix, stringify(*dom, ini, dep), stringify(*img, ini, dep+1))
    },
    DAGPtr::App(link) => unsafe {
      let App { fun, arg, .. } = &mut *link.as_ptr();
      format!("@{}{}", stringify(*fun, ini, dep), stringify(*arg, ini, dep))
    },
    DAGPtr::Let(link) => unsafe {
      let Let { var, exp, bod, .. } = &mut *link.as_ptr();
      var.dep = dep;
      format!("L{}{}", stringify(*exp, ini, dep), stringify(*bod, ini, dep+1))
    },
    DAGPtr::Ann(link) => unsafe {
      stringify((*link.as_ptr()).exp, ini, dep)
    },
  }
}

pub fn equal(defs:&Defs, a: &mut DAG, b: &mut DAG, dep: u64) -> bool {
  let mut triples = vec![(a.head, b.head, dep)];
  let mut set = HashSet::new();
  while let Some ((a, b, dep)) = triples.pop() {
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
        (DAGPtr::Lam(a_link),DAGPtr::Lam(b_link)) => unsafe {
          let Lam { bod: a_bod, .. } = *a_link.as_ptr();
          let Lam { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep+1));
        },
        (DAGPtr::Slf(a_link),DAGPtr::Slf(b_link)) => unsafe {
          let Slf { bod: a_bod, .. } = *a_link.as_ptr();
          let Slf { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep+1));
        },
        (DAGPtr::Cse(a_link),DAGPtr::Cse(b_link)) => unsafe {
          let Cse { bod: a_bod, .. } = *a_link.as_ptr();
          let Cse { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep));
        },
        (DAGPtr::Dat(a_link),DAGPtr::Dat(b_link)) => unsafe {
          let Dat { bod: a_bod, .. } = *a_link.as_ptr();
          let Dat { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep));
        },
        (DAGPtr::All(a_link),DAGPtr::All(b_link)) => unsafe {
          let All { uses: a_uses, dom: a_dom, img: a_img, .. } = *a_link.as_ptr();
          let All { uses: b_uses, dom: b_dom, img: b_img, .. } = *b_link.as_ptr();
          if a_uses != b_uses {
            return false
          }
          triples.push((a_dom, b_dom, dep));
          triples.push((a_img, b_img, dep+1));
        },
        (DAGPtr::App(a_link),DAGPtr::App(b_link)) => unsafe {
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

pub fn check(defs: &Defs, mut pre: PreCtx, uses: Uses, term: &DAG, typ: &mut DAG) -> Result<Ctx, CheckError> {
  match term.head {
    DAGPtr::Lam(link) => {
      let Lam { bod: term_bod, var: lam_var, .. } = unsafe { &mut *link.as_ptr() };
      // A potential problem is that `term` might also mutate, which is an unwanted side-effect,
      // since `term` and `typ` might be coupled. To deal with this we will need to make copies
      // of `term` in the self rule
      typ.whnf(defs);
      match typ.head {
        DAGPtr::All(link) => {
          let All {  uses: lam_uses, var, dom, img, .. } = unsafe { &mut *link.as_ptr() };
          // Annotate the depth of the node that binds each variable
          (*var).dep = pre.len() as u64;
          (*lam_var).dep = pre.len() as u64;
          // Add the domain of the function to the precontext
          pre.push((var.nam.to_string(), DAG::new(*dom).clone()));
          let mut ctx = check(defs, pre, Uses::Once, &DAG::new(*term_bod), &mut DAG::new(*img))?;
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
        },
        _ => Err(CheckError::GenericError(format!("The type of a lambda must be a forall."))),
      }
    }
    DAGPtr::Dat(link) => {
      let Dat { bod: dat_bod, .. } = unsafe { &(*link.as_ptr()) };
      typ.whnf(defs);
      match typ.head {
        DAGPtr::Slf(_) => {
          // Copy the self type
          let new_link = match typ.clone().head {
            DAGPtr::Slf(link) => link,
            _ => panic!("Error in the clone implementation")
          };
          let Slf { var: new_var, bod: new_bod, .. } = unsafe { &mut *new_link.as_ptr() };
          // Substitute the term for its variable
          if new_var.parents.is_some() {
            // Create the term DAG
            let term_copy = term.clone();
            replace_child(DAGPtr::Var(NonNull::new(new_var).unwrap()), term_copy.head);
          }
          // Replace the copied self type with its body and free the dead nodes
          replace_child(DAGPtr::Slf(new_link), *new_bod);
          free_dead_node(DAGPtr::Slf(new_link));
          // Check against `new_bod` and free it later
          let ctx = check(defs, pre, uses, &DAG::new(*dat_bod), &mut DAG::new(*new_bod))?;
          DAG::new(*new_bod).free();
          Ok(ctx)
        },
        _ => Err(CheckError::GenericError(format!("The type of data must be a self."))),
      }
    },
    DAGPtr::Let(_) => {
      Err(CheckError::GenericError(format!("TODO: Let typecheck")))
    }
    _ => {
      let depth = pre.len();
      let (ctx, mut infer_typ) = infer(defs, pre, uses, term)?;
      let eq = equal(defs, typ, &mut infer_typ, depth as u64);
      infer_typ.free();
      if eq {
        Ok(ctx)
      }
      else {
        Err(CheckError::GenericError(format!("Type mismatch.")))
      }
    }
  }
}

pub fn infer(defs: &Defs, mut pre: PreCtx, uses: Uses, term: &DAG) -> Result<(Ctx, DAG), CheckError> {
  match term.head {
    DAGPtr::Var(link) => {
      let Var { dep, .. } = unsafe { &*link.as_ptr() };
      let bind = pre
        .get(*dep as usize)
        .ok_or(CheckError::GenericError(format!("Unbound variable.")))?;
      let typ = bind.1.clone();
      let mut ctx = to_ctx(pre);
      ctx[*dep as usize].1 = uses;
      Ok((ctx, typ))
    }
    DAGPtr::Ref(link) => {
      let Ref { nam, exp: def_link, .. } = unsafe { &*link.as_ptr() };
      let def = defs
        .get(def_link)
        .ok_or(CheckError::GenericError(format!("Undefined reference: {}, {}", nam, def_link)))?;
      let ctx = to_ctx(pre);
      let typ = DAG::from_term(&def.typ_);
      Ok((ctx, typ))
    }
    DAGPtr::App(link) => {
      let App { fun, arg, .. } = unsafe { &*link.as_ptr() };
      let (mut fun_ctx, mut fun_typ) = infer(defs, pre.clone(), uses, &mut DAG::new(*fun))?;
      fun_typ.whnf(defs);
      match fun_typ.head {
        DAGPtr::All(mut link) => {
          let All { uses: lam_uses, dom, .. } = unsafe { link.as_mut() };
          let arg_ctx = check(defs, pre, Uses::mul(*lam_uses, uses), &DAG::new(*arg), &mut DAG::new(*dom))?;
          add_ctx(&mut fun_ctx, arg_ctx);
          // Copy the All type (in a efficient implementation, only image is copied)
          let new_link = match fun_typ.clone().head {
            DAGPtr::All(link) => link,
            _ => panic!("Error in the clone implementation")
          };
          let All { var: new_var, img: new_img, .. } = unsafe { &mut *new_link.as_ptr() };
          // Substitute the argument for the image's variable
          if new_var.parents.is_some() {
            // Create the argument DAG
            replace_child(DAGPtr::Var(NonNull::new(new_var).unwrap()), DAG::new(*arg).clone().head);
          }
          // Replace the copied type with the new image and free the dead nodes
          replace_child(DAGPtr::All(new_link), *new_img);
          free_dead_node(DAGPtr::All(new_link));
          Ok((fun_ctx, DAG::new(*new_img)))
        },
        _ => Err(CheckError::GenericError(format!("Tried to apply something that isn't a lambda."))),
      }
    }
    DAGPtr::Cse(link) => {
      let Cse { bod: exp, .. } = unsafe { &*link.as_ptr() };
      let (exp_ctx, mut exp_typ) = infer(defs, pre, uses, &DAG::new(*exp))?;
      exp_typ.whnf(defs);
      match exp_typ.head {
        DAGPtr::Slf(_) => {
          let new_link = match exp_typ.clone().head {
            DAGPtr::Slf(link) => link,
            _ => panic!("Error in the clone implementation")
          };
          let Slf { var: new_var, bod: new_bod, .. } = unsafe { &mut *new_link.as_ptr() };
          if new_var.parents.is_some() {
            let exp_clone = DAG::new(*exp).clone();
            replace_child(DAGPtr::Var(NonNull::new(new_var).unwrap()), exp_clone.head);
          }
          replace_child(DAGPtr::Slf(new_link), *new_bod);
          free_dead_node(DAGPtr::Slf(new_link));
          Ok((exp_ctx, DAG::new(*new_bod)))
        },
        _ => Err(CheckError::GenericError(format!("Tried to case on something that isn't a datatype."))),
      }
    }
    DAGPtr::All(link) => {
      let All { var, dom, img, .. } = unsafe { &*link.as_ptr() };
      let mut typ = DAG::from_term(&Term::Typ(None));
      let _ = check(defs, pre.clone(), Uses::None, &DAG::new(*dom), &mut typ)?;
      let dom_clone = DAG::new(*dom).clone();
      pre.push((var.nam.to_string(), dom_clone));
      let ctx = check(defs, pre, Uses::None, &DAG::new(*img), &mut typ)?;
      Ok((ctx, typ))
    }
    DAGPtr::Slf(link) => {
      let Slf { var, bod, .. } = unsafe { &*link.as_ptr() };
      let mut typ = DAG::from_term(&Term::Typ(None));
      let term_clone = term.clone();
      pre.push((var.nam.to_string(), term_clone));
      let ctx = check(defs, pre, Uses::None, &DAG::new(*bod), &mut typ)?;
      Ok((ctx, typ))
    }
    DAGPtr::Typ(_) => {
      Ok((to_ctx(pre), term.clone()))
    }
    DAGPtr::Ann(link) => {
      let Ann { typ, exp, .. } = unsafe { &*link.as_ptr() };
      let mut typ_clone = DAG::new(*typ).clone();
      let ctx = check(defs, pre, uses, &DAG::new(*exp), &mut typ_clone)?;
      Ok((ctx, typ_clone))
    }
    DAGPtr::Let(_) => {
      panic!("TODO: Let inference")
    }
    DAGPtr::Lit(_) => {
      panic!("TODO: Lit inference")
    }
    DAGPtr::LTy(_) => {
      panic!("TODO: LTy inference")
    }
    DAGPtr::Opr(_) => {
      panic!("TODO: Opr inference")
    }
    DAGPtr::Lam(_) => {
      Err(CheckError::GenericError(format!("Untyped lambda.")))
    }
    _ => Err(CheckError::GenericError(format!("TODO"))),
  }
}

pub fn check_def(defs: &Defs, refs: &Refs, name: &String) -> Result<(), CheckError> {
  let (def_link, ast_link) = refs
    .get(name)
    .ok_or(CheckError::GenericError(format!("Undefined reference.")))?;
  let def = defs
    .get(def_link)
    .ok_or(CheckError::GenericError(format!("Undefined reference.")))?;
  let mut trm = DAG::from_term(&Term::Ref(None, name.clone(), *def_link, *ast_link));
  trm.deref(defs);
  let mut typ = DAG::from_term(&def.typ_);
  let ctx = check(&defs, vec![], Uses::Once, &trm, &mut typ)?;
  for (_, _, dag) in ctx {
    dag.free()
  }
  typ.free();
  Ok(())
}
