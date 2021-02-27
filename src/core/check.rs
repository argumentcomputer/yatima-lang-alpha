#![allow(unused_variables)]

use std::error::Error;
use std::fmt;

use crate::{
  term::{
    Term,
    Link,
  },
  core::{
    dag::{
      replace_child,
      get_parents,
      free_dead_node,
      Branch,
      BranchTag,
      Leaf,
      LeafTag,
      ParentCell,
      Single,
      SingleTag,
      DAG,
    },
    eval::{
      whnf,
      subst
    },
    uses::*
  },
};

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
    ctx[0].1 = Uses::add(ctx[0].1, ctx2[0].1)
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

pub fn equal(a: DAG, b: DAG, dep: u64) -> bool {
  true
}

pub fn check(mut pre: PreCtx, uses: Uses, term: &Term, mut typ: DAG) -> Result<Ctx, CheckError> {
  match &term {
    Term::Lam(_, name, term_body) => {
      // A potential problem is that `term` might also mutate, which is an unwanted side-effect,
      // since `term` and `typ` might be coupled. To deal with this we will need to make copies
      // of `term` in the self rule
      whnf(typ);
      match typ {
        DAG::Branch(link) => {
          let Branch { tag, var, left: dom, right: img, .. } = unsafe { &*link.as_ptr() };
          match tag {
            BranchTag::All(lam_uses) => {
              // Annotate the depth of the node that binds var
              match var {
                None => panic!("Malformed DAG"),
                Some(link) => unsafe {
                  (*link.as_ptr()).depth = pre.len() as u64;
                }
              }
              // Add the domain of the function to the precontext
              pre.push((name.to_string(), *dom));
              let mut ctx = check(pre, Uses::Once, &**term_body, *img)?;
              let (_, inf_uses, _) = ctx
                .pop()
                .ok_or(CheckError::GenericError(String::from("Empty context.")))?;
              if Uses::gth(inf_uses, *lam_uses) {
                Err(CheckError::GenericError(String::from("Quantity mismatch.")))
              }
              else {
                mul_ctx(uses, &mut ctx);
                Ok(ctx)
              }
            }
            _ => Err(CheckError::GenericError(String::from("The type of a lambda must be a forall."))),
          }
        },
        _ => Err(CheckError::GenericError(String::from("The type of a lambda must be a forall."))),
      }
    },
      
    Term::Dat(_, dat_body) => {
      whnf(typ);
      match typ {
        DAG::Single(link) => {
          let Single { tag, var, body: slf_body, .. } = unsafe { &*link.as_ptr() };
          match tag {
            SingleTag::Slf => {
              // Copy the self type
              let new_link = match typ.full_copy() {
                DAG::Single(link) => link,
                _ => panic!("Error in the full_copy implementation")
              };
              let Single { var: new_var, body: new_body, .. } = unsafe { &*new_link.as_ptr() };
              // Substitute the term for its variable
              new_var.map(|var| {
                if get_parents(DAG::Var(var)).is_some() {
                  // Create the term DAG and remove its root node
                  let mut term_dag = DAG::from_open_term(true, pre.len() as u64, &term);
                  term_dag.uproot();
                  replace_child(DAG::Var(var), term_dag);
                }
              });
              // Replace the copied self type with its body and free the dead nodes
              replace_child(DAG::Single(new_link), *new_body);
              free_dead_node(DAG::Single(new_link));

              // Check against `new_body` and free it later
              let ctx = check(pre, uses, dat_body, *new_body)?;
              new_body.uproot();
              free_dead_node(*new_body);
              Ok(ctx)
            }
            _ => Err(CheckError::GenericError(String::from("The type of data must be a self."))),
          }
        },
        _ => Err(CheckError::GenericError(String::from("The type of data must be a self."))),
      }
    },

    Term::Let(_, _, _, _, _, _, _) => {
      panic!("TODO")
    }
    _ => {
      let depth = pre.len();
      let (ctx, infer_typ) = infer(pre, uses, term)?;
      if equal(typ, infer_typ, depth as u64) {
        Ok(ctx)
      }
      else {
        Err(CheckError::GenericError(String::from("Type mismatch.")))
      }
    },
  }
}

pub fn infer(mut pre: PreCtx, uses: Uses, term: &Term) -> Result<(Ctx, DAG), CheckError> {
  match term {
    Term::Var(_, nam, idx) => {
      let level = pre.len() - (*idx as usize) - 1;
      let bind = pre
        .get(level)
        .ok_or(CheckError::GenericError(String::from("Unbound variable.")))?;
      let typ = bind.1;
      let mut ctx = to_ctx(pre);
      ctx[*idx as usize].1 = uses;
      Ok((ctx, typ))
    }
    Term::Ref(_, _, _, _) => {
      panic!("TODO")
    }
    Term::App(_, func, argm) => {
      let depth = pre.len() as u64;
      let (mut func_ctx, func_typ) = infer(pre.clone(), uses, func)?;
      whnf(func_typ);
      match func_typ {
        DAG::Branch(link) => {
          let Branch { tag, var, left: dom, right: img, .. } = unsafe { &*link.as_ptr() };
          match tag {
            BranchTag::All(lam_uses) => {
              let argm_ctx = check(pre, Uses::mul(*lam_uses, uses), argm, *dom)?;
              add_ctx(&mut func_ctx, argm_ctx);

              // Copy the All type (in a efficient implementation, only image is copied)
              let new_link = match func_typ.full_copy() {
                DAG::Branch(link) => link,
                _ => panic!("Error in the full_copy implementation")
              };
              let Branch { var: new_var, right: new_img, .. } = unsafe { &*new_link.as_ptr() };
              // Substitute the argument for the image's variable
              new_var.map(|var| {
                if get_parents(DAG::Var(var)).is_some() {
                  // Create the argument DAG and remove its root node
                  let mut argm_dag = DAG::from_open_term(true, depth, &argm);
                  argm_dag.uproot();
                  replace_child(DAG::Var(var), argm_dag);
                }
              });
              // Replace the copied type with the new image and free the dead nodes
              replace_child(DAG::Branch(new_link), *new_img);
              free_dead_node(DAG::Branch(new_link));

              Ok((func_ctx, *new_img))
            }
            _ => Err(CheckError::GenericError(String::from("Tried to apply something that isn't a lambda."))),
          }
        },
        _ => Err(CheckError::GenericError(String::from("Tried to apply something that isn't a lambda."))),
      }

    }
    Term::Cse(_, expr) => {
      let depth = pre.len() as u64;
      let (mut expr_ctx, expr_typ) = infer(pre, uses, expr)?;
      whnf(expr_typ);
      match expr_typ {
        DAG::Single(link) => {
          let Single { tag, var, body, .. } = unsafe { &*link.as_ptr() };
          match tag {
            SingleTag::Slf => {
              let new_link = match expr_typ.full_copy() {
                DAG::Single(link) => link,
                _ => panic!("Error in the full_copy implementation")
              };
              let Single { var: new_var, body: new_body, .. } = unsafe { &*new_link.as_ptr() };
              new_var.map(|var| {
                if get_parents(DAG::Var(var)).is_some() {
                  let mut expr_dag = DAG::from_open_term(true, depth, &expr);
                  expr_dag.uproot();
                  replace_child(DAG::Var(var), expr_dag);
                }
              });
              replace_child(DAG::Single(new_link), *new_body);
              free_dead_node(DAG::Single(new_link));

              Ok((expr_ctx, *new_body))
            }
            _ => Err(CheckError::GenericError(String::from("Tried to case on something that isn't a datatype."))),
          }
        },
        _ => Err(CheckError::GenericError(String::from("Tried to case on something that isn't a datatype."))),
      }

    }
    Term::All(_, lam_uses, name, dom, img) => {
      let _ = check(pre.clone(), Uses::None, dom, DAG::from_term(&Term::Typ(None)))?;
      let dom_dag = DAG::from_open_term(true, pre.len() as u64, &dom);
      pre.push((name.to_string(), dom_dag));
      let ctx = check(pre, Uses::None, dom, DAG::from_term(&Term::Typ(None)))?;
      Ok((ctx, DAG::from_term(&Term::Typ(None))))
    }
    Term::Slf(_, name, body) => {
      let term_dag = DAG::from_open_term(true, pre.len() as u64, &term);
      pre.push((name.to_string(), term_dag));
      let ctx = check(pre, Uses::None, body, DAG::from_term(&Term::Typ(None)))?;
      Ok((ctx, DAG::from_term(&Term::Typ(None))))
    }
    Term::Let(_, _, _, _, _, _, _) => {
      panic!("TODO")
    }
    Term::Typ(_) => {
      Ok((to_ctx(pre), DAG::from_term(&Term::Typ(None))))
    }
    Term::Ann(_, typ, expr) => {
      let typ_dag = DAG::from_open_term(true, pre.len() as u64, &expr);
      let ctx = check(pre, uses, expr, typ_dag)?;
      Ok((ctx, typ_dag))
    }
    Term::Lit(_, _) => {
      panic!("TODO")
    }
    Term::LTy(_, _) => {
      panic!("TODO")
    }
    Term::Opr(_, _) => {
      panic!("TODO")
    }
    Term::Lam(_, _, _) => {
      Err(CheckError::GenericError(String::from("Untyped lambda.")))
    }
    _ => {
      Err(CheckError::GenericError(String::from("Cannot infer type.")))
    }
  }
}
