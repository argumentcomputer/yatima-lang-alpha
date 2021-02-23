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
enum CheckError {
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

fn check(mut pre: PreCtx, uses: Uses, term: &Term, mut typ: DAG) -> Result<Ctx, CheckError> {
  match &term {
    Term::Lam(_, name, term_body) => {
      // A potential problem is that `term` might also mutate, which is an unwanted side-effect,
      // since `term` and `typ` might be coupled. To deal with this we will need to make copies
      // of `term` in the self rule
      whnf(typ);
      match typ {
        DAG::Branch(link) => {
          let Branch { tag, left: dom, right: img, .. } = unsafe { &*link.as_ptr() };
          match tag {
            BranchTag::All(lam_uses) => {
              pre.push((name.to_string(), *dom));
              let mut ctx = check(pre, Uses::Once, &**term_body, *img)?;
              let (_, inf_uses, _) = ctx
                .pop()
                .ok_or(CheckError::GenericError(String::from("Empty context")))?;
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
          let Single { tag, .. } = unsafe { &*link.as_ptr() };
          match tag {
            SingleTag::Slf => {
              let mut term_dag = DAG::from_term(&term);
              term_dag.uproot();
              let typ_body = subst(link, term_dag);
              let ctx = check(pre, uses, dat_body, typ_body)?;
              Ok(ctx)
            }
            _ => Err(CheckError::GenericError(String::from("The type of data must be a self."))),
          }
        },
        _ => Err(CheckError::GenericError(String::from("The type of data must be a self."))),
      }
    },

    _ => Err(CheckError::GenericError(String::from("TODO"))),
  }
}

fn infer(pre: PreCtx, uses: Uses, term: &Term) -> Result<(Ctx, DAG), CheckError> {
  match term {
    Term::Var(_, _, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::Ref(_, _, _, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::Lam(_, _, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::App(_, _, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::Cse(_, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::All(_, _, _, _, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::Slf(_, _, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::Let(_, _, _, _, _, _, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::Typ(_) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::Ann(_, _, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::Lit(_, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::LTy(_, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    Term::Opr(_, _) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    _ => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
  }
}
