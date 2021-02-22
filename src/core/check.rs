#![allow(unused_variables)]

use std::error::Error;
use std::fmt;

use crate::core::{
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
    whnf
  },
  uses::*
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

fn check(mut pre: PreCtx, uses: Uses, term: DAG, mut typ: DAG) -> Result<Ctx, CheckError> {
  match term {
    DAG::Var(link) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }
    DAG::Single(link) => {
      let Single { tag, var, body: term_body, .. } = unsafe { &*link.as_ptr() };
      match tag {
        SingleTag::Lam => {
          // A potential problem is that `term` might also mutate, which is an unwanted side-effect,
          // since `term` and `typ` might be coupled. To deal with this we will need to make copies
          // of `term` in the self rule
          whnf(typ);
          match typ {
            DAG::Branch(link) => {
              let Branch { tag, left: dom, right: img, .. } = unsafe { &*link.as_ptr() };
              match tag {
                BranchTag::All(lam_uses) => {
                  let name = match var {
                    Some(link) => unsafe { (*link.as_ptr()).name.clone() },
                    None => panic!("Malformed DAG"),
                  };
                  pre.push((name, *dom));
                  let mut ctx = check(pre, Uses::Once, *term_body, *img)?;
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

        SingleTag::Dat => {
          whnf(typ);
          match typ {
            DAG::Single(link) => {
              let Single { tag, body: dat_body, .. } = unsafe { &*link.as_ptr() };
              match tag {
                SingleTag::Slf => {
                  // let ctx = check(pre, Uses::Once, *term_body, *img)?;
                  Err(CheckError::GenericError(String::from("TODO")))
                }
                _ => Err(CheckError::GenericError(String::from("The type of data must be a forall."))),
              }
            },
            _ => Err(CheckError::GenericError(String::from("The type of data must be a forall."))),
          }
        },

        SingleTag::Fix => {
          Err(CheckError::GenericError(String::from("TODO")))
        },

        _ => Err(CheckError::GenericError(String::from("TODO"))),
      }
    },

    _ => Err(CheckError::GenericError(String::from("TODO"))),
  }
}

fn infer(pre: PreCtx, uses: Uses, term: DAG) -> Result<(Ctx, DAG), CheckError> {
  match term {
    DAG::Var(link) => {
      Err(CheckError::GenericError(String::from("TODO")))
    }

    DAG::Leaf(link) => {
      let Leaf { tag, .. } = unsafe { &*link.as_ptr() };
      match tag {
        LeafTag::Typ => Err(CheckError::GenericError(String::from("TODO"))),
        LeafTag::LTy(lty) => Err(CheckError::GenericError(String::from("TODO"))),
        LeafTag::Lit(lit) => Err(CheckError::GenericError(String::from("TODO"))),
        LeafTag::Opr(opr) => Err(CheckError::GenericError(String::from("TODO"))),
        LeafTag::Ref(nam, _, _) => Err(CheckError::GenericError(String::from("TODO"))),
      }
    },
    
    DAG::Single(link) => {
      let Single { tag, .. } = unsafe { &*link.as_ptr() };
      match tag {
        SingleTag::Lam => Err(CheckError::GenericError(String::from("TODO"))),
        SingleTag::Slf => Err(CheckError::GenericError(String::from("TODO"))),
        SingleTag::Fix => Err(CheckError::GenericError(String::from("TODO"))),
        SingleTag::Cse => Err(CheckError::GenericError(String::from("TODO"))),
        SingleTag::Dat => Err(CheckError::GenericError(String::from("TODO"))),
      }
    },
    
    DAG::Branch(link) => {
      let Branch { tag, .. } = unsafe { &*link.as_ptr() };
      match tag {
        BranchTag::All(uses) => Err(CheckError::GenericError(String::from("TODO"))),
        BranchTag::App => Err(CheckError::GenericError(String::from("TODO"))),
        BranchTag::Ann => Err(CheckError::GenericError(String::from("TODO"))),
      }
    },
  }
}
