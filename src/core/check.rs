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
    DAG::Single(link) => {
      let Single { tag, body: term_body, .. } = unsafe { &*link.as_ptr() };
      match tag {
        SingleTag::Lam => {
          // A potential problem is that `term` might also mutate, which is an unwanted side-effect,
          // since `term` and `typ` might be coupled. To deal with this we will need to make copies
          // of `term` in the self rule
          whnf(typ);
          match typ {
            DAG::Branch(link) => {
              let Branch { tag, left: bind, right: typ_body, .. } = unsafe { &*link.as_ptr() };
              match tag {
                BranchTag::All(uses) => {
                  // pre.push((name.clone(), bind));
                  // let body_ctx = check(pre, Uses::Once, term_body, typ_body)?;
                  Err(CheckError::GenericError(String::from("The type of a lambda must be a forall.")))
                }
                _ => Err(CheckError::GenericError(String::from("The type of a lambda must be a forall."))),
              }
            },
            _ => Err(CheckError::GenericError(String::from("The type of a lambda must be a forall."))),
          }
        },

        SingleTag::Slf => {
          Err(CheckError::GenericError(String::from("TODO")))
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
    DAG::Leaf(link) => {
      let Leaf { tag, .. } = unsafe { &*link.as_ptr() };
      match tag {
        LeafTag::Typ => Err(CheckError::GenericError(String::from("TODO"))),
        LeafTag::LTy(lty) => Err(CheckError::GenericError(String::from("TODO"))),
        LeafTag::Lit(lit) => Err(CheckError::GenericError(String::from("TODO"))),
        LeafTag::Opr(opr) => Err(CheckError::GenericError(String::from("TODO"))),
        LeafTag::Ref(nam, link) => Err(CheckError::GenericError(String::from("TODO"))),
        LeafTag::Var(nam) => Err(CheckError::GenericError(String::from("TODO"))),
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
