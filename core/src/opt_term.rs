use crate::{
  literal::{
    LitType,
    Literal,
  },
  name::Name,
  position::Pos,
  prim::Op,
  term::Term,
  uses::Uses,
};

use sp_cid::Cid;

use sp_std::{
  borrow::ToOwned,
  boxed::Box,
  fmt,
  rc::Rc,
};

#[derive(Clone)]
pub enum OptTerm {
  Var(Pos, Name, u64),
  Lam(Pos, Uses, Name, Option<Box<(OptTerm, OptTerm)>>),
  App(Pos, Uses, Option<Box<(OptTerm, OptTerm, OptTerm)>>),
  All(Pos, Uses, Name, Option<Box<(OptTerm, OptTerm)>>),
  Slf(Pos, Name, Option<Box<OptTerm>>),
  Dat(Pos, Option<Box<(OptTerm, OptTerm)>>),
  Cse(Pos, Option<Box<OptTerm>>),
  Ref(Pos, Name, Cid, Cid),
  Let(Pos, bool, Uses, Name, Option<Box<(OptTerm, OptTerm, OptTerm)>>),
  Typ(Pos),
  Lit(Pos, Literal),
  LTy(Pos, LitType),
  Opr(Pos, Op),
  Rec(Pos),
}
