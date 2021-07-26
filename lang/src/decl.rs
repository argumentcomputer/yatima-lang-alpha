use yatima_core::{
  literal::{
    LitType,
    Literal,
  },
  name::Name,
  position::Pos,
  prim::Op,
};

use crate::expr::Expr;

pub enum Decl {
  Defn(Pos, DefDecl),
}

pub struct DefDecl {
  name: Name,
  typ_: Expr,
  term: Expr,
}
