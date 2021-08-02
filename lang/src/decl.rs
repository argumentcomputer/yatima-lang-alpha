use yatima_core::{
  literal::{
    LitType,
    Literal,
  },
  name::Name,
  position::Pos,
  prim::Op,
};

use sp_std::collections::btree_map::BTreeMap;

use crate::expr::Expr;

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
  Defn(Pos, DefDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct DefDecl {
  name: Name,
  typ_: Expr,
  term: Expr,
}

pub type Decls = BTreeMap<Name, Decl>;
