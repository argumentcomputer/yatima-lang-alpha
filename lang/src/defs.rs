use sp_std::collections::btree_map::BTreeMap;

use yatima_core::{
  name::Name,
  position::Pos,
};

use crate::term::Term;

#[derive(Clone, Debug)]
pub struct Def {
  pub pos: Pos,
  pub typ_: Term,
  pub term: Term,
}

pub type Defs = BTreeMap<Name, Def>;
