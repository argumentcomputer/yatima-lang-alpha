use crate::term::Link;
use hashexpr::Expr;
use im::HashMap;

// TODO: replace this with Hash and Hasher instances
pub type Cache = HashMap<Link, Expr>;
