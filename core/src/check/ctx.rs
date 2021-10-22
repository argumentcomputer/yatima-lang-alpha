use crate::{
  dag::*,
  term::Term,
  uses::*,
};

use alloc::{
  collections::{
    btree_map::BTreeMap,
    vec_deque::VecDeque,
  },
  string::String,
  vec::Vec,
};

// Optimization: use of `*mut DAGPtr` instead of `DAG`. The context does not
// need to hold full, "autonomous" DAGs. Instead it can keep references to parts
// of the terms in the typechecker. The reason it must be a pointer of a DAG
// pointer is because the DAG pointer itself might change during typechecking.
// This double pointer is actually a pointer to the parent's field that
// references the node. It does not ever change since it is already in whnf by
// the time we add the child node to the context.
/// A typechecking ctx which uses λ-DAG pointers
pub type Ctx = Vec<(String, Uses, *mut DAGPtr)>;

/// Add two contexts and multiply by a Uses scalar
#[inline]
pub fn add_mul_ctx(uses: Uses, use_ctx: &mut Ctx, use_ctx2: Ctx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i].1 = (uses * use_ctx[i].1) + use_ctx2[i].1
  }
}

/// Add two contexts
#[inline]
pub fn add_ctx(use_ctx: &mut Ctx, use_ctx2: Ctx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i].1 = use_ctx[i].1 + use_ctx2[i].1
  }
}

/// Multiply a context by a Uses scalar
#[inline]
pub fn mul_ctx(uses: Uses, use_ctx: &mut Ctx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i].1 = use_ctx[i].1 * uses
  }
}

/// Divide a context by a Uses scalar
#[inline]
pub fn div_ctx(uses: Uses, use_ctx: &mut Ctx) -> Ctx {
  let mut rest = use_ctx.clone();
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    rest[i].1 = use_ctx[i].1 % uses;
    use_ctx[i].1 = use_ctx[i].1 / uses
  }
  rest
}

/// An direct representation of context suitable for error reporting
pub type ErrCtx = VecDeque<(String, Uses, Term)>;

/// Make an ErrCtx from a Ctx
pub fn error_context(ctx: &Ctx) -> ErrCtx {
  let mut res: ErrCtx = VecDeque::new();
  for (n, uses, ptr) in ctx {
    let dagptr = unsafe { **ptr };
    let mut map = BTreeMap::new();
    res.push_back((
      n.clone(),
      *uses,
      DAG::dag_ptr_to_term(&dagptr, &mut map, 0, false),
    ));
  }
  res
}

/// Formats the context for pretty-printing
pub fn pretty_context(ctx: &ErrCtx) -> String {
  let mut res: String = String::new();
  for (n, uses, typ) in ctx {
    res.push_str(&format!("- {} {}: {}\n", uses, n, typ))
  }
  res
}
