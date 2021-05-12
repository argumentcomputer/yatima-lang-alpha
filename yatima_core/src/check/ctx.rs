use crate::{
  dag::*,
  term::Term,
  uses::*,
};

use im::{
  HashMap,
  Vector,
};

// Optimization: use of `*mut DAGPtr` instead of `DAG`. The context does not
// need to hold full, "autonomous" DAGs. Instead it can keep references to parts
// of the terms in the typechecker. The reason it must be a pointer of a DAG
// pointer is because the DAG pointer itself might change during typechecking.
// This double pointer is actually a pointer to the parent's field that
// references the node. It does not ever change since it is already in whnf by
// the time we add the child node to the context.
pub type Ctx = Vector<(String, *mut DAGPtr)>;

pub type UseCtx = Vector<Uses>;

#[inline]
pub fn add_use_ctx(use_ctx: &mut UseCtx, use_ctx2: UseCtx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i] = use_ctx[i] + use_ctx2[i]
  }
}

#[inline]
pub fn mul_use_ctx(uses: Uses, use_ctx: &mut UseCtx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i] = use_ctx[i] * uses
  }
}

pub type ErrCtx = Vector<(String, Term)>;

pub fn error_context(ctx: &Ctx) -> ErrCtx {
  let mut res: ErrCtx = Vector::new();
  for (n, ptr) in ctx {
    let dagptr = unsafe { **ptr };
    let mut map = HashMap::new();
    res.push_back((
      n.clone(),
      DAG::dag_ptr_to_term(&dagptr, &mut map, 0, false),
    ));
  }
  res
}

pub fn pretty_context(ctx: &ErrCtx) -> String {
  let mut res: String = String::new();
  for (n, typ) in ctx {
    res.push_str(&format!("- {}: {}\n", n, typ))
  }
  res
}
