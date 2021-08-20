use std::rc::Rc;

use yatima_core::{
  defs::{
    Defs,
  },
  term::Term,
};

mod runtime;
pub mod transform;
use transform::RunIO;

/// Reduce terms to WHNF and execute any IoOp with the provided RuntimeIO
pub fn run(term: &mut Term, checked: Rc<Defs>, runtime: RunIO) {
  let root = runtime::alloc_val(yatima_core::dll::DLL::singleton(runtime::ParentPtr::Root));
  transform::transform(checked.clone(), term, runtime);
  let mut dag = runtime::from_term(checked, &term, Some(root));
  runtime::whnf(&mut dag, false);
}
