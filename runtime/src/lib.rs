use std::rc::Rc;

use yatima_core::defs::{
  Def,
  Defs,
};

mod runtime;
mod transform;
use transform::RuntimeIO;

pub fn run(def: Def, checked: Rc<Defs>) {
  let root = runtime::alloc_val(yatima_core::dll::DLL::singleton(
    runtime::ParentPtr::Root,
  ));
  let mut term = def.term;
  transform::transform(checked.clone(), &mut term, transform::DefaultRuntime {});
  let mut dag = runtime::from_term(checked, &term, Some(root));
  runtime::whnf(&mut dag, false);
}

pub fn run_with_runtime<R: RuntimeIO>(def: Def, checked: Rc<Defs>, runtime: R) {
  let root = runtime::alloc_val(yatima_core::dll::DLL::singleton(
    runtime::ParentPtr::Root,
  ));
  let mut term = def.term;
  transform::transform(checked.clone(), &mut term, runtime);
  let mut dag = runtime::from_term(checked, &term, Some(root));
  runtime::whnf(&mut dag, false);
}