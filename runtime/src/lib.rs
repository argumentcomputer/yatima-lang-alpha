use std::rc::Rc;

use yatima_core::defs::{
  Def,
  Defs,
};

mod runtime;
mod transform;

pub fn run(def: Def, checked: Rc<Defs>) {
  let root = runtime::alloc_val(yatima_core::dll::DLL::singleton(
    runtime::ParentPtr::Root,
  ));
  let mut term = def.term;
  transform::transform(checked.clone(), &mut term);
  let mut dag = runtime::from_term(checked, &term, Some(root));
  runtime::whnf(&mut dag, false);
}
