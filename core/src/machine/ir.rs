use crate::machine::freevars::{
  FreeVars
};

use crate::{
  name::Name,
  term::Term,
};

use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum IR {
  Var(Name, u64),
  Lam(Name, FreeVars, Rc<IR>),
  App(Rc<IR>, Rc<IR>),
}


pub fn term_to_ir(term: &Term) -> Rc<IR> {
  fn go(term: &Term) -> (Rc<IR>, FreeVars) {
    // TODO: add sharing
    match term {
      Term::Var(_, name, idx) => {
        let ir =
          Rc::new(IR::Var(name.clone(), *idx));
        let set = FreeVars::singleton(*idx);
        (ir, set)
      },
      Term::Lam(_, nam, link) => {
        let bod = &**link;
        let (bod, mut set) = go(bod);
        set.bind();
        let ir =
          Rc::new(IR::Lam(nam.clone(), set.clone(), bod));
        (ir, set)
      },
      Term::App(_, link) => {
        let (fun, arg) = &**link;
        let (fun, fun_set) = go(fun);
        let (arg, arg_set) = go(arg);
        let ir = Rc::new(IR::App(fun, arg));
        let set = FreeVars::union(&fun_set, &arg_set);
        (ir, set)
      },
      _ => todo!()
    }
  }
  let (ir, _) = go(term);
  ir
}
