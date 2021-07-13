use crate::machine::freevars::{
  FreeVars
};

use crate::{
  name::Name,
  defs::Defs,
  term::Term,
};

use sp_std::rc::Rc;

#[derive(Clone, Debug)]
pub enum IR {
  Var(Name, u64),
  Lam(Name, FreeVars, Rc<IR>),
  App(Rc<IR>, Rc<IR>),
}


pub fn term_to_ir(term: &Term, defs: &Defs) -> Rc<IR> {
  fn go(term: &Term, defs: &Defs) -> (Rc<IR>, FreeVars) {
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
        let (bod, mut set) = go(bod, defs);
        set.bind();
        let ir =
          Rc::new(IR::Lam(nam.clone(), set.clone(), bod));
        (ir, set)
      },
      Term::App(_, link) => {
        let (fun, arg) = &**link;
        let (fun, fun_set) = go(fun, defs);
        let (arg, arg_set) = go(arg, defs);
        let ir = Rc::new(IR::App(fun, arg));
        let set = FreeVars::union(&fun_set, &arg_set);
        (ir, set)
      },
      Term::Ref(_, nam, def_link, _) => {
        let def = defs
          .defs
          .get(def_link)
          .unwrap(); // TODO better error
        let set = FreeVars::new();
        // TODO add to globals instead of dereferencing
        (term_to_ir(&def.term, defs), set)
      },
      _ => panic!("Not yet implemented {}", term)
    }
  }
  let (ir, _) = go(term, defs);
  ir
}
