use crate::{
  machine::freevars::FreeVars,
  name::Name,
  defs::Defs,
  term::Term,
};

use sp_std::{
  rc::Rc,
  vec::Vec,
  collections::btree_map::BTreeMap,
};

use sp_cid::Cid;

#[derive(Clone, Debug)]
pub enum IR {
  Var(Name, u64),
  Ref(usize),
  Lam(Name, FreeVars, Rc<IR>),
  App(Rc<IR>, Rc<IR>),
}

pub fn defs_to_ir(defs: &Defs) -> Vec<(Name, IR)> {
  let mut done = BTreeMap::new();
  let mut ir_defs = vec![];
  for (nam, cid) in &defs.names {
    if !done.contains_key(cid) {
      done.insert(*cid, ir_defs.len());
      let term = &defs.defs.get(cid).unwrap().term;
      let ir = term_to_ir(&mut done, &mut ir_defs, term, defs);
      ir_defs.push((nam.clone(), ir));
    }
  }
  ir_defs
}

pub fn term_to_ir(done: &mut BTreeMap<Cid, usize>, ir_defs: &mut Vec<(Name, IR)>, term: &Term, defs: &Defs) -> IR {
  fn go(done: &mut BTreeMap<Cid, usize>, ir_defs: &mut Vec<(Name, IR)>, term: &Term, defs: &Defs) -> (IR, FreeVars) {
    // TODO: add sharing
    match term {
      Term::Var(_, name, idx) => {
        let ir = IR::Var(name.clone(), *idx);
        let set = FreeVars::singleton(*idx);
        (ir, set)
      },
      Term::Lam(_, nam, link) => {
        let bod = &**link;
        let (bod, mut set) = go(done, ir_defs, bod, defs);
        set.bind();
        let ir = IR::Lam(nam.clone(), set.clone(), Rc::new(bod));
        (ir, set)
      },
      Term::App(_, link) => {
        let (fun, arg) = &**link;
        let (fun, fun_set) = go(done, ir_defs, fun, defs);
        let (arg, arg_set) = go(done, ir_defs, arg, defs);
        let ir = IR::App(Rc::new(fun), Rc::new(arg));
        let set = FreeVars::union(&fun_set, &arg_set);
        (ir, set)
      },
      Term::Ref(_, nam, def_link, _) => {
        let set = FreeVars::new();
        match done.get(def_link) {
          Some(idx) => (IR::Ref(*idx), set),
          None => {
            let idx = ir_defs.len();
            done.insert(*def_link, idx);
            let term = &defs
              .defs
              .get(def_link)
              .unwrap() // TODO better error
              .term;
            let ir = term_to_ir(done, ir_defs, term, defs);
            ir_defs.push((nam.clone(), ir));
            (IR::Ref(idx), set)
          },
        }
      },
      _ => panic!("Not yet implemented {}", term)
    }
  }
  let (ir, _) = go(done, ir_defs, term, defs);
  ir
}
