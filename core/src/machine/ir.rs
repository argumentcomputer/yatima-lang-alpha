use crate::{
  name::Name,
  uses::Uses,
  defs::Defs,
  term::Term,
  machine::freevars::FreeVars,
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
  Typ,
  All(Uses, Name, Rc<IR>, FreeVars, Rc<IR>),
  Slf(Name, FreeVars, Rc<IR>),
  Dat(Rc<IR>),
  Cse(Rc<IR>),
  Ann(Rc<IR>, Rc<IR>),
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
            let term = &defs
              .defs
              .get(def_link)
              .unwrap()
              .term;
            // No issue recursing down, because refs are not cyclic
            let ir = term_to_ir(done, ir_defs, term, defs);
            let idx = ir_defs.len();
            done.insert(*def_link, idx);
            ir_defs.push((nam.clone(), ir));
            (IR::Ref(idx), set)
          },
        }
      },
      Term::Typ(_) => (IR::Typ, FreeVars::new()),
      Term::All(_, uses, nam, link) => {
        let (dom, img) = &**link;
        let (dom, dom_set) = go(done, ir_defs, dom, defs);
        let (img, mut img_set) = go(done, ir_defs, img, defs);
        img_set.bind();
        let ir = IR::All(*uses, nam.clone(), Rc::new(dom), img_set.clone(), Rc::new(img));
        let set = FreeVars::union(&dom_set, &img_set);
        (ir, set)
      },
      Term::Slf(_, nam, link) => {
        let bod = &**link;
        let (bod, mut set) = go(done, ir_defs, bod, defs);
        set.bind();
        let ir = IR::Slf(nam.clone(), set.clone(), Rc::new(bod));
        (ir, set)
      },
      Term::Dat(_, link) => {
        let bod = &**link;
        let (bod, set) = go(done, ir_defs, bod, defs);
        let ir = IR::Dat(Rc::new(bod));
        (ir, set)
      },
      Term::Cse(_, link) => {
        let bod = &**link;
        let (bod, set) = go(done, ir_defs, bod, defs);
        let ir = IR::Cse(Rc::new(bod));
        (ir, set)
      },
      Term::Ann(_, link) => {
        let (typ, exp) = &**link;
        let (typ, typ_set) = go(done, ir_defs, typ, defs);
        let (exp, exp_set) = go(done, ir_defs, exp, defs);
        let ir = IR::Ann(Rc::new(typ), Rc::new(exp));
        let set = FreeVars::union(&typ_set, &exp_set);
        (ir, set)
      },
      _ => panic!("Not yet implemented {}", term)
    }
  }
  let (ir, _) = go(done, ir_defs, term, defs);
  ir
}
