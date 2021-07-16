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
      let idx = done.len();
      done.insert(*cid, idx);
      ir_defs.push((Name::from(""), IR::Typ));
      let term = &defs.defs.get(cid).unwrap().term;
      let (ir, _) = term_to_ir(&mut done, &mut ir_defs, idx, term, defs);
      ir_defs[idx] = (nam.clone(), ir);
    }
  }
  ir_defs
}

pub fn term_to_ir(
  done: &mut BTreeMap<Cid, usize>,
  ir_defs: &mut Vec<(Name, IR)>,
  rec_idx: usize,
  term: &Term,
  defs: &Defs
) -> (IR, FreeVars) {
  // TODO: add sharing
  match term {
    Term::Var(_, name, idx) => {
      let ir = IR::Var(name.clone(), *idx);
      let set = FreeVars::singleton(*idx);
      (ir, set)
    },
    Term::Lam(_, nam, link) => {
      let bod = &**link;
      let (bod, mut set) = term_to_ir(done, ir_defs, rec_idx, bod, defs);
      set.bind();
      let ir = IR::Lam(nam.clone(), set.clone(), Rc::new(bod));
      (ir, set)
    },
    Term::App(_, link) => {
      let (fun, arg) = &**link;
      let (fun, fun_set) = term_to_ir(done, ir_defs, rec_idx, fun, defs);
      let (arg, arg_set) = term_to_ir(done, ir_defs, rec_idx, arg, defs);
      let ir = IR::App(Rc::new(fun), Rc::new(arg));
      let set = FreeVars::union(&fun_set, &arg_set);
      (ir, set)
    },
    Term::Rec(_) => {
      let set = FreeVars::new();
      (IR::Ref(rec_idx), set)
    }
    Term::Ref(_, nam, def_link, _) => {
      let set = FreeVars::new();
      match done.get(def_link) {
        Some(idx) => (IR::Ref(*idx), set),
        None => {
          let idx = done.len();
          done.insert(*def_link, idx);
          ir_defs.push((Name::from(""), IR::Typ));
          let term = &defs
            .defs
            .get(def_link)
            .unwrap()
            .term;
          let (ir, _) = term_to_ir(done, ir_defs, idx, term, defs);
          ir_defs[idx] = (nam.clone(), ir);
          (IR::Ref(idx), set)
        },
      }
    },
    Term::Typ(_) => (IR::Typ, FreeVars::new()),
    Term::All(_, uses, nam, link) => {
      let (dom, img) = &**link;
      let (dom, dom_set) = term_to_ir(done, ir_defs, rec_idx, dom, defs);
      let (img, mut img_set) = term_to_ir(done, ir_defs, rec_idx, img, defs);
      img_set.bind();
      let ir = IR::All(*uses, nam.clone(), Rc::new(dom), img_set.clone(), Rc::new(img));
      let set = FreeVars::union(&dom_set, &img_set);
      (ir, set)
    },
    Term::Slf(_, nam, link) => {
      let bod = &**link;
      let (bod, mut set) = term_to_ir(done, ir_defs, rec_idx, bod, defs);
      set.bind();
      let ir = IR::Slf(nam.clone(), set.clone(), Rc::new(bod));
      (ir, set)
    },
    Term::Dat(_, link) => {
      let bod = &**link;
      let (bod, set) = term_to_ir(done, ir_defs, rec_idx, bod, defs);
      let ir = IR::Dat(Rc::new(bod));
      (ir, set)
    },
    Term::Cse(_, link) => {
      let bod = &**link;
      let (bod, set) = term_to_ir(done, ir_defs, rec_idx, bod, defs);
      let ir = IR::Cse(Rc::new(bod));
      (ir, set)
    },
    Term::Ann(_, link) => {
      let (typ, exp) = &**link;
      let (typ, typ_set) = term_to_ir(done, ir_defs, rec_idx, typ, defs);
      let (exp, exp_set) = term_to_ir(done, ir_defs, rec_idx, exp, defs);
      let ir = IR::Ann(Rc::new(typ), Rc::new(exp));
      let set = FreeVars::union(&typ_set, &exp_set);
      (ir, set)
    },
    _ => panic!("Not yet implemented {}", term)
  }
}
