use crate::{
  name::Name,
  uses::Uses,
  defs::Defs,
  term::Term,
  literal::{
    LitType,
    Literal,
  },
  prim::Op,
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
  Let(Uses, Name, Rc<IR>, Rc<IR>, FreeVars, Rc<IR>),
  Fix(Name, FreeVars, Rc<IR>),
  Lit(Literal),
  LTy(LitType),
  Opr(Op),
}

pub fn defs_to_ir(defs: &Defs) -> Vec<(Name, IR, IR)> {
  let mut done = BTreeMap::new();
  let mut ir_defs = vec![];
  for (nam, cid) in &defs.names {
    if !done.contains_key(cid) {
      let idx = done.len();
      done.insert(*cid, idx);
      ir_defs.push((Name::from(""), IR::Typ, IR::Typ));
      let def = &defs.defs.get(cid).unwrap();
      let term = &def.term;
      let typ = &def.typ_;
      let (term_ir, _) = term_to_ir(&mut done, &mut ir_defs, idx, term, defs);
      let (typ_ir, _) = term_to_ir(&mut done, &mut ir_defs, idx, typ, defs);
      ir_defs[idx] = (nam.clone(), term_ir, typ_ir);
    }
  }
  ir_defs
}

pub fn term_to_ir(
  done: &mut BTreeMap<Cid, usize>,
  ir_defs: &mut Vec<(Name, IR, IR)>,
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
          ir_defs.push((Name::from(""), IR::Typ, IR::Typ));
          let def = &defs.defs.get(def_link).unwrap();
          let term = &def.term;
          let typ = &def.typ_;
          let (term_ir, _) = term_to_ir(done, ir_defs, idx, term, defs);
          let (typ_ir, _) = term_to_ir(done, ir_defs, idx, typ, defs);
          ir_defs[idx] = (nam.clone(), term_ir, typ_ir);
          (IR::Ref(idx), set)
        },
      }
    },
    Term::Typ(_) => {
      (IR::Typ, FreeVars::new())
    }
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
    Term::Let(_, false, uses, nam, link) => {
      let (typ, exp, bod) = &**link;
      let (typ, typ_set) = term_to_ir(done, ir_defs, rec_idx, typ, defs);
      let (exp, exp_set) = term_to_ir(done, ir_defs, rec_idx, exp, defs);
      let (bod, mut bod_set) = term_to_ir(done, ir_defs, rec_idx, bod, defs);
      bod_set.bind();
      let ir = IR::Let(*uses, nam.clone(), Rc::new(typ), Rc::new(exp), bod_set.clone(), Rc::new(bod));
      let set = FreeVars::union(&typ_set, &exp_set);
      let set = FreeVars::union(&set, &bod_set);
      (ir, set)
    },
    Term::Let(_, true, uses, nam, link) => {
      let (typ, exp, bod) = &**link;
      let (typ, typ_set) = term_to_ir(done, ir_defs, rec_idx, typ, defs);
      let (exp, mut exp_set) = term_to_ir(done, ir_defs, rec_idx, exp, defs);
      let (bod, mut bod_set) = term_to_ir(done, ir_defs, rec_idx, bod, defs);
      exp_set.bind();
      bod_set.bind();
      let fix = IR::Fix(nam.clone(), exp_set.clone(), Rc::new(exp));
      let ir = IR::Let(*uses, nam.clone(), Rc::new(typ), Rc::new(fix), bod_set.clone(), Rc::new(bod));
      let set = FreeVars::union(&typ_set, &exp_set);
      let set = FreeVars::union(&set, &bod_set);
      (ir, set)
    },
    Term::LTy(_, lty) => {
      (IR::LTy(*lty), FreeVars::new())
    }
    Term::Lit(_, lit) => {
      (IR::Lit(lit.clone()), FreeVars::new())
    }
    Term::Opr(_, opr) => {
      (IR::Opr(*opr), FreeVars::new())
    }
  }
}
