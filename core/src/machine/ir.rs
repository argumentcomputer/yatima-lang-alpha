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
  yatima,
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

// Auxiliary definition positions
pub const UNARY_FUNC_TYPE: usize = 0;
pub const BINARY_FUNC_TYPE: usize = 1;
pub const TRUE_EXPANSION: usize = 2;
pub const FALSE_EXPANSION: usize = 3;
pub const ZERO_EXPANSION: usize = 4;
pub const SUCC_EXPANSION: usize = 5;
pub const NIL_EXPANSION: usize = 6;
pub const CONS_EXPANSION: usize = 7;
pub const DEF_START: usize = 8;

pub fn aux_defs() -> Vec<(Name, IR, IR)> {
  let defs = &mut Defs::new();
  let done = &mut BTreeMap::new();
  let ir_defs = &mut vec![];
  let (unary_func, _) = term_to_ir(defs, done, ir_defs, 0, &yatima!("λ A B => ∀ A -> B"));
  let (binary_func, _) = term_to_ir(defs, done, ir_defs, 0, &yatima!("λ A B C => ∀ A B -> C"));
  let (bool_true, _) = term_to_ir(defs, done, ir_defs, 0, &yatima!("λ P t f => t"));
  let (bool_false, _) = term_to_ir(defs, done, ir_defs, 0, &yatima!("λ P t f => f"));
  let (nat_zero, _) = term_to_ir(defs, done, ir_defs, 0, &yatima!("λ P z s => z"));
  let (nat_succ, _) = term_to_ir(defs, done, ir_defs, 0, &yatima!("λ n P z s => s n"));
  let (nil, _) = term_to_ir(defs, done, ir_defs, 0, &yatima!("λ P n c => n"));
  let (cons, _) = term_to_ir(defs, done, ir_defs, 0, &yatima!("λ x xs P n c => c x xs"));
  vec![
    (Name::from("#UNARY_FUNC_TYPE"), unary_func, IR::Typ),
    (Name::from("#BINARY_FUNC_TYPE"), binary_func, IR::Typ),
    (Name::from("#TRUE_EXPANSION"), bool_true, IR::Typ),
    (Name::from("#FALSE_EXPANSION"), bool_false, IR::Typ),
    (Name::from("#ZERO_EXPANSION"), nat_zero, IR::Typ),
    (Name::from("#SUCC_EXPANSION"), nat_succ, IR::Typ),
    (Name::from("#NIL_EXPANSION"), nil, IR::Typ),
    (Name::from("#CONS_EXPANSION"), cons, IR::Typ),
  ]
}

pub fn defs_to_ir(defs: &Defs) -> Vec<(Name, IR, IR)> {
  let mut done = BTreeMap::new();
  let mut ir_defs = aux_defs();
  for (nam, cid) in &defs.names {
    if !done.contains_key(cid) {
      let idx = ir_defs.len();
      done.insert(*cid, idx);
      ir_defs.push((Name::from(""), IR::Typ, IR::Typ));
      let def = &defs.defs.get(cid).unwrap();
      let term = &def.term;
      let typ = &def.typ_;
      let (term_ir, _) = term_to_ir(defs, &mut done, &mut ir_defs, idx, term);
      let (typ_ir, _) = term_to_ir(defs, &mut done, &mut ir_defs, idx, typ);
      ir_defs[idx] = (nam.clone(), term_ir, typ_ir);
    }
  }
  ir_defs
}

pub fn term_to_ir(
  defs: &Defs,
  done: &mut BTreeMap<Cid, usize>,
  ir_defs: &mut Vec<(Name, IR, IR)>,
  rec_idx: usize,
  term: &Term,
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
      let (bod, mut set) = term_to_ir(defs, done, ir_defs, rec_idx, bod);
      set.bind();
      let ir = IR::Lam(nam.clone(), set.clone(), Rc::new(bod));
      (ir, set)
    },
    Term::App(_, link) => {
      let (fun, arg) = &**link;
      let (fun, fun_set) = term_to_ir(defs, done, ir_defs, rec_idx, fun);
      let (arg, arg_set) = term_to_ir(defs, done, ir_defs, rec_idx, arg);
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
          let idx = ir_defs.len();
          done.insert(*def_link, idx);
          ir_defs.push((Name::from(""), IR::Typ, IR::Typ));
          let def = &defs.defs.get(def_link).unwrap();
          let term = &def.term;
          let typ = &def.typ_;
          let (term_ir, _) = term_to_ir(defs, done, ir_defs, idx, term);
          let (typ_ir, _) = term_to_ir(defs, done, ir_defs, idx, typ);
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
      let (dom, dom_set) = term_to_ir(defs, done, ir_defs, rec_idx, dom);
      let (img, mut img_set) = term_to_ir(defs, done, ir_defs, rec_idx, img);
      img_set.bind();
      let ir = IR::All(*uses, nam.clone(), Rc::new(dom), img_set.clone(), Rc::new(img));
      let set = FreeVars::union(&dom_set, &img_set);
      (ir, set)
    },
    Term::Slf(_, nam, link) => {
      let bod = &**link;
      let (bod, mut set) = term_to_ir(defs, done, ir_defs, rec_idx, bod);
      set.bind();
      let ir = IR::Slf(nam.clone(), set.clone(), Rc::new(bod));
      (ir, set)
    },
    Term::Dat(_, link) => {
      let bod = &**link;
      let (bod, set) = term_to_ir(defs, done, ir_defs, rec_idx, bod);
      let ir = IR::Dat(Rc::new(bod));
      (ir, set)
    },
    Term::Cse(_, link) => {
      let bod = &**link;
      let (bod, set) = term_to_ir(defs, done, ir_defs, rec_idx, bod);
      let ir = IR::Cse(Rc::new(bod));
      (ir, set)
    },
    Term::Ann(_, link) => {
      let (typ, exp) = &**link;
      let (typ, typ_set) = term_to_ir(defs, done, ir_defs, rec_idx, typ);
      let (exp, exp_set) = term_to_ir(defs, done, ir_defs, rec_idx, exp);
      let ir = IR::Ann(Rc::new(typ), Rc::new(exp));
      let set = FreeVars::union(&typ_set, &exp_set);
      (ir, set)
    },
    Term::Let(_, false, uses, nam, link) => {
      let (typ, exp, bod) = &**link;
      let (typ, typ_set) = term_to_ir(defs, done, ir_defs, rec_idx, typ);
      let (exp, exp_set) = term_to_ir(defs, done, ir_defs, rec_idx, exp);
      let (bod, mut bod_set) = term_to_ir(defs, done, ir_defs, rec_idx, bod);
      bod_set.bind();
      let ir = IR::Let(*uses, nam.clone(), Rc::new(typ), Rc::new(exp), bod_set.clone(), Rc::new(bod));
      let set = FreeVars::union(&typ_set, &exp_set);
      let set = FreeVars::union(&set, &bod_set);
      (ir, set)
    },
    Term::Let(_, true, uses, nam, link) => {
      let (typ, exp, bod) = &**link;
      let (typ, typ_set) = term_to_ir(defs, done, ir_defs, rec_idx, typ);
      let (exp, mut exp_set) = term_to_ir(defs, done, ir_defs, rec_idx, exp);
      let (bod, mut bod_set) = term_to_ir(defs, done, ir_defs, rec_idx, bod);
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
