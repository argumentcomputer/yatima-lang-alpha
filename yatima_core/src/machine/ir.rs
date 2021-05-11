use crate::machine::freevars::{
  FreeVars
};

use crate::{
  term::Term,
  defs::{
    Def,
    Defs
  },
};

use std::boxed::Box;
use std::collections::HashSet;
use im::HashMap;
use cid::Cid;

pub struct IR {
  globals: Vec<Obj>,
}

pub enum Obj {
  Func(Func),
  Thunk(Thunk),
}

pub struct Func {
  ari: usize,
  env: FreeVars,
  bod: Box<Expr>,
}

pub struct Thunk {
  env: FreeVars,
  bod: Box<Expr>,
}

pub enum Expr {
  Let(Obj, Box<Expr>),
  Stack(Unboxed, Box<Expr>),
  App(Atom, Vec<Atom>),
}

pub enum Unboxed {
  Int(IntExpr),
}
pub enum IntExpr {
  Int(i64),
  Add(Box<(IntExpr, IntExpr)>),
  Mul(Box<(IntExpr, IntExpr)>),
}

pub enum Atom {
  Var(usize),
  Ref(usize),
}

pub fn defs_to_ir(defs: &Defs) -> IR {
  let mut done = HashSet::new();
  let mut globals = vec![];
  for def in &defs.defs {
    if done.insert(*def.0) {
      add_global(&mut done, &mut globals, &def.1.term);
    }
  }
  IR { globals }
}

pub fn add_global(done: &mut HashSet<Cid>, globals: &mut Vec<Obj>, term: &Term) {
  fn build_obj(done: &mut HashSet<Cid>, globals: &mut Vec<Obj>, term: &Term) -> Obj {
    let mut ari = 0;
    let mut term = term;
    loop {
      match term {
        Term::Lam(_, _, bod) => {
          term = bod.as_ref();
          ari = ari+1;
        }
        Term::Ann(_, typ_exp) => {
          let (_, exp) = &**typ_exp;
          term = exp;
        }
        _ => break
      }
    }
    let (mut env, bod) = build_expr(done, globals, term);
    if ari == 0 {
      Obj::Thunk(Thunk {
        env,
        bod,
      })
    }
    else {
      env.bind_many(ari);
      Obj::Func(Func {
        ari,
        env,
        bod,
      })
    }
  }
  fn build_expr(done: &mut HashSet<Cid>, globals: &mut Vec<Obj>, mut term: &Term) -> (FreeVars, Box<Expr>) {
    let mut freevars = FreeVars::new();
    let mut let_trail = vec![];
    let mut arg_trail = vec![];
    let term = collect_lets(term, &mut let_trail, &mut freevars);
    let head = collect_apps(term, &mut let_trail, &mut arg_trail, &mut freevars);
    let mut term = Box::new(
      Expr::App(head, arg_trail)
    );
    while let Some(obj) = let_trail.pop() {
      term = Box::new(
        Expr::Let(obj, term)
      );
    }
    (freevars, term)
  }
  #[inline]
  fn collect_lets<'a>(term: &'a Term, let_trail: &mut Vec<Obj>, freevars: &mut FreeVars) -> &'a Term {
    todo!()
  }
  #[inline]
  fn collect_apps(term: &Term, let_trail: &mut Vec<Obj>, arg_trail: &mut Vec<Atom>, freevars: &mut FreeVars) -> Atom {
    todo!()
  }
  let new_global = build_obj(done, globals, term);
  globals.push(new_global);
}
