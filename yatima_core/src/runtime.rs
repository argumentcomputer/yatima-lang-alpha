use crate::{
  defs::Def,
  dll::*,
  literal::{
    LitType,
    Literal,
  },
  primop::PrimOp,
  term::Term,
};

use core::ptr::NonNull;
use std::mem;

pub type Parents = DLL<ParentPtr>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum DAG {
  Var(NonNull<Var>),
  Lam(NonNull<Lam>),
  App(NonNull<App>),
  Fix(NonNull<Fix>),
  Lit(NonNull<Lit>),
  Opr(NonNull<Opr>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ParentPtr {
  Root,
  LamBod(NonNull<Lam>),
  FixBod(NonNull<Fix>),
  AppLam(NonNull<App>),
  AppArg(NonNull<App>),
}

// Runtime DAG nodes
#[derive(Clone)]
pub struct Var {
  pub parents: Option<NonNull<Parents>>,
}

pub struct Lam {
  pub bod: DAG,
  pub bod_ref: Parents,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

pub struct App {
  pub fun: DAG,
  pub arg: DAG,
  pub fun_ref: Parents,
  pub arg_ref: Parents,
  pub copy: Option<NonNull<App>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Fix {
  pub bod: DAG,
  pub bod_ref: Parents,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Lit {
  pub lit: Literal,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Opr {
  pub opr: PrimOp,
  pub parents: Option<NonNull<Parents>>,
}

// Auxiliary parent functions
#[inline]
pub fn get_parents(term: DAG) -> Option<NonNull<Parents>> {
  unsafe {
    match term {
      DAG::Var(link) => (*link.as_ptr()).parents,
      DAG::Lam(link) => (*link.as_ptr()).parents,
      DAG::App(link) => (*link.as_ptr()).parents,
      DAG::Fix(link) => (*link.as_ptr()).parents,
      DAG::Lit(link) => (*link.as_ptr()).parents,
      DAG::Opr(link) => (*link.as_ptr()).parents,
    }
  }
}

#[inline]
pub fn set_parents(term: DAG, pref: Option<NonNull<Parents>>) {
  unsafe {
    match term {
      DAG::Var(link) => (*link.as_ptr()).parents = pref,
      DAG::Lam(link) => (*link.as_ptr()).parents = pref,
      DAG::App(link) => (*link.as_ptr()).parents = pref,
      DAG::Fix(link) => (*link.as_ptr()).parents = pref,
      DAG::Lit(link) => (*link.as_ptr()).parents = pref,
      DAG::Opr(link) => (*link.as_ptr()).parents = pref,
    }
  }
}

#[inline]
pub fn install_child(parent: &mut ParentPtr, newchild: DAG) {
  unsafe {
    match parent {
      ParentPtr::LamBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::FixBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::AppLam(parent) => (*parent.as_ptr()).fun = newchild,
      ParentPtr::AppArg(parent) => (*parent.as_ptr()).arg = newchild,
      ParentPtr::Root => (),
    }
  }
}
// Replace one child w/another in the tree.
pub fn replace_child(oldchild: DAG, newchild: DAG) {
  unsafe {
    let oldpref = get_parents(oldchild);
    if let Some(old_parents) = oldpref {
      let mut iter = (*old_parents.as_ptr()).iter();
      let newpref = get_parents(newchild);
      let mut last_old = None;
      let first_new = newpref.map(DLL::first);
      while let Some(parent) = iter.next() {
        if iter.is_last() {
          last_old = iter.this();
          last_old.map_or((), |last_old| (*last_old.as_ptr()).next = first_new);
        }
        install_child(parent, newchild);
      }
      first_new.map_or((), |first_new| (*first_new.as_ptr()).prev = last_old);
      set_parents(newchild, oldpref);
    }
    set_parents(oldchild, None);
  }
}

#[inline]
pub fn add_to_parents(node: DAG, plink: NonNull<Parents>) {
  let parents = get_parents(node);
  match parents {
    Some(parents) => unsafe { (*parents.as_ptr()).merge(plink) },
    None => set_parents(node, Some(plink)),
  }
}

// Free parentless nodes.
pub fn free_dead_node(node: DAG) {
  unsafe {
    match node {
      DAG::Lam(link) => {
        let Lam { bod, bod_ref, .. } = &link.as_ref();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        Box::from_raw(link.as_ptr());
      }
      DAG::Fix(mut link) => {
        let Fix { bod, bod_ref, .. } = &link.as_mut();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        Box::from_raw(link.as_ptr());
      }
      DAG::App(link) => {
        let App { fun, arg, fun_ref, arg_ref, .. } = link.as_ref();
        let new_fun_parents = fun_ref.unlink_node();
        set_parents(*fun, new_fun_parents);
        if new_fun_parents.is_none() {
          free_dead_node(*fun)
        }
        let new_arg_parents = arg_ref.unlink_node();
        set_parents(*arg, new_arg_parents);
        if new_arg_parents.is_none() {
          free_dead_node(*arg)
        }
        Box::from_raw(link.as_ptr());
      }
      DAG::Lit(link) => {
        Box::from_raw(link.as_ptr());
      }
      DAG::Opr(link) => {
        Box::from_raw(link.as_ptr());
      }
      _ => panic!("Runtime error")
    }
  }
}

pub fn clean_up(cc: &ParentPtr) {
  match cc {
    ParentPtr::LamBod(link) => unsafe {
      let Lam { parents, var, .. } = link.as_ref();
      for parent in DLL::iter_option(var.parents) {
        clean_up(parent);
      }
      for parent in DLL::iter_option(*parents) {
        clean_up(parent);
      }
    },
    ParentPtr::FixBod(link) => unsafe {
      let Fix { parents, var, .. } = link.as_ref();
      for parent in DLL::iter_option(var.parents) {
        clean_up(parent);
      }
      for parent in DLL::iter_option(*parents) {
        clean_up(parent);
      }
    },
    ParentPtr::AppLam(mut link) | ParentPtr::AppArg(mut link) => unsafe {
      let app = link.as_mut();
      if let Some(app_copy) = app.copy {
        let App { fun, arg, fun_ref, arg_ref, .. } = &mut *app_copy.as_ptr();
        app.copy = None;
        add_to_parents(*fun, NonNull::new(fun_ref).unwrap());
        add_to_parents(*arg, NonNull::new(arg_ref).unwrap());
        for parent in DLL::iter_option(app.parents) {
          clean_up(parent);
        }
      }
    },
    ParentPtr::Root => (),
  }
}

#[inline]
pub fn alloc_val<T>(val: T) -> NonNull<T> {
  NonNull::new(Box::leak(Box::new(val))).unwrap()
}

#[inline]
pub fn alloc_lam(
  bod: DAG,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Lam> {
  unsafe {
    let lam = alloc_val(Lam {
      var: Var { parents: None },
      bod,
      bod_ref: mem::zeroed(),
      parents,
    });
    (*lam.as_ptr()).bod_ref = DLL::singleton(ParentPtr::LamBod(lam));
    lam
  }
}

#[inline]
pub fn alloc_fix(
  bod: DAG,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Fix> {
  unsafe {
    let fix = alloc_val(Fix {
      var: Var { parents: None },
      bod,
      bod_ref: mem::zeroed(),
      parents,
    });
    (*fix.as_ptr()).bod_ref = DLL::singleton(ParentPtr::FixBod(fix));
    fix
  }
}

#[inline]
pub fn alloc_app(
  fun: DAG,
  arg: DAG,
  parents: Option<NonNull<Parents>>,
) -> NonNull<App> {
  unsafe {
    let app = alloc_val(App {
      fun,
      arg,
      copy: None,
      fun_ref: mem::zeroed(),
      arg_ref: mem::zeroed(),
      parents,
    });
    (*app.as_ptr()).fun_ref = DLL::singleton(ParentPtr::AppLam(app));
    (*app.as_ptr()).arg_ref = DLL::singleton(ParentPtr::AppArg(app));
    app
  }
}


// The core up-copy function.
pub fn upcopy(new_child: DAG, cc: ParentPtr) {
  unsafe {
    match cc {
      ParentPtr::LamBod(link) => {
        let Lam { var, parents, .. } = link.as_ref();
        let new_lam = alloc_lam(new_child, None);
        let ptr: *mut Parents = &mut (*new_lam.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = &mut (*new_lam.as_ptr()).var;
        for parent in DLL::iter_option(var.parents) {
          upcopy(DAG::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        for parent in DLL::iter_option(*parents) {
          upcopy(DAG::Lam(new_lam), *parent)
        }
      }
      ParentPtr::FixBod(link) => {
        let Fix { var, parents, .. } = link.as_ref();
        let new_fix = alloc_fix(new_child, None);
        let ptr: *mut Parents = &mut (*new_fix.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = &mut (*new_fix.as_ptr()).var;
        for parent in DLL::iter_option(var.parents) {
          upcopy(DAG::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        for parent in DLL::iter_option(*parents) {
          upcopy(DAG::Fix(new_fix), *parent)
        }
      }
      ParentPtr::AppLam(link) => {
        let App { copy, arg, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).fun = new_child;
          }
          None => {
            let new_app = alloc_app(new_child, *arg, None);
            (*link.as_ptr()).copy = Some(new_app);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAG::App(new_app), *parent)
            }
          }
        }
      }
      ParentPtr::AppArg(link) => {
        let App { copy, fun, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).arg = new_child;
          }
          None => {
            let new_app = alloc_app(*fun, new_child, None);
            (*link.as_ptr()).copy = Some(new_app);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAG::App(new_app), *parent)
            }
          }
        }
      }
      ParentPtr::Root => (),
    }
  }
}

enum Single {
  Lam(Var),
  Fix(Var),
}

// Substitute a variable
#[inline]
pub fn subst(bod: DAG, var: &Var, arg: DAG, fix: bool) -> DAG {
  let mut input = bod;
  let mut top_app = None;
  let mut spine = vec![];
  let mut result = loop {
    match input {
      DAG::Lam(link) => {
        let Lam { var, bod, .. } = unsafe { link.as_ref() };
        input = *bod;
        spine.push(Single::Lam(var.clone()));
      }
      DAG::Fix(link) => {
        let Fix { var, bod, .. } = unsafe { link.as_ref() };
        input = *bod;
        spine.push(Single::Fix(var.clone()));
      }
      DAG::App(link) => {
        let App { fun, arg: app_arg, .. } = unsafe { link.as_ref() };
        let new_app = alloc_app(*fun, *app_arg, None);
        unsafe {
          (*link.as_ptr()).copy = Some(new_app);
        }
        top_app = Some(link);
        for parent in DLL::iter_option(var.parents) {
          upcopy(arg, *parent);
        }
        break DAG::App(new_app);
      }
      _ => break arg,
    }
  };
  if fix && top_app.is_none() && spine.is_empty() {
    panic!("Infinite loop found");
  }
  while let Some(single) = spine.pop() {
    match single {
      Single::Lam(var) => {
        let new_lam = alloc_lam(result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_lam.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = unsafe { &mut (*new_lam.as_ptr()).var };
        for parent in DLL::iter_option(var.parents) {
          upcopy(DAG::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        result = DAG::Lam(new_lam);
      }
      Single::Fix(var) => {
        let new_fix = alloc_fix(result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_fix.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = unsafe { &mut (*new_fix.as_ptr()).var };
        for parent in DLL::iter_option(var.parents) {
          upcopy(DAG::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        result = DAG::Fix(new_fix);
      }
    }
  }
  // If the top branch is non-null, then clear the copies and fix the uplinks
  if let Some(link) = top_app {
    let top_app = unsafe { &mut *link.as_ptr() };
    let link = top_app.copy.unwrap();
    top_app.copy = None;
    let App { fun, fun_ref, arg, arg_ref, .. } = unsafe { &mut *link.as_ptr() };
    add_to_parents(*fun, NonNull::new(fun_ref).unwrap());
    add_to_parents(*arg, NonNull::new(arg_ref).unwrap());
    for parent in DLL::iter_option(var.parents) {
      clean_up(parent);
    }
    let mut spine = bod;
    loop {
      match spine {
        DAG::Lam(link) => unsafe {
          let Lam { var, bod, .. } = &mut *link.as_ptr();
          for parent in DLL::iter_option(var.parents) {
            clean_up(parent);
          }
          spine = *bod;
        },
        DAG::Fix(link) => unsafe {
          let Fix { var, bod, .. } = &mut *link.as_ptr();
          for parent in DLL::iter_option(var.parents) {
            clean_up(parent);
          }
          spine = *bod;
        },
        _ => break,
      }
    }
  }
  result
}

// Contract a lambda redex, return the body.
#[inline]
pub fn reduce_lam(redex: NonNull<App>, lam: NonNull<Lam>) -> DAG {
  let App { arg, .. } = unsafe { redex.as_ref() };
  let Lam { var, bod, parents, .. } = unsafe { &mut *lam.as_ptr() };
  let top_node = if DLL::is_singleton(*parents) {
    replace_child(DAG::Var(NonNull::new(var).unwrap()), *arg);
    *bod
  }
  else if var.parents.is_none() {
    *bod
  }
  else {
    subst(*bod, var, *arg, false)
  };
  replace_child(DAG::App(redex), top_node);
  free_dead_node(DAG::App(redex));
  top_node
}

pub fn whnf(dag: &mut DAG) {
  let mut node = *dag;
  let mut trail: Vec<NonNull<App>> = vec![];
  loop {
    match node {
      DAG::App(link) => {
        let App { fun, .. } = unsafe { link.as_ref() };
        trail.push(link);
        node = *fun;
      }
      DAG::Lam(link) => {
        if let Some(app_link) = trail.pop() {
          node = reduce_lam(app_link, link);
        }
        else {
          break;
        }
      }
      DAG::Fix(link) => unsafe {
        let Fix { var, bod, .. } = &mut *link.as_ptr();
        replace_child(node, *bod);
        if !var.parents.is_none() {
          let new_fix = alloc_fix(mem::zeroed(), None).as_mut();
          let result = subst(*bod, var, DAG::Var(NonNull::new_unchecked(&mut new_fix.var)), true);
          new_fix.bod = result;
          add_to_parents(result, NonNull::new_unchecked(&mut new_fix.bod_ref));
          replace_child(
            DAG::Var(NonNull::new(var).unwrap()),
            DAG::Fix(NonNull::new_unchecked(new_fix))
          );
        }
        free_dead_node(node);
        node = *bod;
      },
      DAG::Opr(link) => {
        todo!()
      //   let opr = unsafe { (*link.as_ptr()).opr };
      //   let len = trail.len();
      //   if len >= 1 && opr.arity() == 1 {
      //     let mut arg = unsafe { (*trail[len - 1].as_ptr()).arg };
      //     whnf(arg);
      //     match arg {
      //       DAG::Lit(link) => {
      //         let x = unsafe { (*link.as_ptr()).lit.clone() };
      //         let res = apply_una_op(opr, x);
      //         if let Some(res) = res {
      //           trail.pop();
      //           node = DAG::Lit(alloc_val(Lit {
      //             lit: res,
      //             parents: None,
      //           }));
      //           replace_child(arg, node);
      //           free_dead_node(arg);
      //         }
      //         else {
      //           break;
      //         }
      //       }
      //       _ => break,
      //     }
      //   }
      //   else if len >= 2 && opr.arity() == 2 {
      //     let mut arg1 = unsafe { (*trail[len - 2].as_ptr()).arg };
      //     let mut arg2 = unsafe { (*trail[len - 1].as_ptr()).arg };
      //     whnf(arg1);
      //     whnf(arg2);
      //     match (arg1, arg2) {
      //       (DAG::Lit(x_link), DAG::Lit(y_link)) => {
      //         let x = unsafe { (*x_link.as_ptr()).lit.clone() };
      //         let y = unsafe { (*y_link.as_ptr()).lit.clone() };
      //         let res = apply_bin_op(opr, y, x);
      //         if let Some(res) = res {
      //           trail.pop();
      //           trail.pop();
      //           node = DAG::Lit(alloc_val(Lit {
      //             lit: res,
      //             parents: None,
      //           }));
      //           replace_child(arg1, node);
      //           free_dead_node(arg1);
      //         }
      //         else {
      //           break;
      //         }
      //       }
      //       _ => break,
      //     }
      //   }
      //   else {
      //     break;
      //   }
      }
      _ => break,
    }
  }
  if trail.is_empty() {
    *dag = node;
  }
  else {
    *dag = DAG::App(trail[0]);
  }
}
