use crate::{
  check::*,
  defs::Defs,
  dll::*,
  literal::Literal,
  prim::Op,
  uses::*,
};

use core::ptr::NonNull;
use std::mem;
use error::CheckError;

pub type Parents = DLL<ParentPtr>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum DAG {
  Var(NonNull<Var>),
  Lam(NonNull<Lam>),
  App(NonNull<App>),
  Fix(NonNull<Fix>),
  Cse(NonNull<Cse>),
  Lit(NonNull<Lit>),
  Opr(NonNull<Opr>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ParentPtr {
  Root,
  LamBod(NonNull<Lam>),
  FixBod(NonNull<Fix>),
  CseBod(NonNull<Cse>),
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

pub struct Cse {
  pub bod: DAG,
  pub bod_ref: Parents,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Lit {
  pub lit: Literal,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Opr {
  pub opr: Op,
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
      DAG::Cse(link) => (*link.as_ptr()).parents,
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
      DAG::Cse(link) => (*link.as_ptr()).parents = pref,
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
      ParentPtr::CseBod(parent) => (*parent.as_ptr()).bod = newchild,
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
      DAG::Cse(mut link) => {
        let Cse { bod, bod_ref, .. } = &link.as_mut();
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
    ParentPtr::CseBod(link) => unsafe {
      let Cse { parents, .. } = link.as_ref();
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
pub fn alloc_cse(
  bod: DAG,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Cse> {
  unsafe {
    let cse = alloc_val(Cse {
      bod,
      bod_ref: mem::zeroed(),
      parents,
    });
    (*cse.as_ptr()).bod_ref = DLL::singleton(ParentPtr::CseBod(cse));
    cse
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
      ParentPtr::CseBod(link) => {
        let Cse { parents, .. } = link.as_ref();
        let new_cse = alloc_cse(new_child, None);
        let ptr: *mut Parents = &mut (*new_cse.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        for parent in DLL::iter_option(*parents) {
          upcopy(DAG::Cse(new_cse), *parent)
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
  Cse,
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
      DAG::Cse(link) => {
        let Cse { bod, .. } = unsafe { link.as_ref() };
        input = *bod;
        spine.push(Single::Cse);
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
      Single::Cse => {
        let new_cse = alloc_cse(result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_cse.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        result = DAG::Cse(new_cse);
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
        DAG::Cse(link) => unsafe {
          let Cse { bod, .. } = &mut *link.as_ptr();
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
      DAG::Cse(link) => {
        let body = unsafe { &mut (*link.as_ptr()).bod };
        whnf(body);
        match body {
          DAG::Lit(_link) => {
            // TODO Proper expansion of literals
            todo!()
            // let Lit { lit, parents, .. } = unsafe { link.as_ref() };
            // match &lit.clone().expand() {
            //   None => break,
            //   Some(expand) => {
            //     let expand = DAG::from_term_inner(
            //       expand,
            //       0,
            //       HashMap::new(),
            //       *parents,
            //       None,
            //     );
            //     replace_child(node, expand);
            //     free_dead_node(node);
            //     node = expand;
            //   }
            // }
          }
          _ => break,
        }
      },
      DAG::Opr(link) => {
        let opr = unsafe { (*link.as_ptr()).opr };
        let len = trail.len();
        if len == 0 && opr.arity() == 0 {
          let res = opr.apply0();
          if let Some(res) = res {
            node = DAG::Lit(alloc_val(Lit { lit: res, parents: None }));
          }
          else {
            break;
          }
        }
        else if len >= 1 && opr.arity() == 1 {
          let arg = unsafe { &mut (*trail[len - 1].as_ptr()).arg };
          whnf(arg);
          match *arg {
            DAG::Lit(link) => {
              let x = unsafe { &(*link.as_ptr()).lit };
              let res = opr.apply1(x);
              if let Some(res) = res {
                let top = DAG::App(trail.pop().unwrap());
                let new_node = DAG::Lit(alloc_val(Lit { lit: res, parents: None }));
                replace_child(top, new_node);
                free_dead_node(top);
                node = new_node;
              }
              else {
                break;
              }
            }
            _ => break,
          }
        }
        else if len >= 2 && opr.arity() == 2 {
          let arg1 = unsafe { &mut (*trail[len - 1].as_ptr()).arg };
          let arg2 = unsafe { &mut (*trail[len - 2].as_ptr()).arg };
          whnf(arg1);
          whnf(arg2);
          match (*arg1, *arg2) {
            (DAG::Lit(x_link), DAG::Lit(y_link)) => {
              let x = unsafe { &(*x_link.as_ptr()).lit };
              let y = unsafe { &(*y_link.as_ptr()).lit };
              let res = opr.apply2(x, y);
              if let Some(res) = res {
                trail.pop();
                let top = DAG::App(trail.pop().unwrap());
                let new_node = DAG::Lit(alloc_val(Lit { lit: res, parents: None }));
                replace_child(top, new_node);
                free_dead_node(top);
                node = new_node;
              }
              else {
                break;
              }
            }
            _ => break,
          }
        }
        else if len >= 3 && opr.arity() == 3 {
          let arg1 = unsafe { &mut (*trail[len - 1].as_ptr()).arg };
          let arg2 = unsafe { &mut (*trail[len - 2].as_ptr()).arg };
          let arg3 = unsafe { &mut (*trail[len - 3].as_ptr()).arg };
          whnf(arg1);
          whnf(arg2);
          whnf(arg3);
          match (*arg1, *arg2, *arg3) {
            (
              DAG::Lit(x_link),
              DAG::Lit(y_link),
              DAG::Lit(z_link),
            ) => {
              let x = unsafe { &(*x_link.as_ptr()).lit };
              let y = unsafe { &(*y_link.as_ptr()).lit };
              let z = unsafe { &(*z_link.as_ptr()).lit };
              let res = opr.apply3(x, y, z);
              if let Some(res) = res {
                trail.pop();
                trail.pop();
                let top = DAG::App(trail.pop().unwrap());
                let new_node = DAG::Lit(alloc_val(Lit { lit: res, parents: None }));
                replace_child(top, new_node);
                free_dead_node(top);
                node = new_node;
              }
              else {
                break;
              }
            }
            _ => break,
          }
        }
        else {
          break;
        }
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

pub fn from_def(
  defs: &Defs,
  name: &str,
  parents: Option<NonNull<Parents>>
) -> Result<DAG, CheckError> {
  let term_ir = &check_def(&defs, &name)?;
  let (bod, maybe_fix) = from_ir_inner(defs, term_ir, &mut vec![], None, None)?;
  match maybe_fix {
    Some(mut link) => unsafe {
      let fix = link.as_mut();
      fix.parents = parents;
      fix.bod = bod;
      add_to_parents(bod, NonNull::new_unchecked(&mut fix.bod_ref));
      Ok(DAG::Fix(link))
    },
    None => {
      set_parents(bod, parents);
      Ok(bod)
    },
  }
}

pub fn from_ir_inner(
  defs: &Defs,
  term: &IR,
  ctx: &mut Vec<DAG>,
  parents: Option<NonNull<Parents>>,
  maybe_fix: Option<NonNull<Fix>>
) -> Result<(DAG, Option<NonNull<Fix>>), CheckError> {
  match term {
    IR::Rec => unsafe {
      let mut fix = match maybe_fix {
        Some(fix) => fix,
        None => alloc_fix(mem::zeroed(), None),
      };
      let Fix { var, .. } = fix.as_mut();
      let var = DAG::Var(NonNull::new_unchecked(var));
      if let Some(parents) = parents {
        DLL::concat(parents, get_parents(var));
        set_parents(var, Some(parents));
      }
      Ok((var, Some(fix)))
    },
    IR::Var(_, idx) => match ctx.get(ctx.len() - 1 - *idx as usize) {
      Some(val) => {
        if let Some(parents) = parents {
          DLL::concat(parents, get_parents(*val));
          set_parents(*val, Some(parents));
        }
        Ok((*val, maybe_fix))
      }
      None => panic!("Free variable found"),
    }
    IR::Lit(lit) => {
      Ok((DAG::Lit(alloc_val(Lit { lit: lit.clone(), parents })), maybe_fix))
    }
    IR::Opr(opr) => {
      Ok((DAG::Opr(alloc_val(Opr { opr: *opr, parents })), maybe_fix))
    }
    IR::Ref(nam, _, _) => {
      let deref_ir = from_def(&defs, &nam, parents)?;
      Ok((deref_ir, maybe_fix))
    },
    IR::Lam(Uses::None, _, bod) => from_ir_inner(defs, &**bod, ctx, parents, maybe_fix),
    IR::Lam(_, _, bod) => unsafe {
      let lam = alloc_lam(mem::zeroed(), parents);
      let Lam { var, bod_ref, .. } = &mut *lam.as_ptr();
      ctx.push(DAG::Var(NonNull::new(var).unwrap()));
      let (bod, maybe_fix) = from_ir_inner(
        defs,
        &**bod,
        ctx,
        NonNull::new(bod_ref),
        maybe_fix,
      )?;
      (*lam.as_ptr()).bod = bod;
      Ok((DAG::Lam(lam), maybe_fix))
    },
    IR::Cse(_, bod) => unsafe {
      let cse = alloc_cse(mem::zeroed(), parents);
      let Cse { bod_ref, .. } = &mut *cse.as_ptr();
      let (bod, maybe_fix) = from_ir_inner(
        defs,
        bod,
        &mut ctx.clone(),
        NonNull::new(bod_ref),
        maybe_fix,
      )?;
      (*cse.as_ptr()).bod = bod;
      Ok((DAG::Cse(cse), maybe_fix))
    },
    IR::App(Uses::None, fun_arg) => {
      let (fun, _) = &**fun_arg;
      from_ir_inner(defs, fun, ctx, parents, maybe_fix)
    },
    IR::App(_, fun_arg) => unsafe {
      let (fun, arg) = &**fun_arg;
      let app = alloc_app(mem::zeroed(), mem::zeroed(), parents);
      let App { fun_ref, arg_ref, .. } = &mut *app.as_ptr();
      let (fun, maybe_fix) = from_ir_inner(
        defs,
        fun,
        &mut ctx.clone(),
        NonNull::new(fun_ref),
        maybe_fix,
      )?;
      let (arg, maybe_fix) = from_ir_inner(
        defs,
        arg,
        ctx,
        NonNull::new(arg_ref),
        maybe_fix,
      )?;
      (*app.as_ptr()).fun = fun;
      (*app.as_ptr()).arg = arg;
      Ok((DAG::App(app), maybe_fix))
    },
    IR::Let(rec, _, _, exp_bod) => unsafe {
      let (exp, bod) = &**exp_bod;
      let (exp, maybe_fix) = if *rec {
        let new_fix = alloc_fix(mem::zeroed(), None).as_mut();
        let (bod, maybe_fix) = from_ir_inner(defs, &exp, &mut ctx.clone(), parents, maybe_fix)?;
        new_fix.bod = bod;
        add_to_parents(bod, NonNull::new_unchecked(&mut new_fix.bod_ref));
        Ok((DAG::Fix(NonNull::new_unchecked(new_fix)), maybe_fix))
      }
      else {
        from_ir_inner(defs, &exp, &mut ctx.clone(), None, maybe_fix)
      }?;
      ctx.push(exp);
      from_ir_inner(defs, &bod, ctx, parents, maybe_fix)
    },
    _ => panic!("Runtime cannot contain type level terms")
  }
}
