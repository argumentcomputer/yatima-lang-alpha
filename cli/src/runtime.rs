use yatima_core::{
  defs::Defs,
  dll::*,
  literal::Literal,
  prim::Op,
  term::Term,
};

use std::{
  boxed::Box,
  collections::BTreeSet,
  fmt,
  mem,
  ptr::NonNull,
  rc::Rc,
  sync::atomic::{
    AtomicUsize,
    Ordering,
  },
  vec::Vec,
};

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
  AppFun(NonNull<App>),
  AppArg(NonNull<App>),
}

// Runtime DAG nodes
#[derive(Clone, Debug)]
pub struct Var {
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Debug)]
pub struct Lam {
  pub bod: DAG,
  pub bod_ref: Parents,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Debug)]
pub struct App {
  pub fun: DAG,
  pub arg: DAG,
  pub fun_ref: Parents,
  pub arg_ref: Parents,
  pub copy: Option<NonNull<App>>,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Debug)]
pub struct Fix {
  pub bod: DAG,
  pub bod_ref: Parents,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Debug)]
pub struct Lit {
  pub lit: Literal,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Debug)]
pub struct Opr {
  pub opr: Op,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Copy, Clone, Debug)]
pub enum IoOp {
  Print,
  Return,
  Read,
  Bind,
}

impl fmt::Debug for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    #[inline]
    fn format_uplink(p: ParentPtr) -> String {
      match p {
        ParentPtr::Root => String::from("ROOT"),
        ParentPtr::LamBod(link) => format!("LamBod<{:?}>", link.as_ptr()),
        ParentPtr::FixBod(link) => format!("FixBod<{:?}>", link.as_ptr()),
        ParentPtr::AppFun(link) => format!("AppFun<{:?}>", link.as_ptr()),
        ParentPtr::AppArg(link) => format!("AppArg<{:?}>", link.as_ptr()),
      }
    }
    #[inline]
    fn format_parents(dll: Option<NonNull<Parents>>) -> String {
      match dll {
        Some(dll) => unsafe {
          let mut iter = (*dll.as_ptr()).iter();
          let head =
            &iter.next().map_or(String::from(""), |head| format_uplink(*head));
          let mut msg = String::from("[ ") + head;
          for val in iter {
            msg = msg + " <-> " + &format_uplink(*val);
          }
          msg + " ]"
        },
        _ => String::from("[]"),
      }
    }
    fn go(term: DAG, set: &mut BTreeSet<usize>) -> String {
      match term {
        DAG::Var(link) => {
          let Var { parents } = unsafe { link.as_ref() };
          if set.get(&(link.as_ptr() as usize)).is_none() {
            set.insert(link.as_ptr() as usize);
            format!(
              "\nVar<{:?}> parents: {}",
              link.as_ptr(),
              format_parents(*parents)
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAG::Lit(link) => {
          let Lit { parents, lit, .. } = unsafe { link.as_ref() };
          format!(
            "\nLit<{:?}> parents: {} (value: {:?})",
            (link.as_ptr()),
            format_parents(*parents),
            lit
          )
        }
        DAG::Opr(link) => {
          let Opr { parents, .. } = unsafe { link.as_ref() };
          format!(
            "\nOpr<{:?}> parents: {}",
            (link.as_ptr()),
            format_parents(*parents)
          )
        }
        DAG::Lam(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            let Lam { var, parents, bod, .. } = unsafe { link.as_ref() };
            set.insert(link.as_ptr() as usize);
            format!(
              "\nLam<{:?}> parents: {}{}",
              link.as_ptr(),
              format_parents(*parents),
              go(*bod, set)
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAG::Fix(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            let Fix { var, parents, bod, .. } = unsafe { link.as_ref() };
            set.insert(link.as_ptr() as usize);
            format!(
              "\nFix<{:?}> parents: {}{}",
              link.as_ptr(),
              format_parents(*parents),
              go(*bod, set)
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAG::App(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            set.insert(link.as_ptr() as usize);
            let App { fun, arg, parents, copy, .. } = unsafe { link.as_ref() };
            let copy = copy.map(|link| link.as_ptr() as usize);
            format!(
              "\nApp<{:?}> parents: {} copy: {:?}{}{}",
              link.as_ptr(),
              format_parents(*parents),
              copy,
              go(*fun, set),
              go(*arg, set)
            )
          }
          else {
            format!("\nSHARE<{}>", link.as_ptr() as usize)
          }
        }
      }
    }
    write!(f, "{}", go(*self, &mut BTreeSet::new()))
  }
}

pub static UPCOPY_COUNT: AtomicUsize = AtomicUsize::new(0);

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
      ParentPtr::AppFun(parent) => (*parent.as_ptr()).fun = newchild,
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
      DAG::Var(link) => (),
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
    ParentPtr::AppFun(mut link) | ParentPtr::AppArg(mut link) => unsafe {
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
pub fn alloc_lam(bod: DAG, parents: Option<NonNull<Parents>>) -> NonNull<Lam> {
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
pub fn alloc_fix(bod: DAG, parents: Option<NonNull<Parents>>) -> NonNull<Fix> {
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
    (*app.as_ptr()).fun_ref = DLL::singleton(ParentPtr::AppFun(app));
    (*app.as_ptr()).arg_ref = DLL::singleton(ParentPtr::AppArg(app));
    app
  }
}

// The core up-copy function.
pub fn upcopy(new_child: DAG, cc: ParentPtr, should_count: bool) {
  if should_count {
    UPCOPY_COUNT.fetch_add(1, Ordering::SeqCst);
  }
  unsafe {
    match cc {
      ParentPtr::LamBod(link) => {
        let Lam { var, parents, .. } = link.as_ref();
        let new_lam = alloc_lam(new_child, None);
        let ptr: *mut Parents = &mut (*new_lam.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = &mut (*new_lam.as_ptr()).var;
        for parent in DLL::iter_option(var.parents) {
          upcopy(DAG::Var(NonNull::new(ptr).unwrap()), *parent, should_count)
        }
        for parent in DLL::iter_option(*parents) {
          upcopy(DAG::Lam(new_lam), *parent, should_count)
        }
      }
      ParentPtr::FixBod(link) => {
        let Fix { var, parents, .. } = link.as_ref();
        let new_fix = alloc_fix(new_child, None);
        let ptr: *mut Parents = &mut (*new_fix.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = &mut (*new_fix.as_ptr()).var;
        for parent in DLL::iter_option(var.parents) {
          upcopy(DAG::Var(NonNull::new(ptr).unwrap()), *parent, should_count)
        }
        for parent in DLL::iter_option(*parents) {
          upcopy(DAG::Fix(new_fix), *parent, should_count)
        }
      }
      ParentPtr::AppFun(link) => {
        let App { copy, arg, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).fun = new_child;
          }
          None => {
            let new_app = alloc_app(new_child, *arg, None);
            (*link.as_ptr()).copy = Some(new_app);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAG::App(new_app), *parent, should_count)
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
              upcopy(DAG::App(new_app), *parent, should_count)
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
pub fn subst(
  bod: DAG,
  var: &Var,
  arg: DAG,
  fix: bool,
  should_count: bool,
) -> DAG {
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
          upcopy(arg, *parent, should_count);
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
          upcopy(DAG::Var(NonNull::new(ptr).unwrap()), *parent, should_count)
        }
        result = DAG::Lam(new_lam);
      }
      Single::Fix(var) => {
        let new_fix = alloc_fix(result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_fix.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = unsafe { &mut (*new_fix.as_ptr()).var };
        for parent in DLL::iter_option(var.parents) {
          upcopy(DAG::Var(NonNull::new(ptr).unwrap()), *parent, should_count)
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
pub fn reduce_lam(
  redex: NonNull<App>,
  lam: NonNull<Lam>,
  should_count: bool,
) -> DAG {
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
    subst(*bod, var, *arg, false, should_count)
  };
  replace_child(DAG::App(redex), top_node);
  free_dead_node(DAG::App(redex));
  top_node
}

pub fn whnf(dag: &mut DAG, should_count: bool) {
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
          node = reduce_lam(app_link, link, should_count);
        }
        else {
          break;
        }
      }
      DAG::Fix(link) => unsafe {
        let Fix { var, bod, .. } = &mut *link.as_ptr();
        replace_child(node, *bod);
        if var.parents.is_some() {
          let new_fix = alloc_fix(mem::zeroed(), None).as_mut();
          let result = subst(
            *bod,
            var,
            DAG::Var(NonNull::new_unchecked(&mut new_fix.var)),
            true,
            should_count,
          );
          new_fix.bod = result;
          add_to_parents(result, NonNull::new_unchecked(&mut new_fix.bod_ref));
          replace_child(
            DAG::Var(NonNull::new(var).unwrap()),
            DAG::Fix(NonNull::new_unchecked(new_fix)),
          );
        }
        free_dead_node(node);
        node = *bod;
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
          whnf(arg, should_count);
          match *arg {
            DAG::Lit(link) => {
              let x = unsafe { &(*link.as_ptr()).lit };
              let res = opr.apply1(x);
              if let Some(res) = res {
                let top = DAG::App(trail.pop().unwrap());
                let new_node =
                  DAG::Lit(alloc_val(Lit { lit: res, parents: None }));
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
          whnf(arg1, should_count);
          whnf(arg2, should_count);
          match (*arg1, *arg2) {
            (DAG::Lit(x_link), DAG::Lit(y_link)) => {
              let x = unsafe { &(*x_link.as_ptr()).lit };
              let y = unsafe { &(*y_link.as_ptr()).lit };
              let res = opr.apply2(x, y);
              if let Some(res) = res {
                trail.pop();
                let top = DAG::App(trail.pop().unwrap());
                let new_node =
                  DAG::Lit(alloc_val(Lit { lit: res, parents: None }));
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
          whnf(arg1, should_count);
          whnf(arg2, should_count);
          whnf(arg3, should_count);
          match (*arg1, *arg2, *arg3) {
            (DAG::Lit(x_link), DAG::Lit(y_link), DAG::Lit(z_link)) => {
              let x = unsafe { &(*x_link.as_ptr()).lit };
              let y = unsafe { &(*y_link.as_ptr()).lit };
              let z = unsafe { &(*z_link.as_ptr()).lit };
              let res = opr.apply3(x, y, z);
              if let Some(res) = res {
                trail.pop();
                trail.pop();
                let top = DAG::App(trail.pop().unwrap());
                let new_node =
                  DAG::Lit(alloc_val(Lit { lit: res, parents: None }));
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

// Assumes erased terms
pub fn from_term(
  defs: Rc<Defs>,
  term: &Term,
  parents: Option<NonNull<Parents>>,
) -> DAG {
  let (bod, maybe_fix) = from_term_inner(defs, term, &mut vec![], None, None);
  match maybe_fix {
    Some(mut link) => unsafe {
      let fix = link.as_mut();
      fix.parents = parents;
      fix.bod = bod;
      add_to_parents(bod, NonNull::new_unchecked(&mut fix.bod_ref));
      DAG::Fix(link)
    },
    None => {
      set_parents(bod, parents);
      bod
    }
  }
}

pub fn from_term_inner(
  defs: Rc<Defs>,
  term: &Term,
  ctx: &mut Vec<DAG>,
  parents: Option<NonNull<Parents>>,
  maybe_fix: Option<NonNull<Fix>>,
) -> (DAG, Option<NonNull<Fix>>) {
  match term {
    Term::Rec(_) => unsafe {
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
      (var, Some(fix))
    },
    Term::Var(_, _, idx) => match ctx.get(ctx.len() - 1 - *idx as usize) {
      Some(val) => {
        if let Some(parents) = parents {
          DLL::concat(parents, get_parents(*val));
          set_parents(*val, Some(parents));
        }
        (*val, maybe_fix)
      }
      None => panic!("Free variable found"),
    },
    Term::Lit(_, lit) => {
      (DAG::Lit(alloc_val(Lit { lit: lit.clone(), parents })), maybe_fix)
    }
    Term::Opr(_, opr) => {
      (DAG::Opr(alloc_val(Opr { opr: *opr, parents })), maybe_fix)
    }
    Term::Ref(_, nam, exp, _) => {
      if let Some(def) = defs.defs.get(exp) {
        (from_term(defs.clone(), &def.term, parents), maybe_fix)
      }
      else {
        panic!("undefined runtime reference: {}, {}", nam, exp);
      }
    }
    Term::Lam(_, _, bod) => unsafe {
      let lam = alloc_lam(mem::zeroed(), parents);
      let Lam { var, bod_ref, .. } = &mut *lam.as_ptr();
      ctx.push(DAG::Var(NonNull::new(var).unwrap()));
      let (bod, maybe_fix) =
        from_term_inner(defs, &**bod, ctx, NonNull::new(bod_ref), maybe_fix);
      (*lam.as_ptr()).bod = bod;
      (DAG::Lam(lam), maybe_fix)
    },
    Term::Dat(_, bod) => from_term_inner(defs, &**bod, ctx, parents, maybe_fix),
    Term::Cse(_, bod) => from_term_inner(defs, &**bod, ctx, parents, maybe_fix),
    Term::App(_, fun_arg) => unsafe {
      let (fun, arg) = &**fun_arg;
      let app = alloc_app(mem::zeroed(), mem::zeroed(), parents);
      let App { fun_ref, arg_ref, .. } = &mut *app.as_ptr();
      let (fun, maybe_fix) = from_term_inner(
        defs.clone(),
        fun,
        &mut ctx.clone(),
        NonNull::new(fun_ref),
        maybe_fix,
      );
      let (arg, maybe_fix) =
        from_term_inner(defs, arg, ctx, NonNull::new(arg_ref), maybe_fix);
      (*app.as_ptr()).fun = fun;
      (*app.as_ptr()).arg = arg;
      (DAG::App(app), maybe_fix)
    },
    Term::Ann(_, typ_exp) => {
      let (_, exp) = (**typ_exp).clone();
      from_term_inner(defs, &exp, ctx, parents, maybe_fix)
    }
    Term::Let(_, rec, _, _, typ_exp_bod) => unsafe {
      let (_, exp, bod) = &**typ_exp_bod;
      let (exp, maybe_fix) = if *rec {
        let new_fix = alloc_fix(mem::zeroed(), None).as_mut();
        let (bod, maybe_fix) = from_term_inner(
          defs.clone(),
          exp,
          &mut ctx.clone(),
          parents,
          maybe_fix,
        );
        new_fix.bod = bod;
        add_to_parents(bod, NonNull::new_unchecked(&mut new_fix.bod_ref));
        (DAG::Fix(NonNull::new_unchecked(new_fix)), maybe_fix)
      }
      else {
        from_term_inner(defs.clone(), exp, &mut ctx.clone(), None, maybe_fix)
      };
      ctx.push(exp);
      from_term_inner(defs, bod, ctx, parents, maybe_fix)
    },
    _ => {
      (DAG::Lit(alloc_val(Lit { lit: Literal::I32(0), parents })), maybe_fix)
    }
  }
}
