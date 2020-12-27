#![allow(unused_variables)]

use core::ptr::NonNull;
use std::alloc::{alloc, dealloc, Layout};
use std::fmt;
use crate::dll::*;

// A λ-DAG pointer. Keeps track of what kind of node it points to.
#[derive(Clone, Copy)]
pub enum DAG {
  Lam(NonNull<Lam>),
  App(NonNull<App>),
  Var(NonNull<Var>),
}

// Doubly-linked list of parent nodes
type Parents = DLL<ParentCell>;

// A parent pointer. Keeps track of the relation between the child and the parent.
#[derive(Clone, Copy)]
pub enum ParentCell {
  AppFun(NonNull<App>),
  AppArg(NonNull<App>),
  LamBod(NonNull<Lam>),
  Root
}

// The three λ-DAG nodes: Lam, App and Var
pub struct Lam {
  pub var: NonNull<Var>,
  pub body: DAG,
  pub body_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
  // uniq: u64,
}

pub struct App {
  pub func: DAG,
  pub arg: DAG,
  pub func_ref: NonNull<Parents>,
  pub arg_ref: NonNull<Parents>,
  pub copy: Option<NonNull<App>>,
  pub parents: Option<NonNull<Parents>>,
  // uniq: u64,
}

pub struct Var {
  pub name: String,
  pub parents: Option<NonNull<Parents>>,
  // uniq: u64,
}

impl fmt::Display for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

    fn lams(link: &NonNull<Lam>) -> String {
      unsafe {
        let body = &(*link.as_ptr()).body;
        let name = &(*(*link.as_ptr()).var.as_ptr()).name;
        match body {
          DAG::Lam(link) => format!("{} {}", name, lams(link)),
          _ => format!("{} => {}", name, body),
        }
      }
    }

    fn apps(link: &NonNull<App>) -> String {
      unsafe {
        let func = &(*link.as_ptr()).func;
        let arg = &(*link.as_ptr()).arg;
        match (func, arg) {
          (DAG::App(link_func), DAG::App(link_arg)) => {
            format!("{} ({})", apps(link_func), apps(link_arg))
          }
          (DAG::App(link_func), DAG::Lam(_)) => {
            format!("{} ({})", apps(link_func), arg)
          }
          (DAG::App(link_func), DAG::Var(_)) => {
            format!("{} {}", apps(link_func), arg)
          }
          (DAG::Lam(_), DAG::App(link_arg)) => {
            format!("({}) ({})", func, apps(link_arg))
          }
          (DAG::Var(_), DAG::App(link_arg)) => {
            format!("{} ({})", func, apps(link_arg))
          }
          (DAG::Var(_), DAG::Lam(_)) => {
            format!("{} ({})", func, arg)
          }
          (DAG::Lam(_), DAG::Var(_)) => {
            format!("({}) {}", func, arg)
          }
          (DAG::Var(_), DAG::Var(_)) => {
            format!("{} {}", func, arg)
          }
          (DAG::Lam(_), DAG::Lam(_)) => {
            format!("({}) ({})", func, arg)
          }
        }
      }
    };

    match self {
      DAG::Var(link) => unsafe {
        let name = &(*link.as_ptr()).name;
        write!(f, "{}", name)
      },
      DAG::Lam(link) => {
        write!(f, "λ {}", lams(link))
      },
      DAG::App(link) => {
        write!(f, "{}", apps(link))
      }
    }
  }
}

// Get the parents of a term.
#[inline]
pub fn get_parents(term: DAG) -> Option<NonNull<Parents>> {
  unsafe {
    match term {
      DAG::Lam(link) => (*link.as_ptr()).parents,
      DAG::App(link) => (*link.as_ptr()).parents,
      DAG::Var(link) => (*link.as_ptr()).parents,
    }
  }
}

// Set the parent slot of a term
#[inline]
pub fn set_parents(term: DAG, pref: Option<NonNull<Parents>>) {
  unsafe {
    match term {
      DAG::Lam(link) => (*link.as_ptr()).parents = pref,
      DAG::App(link) => (*link.as_ptr()).parents = pref,
      DAG::Var(link) => (*link.as_ptr()).parents = pref,
    }
  }
}

// Given a term and a parent node, add the node to term's parents.
#[inline]
pub fn add_to_parents(node: DAG, plink: NonNull<Parents>) {
  let new_parent = unsafe {
    (*plink.as_ptr()).elem
  };
  let parents = get_parents(node);
  parents.map_or((), |parents| unsafe {
    (*parents.as_ptr()).add_before(new_parent)
  });
}

// Resets the cache slots of the app nodes.
pub fn clear_copies(mut redlam: &Lam, topapp: &mut App) {
  fn clean_up(cc: &ParentCell) {
    match cc {
      ParentCell::AppFun(parent) => unsafe {
        let parent = &mut *parent.as_ptr();
        parent.copy.map_or((), |app| {
          parent.copy = None;
          let App {arg, arg_ref, func, func_ref, ..} = *app.as_ptr();
          add_to_parents(arg, arg_ref);
          add_to_parents(func, func_ref);
          for grandparent in DLL::iter_option(parent.parents){
            clean_up(grandparent);
          }
        })
      },
      ParentCell::AppArg(parent) => unsafe {
        let parent = &mut *parent.as_ptr();
        parent.copy.map_or((), |app| {
          parent.copy = None;
          let App {arg, arg_ref, func, func_ref, ..} = *app.as_ptr();
          add_to_parents(arg, arg_ref);
          add_to_parents(func, func_ref);
          for grandparent in DLL::iter_option(parent.parents){
            clean_up(grandparent);
          }
        })
      },
      ParentCell::LamBod(parent) => unsafe {
        let Lam { parents, var, .. } = &*parent.as_ptr();
        let var = &mut *var.as_ptr();
        for var_parent in DLL::iter_option((*var).parents){
          clean_up(var_parent);
        }
        for grandparent in DLL::iter_option(*parents){
          clean_up(grandparent);
        }
      },
      ParentCell::Root => (),
    }
  }
  // Clears the top app cache and adds itself to its children's list of parents
  topapp.copy.map_or((), |ptr| unsafe {
    let App{arg, arg_ref, func, func_ref, ..} = *ptr.as_ptr();
    topapp.copy = None;
    add_to_parents(arg, arg_ref);
    add_to_parents(func, func_ref);
  });
  loop {
    let var = unsafe { &*redlam.var.as_ptr() };
    for parent in DLL::iter_option(var.parents){
      clean_up(parent);
    }
    match redlam.body {
      DAG::Lam(lam) => unsafe { redlam = &*lam.as_ptr() },
      _ => break,
    }
  }
}

// Free parentless nodes.
pub fn free_dead_node(node: DAG) {
  unsafe {
    match node {
      DAG::Lam(link) => {
        let Lam {body, body_ref, ..} = &*link.as_ptr();
        // maybe deallocate var as well?
        match DLL::remove_node(*body_ref) {
          None => free_dead_node(*body),
          _ => (),
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Lam>());
      },
      DAG::App(link) => {
        let App {func, arg, func_ref, arg_ref, ..} = &*link.as_ptr();
        match DLL::remove_node(*func_ref) {
          None => free_dead_node(*func),
          _ => (),
        }
        match DLL::remove_node(*arg_ref) {
          None => free_dead_node(*arg),
          _ => (),
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<App>());
      },
      DAG::Var(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<App>());
      }
    }
  }
}

// Replace one child w/another in the tree.
pub fn replace_child(oldchild: DAG, newchild: DAG){
  #[inline]
  fn install_child(parent: &mut ParentCell, newchild: DAG){
    unsafe {
      match parent {
        ParentCell::AppFun(parent) => (*parent.as_ptr()).func = newchild,
        ParentCell::AppArg(parent) => (*parent.as_ptr()).arg = newchild,
        ParentCell::LamBod(parent) => (*parent.as_ptr()).body = newchild,
        ParentCell::Root => (),
      }
    }
  }
  unsafe {
    let oldpref = get_parents(oldchild);
    if let Some(old_parents) = oldpref {
      let mut iter = (*old_parents.as_ptr()).iter();
      let newpref = get_parents(newchild);
      let mut last_old = None;
      let first_new = newpref.map(|dll| DLL::first(dll));
      while let Some(parent) = iter.next() {
        if iter.is_last() {
          last_old = iter.this();
          last_old.map_or((), |last_old| {
            (*last_old.as_ptr()).next = first_new
          });
        }
        install_child(parent, newchild);
      }
      first_new.map_or((), |first_new| {
        (*first_new.as_ptr()).prev = last_old
      });
      set_parents(newchild, oldpref);
    }
    set_parents(oldchild, None);
  }
}

// Allocate memory with a given value in it.
#[inline]
pub fn alloc_val<T>(val: T) -> NonNull<T>{
  unsafe { NonNull::new_unchecked(Box::leak(Box::new(val))) }
}

// Allocate unitialized memory.
#[inline]
pub fn alloc_uninit<T>() -> NonNull<T>{
  unsafe{
    let ptr = alloc(Layout::new::<T>()) as *mut T;
    NonNull::new_unchecked(ptr)
  }
}

// Allocate a fresh app node, with the two given params as its children.
#[inline]
pub fn new_app(fun: DAG, arg: DAG) -> NonNull<App> {
  unsafe {
    let func_ref = alloc_uninit();
    let arg_ref = alloc_uninit();
    let new_app = alloc_val(App{
      copy: None,
      func: fun,
      arg: arg,
      func_ref: func_ref,
      arg_ref: arg_ref,
      parents: None
      // uniq: new_uniq(),
    });
    *func_ref.as_ptr() = DLL::singleton(ParentCell::AppFun(new_app));
    *arg_ref.as_ptr() = DLL::singleton(ParentCell::AppArg(new_app));
    new_app
  }
}

// Allocate a fresh lambda L and a fresh var V.
#[inline]
pub fn new_lambda(oldvar: NonNull<Var>, body: DAG) -> NonNull<Lam> {
  unsafe {
    let Var { name, parents: varparents, ..} = &*oldvar.as_ptr();
    let var = alloc_val(Var {
      name: name.clone(),
      parents: None,
      // uniq: new_uniq(),
    });
    let dll = alloc_uninit();
    let lam = alloc_val(Lam{
      var: var,
      body: body,
      body_ref: dll,
      parents: None
      // uniq: new_uniq(),
    });
    *dll.as_ptr() = DLL::singleton(ParentCell::LamBod(lam));
    add_to_parents(body, dll);
    for parent in DLL::iter_option(*varparents){
      upcopy(DAG::Var(var), *parent)
    }
    lam
  }
}

// The core up-copy function.
pub fn upcopy(new_child: DAG, cc: ParentCell){
  unsafe {
    match cc {
      ParentCell::LamBod(parent) => {
        let Lam {var, parents: grandparents, ..} = *parent.as_ptr();
        let new_lam = new_lambda(var, new_child);
        for grandparent in DLL::iter_option(grandparents){
          upcopy(DAG::Lam(new_lam), *grandparent)
        }
      },
      ParentCell::AppFun(parent) => {
        let App {copy, arg, parents: grandparents, ..} = *parent.as_ptr();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).func = new_child;
          },
          None => {
            let new_app = new_app(new_child, arg);
            (*parent.as_ptr()).copy = Some(new_app);
            for grandparent in DLL::iter_option(grandparents){
              upcopy(DAG::App(new_app), *grandparent)
            }
          },
        }
      },
      ParentCell::AppArg(parent) => {
        let App {copy, func, parents: grandparents, ..} = *parent.as_ptr();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).arg = new_child;
          },
          None => {
            let new_app = new_app(func, new_child);
            (*parent.as_ptr()).copy = Some(new_app);
            for grandparent in DLL::iter_option(grandparents){
              upcopy(DAG::App(new_app), *grandparent)
            }
          },
        }
      },
      ParentCell::Root => (),
    }
  }
}

// Contract a redex, return the body.
pub fn reduce(term: DAG) -> DAG {
  unsafe {
    let redex = match term {
      DAG::App(link) => link,
      _ => return term,
    };
    let App{func_ref, func, arg_ref, arg, parents, ..} = *redex.as_ptr();
    let lam = match func {
      DAG::Lam(lam) => lam,
      _ => return term
    };
    let Lam{var, body, body_ref, parents: lampars, ..} = *lam.as_ptr();
    let Var{parents: vpars, ..} = *var.as_ptr();
    let ans = if DLL::is_singleton(lampars){
      replace_child(DAG::Var(var), arg);
      // We have to read `body` again because `lam`'s body could be mutated through `replace_child`
      (*lam.as_ptr()).body
    } else if vpars.is_none() {
      body
    } else {
      let mut input = body;
      let mut topapp = None;
      let mut result = arg;
      let mut vars = vec![];
      loop {
        match input {
          DAG::Var(_) => break,
          DAG::Lam(lam) => {
            let Lam {body, var, ..} = *lam.as_ptr();
            input = body;
            vars.push(var);
          },
          DAG::App(app) => {
            let App { arg: top_arg, func, .. } = *app.as_ptr();
            let new_app = new_app(func, top_arg);
            (*app.as_ptr()).copy = Some(new_app);
            topapp = Some(app);
            for parent in DLL::iter_option(vpars){
              upcopy(arg, *parent);
            }
            result = DAG::App(new_app);
            break;
          },
        }
      }
      while let Some(var) = vars.pop() {
        result = DAG::Lam(new_lambda(var, result));
      }
      topapp.map_or((), |app| clear_copies(lam.as_ref(), &mut *app.as_ptr()));
      result
    };
    replace_child(DAG::App(redex), ans);
    free_dead_node(DAG::App(redex));
    ans
  }
}

pub fn whnf(node: DAG) -> DAG {
  match node {
    DAG::App(link) => unsafe {
      let app = &mut *link.as_ptr();
      (*app).func = whnf((*app).func);
      match (*app).func {
        DAG::Lam(_) => whnf(reduce(node)),
        _ => node,
      }

    },
    _ => node,
  }
}

pub fn norm(node: DAG) -> DAG {
  let node = whnf(node);
  match node {
    DAG::App(link) => unsafe {
      let app = &mut *link.as_ptr();
      app.func = norm(app.func);
      app.arg = norm(app.arg);
    },
    DAG::Lam(link) => unsafe {
      let lam = &mut *link.as_ptr();
      lam.body = norm(lam.body);
    },
    DAG::Var(link) => ()
  }
  node
}
