#![allow(unused_variables)]

pub mod prim;

use crate::{
  term,
  term::{
    Literal,
    Term,
  },
  valus::{
    dag::prim::{
      PrimOpr,
      PrimVal,
    },
    dll::*,
    eval,
  },
};

use core::ptr::NonNull;
use std::{
  alloc::{
    alloc,
    dealloc,
    Layout,
  },
  collections::HashMap,
  convert::TryInto,
  fmt,
};

// A λ-DAG pointer. Keeps track of what kind of node it points to.
#[derive(Clone, Copy)]
pub enum DAG {
  Lam(NonNull<Lam>),
  App(NonNull<App>),
  Var(NonNull<Var>),
  Lit(NonNull<Lit>),
  Opr(NonNull<Opr>),
}

// Doubly-linked list of parent nodes
type Parents = DLL<ParentCell>;

// A parent pointer. Keeps track of the relation between the child and the
// parent.
#[derive(Clone, Copy)]
pub enum ParentCell {
  AppFun(NonNull<App>),
  AppArg(NonNull<App>),
  LamBod(NonNull<Lam>),
  Root,
}

// The five λ-DAG nodes: Lam, App, Var, Lit and Opr
pub struct Lam {
  pub var: NonNull<Var>,
  pub body: DAG,
  pub body_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct App {
  pub func: DAG,
  pub arg: DAG,
  pub func_ref: NonNull<Parents>,
  pub arg_ref: NonNull<Parents>,
  pub copy: Option<NonNull<App>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Var {
  pub name: String,
  pub parents: Option<NonNull<Parents>>,
  // uniq: u64,
}

pub struct Lit {
  pub val: PrimVal,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Opr {
  pub opr: PrimOpr,
  pub parents: Option<NonNull<Parents>>,
}

// Get the parents of a term.
#[inline]
pub fn get_parents(term: DAG) -> Option<NonNull<Parents>> {
  unsafe {
    match term {
      DAG::Lam(link) => (*link.as_ptr()).parents,
      DAG::App(link) => (*link.as_ptr()).parents,
      DAG::Var(link) => (*link.as_ptr()).parents,
      DAG::Lit(link) => (*link.as_ptr()).parents,
      DAG::Opr(link) => (*link.as_ptr()).parents,
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
      DAG::Lit(link) => (*link.as_ptr()).parents = pref,
      DAG::Opr(link) => (*link.as_ptr()).parents = pref,
    }
  }
}

// Given a term and a parent node, add the node to term's parents.
#[inline]
pub fn add_to_parents(node: DAG, plink: NonNull<Parents>) {
  let parents = get_parents(node);
  match parents {
    Some(parents) => unsafe { (*parents.as_ptr()).merge(plink) },
    None => set_parents(node, Some(plink)),
  }
}

// Resets the cache slots of the app nodes.
pub fn clear_copies(mut redlam: &Lam, topapp: &mut App) {
  fn clean_up(cc: &ParentCell) {
    match cc {
      ParentCell::AppFun(parent) => unsafe {
        let parent = &mut *parent.as_ptr();
        parent.copy.map_or((), |app| {
          parent.copy = None;
          let App { arg, arg_ref, func, func_ref, .. } = *app.as_ptr();
          add_to_parents(arg, arg_ref);
          add_to_parents(func, func_ref);
          for grandparent in DLL::iter_option(parent.parents) {
            clean_up(grandparent);
          }
        })
      },
      ParentCell::AppArg(parent) => unsafe {
        let parent = &mut *parent.as_ptr();
        parent.copy.map_or((), |app| {
          parent.copy = None;
          let App { arg, arg_ref, func, func_ref, .. } = *app.as_ptr();
          add_to_parents(arg, arg_ref);
          add_to_parents(func, func_ref);
          for grandparent in DLL::iter_option(parent.parents) {
            clean_up(grandparent);
          }
        })
      },
      ParentCell::LamBod(parent) => unsafe {
        let Lam { parents, var, .. } = &*parent.as_ptr();
        let var = &mut *var.as_ptr();
        for var_parent in DLL::iter_option((*var).parents) {
          clean_up(var_parent);
        }
        for grandparent in DLL::iter_option(*parents) {
          clean_up(grandparent);
        }
      },
      ParentCell::Root => (),
    }
  }
  // Clears the top app cache and adds itself to its children's list of parents
  topapp.copy.map_or((), |ptr| unsafe {
    let App { arg, arg_ref, func, func_ref, .. } = *ptr.as_ptr();
    topapp.copy = None;
    add_to_parents(arg, arg_ref);
    add_to_parents(func, func_ref);
  });
  loop {
    let var = unsafe { &*redlam.var.as_ptr() };
    for parent in DLL::iter_option(var.parents) {
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
        let Lam { body, body_ref, var, .. } = &*link.as_ptr();
        if (*var.as_ptr()).parents.is_none() {
          free_dead_node(DAG::Var(*var))
        }
        let new_body_parents = DLL::remove_node(*body_ref);
        set_parents(*body, new_body_parents);
        match new_body_parents {
          None => free_dead_node(*body),
          _ => (),
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Lam>());
      }
      DAG::App(link) => {
        let App { func, arg, func_ref, arg_ref, .. } = &*link.as_ptr();
        let new_func_parents = DLL::remove_node(*func_ref);
        set_parents(*func, new_func_parents);
        match new_func_parents {
          None => free_dead_node(*func),
          _ => (),
        }
        let new_arg_parents = DLL::remove_node(*arg_ref);
        set_parents(*arg, new_arg_parents);
        match new_arg_parents {
          None => free_dead_node(*arg),
          _ => (),
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<App>());
      }
      DAG::Var(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Var>());
      }
      DAG::Lit(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Lit>());
      }
      DAG::Opr(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Opr>());
      }
    }
  }
}

// Replace one child w/another in the tree.
pub fn replace_child(oldchild: DAG, newchild: DAG) {
  #[inline]
  fn install_child(parent: &mut ParentCell, newchild: DAG) {
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

// Allocate memory with a given value in it.
#[inline]
pub fn alloc_val<T>(val: T) -> NonNull<T> {
  unsafe { NonNull::new_unchecked(Box::leak(Box::new(val))) }
}

// Allocate unitialized memory.
#[inline]
pub fn alloc_uninit<T>() -> NonNull<T> {
  unsafe {
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
    let new_app = alloc_val(App {
      copy: None,
      func: fun,
      arg,
      func_ref,
      arg_ref,
      parents: None,
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
    let Var { name, parents: varparents, .. } = &*oldvar.as_ptr();
    let var = alloc_val(Var { name: name.clone(), parents: None });
    let dll = alloc_uninit();
    let lam = alloc_val(Lam {
      var,
      body,
      body_ref: dll,
      parents: None, // uniq: new_uniq(),
    });
    *dll.as_ptr() = DLL::singleton(ParentCell::LamBod(lam));
    add_to_parents(body, dll);
    for parent in DLL::iter_option(*varparents) {
      eval::upcopy(DAG::Var(var), *parent)
    }
    lam
  }
}

// Allocate a fresh lit node
#[inline]
pub fn alloc_lit(n: PrimVal) -> NonNull<Lit> {
  alloc_val(Lit { val: n, parents: None })
}

impl fmt::Display for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_term())
  }
}

impl DAG {
  pub fn to_term(&self) -> Term {
    let mut map: HashMap<*mut Var, u64> = HashMap::new();

    pub fn go(
      node: &DAG,
      mut map: &mut HashMap<*mut Var, u64>,
      depth: u64,
    ) -> Term {
      match node {
        DAG::Var(link) => {
          let nam = unsafe { &(*link.as_ptr()).name };
          let level = map.get(&link.as_ptr()).unwrap();
          Term::Var(None, nam.clone(), depth - level - 1)
        }
        DAG::Lam(link) => {
          let var = unsafe { &(*link.as_ptr()).var };
          let bod = unsafe { &(*link.as_ptr()).body };
          let nam = unsafe { &(*var.as_ptr()).name };
          map.insert(var.as_ptr(), depth);
          let body = go(bod, &mut map, depth + 1);
          Term::Lam(None, nam.clone(), Box::new(body))
        }
        DAG::App(link) => {
          let fun = unsafe { &(*link.as_ptr()).func };
          let arg = unsafe { &(*link.as_ptr()).arg };
          let fun = go(fun, &mut map, depth);
          let arg = go(arg, &mut map, depth);
          Term::App(None, Box::new(fun), Box::new(arg))
        }
        DAG::Lit(link) => {
          let lit = unsafe { &(*link.as_ptr()).val };
          match lit {
            PrimVal::U8(x) => Term::Lit(None, Literal::Nat(8, (*x).into())),
            PrimVal::U16(x) => Term::Lit(None, Literal::Nat(16, (*x).into())),
            PrimVal::U32(x) => Term::Lit(None, Literal::Nat(32, (*x).into())),
            PrimVal::U64(x) => Term::Lit(None, Literal::Nat(64, (*x).into())),
            PrimVal::Nat(x) => Term::Lit(None, Literal::Natural(x.clone())),
          }
        }
        DAG::Opr(link) => {
          let opr = unsafe { &(*link.as_ptr()).opr };
          match opr {
            PrimOpr::Add => Term::Opr(None, term::PrimOp::Add),
            PrimOpr::Sub => Term::Opr(None, term::PrimOp::Add),
            PrimOpr::Mul => Term::Opr(None, term::PrimOp::Add),
            PrimOpr::Div => Term::Opr(None, term::PrimOp::Add),
          }
        }
      }
    }
    go(&self, &mut map, 0)
  }

  pub fn from_term(tree: Term) -> DAG {
    pub fn go(
      tree: Term,
      mut map: HashMap<String, NonNull<Var>>,
      parents: NonNull<DLL<ParentCell>>,
    ) -> DAG {
      match tree {
        Term::Lam(_, name, body) => {
          // Allocate nodes
          let var = alloc_val(Var { name: name.clone(), parents: None });
          let sons_parents = alloc_uninit();
          let lam = alloc_val(Lam {
            var,
            // Temporary, dangling DAG pointer
            body: DAG::Lam(NonNull::dangling()),
            body_ref: sons_parents,
            parents: Some(parents),
          });

          // Update `sons_parents` to refer to current node
          unsafe {
            *sons_parents.as_ptr() = DLL::singleton(ParentCell::LamBod(lam));
          }

          // Map `name` to `var` node
          map.insert(name.clone(), var);
          let body = go(*body, map, sons_parents);

          // Update `lam` with the correct body
          unsafe {
            (*lam.as_ptr()).body = body;
          }
          DAG::Lam(lam)
        }

        Term::App(_, fun, arg) => {
          // Allocation and updates
          let arg_parents = alloc_uninit();
          let func_parents = alloc_uninit();
          let app = alloc_val(App {
            // Temporary, dangling DAG pointers
            func: DAG::Lam(NonNull::dangling()),
            arg: DAG::Lam(NonNull::dangling()),
            func_ref: func_parents,
            arg_ref: arg_parents,
            copy: None,
            parents: Some(parents),
          });
          unsafe {
            *arg_parents.as_ptr() = DLL::singleton(ParentCell::AppArg(app));
            *func_parents.as_ptr() = DLL::singleton(ParentCell::AppFun(app));
          }

          let fun = go(*fun, map.clone(), func_parents);
          let arg = go(*arg, map, arg_parents);

          // Update `app` with the correct fields
          unsafe {
            (*app.as_ptr()).arg = arg;
            (*app.as_ptr()).func = fun;
          }
          DAG::App(app)
        }

        Term::Var(_, name, _) => {
          let var = match map.get(&name.clone()) {
            Some(var) => unsafe {
              DLL::concat(parents, (*var.as_ptr()).parents);
              (*var.as_ptr()).parents = Some(parents);
              *var
            },
            None => {
              alloc_val(Var { name: name.clone(), parents: Some(parents) })
            }
          };
          DAG::Var(var)
        }
        Term::Lit(_, Literal::Nat(8, x)) => {
          let x: u8 = x.try_into().expect("to_dag u8 error");
          DAG::Lit(alloc_lit(PrimVal::U8(x)))
        }
        Term::Lit(_, Literal::Nat(16, x)) => {
          let x: u16 = x.try_into().expect("to_dag u16 error");
          DAG::Lit(alloc_lit(PrimVal::U16(x)))
        }
        Term::Lit(_, Literal::Nat(32, x)) => {
          let x: u32 = x.try_into().expect("to_dag u32 error");
          DAG::Lit(alloc_lit(PrimVal::U32(x)))
        }
        Term::Lit(_, Literal::Nat(64, x)) => {
          let x: u64 = x.try_into().expect("to_dag u64 error");
          DAG::Lit(alloc_lit(PrimVal::U64(x)))
        }
        Term::Lit(_, Literal::Natural(x)) => {
          DAG::Lit(alloc_lit(PrimVal::Nat(x)))
        }
        _ => panic!("TODO: implement Term::to_dag variants"),
      }
    }
    let root = alloc_val(DLL::singleton(ParentCell::Root));
    go(tree, HashMap::new(), root)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[quickcheck]
  fn term_encode_decode(x: Term) -> bool {
    println!("x: {}", x);
    println!("x: {:?}", x);
    let y = DAG::to_term(&DAG::from_term(x.clone()));
    println!("y: {}", y);
    println!("y: {:?}", y);
    x == y
  }
}
