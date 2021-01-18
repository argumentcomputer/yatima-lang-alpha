#![allow(unused_variables)]

use crate::{
  term,
  term::Term,
  valus::{
    dll::*,
    eval,
    literal::Literal,
    literal::LitType,
    primop::PrimOp,
    uses::Uses,
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

// A top-down λ-DAG pointer. Keeps track of what kind of node it points to.
#[derive(Clone, Copy)]
pub enum DAG {
  Lam(NonNull<Lam>),
  App(NonNull<App>),
  Var(NonNull<Var>),
  All(NonNull<All>),
  Fix(NonNull<Fix>),
  Slf(NonNull<Slf>),
  Dat(NonNull<Dat>),
  Cse(NonNull<Cse>),
  Let(NonNull<Let>),
  Typ(NonNull<Typ>),
  Ann(NonNull<Ann>),
  LTy(NonNull<LTy>),
  Lit(NonNull<Lit>),
  Opr(NonNull<Opr>),
  // Ref(NonNull<Ref>),
}

// Doubly-linked list of parent nodes
type Parents = DLL<ParentCell>;

// A bottom-up (parent) λ-DAG pointer. Keeps track of the relation between
// the child and the parent.
#[derive(Clone, Copy)]
pub enum ParentCell {
  Root,
  AppFun(NonNull<App>),
  AppArg(NonNull<App>),
  LamBod(NonNull<Lam>),
  AllDom(NonNull<All>),
  AllImg(NonNull<All>),
  FixBod(NonNull<Fix>),
  SlfBod(NonNull<Slf>),
  DatBod(NonNull<Dat>),
  CseBod(NonNull<Cse>),
  LetExp(NonNull<Let>),
  LetTyp(NonNull<Let>),
  LetBod(NonNull<Let>),
  AnnExp(NonNull<Ann>),
  AnnTyp(NonNull<Ann>),
}

// The λ-DAG nodes
// TODO: Each constructor is its own struct. This is necessary for two things:
// variable binding constructors need a pointer directly to a variable node and
// the parent pointers must only point to nodes that have children, not general
// DAG nodes. Note that each variable binding node must currently have a `var` field
// and each constructor that have at least two children must have a `copy` field.
// As we will see, this will create a lot of boilerplate in the upcoming functions.
// It is possible, however, to reduce the boilerplate by instead adding general 0,
// 1 and 2 children nodes with a tag and a generic "other data field" (such as
// multiplicity and name). To account for variable binding nodes, we could have a
// single variable binding node, such as `Lam` and have every other var. binding
// constructor take a `Lam` node as argument; i.e., `Slf n b` becomes `Slf (Lam n b)`.
// Alternatively, we could have each generic node have an optional `var` field; i.e.,
// by changing the type of `var` from `NonNull<Var>` to `Option<NonNull<Var>>`.
// Doing this would make `App`, `Ann` and `All` instances of two children node, `Slf`,
// `Cse`, `Fix` and `Dat` (and possibly `Lam`) instances of one child node and `Var`,
// `Opr`, `Lit`, `Typ`, `LTy` and `Ref` instances of childless nodes, leaving only
// `Let` (and possibly `Lam`) as specific constructor nodes.
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
}

pub struct All {
  pub uses: Uses,
  pub var: NonNull<Var>,
  pub dom: DAG,
  pub img: DAG,
  pub dom_ref: NonNull<Parents>,
  pub img_ref: NonNull<Parents>,
  pub copy: Option<NonNull<All>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Let {
  pub uses: Uses,
  pub var: NonNull<Var>,
  pub exp: DAG,
  pub typ: DAG,
  pub bod: DAG,
  pub exp_ref: NonNull<Parents>,
  pub typ_ref: NonNull<Parents>,
  pub bod_ref: NonNull<Parents>,
  pub copy: Option<NonNull<Let>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Ann {
  pub exp: DAG,
  pub typ: DAG,
  pub exp_ref: NonNull<Parents>,
  pub typ_ref: NonNull<Parents>,
  pub copy: Option<NonNull<Ann>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Fix {
  pub var: NonNull<Var>,
  pub body: DAG,
  pub body_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Slf {
  pub var: NonNull<Var>,
  pub body: DAG,
  pub body_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Cse {
  pub body: DAG,
  pub body_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Dat {
  pub body: DAG,
  pub body_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

// TODO: Childless nodes apart from `Var` do not really need a
// `parents` field, because the upcopy will never reach such nodes
// and, apart from `Lit`, these other nodes do not really need to
// be garbage collected or allocated on the fly, and could instead
// be shared constants, so this field would not even be used as
// reference counting.
// We currently keep these fields to make the nodes more homogeneous.
pub struct Typ {
  pub parents: Option<NonNull<Parents>>,
}

pub struct LTy {
  pub val: LitType,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Lit {
  pub val: Literal,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Opr {
  pub opr: PrimOp,
  pub parents: Option<NonNull<Parents>>,
}

// pub struct Ref {
//   pub parents: Option<NonNull<Parents>>,
// }

// Get the parents of a term.
#[inline]
pub fn get_parents(term: DAG) -> Option<NonNull<Parents>> {
  unsafe {
    match term {
      DAG::Lam(link) => (*link.as_ptr()).parents,
      DAG::App(link) => (*link.as_ptr()).parents,
      DAG::Var(link) => (*link.as_ptr()).parents,
      DAG::All(link) => (*link.as_ptr()).parents,
      DAG::Fix(link) => (*link.as_ptr()).parents,
      DAG::Slf(link) => (*link.as_ptr()).parents,
      DAG::Dat(link) => (*link.as_ptr()).parents,
      DAG::Cse(link) => (*link.as_ptr()).parents,
      DAG::Let(link) => (*link.as_ptr()).parents,
      DAG::Typ(link) => (*link.as_ptr()).parents,
      DAG::Ann(link) => (*link.as_ptr()).parents,
      DAG::LTy(link) => (*link.as_ptr()).parents,
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
      DAG::All(link) => (*link.as_ptr()).parents = pref,
      DAG::Fix(link) => (*link.as_ptr()).parents = pref,
      DAG::Slf(link) => (*link.as_ptr()).parents = pref,
      DAG::Dat(link) => (*link.as_ptr()).parents = pref,
      DAG::Cse(link) => (*link.as_ptr()).parents = pref,
      DAG::Let(link) => (*link.as_ptr()).parents = pref,
      DAG::Typ(link) => (*link.as_ptr()).parents = pref,
      DAG::Ann(link) => (*link.as_ptr()).parents = pref,
      DAG::LTy(link) => (*link.as_ptr()).parents = pref,
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
pub fn clear_copies(redlam: &Lam, topapp: &mut App) {
  fn clean_up(cc: &ParentCell) {
    // TODO: add all the other parent nodes. All constructors
    // with a `copy` field would follow the `AppFun`/`AppArg`
    // pattern: if the copy field is `None`, then stop the return,
    // else add each unlinked parent (func_ref, etc) to its respective
    // child and continue the `clean_up` function. All constructors
    // with a `var` field will also need to spawn a `clean_up` in
    // its variable node. If a constructor has both a `copy` field
    // and a `var` field, it should check the `copy` field first,
    // and only if its not `None` should it initiate `clean_up`
    // in its variable node. All other nodes will simply recurse upwards. 
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
  // TODO: Since there are other nodes beyond `App` with a `copy` field, then
  // `topapp` could actually be any such nodes, and this must be accounted for.
  // Maybe a special enum type for such nodes should be created. Generic two
  // children nodes would account for that if it weren't for the single three
  // children node `Let`. Is there a way out?
  topapp.copy.map_or((), |ptr| unsafe {
    let App { arg, arg_ref, func, func_ref, .. } = *ptr.as_ptr();
    topapp.copy = None;
    add_to_parents(arg, arg_ref);
    add_to_parents(func, func_ref);
  });
  let mut node = redlam;
  loop {
    // TODO: This loop will traverse the line of one child nodes, until it finds
    // the node with more than one children (the one with a `copy` field). If any
    // such nodes have a `var` field, then it should initiate `clean_up` in its
    // variable node. Because of this, `node` cannot be assumed to be only lambda
    // nodes anymore. Here, again the generic nodes would help.
    let var = unsafe { &*node.var.as_ptr() };
    for parent in DLL::iter_option(var.parents) {
      clean_up(parent);
    }
    match node.body {
      DAG::Lam(lam) => unsafe { node = &*lam.as_ptr() },
      _ => break,
    }
  }
}

// Free parentless nodes.
pub fn free_dead_node(node: DAG) {
  unsafe {
    // TODO: add all the other `DAG` cases. The rest should be
    // pretty easy: remove itself from its children's list of parents,
    // recursing down every time a child becomes parentless, and freeing
    // itself. Nodes with `var` field should only free its var when such
    // nodes are parentless, and this should be done before removing
    // itself from its children's list of parents. Childless nodes
    // are immediately freed.
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
      _ => panic!("TODO"),
    }
  }
}

// Replace one child w/another in the tree.
pub fn replace_child(oldchild: DAG, newchild: DAG) {
  #[inline]
  fn install_child(parent: &mut ParentCell, newchild: DAG) {
    // TODO: add the rest of the `ParentCell` cases. The other
    // cases are completely analogous.
    unsafe {
      match parent {
        ParentCell::AppFun(parent) => (*parent.as_ptr()).func = newchild,
        ParentCell::AppArg(parent) => (*parent.as_ptr()).arg = newchild,
        ParentCell::LamBod(parent) => (*parent.as_ptr()).body = newchild,
        ParentCell::Root => (),
        _ => panic!("TODO"),
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
pub fn alloc_lit(n: Literal) -> NonNull<Lit> {
  alloc_val(Lit { val: n, parents: None })
}

// Allocate a fresh lit node
#[inline]
pub fn alloc_opr(n: PrimOp) -> NonNull<Opr> {
  alloc_val(Opr { opr: n, parents: None })
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
          Term::Lit(None, lit.clone())
        }
        DAG::Opr(link) => {
          let opr = unsafe { &(*link.as_ptr()).opr };
          Term::Opr(None, *opr)
        }
        _ => panic!("TODO"),
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
        Term::Lit(_, lit) => {
          DAG::Lit(alloc_val(Lit { val: lit, parents: Some(parents) }))
        }
        Term::Opr(_, opr) => {
          DAG::Opr(alloc_val(Opr { opr, parents: Some(parents) }))
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
