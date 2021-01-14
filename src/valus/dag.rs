#![allow(unused_variables)]

use crate::{
  term::Term,
  valus::dll::*,
};

use hashexpr::span::Span;
use nom::IResult;

use core::ptr::NonNull;
use std::{
  alloc::{
    alloc,
    dealloc,
    Layout,
  },
  collections::HashMap,
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


// Primitive values and operations
#[derive(Clone, Copy)]
pub enum PrimVal {
  Nat(u32),
}

#[derive(Clone, Copy)]
pub enum PrimOpr {
  Add,
  Sub,
  Mul,
  Div,
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
    let var = alloc_val(Var {
      name: name.clone(),
      parents: None,
    });
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
      upcopy(DAG::Var(var), *parent)
    }
    lam
  }
}

// Allocate a fresh lit node
#[inline]
pub fn new_lit(n: PrimVal) -> NonNull<Lit> {
  alloc_val(Lit {
    val: n,
    parents: None,
  })
}

// The core up-copy function.
pub fn upcopy(new_child: DAG, cc: ParentCell) {
  unsafe {
    match cc {
      ParentCell::LamBod(parent) => {
        let Lam { var, parents: grandparents, .. } = *parent.as_ptr();
        let new_lam = new_lambda(var, new_child);
        for grandparent in DLL::iter_option(grandparents) {
          upcopy(DAG::Lam(new_lam), *grandparent)
        }
      }
      ParentCell::AppFun(parent) => {
        let App { copy, arg, parents: grandparents, .. } = *parent.as_ptr();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).func = new_child;
          }
          None => {
            let new_app = new_app(new_child, arg);
            (*parent.as_ptr()).copy = Some(new_app);
            for grandparent in DLL::iter_option(grandparents) {
              upcopy(DAG::App(new_app), *grandparent)
            }
          }
        }
      }
      ParentCell::AppArg(parent) => {
        let App { copy, func, parents: grandparents, .. } = *parent.as_ptr();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).arg = new_child;
          }
          None => {
            let new_app = new_app(func, new_child);
            (*parent.as_ptr()).copy = Some(new_app);
            for grandparent in DLL::iter_option(grandparents) {
              upcopy(DAG::App(new_app), *grandparent)
            }
          }
        }
      }
      ParentCell::Root => (),
    }
  }
}

// Contract a lambda redex, return the body.
pub fn reduce_lam(redex: NonNull<App>, lam: NonNull<Lam>) -> DAG {
  unsafe {
    let App { arg, .. } = *redex.as_ptr();
    let Lam { var, body, parents: lam_parents, .. } = *lam.as_ptr();
    let Var { parents: var_parents, .. } = *var.as_ptr();
    let ans = if DLL::is_singleton(lam_parents) {
      replace_child(DAG::Var(var), arg);
      // We have to read `body` again because `lam`'s body could be mutated
      // through `replace_child`
      (*lam.as_ptr()).body
    }
    else if var_parents.is_none() {
      body
    }
    else {
      let mut input = body;
      let mut topapp = None;
      let mut result = arg;
      let mut vars = vec![];
      loop {
        match input {
          DAG::Lam(lam) => {
            let Lam { body, var, .. } = *lam.as_ptr();
            input = body;
            vars.push(var);
          }
          DAG::App(app) => {
            let App { arg: top_arg, func, .. } = *app.as_ptr();
            let new_app = new_app(func, top_arg);
            (*app.as_ptr()).copy = Some(new_app);
            topapp = Some(app);
            for parent in DLL::iter_option(var_parents) {
              upcopy(arg, *parent);
            }
            result = DAG::App(new_app);
            break;
          },
          // Otherwise it must be `var`, since `var` necessarily appears inside `body`
          _ => break
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

pub fn apply_opr(opr: PrimOpr, x: PrimVal, y: PrimVal) -> Option<PrimVal> {
  match (opr, x, y) {
    (PrimOpr::Add, PrimVal::Nat(x), PrimVal::Nat(y)) => Some(PrimVal::Nat(x+y)),
    (PrimOpr::Sub, PrimVal::Nat(x), PrimVal::Nat(y)) => Some(PrimVal::Nat(x-y)),
    (PrimOpr::Mul, PrimVal::Nat(x), PrimVal::Nat(y)) => Some(PrimVal::Nat(x*y)),
    (PrimOpr::Div, PrimVal::Nat(x), PrimVal::Nat(y)) => Some(PrimVal::Nat(x/y)),
    // _ => None,
  }
}

// Reduce term to its weak head normal form
pub fn whnf(mut node: DAG) -> DAG {
  let mut trail = vec![];
  loop {
    match node {
      DAG::App(link) => unsafe {
        trail.push(link);
        node = (*link.as_ptr()).func;
      },
      DAG::Lam(lam_link) => {
        if let Some(app_link) = trail.pop() {
          node = reduce_lam(app_link, lam_link);
        }
        else {
          break;
        }
      }
      DAG::Opr(link) => unsafe {
        let len = trail.len();
        if len >= 2 {
          let arg1 = whnf((*trail[len-2].as_ptr()).arg);
          let arg2 = whnf((*trail[len-1].as_ptr()).arg);
          match (arg1, arg2) {
            (DAG::Lit(x), DAG::Lit(y)) => {
              let opr = (*link.as_ptr()).opr;
              let x = (*x.as_ptr()).val;
              let y = (*y.as_ptr()).val;
              let res = apply_opr(opr, x, y);
              if let Some(res) = res {
                trail.pop();
                trail.pop();
                node = DAG::Lit(new_lit(res));
                replace_child(arg1, node);
                free_dead_node(arg1);
              }
              else {
                break;
              }
            },
            _ => break,
          }
        }
        break
      },
      _ => break,
    }
  }
  if trail.is_empty() {
    return node;
  }
  DAG::App(trail[0])
}

// Reduce term to its normal form
pub fn norm(mut top_node: DAG) -> DAG {
  top_node = whnf(top_node);
  let mut trail = vec![top_node];
  while let Some(node) = trail.pop() {
    match node {
      DAG::App(link) => unsafe {
        let app = &mut *link.as_ptr();
        trail.push(whnf(app.func));
        trail.push(whnf(app.arg));
      },
      DAG::Lam(link) => unsafe {
        let lam = &mut *link.as_ptr();
        trail.push(whnf(lam.body));
      },
      _ => ()
    }
  }
  top_node
}

// TODO: rewrite in terms of `to_term`
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
          (DAG::Lam(_), DAG::App(link_arg)) => {
            format!("({}) ({})", func, apps(link_arg))
          }
          (DAG::Lam(_), DAG::Lam(_)) => {
            format!("({}) ({})", func, arg)
          }
          (DAG::App(link_func), _) => {
            format!("{} {}", apps(link_func), arg)
          }
          (DAG::Lam(_), _) => {
            format!("({}) {}", func, arg)
          }
          (_, DAG::App(link_arg)) => {
            format!("{} ({})", func, apps(link_arg))
          }
          (_, DAG::Lam(_)) => {
            format!("{} ({})", func, arg)
          }
          _ => {
            format!("{} {}", func, arg)
          }
        }
      }
    };

    #[inline]
    fn lit(link: &NonNull<Lit>) -> String {
      let val = unsafe { &(*link.as_ptr()).val };
      match val {
        PrimVal::Nat(n) => format!("{}", n),
      }
    }

    #[inline]
    fn opr(link: &NonNull<Opr>) -> String {
      let opr = unsafe { &(*link.as_ptr()).opr };
      match opr {
        PrimOpr::Add => String::from("+"),
        PrimOpr::Sub => String::from("-"),
        PrimOpr::Mul => String::from("*"),
        PrimOpr::Div => String::from("/"),
      }
    }

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
      DAG::Lit(link) => {
        write!(f, "{}", lit(link))
      }
      DAG::Opr(link) => {
        write!(f, "{}", opr(link))
      }
    }
  }
}

impl DAG {
  // TODO: pub fn to_term(self) -> Term {}
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
        _ => panic!("TODO: implement Term::to_dag variants"),
      }
    }
    let root = alloc_val(DLL::singleton(ParentCell::Root));
    go(tree, HashMap::new(), root)
  }
}

pub fn parse(
  i: &str,
) -> IResult<Span, DAG, crate::parse::error::ParseError<Span>> {
  let (i, tree) = crate::parse::term::parse(i)?;
  let (i, _) = nom::character::complete::multispace0(i)?;
  let (i, _) = nom::combinator::eof(i)?;
  let dag = DAG::from_term(tree);
  Ok((i, dag))
}

#[cfg(test)]
mod test {
  use super::{
    norm,
    parse,
  };

  #[test]
  pub fn parser() {
    fn parse_assert(input: &str) {
      match parse(&input) {
        Ok((_, dag)) => assert_eq!(format!("{}", dag), input),
        Err(_) => panic!("Did not parse."),
      }
    }
    parse_assert("λ x => x");
    parse_assert("λ x y => x y");
    parse_assert("λ y => (λ x => x) y");
    parse_assert("λ y => (λ z => z z) ((λ x => x) y)");
  }

  #[test]
  pub fn reducer() {
    fn norm_assert(input: &str, result: &str) {
      match parse(&input) {
        Ok((_, dag)) => assert_eq!(format!("{}", norm(dag)), result),
        Err(_) => panic!("Did not parse."),
      }
    }
    // Already normalized
    norm_assert("λ x => x", "λ x => x");
    norm_assert("λ x y => x y", "λ x y => x y");
    // Not normalized cases
    norm_assert("λ y => (λ x => x) y", "λ y => y");
    norm_assert("λ y => (λ z => z z) ((λ x => x) y)", "λ y => y y");
    // // Church arithmetic
    let zero = "λ s z => z";
    let three = "λ s z => s (s (s z))";
    let four = "λ s z => s (s (s (s z)))";
    let seven = "λ s z => s (s (s (s (s (s (s z))))))";
    let add = "λ m n s z => m s (n s z)";
    let is_three = format!("(({}) ({}) {})", add, zero, three);
    let is_seven = format!("(({}) ({}) {})", add, four, three);
    norm_assert(&is_three, three);
    norm_assert(&is_seven, seven);
    let id = "λ x => x";
    norm_assert(
      &format!("({three}) (({three}) ({id})) ({id})", id = id, three = three),
      id,
    );
  }
}
