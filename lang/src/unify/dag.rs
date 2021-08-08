// Bottom-up reduction of lambda DAGs. Based on the paper by Olin Shivers and
// Mitchel Wand "Bottom-up β-reduction: uplinks and λ-DAGs" (https://www.brics.dk/RS/04/38/BRICS-RS-04-38.pdf)
use yatima_core::{
  dll::*,
  literal::{
    LitType,
    Literal,
  },
  name::Name,
  position::Pos,
  prim::Op,
};

use crate::{
  term::Term,
  uses::Uses,
};

use core::ptr::NonNull;

use sp_std::{
  boxed::Box,
  collections::{
    btree_map::BTreeMap,
    btree_set::BTreeSet,
  },
  fmt,
};

use alloc::string::String;
use sp_cid::Cid;

pub struct DAG {
  pub head: DAGPtr,
}

// A top-down λ-DAG pointer. Keeps track of what kind of node it points to.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DAGPtr {
  Null,
  Bind(NonNull<Bind>),
  Var(NonNull<Var>),
  Hol(NonNull<Hol>),
  Lam(NonNull<Lam>),
  App(NonNull<App>),
  All(NonNull<All>),
  Slf(NonNull<Slf>),
  Fix(NonNull<Fix>),
  Dat(NonNull<Dat>),
  Cse(NonNull<Cse>),
  Ref(NonNull<Ref>),
  Let(NonNull<Let>),
  Typ(NonNull<Typ>),
  Lit(NonNull<Lit>),
  LTy(NonNull<LTy>),
  Opr(NonNull<Opr>),
}

// Doubly-linked list of parent nodes
pub type Parents = DLL<ParentPtr>;

// A bottom-up (parent) λ-DAG pointer. Keeps track of the relation between
// the child and the parent.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ParentPtr {
  Root,
  Bound(NonNull<Bind>),
  LamTyp(NonNull<Lam>),
  LamBod(NonNull<Lam>),
  SlfBod(NonNull<Slf>),
  FixBod(NonNull<Fix>),
  DatTyp(NonNull<Dat>),
  DatBod(NonNull<Dat>),
  CseBod(NonNull<Cse>),
  AppFun(NonNull<App>),
  AppTyp(NonNull<App>),
  AppArg(NonNull<App>),
  AllDom(NonNull<All>),
  AllImg(NonNull<All>),
  LetTyp(NonNull<Let>),
  LetExp(NonNull<Let>),
  LetBod(NonNull<Let>),
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum BinderPtr {
  Free,
  Bind(NonNull<Bind>),
  Slf(NonNull<Slf>),
  Fix(NonNull<Fix>),
}

// The λ-DAG nodes
#[derive(Clone, Debug)]
#[repr(C)]
pub struct Var {
  pub nam: Name,
  // The field `depth` is only used by the type checker to track free
  // variables. Otherwise it is irrelevant.
  pub dep: u64,
  pub binder: BinderPtr,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct Hol {
  pub nam: Name,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Bind {
  pub bod: DAGPtr,
  pub bod_ref: Option<Parents>,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Lam {
  pub uses: Uses,
  pub typ: DAGPtr,
  pub bod: NonNull<Bind>,
  pub typ_ref: Option<Parents>,
  pub bod_ref: Option<Parents>,
  pub copy: Option<NonNull<Lam>>,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct App {
  pub uses: Uses,
  pub fun: DAGPtr,
  pub typ: DAGPtr,
  pub arg: DAGPtr,
  pub fun_ref: Option<Parents>,
  pub typ_ref: Option<Parents>,
  pub arg_ref: Option<Parents>,
  pub copy: Option<NonNull<App>>,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct All {
  pub uses: Uses,
  pub dom: DAGPtr,
  pub img: NonNull<Bind>,
  pub dom_ref: Option<Parents>,
  pub img_ref: Option<Parents>,
  pub copy: Option<NonNull<All>>,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Slf {
  pub bod: DAGPtr,
  pub bod_ref: Option<Parents>,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Fix {
  pub bod: DAGPtr,
  pub bod_ref: Option<Parents>,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Dat {
  pub typ: DAGPtr,
  pub bod: DAGPtr,
  pub typ_ref: Option<Parents>,
  pub bod_ref: Option<Parents>,
  pub copy: Option<NonNull<Dat>>,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Cse {
  pub bod: DAGPtr,
  pub bod_ref: Option<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Let {
  pub uses: Uses,
  pub typ: DAGPtr,
  pub exp: DAGPtr,
  pub bod: NonNull<Bind>,
  pub typ_ref: Option<Parents>,
  pub exp_ref: Option<Parents>,
  pub bod_ref: Option<Parents>,
  pub copy: Option<NonNull<Let>>,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Ref {
  pub nam: Name,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Typ {
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Lit {
  pub lit: Literal,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct LTy {
  pub lty: LitType,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Opr {
  pub opr: Op,
  pub parents: Option<NonNull<Parents>>,
}

// Auxiliary allocation functions
#[inline]
pub fn alloc_val<T>(val: T) -> NonNull<T> {
  NonNull::new(Box::leak(Box::new(val))).unwrap()
}

#[inline]
pub fn alloc_bind(
  var_nam: Name,
  var_dep: u64,
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Bind> {
  unsafe {
    let bind = alloc_val(Bind {
      var: Var {
        nam: var_nam,
        dep: var_dep,
        binder: BinderPtr::Free,
        parents: None,
      },
      bod,
      bod_ref: None,
      parents,
    });
    (*bind.as_ptr()).var.binder = BinderPtr::Bind(bind);
    (*bind.as_ptr()).bod_ref = Some(DLL::singleton(ParentPtr::Bound(bind)));
    bind
  }
}

#[inline]
pub fn alloc_lam(
  uses: Uses,
  typ: DAGPtr,
  bod: NonNull<Bind>,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Lam> {
  unsafe {
    let lam = alloc_val(Lam {
      uses,
      typ,
      bod,
      copy: None,
      typ_ref: None,
      bod_ref: None,
      parents,
    });
    (*lam.as_ptr()).typ_ref = Some(DLL::singleton(ParentPtr::LamTyp(lam)));
    (*lam.as_ptr()).bod_ref = Some(DLL::singleton(ParentPtr::LamBod(lam)));
    lam
  }
}

#[inline]
pub fn alloc_all(
  uses: Uses,
  dom: DAGPtr,
  img: NonNull<Bind>,
  parents: Option<NonNull<Parents>>,
) -> NonNull<All> {
  unsafe {
    let all = alloc_val(All {
      uses,
      dom,
      img,
      copy: None,
      dom_ref: None,
      img_ref: None,
      parents,
    });
    (*all.as_ptr()).dom_ref = Some(DLL::singleton(ParentPtr::AllDom(all)));
    (*all.as_ptr()).img_ref = Some(DLL::singleton(ParentPtr::AllImg(all)));
    all
  }
}

#[inline]
pub fn alloc_slf(
  var_nam: Name,
  var_dep: u64,
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Slf> {
  unsafe {
    let slf = alloc_val(Slf {
      var: Var {
        nam: var_nam,
        dep: var_dep,
        binder: BinderPtr::Free,
        parents: None,
      },
      bod,
      bod_ref: None,
      parents,
    });
    (*slf.as_ptr()).var.binder = BinderPtr::Slf(slf);
    (*slf.as_ptr()).bod_ref = Some(DLL::singleton(ParentPtr::SlfBod(slf)));
    slf
  }
}

#[inline]
pub fn alloc_fix(
  var_nam: Name,
  var_dep: u64,
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Fix> {
  unsafe {
    let fix = alloc_val(Fix {
      var: Var {
        nam: var_nam,
        dep: var_dep,
        binder: BinderPtr::Free,
        parents: None,
      },
      bod,
      bod_ref: None,
      parents,
    });
    (*fix.as_ptr()).var.binder = BinderPtr::Fix(fix);
    (*fix.as_ptr()).bod_ref = Some(DLL::singleton(ParentPtr::FixBod(fix)));
    fix
  }
}

#[inline]
pub fn alloc_dat(
  typ: DAGPtr,
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Dat> {
  unsafe {
    let dat = alloc_val(Dat {
      typ,
      bod,
      typ_ref: None,
      bod_ref: None,
      copy: None,
      parents,
    });
    (*dat.as_ptr()).typ_ref = Some(DLL::singleton(ParentPtr::DatTyp(dat)));
    (*dat.as_ptr()).bod_ref = Some(DLL::singleton(ParentPtr::DatBod(dat)));
    dat
  }
}

#[inline]
pub fn alloc_cse(
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Cse> {
  unsafe {
    let cse = alloc_val(Cse { bod, bod_ref: None, parents });
    (*cse.as_ptr()).bod_ref = Some(DLL::singleton(ParentPtr::CseBod(cse)));
    cse
  }
}

#[inline]
pub fn alloc_app(
  uses: Uses,
  fun: DAGPtr,
  typ: DAGPtr,
  arg: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<App> {
  unsafe {
    let app = alloc_val(App {
      uses,
      fun,
      typ,
      arg,
      copy: None,
      fun_ref: None,
      typ_ref: None,
      arg_ref: None,
      parents,
    });
    (*app.as_ptr()).fun_ref = Some(DLL::singleton(ParentPtr::AppFun(app)));
    (*app.as_ptr()).typ_ref = Some(DLL::singleton(ParentPtr::AppTyp(app)));
    (*app.as_ptr()).arg_ref = Some(DLL::singleton(ParentPtr::AppArg(app)));
    app
  }
}

#[inline]
#[allow(clippy::too_many_arguments)]
pub fn alloc_let(
  uses: Uses,
  typ: DAGPtr,
  exp: DAGPtr,
  bod: NonNull<Bind>,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Let> {
  unsafe {
    let let_ = alloc_val(Let {
      uses,
      typ,
      exp,
      bod,
      copy: None,
      typ_ref: None,
      exp_ref: None,
      bod_ref: None,
      parents,
    });
    (*let_.as_ptr()).typ_ref = Some(DLL::singleton(ParentPtr::LetTyp(let_)));
    (*let_.as_ptr()).exp_ref = Some(DLL::singleton(ParentPtr::LetExp(let_)));
    (*let_.as_ptr()).bod_ref = Some(DLL::singleton(ParentPtr::LetBod(let_)));
    let_
  }
}

// Auxiliary parent functions
#[inline]
pub fn get_parents(term: DAGPtr) -> Option<NonNull<Parents>> {
  unsafe {
    match term {
      DAGPtr::Null => None,
      DAGPtr::Bind(link) => (*link.as_ptr()).parents,
      DAGPtr::Var(link) => (*link.as_ptr()).parents,
      DAGPtr::Hol(link) => (*link.as_ptr()).parents,
      DAGPtr::Lam(link) => (*link.as_ptr()).parents,
      DAGPtr::App(link) => (*link.as_ptr()).parents,
      DAGPtr::All(link) => (*link.as_ptr()).parents,
      DAGPtr::Slf(link) => (*link.as_ptr()).parents,
      DAGPtr::Fix(link) => (*link.as_ptr()).parents,
      DAGPtr::Dat(link) => (*link.as_ptr()).parents,
      DAGPtr::Cse(link) => (*link.as_ptr()).parents,
      DAGPtr::Ref(link) => (*link.as_ptr()).parents,
      DAGPtr::Let(link) => (*link.as_ptr()).parents,
      DAGPtr::Typ(link) => (*link.as_ptr()).parents,
      DAGPtr::Lit(link) => (*link.as_ptr()).parents,
      DAGPtr::LTy(link) => (*link.as_ptr()).parents,
      DAGPtr::Opr(link) => (*link.as_ptr()).parents,
    }
  }
}

#[inline]
pub fn set_parents(term: DAGPtr, pref: Option<NonNull<Parents>>) {
  unsafe {
    match term {
      DAGPtr::Null => {}
      DAGPtr::Bind(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Var(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Hol(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Lam(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::App(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::All(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Slf(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Fix(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Dat(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Cse(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Ref(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Let(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Typ(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Lit(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::LTy(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Opr(link) => (*link.as_ptr()).parents = pref,
    }
  }
}

#[inline]
pub fn install_child(parent: &mut ParentPtr, newchild: DAGPtr) {
  unsafe {
    match parent {
      ParentPtr::Bound(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::LamTyp(parent) => (*parent.as_ptr()).typ = newchild,
      ParentPtr::LamBod(parent) => match newchild {
        DAGPtr::Bind(link) => (*parent.as_ptr()).bod = link,
        _ => panic!("Cannot install a non-Bind node as Lam body"),
      },
      ParentPtr::SlfBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::FixBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::DatTyp(parent) => (*parent.as_ptr()).typ = newchild,
      ParentPtr::DatBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::CseBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::AppFun(parent) => (*parent.as_ptr()).fun = newchild,
      ParentPtr::AppTyp(parent) => (*parent.as_ptr()).typ = newchild,
      ParentPtr::AppArg(parent) => (*parent.as_ptr()).arg = newchild,
      ParentPtr::AllDom(parent) => (*parent.as_ptr()).dom = newchild,
      ParentPtr::AllImg(parent) => match newchild {
        DAGPtr::Bind(link) => (*parent.as_ptr()).img = link,
        _ => panic!("Cannot install a non-Bind node as All image"),
      },
      ParentPtr::LetTyp(parent) => (*parent.as_ptr()).typ = newchild,
      ParentPtr::LetExp(parent) => (*parent.as_ptr()).exp = newchild,
      ParentPtr::LetBod(parent) => match newchild {
        DAGPtr::Bind(link) => (*parent.as_ptr()).bod = link,
        _ => panic!("Cannot install a non-Bind node as Let body"),
      },
      ParentPtr::Root => (),
    }
  }
}

// Replace one child w/another in the tree.
pub fn replace_child(oldchild: DAGPtr, newchild: DAGPtr) {
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
pub fn add_to_parents(node: DAGPtr, plink: NonNull<Parents>) {
  let parents = get_parents(node);
  match parents {
    Some(parents) => unsafe { (*parents.as_ptr()).merge(plink) },
    None => set_parents(node, Some(plink)),
  }
}

/// Frees this pointer
/// For more readability of the code
fn free<T>(link: NonNull<T>) {
  unsafe {
    Box::from_raw(link.as_ptr());
  }
}

enum State {
  A,
  B,
  C,
  D,
}

// Free parentless nodes.
pub fn free_dead_node(node: DAGPtr) {
  unsafe {
    let mut stack = vec![(node, State::A)];
    while let Some((node, state)) = stack.pop() {
      match node {
        DAGPtr::Null => {}
        DAGPtr::Bind(link) => match state {
          State::A => {
            let Bind { bod, bod_ref, .. } = &link.as_ref();
            let new_bod_parents = bod_ref.as_ref().unwrap().unlink_node();
            set_parents(*bod, new_bod_parents);
            if new_bod_parents.is_none() {
              stack.push((node, State::B));
              stack.push((*bod, State::A));
            }
            else {
              stack.push((node, State::B));
            }
          }
          _ => {
            free(link);
          }
        },
        DAGPtr::Slf(mut link) => match state {
          State::A => {
            let Slf { bod, bod_ref, .. } = &link.as_mut();
            let new_bod_parents = bod_ref.as_ref().unwrap().unlink_node();
            set_parents(*bod, new_bod_parents);
            if new_bod_parents.is_none() {
              stack.push((node, State::B));
              stack.push((*bod, State::A));
            }
            else {
              stack.push((node, State::B));
            }
          }
          _ => {
            free(link);
          }
        },
        DAGPtr::Fix(mut link) => match state {
          State::A => {
            let Fix { bod, bod_ref, .. } = &link.as_mut();
            let new_bod_parents = bod_ref.as_ref().unwrap().unlink_node();
            set_parents(*bod, new_bod_parents);
            if new_bod_parents.is_none() {
              stack.push((node, State::B));
              stack.push((*bod, State::A));
            }
            else {
              stack.push((node, State::B));
            }
          }
          _ => {
            free(link);
          }
        },
        DAGPtr::Cse(link) => match state {
          State::A => {
            let Cse { bod, bod_ref, .. } = link.as_ref();
            let new_bod_parents = bod_ref.as_ref().unwrap().unlink_node();
            set_parents(*bod, new_bod_parents);
            if new_bod_parents.is_none() {
              stack.push((node, State::B));
              stack.push((*bod, State::A));
            }
            else {
              stack.push((node, State::B));
            }
          }
          _ => {
            free(link);
          }
        },
        DAGPtr::Dat(link) => match state {
          State::A => {
            let Dat { bod, bod_ref, .. } = &link.as_ref();
            let new_bod_parents = bod_ref.as_ref().unwrap().unlink_node();
            set_parents(*bod, new_bod_parents);
            if new_bod_parents.is_none() {
              stack.push((node, State::B));
              stack.push((*bod, State::A));
            }
            else {
              stack.push((node, State::B));
            }
          }
          _ => {
            free(link);
          }
        },
        DAGPtr::Lam(link) => {
          let Lam { typ, bod, typ_ref, bod_ref, .. } = link.as_ref();
          match state {
            State::A => {
              let new_typ_parents = typ_ref.as_ref().unwrap().unlink_node();
              set_parents(*typ, new_typ_parents);
              if new_typ_parents.is_none() {
                stack.push((node, State::B));
                stack.push((*typ, State::A));
              }
              else {
                stack.push((node, State::B));
              }
            }
            State::B => {
              let bod = DAGPtr::Bind(*bod);
              let new_bod_parents = bod_ref.as_ref().unwrap().unlink_node();
              set_parents(bod, new_bod_parents);
              if new_bod_parents.is_none() {
                stack.push((node, State::C));
                stack.push((bod, State::A));
              }
              else {
                stack.push((node, State::C));
              }
            }
            _ => {
              free(link);
            }
          }
        }
        DAGPtr::All(link) => {
          let All { dom, img, dom_ref, img_ref, .. } = link.as_ref();
          match state {
            State::A => {
              let new_dom_parents = dom_ref.as_ref().unwrap().unlink_node();
              set_parents(*dom, new_dom_parents);
              if new_dom_parents.is_none() {
                stack.push((node, State::B));
                stack.push((*dom, State::A));
              }
              else {
                stack.push((node, State::B));
              }
            }
            State::B => {
              let img = DAGPtr::Bind(*img);
              let new_img_parents = img_ref.as_ref().unwrap().unlink_node();
              set_parents(img, new_img_parents);
              if new_img_parents.is_none() {
                stack.push((node, State::C));
                stack.push((img, State::A));
              }
              else {
                stack.push((node, State::C));
              }
            }
            _ => {
              free(link);
            }
          }
        }
        DAGPtr::App(link) => {
          let App { fun, typ, arg, fun_ref, typ_ref, arg_ref, .. } =
            link.as_ref();
          match state {
            State::A => {
              let new_fun_parents = fun_ref.as_ref().unwrap().unlink_node();
              set_parents(*fun, new_fun_parents);
              if new_fun_parents.is_none() {
                stack.push((node, State::B));
                stack.push((*fun, State::A));
              }
              else {
                stack.push((node, State::B));
              }
            }
            State::B => {
              let new_typ_parents = typ_ref.as_ref().unwrap().unlink_node();
              set_parents(*typ, new_typ_parents);
              if new_typ_parents.is_none() {
                stack.push((node, State::C));
                stack.push((*typ, State::A));
              }
              else {
                stack.push((node, State::C));
              }
            }
            State::C => {
              let new_arg_parents = arg_ref.as_ref().unwrap().unlink_node();
              set_parents(*arg, new_arg_parents);
              if new_arg_parents.is_none() {
                stack.push((node, State::C));
                stack.push((*arg, State::A));
              }
              else {
                stack.push((node, State::C));
              }
            }
            _ => {
              free(link);
            }
          }
        }
        DAGPtr::Let(link) => {
          let Let { exp, typ, exp_ref, typ_ref, bod, bod_ref, .. } =
            link.as_ref();
          match state {
            State::A => {
              let new_exp_parents = exp_ref.as_ref().unwrap().unlink_node();
              set_parents(*exp, new_exp_parents);
              if new_exp_parents.is_none() {
                stack.push((node, State::B));
                stack.push((*exp, State::A));
              }
              else {
                stack.push((node, State::B));
              }
            }
            State::B => {
              let new_typ_parents = typ_ref.as_ref().unwrap().unlink_node();
              set_parents(*typ, new_typ_parents);
              if new_typ_parents.is_none() {
                stack.push((node, State::C));
                stack.push((*typ, State::A));
              }
              else {
                stack.push((node, State::C));
              }
            }
            State::C => {
              let bod = DAGPtr::Bind(*bod);
              let new_bod_parents = bod_ref.as_ref().unwrap().unlink_node();
              set_parents(bod, new_bod_parents);
              if new_bod_parents.is_none() {
                stack.push((node, State::D));
                stack.push((bod, State::A));
              }
              else {
                stack.push((node, State::D));
              }
            }
            _ => {
              free(link);
            }
          }
        }
        DAGPtr::Var(link) => {
          let Var { binder, .. } = link.as_ref();
          // only free Free variables, bound variables are freed with their
          // binder
          if let BinderPtr::Free = binder {
            free(link);
          }
        }
        DAGPtr::Hol(link) => {
          free(link);
        }
        DAGPtr::Ref(link) => {
          free(link);
        }
        DAGPtr::Typ(link) => {
          free(link);
        }
        DAGPtr::Lit(link) => {
          free(link);
        }
        DAGPtr::LTy(link) => {
          free(link);
        }
        DAGPtr::Opr(link) => {
          free(link);
        }
      }
    }
  }
}
enum Frame<A, B> {
  Enter(A),
  Return(B),
}

impl DAG {
  pub fn new(head: DAGPtr) -> DAG { DAG { head } }

  pub fn free(self) {
    match get_parents(self.head) {
      None => (),
      Some(pref) => unsafe {
        Box::from_raw(pref.as_ptr());
        set_parents(self.head, None);
      },
    }
    free_dead_node(self.head)
  }

  pub fn dag_ptr_to_term(
    node: &DAGPtr,
    map: &mut BTreeMap<*mut Var, u64>,
    depth: u64,
  ) -> Term {
    let mut stack = vec![Frame::Enter((node, map.clone(), depth))];
    let mut ret_stack = vec![];
    while let Some(either) = stack.pop() {
      match either {
        Frame::Enter((node, mut map, depth)) => match node {
          DAGPtr::Null => panic!("cannot convert a null DagPtr to a Term"),
          DAGPtr::Bind(..) => panic!("malformed DAG with raw Bind node"),
          DAGPtr::Var(link) => {
            let Var { nam, dep: var_depth, .. } = unsafe { link.as_ref() };
            if let Some(level) = map.get(&link.as_ptr()) {
              ret_stack.push(Term::Var(
                Pos::None,
                nam.clone(),
                depth - level - 1,
              ));
            }
            else {
              ret_stack.push(Term::Var(Pos::None, nam.clone(), *var_depth));
            }
          }
          DAGPtr::Typ(_) => ret_stack.push(Term::Typ(Pos::None)),
          DAGPtr::LTy(link) => {
            let LTy { lty, .. } = unsafe { link.as_ref() };
            ret_stack.push(Term::LTy(Pos::None, *lty));
          }
          DAGPtr::Lit(link) => {
            let Lit { lit, .. } = unsafe { link.as_ref() };
            ret_stack.push(Term::Lit(Pos::None, lit.clone()));
          }
          DAGPtr::Hol(link) => {
            let Hol { nam, .. } = unsafe { link.as_ref() };
            ret_stack.push(Term::Hol(Pos::None, nam.clone()));
          }
          DAGPtr::Opr(link) => {
            let Opr { opr, .. } = unsafe { link.as_ref() };
            ret_stack.push(Term::Opr(Pos::None, *opr));
          }
          DAGPtr::Ref(link) => {
            let Ref { nam, .. } = unsafe { link.as_ref() };
            ret_stack.push(Term::Ref(Pos::None, nam.clone()));
          }
          DAGPtr::Lam(link) => {
            let Lam { uses, typ, bod: bind_link, .. } =
              unsafe { &mut *link.as_ptr() };
            let Bind { var, bod, .. } = unsafe { &mut *bind_link.as_ptr() };
            let nam = var.nam.clone();
            let typ_map = map.clone();
            map.insert(var, depth);
            stack.push(Frame::Return(Term::Lam(
              Pos::None,
              uses.clone(),
              nam,
              Box::new(Term::Typ(Pos::None)),
              Box::new(Term::Typ(Pos::None)),
            )));
            stack.push(Frame::Enter((typ, typ_map, depth)));
            stack.push(Frame::Enter((bod, map, depth + 1)));
          }
          DAGPtr::Slf(link) => {
            let Slf { var, bod, .. } = unsafe { &mut *link.as_ptr() };
            let nam = var.nam.clone();
            map.insert(var, depth);
            stack.push(Frame::Return(Term::Slf(
              Pos::None,
              nam,
              Box::new(Term::Typ(Pos::None)),
            )));
            stack.push(Frame::Enter((bod, map, depth + 1)));
          }
          DAGPtr::Fix(_) => {
            panic!("Fix conversion TODO")
          }
          DAGPtr::Cse(link) => {
            let Cse { bod, .. } = unsafe { link.as_ref() };
            stack.push(Frame::Return(Term::Cse(
              Pos::None,
              Box::new(Term::Typ(Pos::None)),
            )));
            stack.push(Frame::Enter((bod, map, depth)));
          }
          DAGPtr::Dat(link) => {
            let Dat { typ, bod, .. } = unsafe { link.as_ref() };
            stack.push(Frame::Return(Term::Dat(
              Pos::None,
              Box::new(Term::Typ(Pos::None)),
              Box::new(Term::Typ(Pos::None)),
            )));
            stack.push(Frame::Enter((typ, map.clone(), depth)));
            stack.push(Frame::Enter((bod, map, depth)));
          }
          DAGPtr::App(link) => {
            let App { uses, fun, typ, arg, .. } = unsafe { link.as_ref() };
            stack.push(Frame::Return(Term::App(
              Pos::None,
              uses.clone(),
              Box::new(Term::Typ(Pos::None)),
              Box::new(Term::Typ(Pos::None)),
              Box::new(Term::Typ(Pos::None)),
            )));
            stack.push(Frame::Enter((fun, map.clone(), depth)));
            stack.push(Frame::Enter((typ, map.clone(), depth)));
            stack.push(Frame::Enter((arg, map, depth)));
          }
          DAGPtr::All(link) => {
            let All { uses, dom, img: bind_link, .. } =
              unsafe { &mut *link.as_ptr() };
            let Bind { var, bod: img, .. } =
              unsafe { &mut *bind_link.as_ptr() };
            let nam = var.nam.clone();
            let dom_map = map.clone();
            map.insert(var, depth);
            stack.push(Frame::Return(Term::All(
              Pos::None,
              uses.clone(),
              nam,
              Box::new(Term::Typ(Pos::None)),
              Box::new(Term::Typ(Pos::None)),
            )));
            stack.push(Frame::Enter((dom, dom_map, depth)));
            stack.push(Frame::Enter((img, map, depth + 1)));
          }
          DAGPtr::Let(link) => {
            let Let { uses, typ, exp, bod: bind_link, .. } =
              unsafe { &mut *link.as_ptr() };
            let Bind { var, bod, .. } = unsafe { &mut *bind_link.as_ptr() };
            let nam = var.nam.clone();
            let typ_map = map.clone();
            let mut exp_map = map.clone();
            let mut exp_depth = depth;
            let (rec, exp) = match exp {
              DAGPtr::Fix(link) => unsafe {
                let Fix { var, bod, .. } = &mut *link.as_ptr();
                exp_map.insert(var, depth);
                exp_depth += 1;
                (true, bod)
              },
              _ => (false, exp),
            };
            map.insert(var, depth);
            stack.push(Frame::Return(Term::Let(
              Pos::None,
              rec,
              uses.clone(),
              nam,
              Box::new(Term::Typ(Pos::None)),
              Box::new(Term::Typ(Pos::None)),
              Box::new(Term::Typ(Pos::None)),
            )));
            stack.push(Frame::Enter((typ, typ_map, depth)));
            stack.push(Frame::Enter((exp, exp_map, exp_depth)));
            stack.push(Frame::Enter((bod, map, depth + 1)));
          }
        },
        Frame::Return(mut term) => {
          match &mut term {
            Term::Lam(_, _, _, typ, bod) => {
              *typ = Box::new(ret_stack.pop().unwrap());
              *bod = Box::new(ret_stack.pop().unwrap());
            }
            Term::App(_, _, fun, typ, arg) => {
              *fun = Box::new(ret_stack.pop().unwrap());
              *typ = Box::new(ret_stack.pop().unwrap());
              *arg = Box::new(ret_stack.pop().unwrap());
            }
            Term::All(_, _, _, dom, img) => {
              *dom = Box::new(ret_stack.pop().unwrap());
              *img = Box::new(ret_stack.pop().unwrap());
            }
            Term::Slf(_, _, bod) => {
              *bod = Box::new(ret_stack.pop().unwrap());
            }
            Term::Dat(_, typ, bod) => {
              *typ = Box::new(ret_stack.pop().unwrap());
              *bod = Box::new(ret_stack.pop().unwrap());
            }
            Term::Cse(_, bod) => {
              let new_bod = Box::new(ret_stack.pop().unwrap());
              *bod = new_bod;
            }
            Term::Let(_, _, _, _, typ, exp, bod) => {
              *typ = Box::new(ret_stack.pop().unwrap());
              *exp = Box::new(ret_stack.pop().unwrap());
              *bod = Box::new(ret_stack.pop().unwrap());
            }
            _ => (),
          }
          ret_stack.push(term);
        }
      }
    }
    ret_stack.pop().unwrap()
  }

  pub fn to_term(&self) -> Term {
    let mut map = BTreeMap::new();
    DAG::dag_ptr_to_term(&self.head, &mut map, 0)
  }

  pub fn from_term(tree: &Term) -> Self {
    let root = alloc_val(DLL::singleton(ParentPtr::Root));
    DAG::new(DAG::from_term_inner(tree, 0, BTreeMap::new(), Some(root), None))
  }

  // pub fn from_def(def: &Def, name: Name) -> Self {
  //  let root = alloc_val(DLL::singleton(ParentPtr::Root));
  //  let (d, _, a) = def.embed();
  //  let def_cid = d.cid();
  //  let ast_cid = a.cid();
  //  DAG::new(DAG::from_term_inner(
  //    &def.term,
  //    0,
  //    BTreeMap::new(),
  //    Some(root),
  //    Some((name, def_cid, ast_cid)),
  //  ))
  //}

  // pub fn from_ref(
  //  def: &Def,
  //  name: Name,
  //  def_cid: Cid,
  //  ast_cid: Cid,
  //  parents: Option<NonNull<Parents>>,
  //) -> DAGPtr {
  //  DAG::from_term_inner(
  //    &def.term,
  //    0,
  //    BTreeMap::new(),
  //    parents,
  //    Some((name, def_cid, ast_cid)),
  //  )
  //}

  pub fn from_term_inner(
    tree: &Term,
    depth: u64,
    mut ctx: BTreeMap<usize, DAGPtr>,
    parents: Option<NonNull<Parents>>,
    rec_ref: Option<(Name, Cid, Cid)>,
  ) -> DAGPtr {
    match tree {
      Term::Var(_, name, idx) => {
        let dep = (depth - 1 - *idx) as usize;
        match ctx.get(&dep) {
          Some(val) => {
            if let Some(parents) = parents {
              DLL::concat(parents, get_parents(*val));
              set_parents(*val, Some(parents));
            }
            *val
          }
          None => {
            if depth < 1 + idx {
              panic!("Negative index found.")
            }
            let var = alloc_val(Var {
              nam: name.clone(),
              dep: depth - 1 - idx,
              binder: BinderPtr::Free,
              parents,
            });
            DAGPtr::Var(var)
          }
        }
      }
      Term::Hol(_, nam) => {
        DAGPtr::Hol(alloc_val(Hol { nam: nam.clone(), parents }))
      }
      Term::Typ(_) => DAGPtr::Typ(alloc_val(Typ { parents })),
      Term::LTy(_, lty) => DAGPtr::LTy(alloc_val(LTy { lty: *lty, parents })),
      Term::Lit(_, lit) => {
        DAGPtr::Lit(alloc_val(Lit { lit: lit.clone(), parents }))
      }
      Term::Opr(_, opr) => DAGPtr::Opr(alloc_val(Opr { opr: *opr, parents })),
      Term::Ref(_, nam) => {
        DAGPtr::Ref(alloc_val(Ref { nam: nam.clone(), parents }))
      }
      Term::Lam(_, uses, nam, typ, bod) => unsafe {
        let (typ, bod) = (&**typ, &**bod);
        let bind = alloc_bind(nam.clone(), 0, DAGPtr::Null, None);
        let lam = alloc_lam(uses.clone(), DAGPtr::Null, bind, parents);
        let Lam { typ_ref, bod_ref, .. } = &mut *lam.as_ptr();
        (*bind.as_ptr()).parents = NonNull::new(bod_ref.as_mut().unwrap());
        let Bind { var, bod_ref, .. } = &mut *bind.as_ptr();
        let mut bod_ctx = ctx.clone();
        let typ = DAG::from_term_inner(
          typ,
          depth,
          ctx,
          NonNull::new(typ_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        bod_ctx.insert(depth as usize, DAGPtr::Var(NonNull::new(var).unwrap()));
        let bod = DAG::from_term_inner(
          bod,
          depth + 1,
          bod_ctx,
          NonNull::new(bod_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        (*lam.as_ptr()).typ = typ;
        (*bind.as_ptr()).bod = bod;
        DAGPtr::Lam(lam)
      },
      Term::All(_, uses, nam, dom, img) => unsafe {
        let (dom, img) = (&**dom, &**img);
        let bind = alloc_bind(nam.clone(), 0, DAGPtr::Null, None);
        let all = alloc_all(uses.clone(), DAGPtr::Null, bind, parents);
        let All { dom_ref, img_ref, .. } = &mut *all.as_ptr();
        (*bind.as_ptr()).parents = NonNull::new(img_ref.as_mut().unwrap());
        let Bind { var, bod_ref, .. } = &mut *bind.as_ptr();
        let mut img_ctx = ctx.clone();
        let dom = DAG::from_term_inner(
          dom,
          depth,
          ctx,
          NonNull::new(dom_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        img_ctx.insert(depth as usize, DAGPtr::Var(NonNull::new(var).unwrap()));
        let img = DAG::from_term_inner(
          img,
          depth + 1,
          img_ctx,
          NonNull::new(bod_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        (*all.as_ptr()).dom = dom;
        (*bind.as_ptr()).bod = img;
        DAGPtr::All(all)
      },
      Term::Slf(_, nam, bod) => unsafe {
        let slf = alloc_slf(nam.clone(), 0, DAGPtr::Null, parents);
        let Slf { var, bod_ref, .. } = &mut *slf.as_ptr();
        ctx.insert(depth as usize, DAGPtr::Var(NonNull::new(var).unwrap()));
        let bod = DAG::from_term_inner(
          &**bod,
          depth + 1,
          ctx,
          NonNull::new(bod_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        (*slf.as_ptr()).bod = bod;
        DAGPtr::Slf(slf)
      },
      Term::Dat(_, typ, bod) => unsafe {
        let (typ, bod) = (&**typ, &**bod);
        let dat = alloc_dat(DAGPtr::Null, DAGPtr::Null, parents);
        let Dat { typ_ref, bod_ref, .. } = &mut *dat.as_ptr();
        let typ = DAG::from_term_inner(
          typ,
          depth,
          ctx.clone(),
          NonNull::new(typ_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        let bod = DAG::from_term_inner(
          bod,
          depth,
          ctx,
          NonNull::new(bod_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        (*dat.as_ptr()).typ = typ;
        (*dat.as_ptr()).bod = bod;
        DAGPtr::Dat(dat)
      },
      Term::Cse(_, bod) => unsafe {
        let cse = alloc_cse(DAGPtr::Null, parents);
        let Cse { bod_ref, .. } = &mut *cse.as_ptr();
        let bod = DAG::from_term_inner(
          &**bod,
          depth,
          ctx,
          NonNull::new(bod_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        (*cse.as_ptr()).bod = bod;
        DAGPtr::Cse(cse)
      },
      Term::App(_, uses, fun, typ, arg) => unsafe {
        let (fun, typ, arg) = (&*fun, &*typ, &*arg);
        let app = alloc_app(
          uses.clone(),
          DAGPtr::Null,
          DAGPtr::Null,
          DAGPtr::Null,
          parents,
        );
        let App { fun_ref, typ_ref, arg_ref, .. } = &mut *app.as_ptr();
        let fun = DAG::from_term_inner(
          fun,
          depth,
          ctx.clone(),
          NonNull::new(fun_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        let typ = DAG::from_term_inner(
          typ,
          depth,
          ctx.clone(),
          NonNull::new(typ_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        let arg = DAG::from_term_inner(
          arg,
          depth,
          ctx,
          NonNull::new(arg_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        (*app.as_ptr()).fun = fun;
        (*app.as_ptr()).typ = typ;
        (*app.as_ptr()).arg = arg;
        DAGPtr::App(app)
      },
      Term::Let(_, rec, uses, nam, typ, exp, bod) => unsafe {
        let (typ, exp, bod) = (&*typ, &*exp, &*bod);
        // Allocates the `Let` node and a `Bind` node for `bod`
        let bind = alloc_bind(nam.clone(), 0, DAGPtr::Null, None);
        let let_ =
          alloc_let(uses.clone(), DAGPtr::Null, DAGPtr::Null, bind, parents);
        let Let { typ_ref, exp_ref, bod_ref, .. } = &mut *let_.as_ptr();
        (*bind.as_ptr()).parents = NonNull::new(bod_ref.as_mut().unwrap());
        let Bind { var, bod_ref, .. } = &mut *bind.as_ptr();
        let typ_ctx = ctx.clone();
        let mut bod_ctx = ctx.clone();
        bod_ctx.insert(depth as usize, DAGPtr::Var(NonNull::new(var).unwrap()));
        // Convert `typ` and `bod` to DAG and add it to the newly created `Let` node
        let typ = DAG::from_term_inner(
          typ,
          depth,
          typ_ctx,
          NonNull::new(typ_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        let bod = DAG::from_term_inner(
          bod,
          depth + 1,
          bod_ctx,
          NonNull::new(bod_ref.as_mut().unwrap()),
          rec_ref.clone(),
        );
        (*let_.as_ptr()).typ = typ;
        (*let_.as_ptr()).bod = bind;
        (*bind.as_ptr()).bod = bod;
        // Do the same for `exp`, but set a `Fix` before if this is a recursive `Let`
        if *rec {
          let fix = alloc_fix(
            nam.clone(),
            0,
            DAGPtr::Null,
            NonNull::new(exp_ref.as_mut().unwrap()),
          );
          let Fix { var: fix_var, bod_ref: fix_bod_ref, .. } =
            &mut *fix.as_ptr();
          ctx.insert(
            depth as usize,
            DAGPtr::Var(NonNull::new(fix_var).unwrap()),
          );
          let exp = DAG::from_term_inner(
            exp,
            depth + 1,
            ctx,
            NonNull::new(fix_bod_ref.as_mut().unwrap()),
            rec_ref.clone(),
          );
          (*let_.as_ptr()).exp = DAGPtr::Fix(fix);
          (*fix.as_ptr()).bod = exp;
        }
        else {
          let exp = DAG::from_term_inner(
            exp,
            depth,
            ctx,
            NonNull::new(exp_ref.as_mut().unwrap()),
            rec_ref.clone(),
          );
          (*let_.as_ptr()).exp = exp;
        }
        DAGPtr::Let(let_)
      },
    }
  }

  pub fn from_subdag(
    node: DAGPtr,
    map: &mut BTreeMap<DAGPtr, DAGPtr>,
    parents: Option<NonNull<Parents>>,
  ) -> DAGPtr {
    let mut res = DAGPtr::Null;
    let mut stack = vec![(node, &mut res as *mut DAGPtr, parents)];
    while let Some((node, res_ptr, parents)) = stack.pop() {
      // If the node is in the hash map then it was already copied,
      // so we update the parent list and return the copy
      if let Some(copy) = map.get(&node) {
        if let Some(parents) = parents {
          DLL::concat(parents, get_parents(*copy));
          set_parents(*copy, Some(parents));
        }
        unsafe { *res_ptr = *copy };
        continue;
      }

      // Otherwise create a new DAG node and add it to the map
      match node {
        DAGPtr::Null => {}
        DAGPtr::Bind(link) => unsafe {
          let Bind { var, bod, .. } = &mut *link.as_ptr();
          let bind =
            alloc_bind(var.nam.clone(), var.dep, DAGPtr::Null, parents);
          let Bind { var: new_var, bod: new_bod, bod_ref, .. } =
            &mut *bind.as_ptr();
          map.insert(
            DAGPtr::Var(NonNull::new(var).unwrap()),
            DAGPtr::Var(NonNull::new(new_var).unwrap()),
          );
          let new_node = DAGPtr::Bind(bind);
          map.insert(node, new_node);
          *res_ptr = new_node;
          stack.push((*bod, new_bod, NonNull::new(bod_ref.as_mut().unwrap())));
        },
        DAGPtr::Var(link) => unsafe {
          let Var { nam, dep, .. } = &*link.as_ptr();
          let var = alloc_val(Var {
            nam: nam.clone(),
            dep: *dep,
            binder: BinderPtr::Free,
            parents,
          });
          let new_node = DAGPtr::Var(var);
          map.insert(node, new_node);
          *res_ptr = new_node;
        },
        DAGPtr::Ref(link) => unsafe {
          let Ref { nam, .. } = &*link.as_ptr();
          let ref_ = alloc_val(Ref { nam: nam.clone(), parents });
          let new_node = DAGPtr::Ref(ref_);
          map.insert(node, new_node);
          *res_ptr = new_node;
        },
        DAGPtr::Hol(link) => unsafe {
          let Hol { nam, .. } = &*link.as_ptr();
          let hol = alloc_val(Hol { nam: nam.clone(), parents });
          let new_node = DAGPtr::Hol(hol);
          map.insert(node, new_node);
          *res_ptr = new_node;
        },
        DAGPtr::Lit(link) => unsafe {
          let Lit { lit, .. } = &*link.as_ptr();
          let lit = alloc_val(Lit { lit: lit.clone(), parents });
          let new_node = DAGPtr::Lit(lit);
          map.insert(node, new_node);
          *res_ptr = new_node;
        },
        DAGPtr::LTy(link) => unsafe {
          let LTy { lty, .. } = *link.as_ptr();
          let lty = alloc_val(LTy { lty, parents });
          let new_node = DAGPtr::LTy(lty);
          map.insert(node, new_node);
          *res_ptr = new_node;
        },
        DAGPtr::Opr(link) => unsafe {
          let Opr { opr, .. } = *link.as_ptr();
          let opr = alloc_val(Opr { opr, parents });
          let new_node = DAGPtr::Opr(opr);
          map.insert(node, new_node);
          *res_ptr = new_node;
        },
        DAGPtr::Typ(_) => {
          let typ = alloc_val(Typ { parents });
          let new_node = DAGPtr::Typ(typ);
          map.insert(node, new_node);
          unsafe { *res_ptr = new_node };
        }
        DAGPtr::Slf(link) => unsafe {
          let Slf { var, bod, .. } = &mut *link.as_ptr();
          let slf = alloc_slf(var.nam.clone(), var.dep, DAGPtr::Null, parents);
          let Slf { var: new_var, bod: new_bod, bod_ref, .. } =
            &mut *slf.as_ptr();
          map.insert(
            DAGPtr::Var(NonNull::new(var).unwrap()),
            DAGPtr::Var(NonNull::new(new_var).unwrap()),
          );
          let new_node = DAGPtr::Slf(slf);
          map.insert(node, new_node);
          *res_ptr = new_node;
          stack.push((*bod, new_bod, NonNull::new(bod_ref.as_mut().unwrap())));
        },
        DAGPtr::Fix(link) => unsafe {
          let Fix { var, bod, .. } = &mut *link.as_ptr();
          let fix = alloc_fix(var.nam.clone(), var.dep, DAGPtr::Null, parents);
          let Fix { var: new_var, bod: new_bod, bod_ref, .. } =
            &mut *fix.as_ptr();
          map.insert(
            DAGPtr::Var(NonNull::new(var).unwrap()),
            DAGPtr::Var(NonNull::new(new_var).unwrap()),
          );
          let new_node = DAGPtr::Fix(fix);
          map.insert(node, new_node);
          *res_ptr = new_node;
          stack.push((*bod, new_bod, NonNull::new(bod_ref.as_mut().unwrap())));
        },
        DAGPtr::Cse(link) => unsafe {
          let Cse { bod, .. } = &mut *link.as_ptr();
          let cse = alloc_cse(DAGPtr::Null, parents);
          let Cse { bod: new_bod, bod_ref, .. } = &mut *cse.as_ptr();
          let new_node = DAGPtr::Cse(cse);
          map.insert(node, new_node);
          *res_ptr = new_node;
          stack.push((*bod, new_bod, NonNull::new(bod_ref.as_mut().unwrap())));
        },
        DAGPtr::Dat(link) => unsafe {
          let Dat { typ, bod, .. } = &mut *link.as_ptr();
          let dat = alloc_dat(DAGPtr::Null, DAGPtr::Null, parents);
          let Dat { typ: new_typ, bod: new_bod, typ_ref, bod_ref, .. } =
            &mut *dat.as_ptr();
          let new_node = DAGPtr::Dat(dat);
          map.insert(node, new_node);
          *res_ptr = new_node;
          stack.push((*typ, new_typ, NonNull::new(typ_ref.as_mut().unwrap())));
          stack.push((*bod, new_bod, NonNull::new(bod_ref.as_mut().unwrap())));
        },
        DAGPtr::App(link) => unsafe {
          let App { uses, fun, typ, arg, .. } = &mut *link.as_ptr();
          let app = alloc_app(
            uses.clone(),
            DAGPtr::Null,
            DAGPtr::Null,
            DAGPtr::Null,
            parents,
          );
          let App {
            fun: new_fun,
            typ: new_typ,
            arg: new_arg,
            fun_ref,
            typ_ref,
            arg_ref,
            ..
          } = &mut *app.as_ptr();
          let new_node = DAGPtr::App(app);
          map.insert(node, new_node);
          *res_ptr = new_node;
          stack.push((*fun, new_fun, NonNull::new(fun_ref.as_mut().unwrap())));
          stack.push((*typ, new_typ, NonNull::new(typ_ref.as_mut().unwrap())));
          stack.push((*arg, new_arg, NonNull::new(arg_ref.as_mut().unwrap())));
        },
        DAGPtr::Lam(link) => unsafe {
          let bod = (*link.as_ptr()).bod;
          let old_bind = DAGPtr::Bind(bod);
          let Bind { var, bod, .. } = &mut *bod.as_ptr();
          let bind =
            alloc_bind(var.nam.clone(), var.dep, DAGPtr::Null, parents);
          let Bind { var: new_var, bod: new_bod, bod_ref, .. } =
            &mut *bind.as_ptr();
          map.insert(
            DAGPtr::Var(NonNull::new(var).unwrap()),
            DAGPtr::Var(NonNull::new(new_var).unwrap()),
          );
          let new_bind = DAGPtr::Bind(bind);
          map.insert(old_bind, new_bind);
          stack.push((*bod, new_bod, NonNull::new(bod_ref.as_mut().unwrap())));

          let Lam { uses, typ, .. } = &mut *link.as_ptr();
          let lam = alloc_lam(uses.clone(), DAGPtr::Null, bind, parents);
          let Lam { typ: new_typ, typ_ref, .. } = &mut *lam.as_ptr();
          let new_node = DAGPtr::Lam(lam);
          map.insert(node, new_node);
          *res_ptr = new_node;
          stack.push((*typ, new_typ, NonNull::new(typ_ref.as_mut().unwrap())));
        },
        DAGPtr::All(link) => unsafe {
          let img = (*link.as_ptr()).img;
          let old_bind = DAGPtr::Bind(img);
          let Bind { var, bod, .. } = &mut *img.as_ptr();
          let bind =
            alloc_bind(var.nam.clone(), var.dep, DAGPtr::Null, parents);
          let Bind { var: new_var, bod: new_bod, bod_ref, .. } =
            &mut *bind.as_ptr();
          map.insert(
            DAGPtr::Var(NonNull::new(var).unwrap()),
            DAGPtr::Var(NonNull::new(new_var).unwrap()),
          );
          let new_bind = DAGPtr::Bind(bind);
          map.insert(old_bind, new_bind);
          stack.push((*bod, new_bod, NonNull::new(bod_ref.as_mut().unwrap())));

          let All { uses, dom, .. } = &mut *link.as_ptr();
          let all = alloc_all(uses.clone(), DAGPtr::Null, bind, parents);
          let All { dom: new_dom, dom_ref, .. } = &mut *all.as_ptr();
          let new_node = DAGPtr::All(all);
          map.insert(node, new_node);
          *res_ptr = new_node;
          stack.push((*dom, new_dom, NonNull::new(dom_ref.as_mut().unwrap())));
        },
        DAGPtr::Let(link) => unsafe {
          let bod = (*link.as_ptr()).bod;
          let old_bind = DAGPtr::Bind(bod);
          let Bind { var, bod, .. } = &mut *bod.as_ptr();
          let bind =
            alloc_bind(var.nam.clone(), var.dep, DAGPtr::Null, parents);
          let Bind { var: new_var, bod: new_bod, bod_ref, .. } =
            &mut *bind.as_ptr();
          map.insert(
            DAGPtr::Var(NonNull::new(var).unwrap()),
            DAGPtr::Var(NonNull::new(new_var).unwrap()),
          );
          let new_bind = DAGPtr::Bind(bind);
          map.insert(old_bind, new_bind);
          stack.push((*bod, new_bod, NonNull::new(bod_ref.as_mut().unwrap())));

          let Let { uses, typ, exp, .. } = &mut *link.as_ptr();
          let let_ =
            alloc_let(uses.clone(), DAGPtr::Null, DAGPtr::Null, bind, parents);
          let Let { typ: new_typ, exp: new_exp, typ_ref, exp_ref, .. } =
            &mut *let_.as_ptr();
          let new_node = DAGPtr::Let(let_);
          map.insert(node, new_node);
          *res_ptr = new_node;
          stack.push((*typ, new_typ, NonNull::new(typ_ref.as_mut().unwrap())));
          stack.push((*exp, new_exp, NonNull::new(exp_ref.as_mut().unwrap())));
        },
      };
    }
    res
  }

  // Substitution of free variable
  pub fn subst(&mut self, idx: u64, val: DAGPtr) {
    pub fn go(node: DAGPtr, idx: u64, val: DAGPtr) {
      match node {
        DAGPtr::Var(link) => {
          let Var { dep, binder, .. } = unsafe { &*link.as_ptr() };
          if *dep == idx {
            match binder {
              BinderPtr::Free => (),
              _ => panic!("Variable not free"),
            }
            replace_child(node, val);
            free_dead_node(node);
          }
        }
        DAGPtr::Bind(link) => {
          let Bind { bod, .. } = unsafe { &*link.as_ptr() };
          go(*bod, idx, val)
        }
        DAGPtr::Slf(link) => {
          let Slf { bod, .. } = unsafe { &*link.as_ptr() };
          go(*bod, idx, val)
        }
        DAGPtr::Fix(link) => {
          let Fix { bod, .. } = unsafe { &*link.as_ptr() };
          go(*bod, idx, val)
        }
        DAGPtr::Cse(link) => {
          let Cse { bod, .. } = unsafe { &*link.as_ptr() };
          go(*bod, idx, val)
        }
        DAGPtr::Dat(link) => {
          let Dat { bod, .. } = unsafe { &*link.as_ptr() };
          go(*bod, idx, val)
        }
        DAGPtr::App(link) => {
          let App { fun, arg, .. } = unsafe { &*link.as_ptr() };
          go(*fun, idx, val);
          go(*arg, idx, val)
        }
        DAGPtr::Lam(link) => {
          let Lam { typ, bod, .. } = unsafe { &*link.as_ptr() };
          go(*typ, idx, val);
          go(DAGPtr::Bind(*bod), idx, val)
        }
        DAGPtr::All(link) => {
          let All { dom, img, .. } = unsafe { &*link.as_ptr() };
          go(*dom, idx, val);
          go(DAGPtr::Bind(*img), idx, val)
        }
        DAGPtr::Let(link) => {
          let Let { typ, exp, bod, .. } = unsafe { &*link.as_ptr() };
          go(*typ, idx, val);
          go(*exp, idx, val);
          go(DAGPtr::Bind(*bod), idx, val)
        }
        _ => (),
      }
    }
    go(self.head, idx, val)
  }
}
impl Clone for DAG {
  fn clone(&self) -> Self {
    let mut map: BTreeMap<DAGPtr, DAGPtr> = BTreeMap::new();
    let root = alloc_val(DLL::singleton(ParentPtr::Root));
    DAG::new(DAG::from_subdag(self.head, &mut map, Some(root)))
  }
}
impl fmt::Debug for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    #[inline]
    fn format_uplink(p: ParentPtr) -> String {
      match p {
        ParentPtr::Root => String::from("ROOT"),
        ParentPtr::Bound(link) => format!("Bound<{:?}>", link.as_ptr()),
        ParentPtr::LamTyp(link) => format!("LamTyp<{:?}>", link.as_ptr()),
        ParentPtr::LamBod(link) => format!("LamBod<{:?}>", link.as_ptr()),
        ParentPtr::SlfBod(link) => format!("SlfBod<{:?}>", link.as_ptr()),
        ParentPtr::FixBod(link) => format!("FixBod<{:?}>", link.as_ptr()),
        ParentPtr::DatTyp(link) => format!("DatTyp<{:?}>", link.as_ptr()),
        ParentPtr::DatBod(link) => format!("DatBod<{:?}>", link.as_ptr()),
        ParentPtr::CseBod(link) => format!("CseBod<{:?}>", link.as_ptr()),
        ParentPtr::AppFun(link) => format!("AppFun<{:?}>", link.as_ptr()),
        ParentPtr::AppTyp(link) => format!("AppTyp<{:?}>", link.as_ptr()),
        ParentPtr::AppArg(link) => format!("AppArg<{:?}>", link.as_ptr()),
        ParentPtr::AllDom(link) => format!("AllDom<{:?}>", link.as_ptr()),
        ParentPtr::AllImg(link) => format!("AllImg<{:?}>", link.as_ptr()),
        ParentPtr::LetTyp(link) => format!("LetTyp<{:?}>", link.as_ptr()),
        ParentPtr::LetExp(link) => format!("LetExp<{:?}>", link.as_ptr()),
        ParentPtr::LetBod(link) => format!("LetBod<{:?}>", link.as_ptr()),
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
    fn go(term: DAGPtr, set: &mut BTreeSet<usize>) -> String {
      match term {
        DAGPtr::Null => format!("Null"),
        DAGPtr::Var(link) => {
          let Var { nam, parents, binder, dep, .. } = unsafe { link.as_ref() };
          let binder = match binder {
            BinderPtr::Free => format!("Free<{}>", *dep),
            BinderPtr::Bind(link) => format!("Bind<{:?}>", link.as_ptr()),
            BinderPtr::Slf(link) => format!("Slf<{:?}>", link.as_ptr()),
            BinderPtr::Fix(link) => format!("Fix<{:?}>", link.as_ptr()),
          };
          if set.get(&(link.as_ptr() as usize)).is_none() {
            set.insert(link.as_ptr() as usize);
            format!(
              "\nVar<{:?}> {} bound at {} parents: {}",
              link.as_ptr(),
              binder,
              nam,
              format_parents(*parents)
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::Typ(link) => {
          let Typ { parents, .. } = unsafe { link.as_ref() };
          format!(
            "\nTyp<{:?}> parents: {}",
            (link.as_ptr()),
            format_parents(*parents)
          )
        }
        DAGPtr::LTy(link) => {
          let LTy { parents, .. } = unsafe { link.as_ref() };
          format!(
            "\nLTy<{:?}> parents: {}",
            (link.as_ptr()),
            format_parents(*parents)
          )
        }
        DAGPtr::Hol(link) => {
          let Hol { nam, parents, .. } = unsafe { link.as_ref() };
          format!(
            "\nHol<{:?}> {} parents: {}",
            (link.as_ptr()),
            nam,
            format_parents(*parents)
          )
        }
        DAGPtr::Lit(link) => {
          let Lit { parents, .. } = unsafe { link.as_ref() };
          format!(
            "\nLit<{:?}> parents: {}",
            (link.as_ptr()),
            format_parents(*parents)
          )
        }
        DAGPtr::Opr(link) => {
          let Opr { parents, .. } = unsafe { link.as_ref() };
          format!(
            "\nOpr<{:?}> parents: {}",
            (link.as_ptr()),
            format_parents(*parents)
          )
        }
        DAGPtr::Ref(link) => {
          let Ref { nam, parents, .. } = unsafe { link.as_ref() };
          format!(
            "\nRef<{:?}> {} parents: {}",
            (link.as_ptr()),
            nam,
            format_parents(*parents)
          )
        }
        DAGPtr::Bind(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            let Bind { var, parents, bod, .. } = unsafe { link.as_ref() };
            let name = var.nam.clone();
            set.insert(link.as_ptr() as usize);
            format!(
              "\nBind<{:?}> {} parents: {}{}",
              link.as_ptr(),
              name,
              format_parents(*parents),
              go(*bod, set)
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::Slf(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            let Slf { var, parents, bod, .. } = unsafe { link.as_ref() };
            let name = var.nam.clone();
            set.insert(link.as_ptr() as usize);
            format!(
              "\nSlf<{:?}> {} parents: {}{}",
              link.as_ptr(),
              name,
              format_parents(*parents),
              go(*bod, set)
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::Fix(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            let Fix { var, parents, bod, .. } = unsafe { link.as_ref() };
            let name = var.nam.clone();
            set.insert(link.as_ptr() as usize);
            format!(
              "\nFix<{:?}> {} parents: {}{}",
              link.as_ptr(),
              name,
              format_parents(*parents),
              go(*bod, set)
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::Dat(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            let Dat { parents, bod, .. } = unsafe { link.as_ref() };
            set.insert(link.as_ptr() as usize);
            format!(
              "\nDat<{:?}> parents: {}{}",
              link.as_ptr(),
              format_parents(*parents),
              go(*bod, set)
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::Cse(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            let Cse { parents, bod, .. } = unsafe { link.as_ref() };
            set.insert(link.as_ptr() as usize);
            format!(
              "\nCse<{:?}> parents: {}{}",
              link.as_ptr(),
              format_parents(*parents),
              go(*bod, set)
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::App(link) => {
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
        DAGPtr::Lam(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            set.insert(link.as_ptr() as usize);
            let Lam { typ, bod, parents, copy, .. } = unsafe { link.as_ref() };
            let copy = copy.map(|link| link.as_ptr() as usize);
            format!(
              "\nLam<{:?}> parents: {} copy: {:?}{}{}",
              link.as_ptr(),
              format_parents(*parents),
              copy,
              go(*typ, set),
              go(DAGPtr::Bind(*bod), set),
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::All(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            set.insert(link.as_ptr() as usize);
            let All { dom, img, parents, copy, .. } = unsafe { link.as_ref() };
            let copy = copy.map(|link| link.as_ptr() as usize);
            format!(
              "\nAll<{:?}> parents: {} copy: {:?}{}{}",
              link.as_ptr(),
              format_parents(*parents),
              copy,
              go(*dom, set),
              go(DAGPtr::Bind(*img), set),
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::Let(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            set.insert(link.as_ptr() as usize);
            let Let { exp, typ, bod, parents, copy, .. } =
              unsafe { link.as_ref() };
            let copy = copy.map(|link| link.as_ptr() as usize);
            format!(
              "\nLet<{:?}> parents: {} copy: {:?}{}{}{}",
              link.as_ptr(),
              format_parents(*parents),
              copy,
              go(*typ, set),
              go(*exp, set),
              go(DAGPtr::Bind(*bod), set),
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
      }
    }
    write!(f, "{}", go(self.head, &mut BTreeSet::new()))
  }
}
impl fmt::Display for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_term())
  }
}
impl fmt::Display for DAGPtr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", DAG::new(self.clone()).to_term())
  }
}

//#[cfg(test)]
// pub mod test {
//  use super::*;
//  // use crate::parse::term::parse;
//  use crate::yatima;
//
//  #[quickcheck]
//  fn dag_clone_iso(x: Term) -> bool {
//    let x_dag = DAG::from_term(&x);
//    let y_dag = x_dag.clone();
//    let y = DAG::to_term(&y_dag, true);
//    x == y
//  }
//
//  #[quickcheck]
//  fn dag_term_iso(x: Term) -> bool {
//    // println!("x: {}", x);
//    // println!("x: {:?}", x);
//    let y = DAG::to_term(&DAG::from_term(&x), true);
//    // println!("y: {}", y);
//    // println!("y: {:?}", y);
//    x == y
//  }
//
//  #[quickcheck]
//  fn dag_def_iso(x: Def) -> bool {
//    let y = DAG::to_term(&DAG::from_def(&x, Name::from("test")), true);
//    *x.term == y
//  }
//}
