// Bottom-up reduction of lambda DAGs. Based on the paper by Olin Shivers and
// Mitchel Wand "Bottom-up β-reduction: uplinks and λ-DAGs" (https://www.brics.dk/RS/04/38/BRICS-RS-04-38.pdf)

use crate::{
  defs::Def,
  dll::*,
  literal::{
    LitType,
    Literal,
  },
  position::Pos,
  primop::PrimOp,
  term::Term,
  uses::Uses,
};

use core::ptr::NonNull;
use im::{
  HashMap,
  Vector,
};
use std::{
  collections::HashSet,
  fmt,
  mem,
};

use libipld::Cid;

pub struct DAG {
  pub head: DAGPtr,
}

// A top-down λ-DAG pointer. Keeps track of what kind of node it points to.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum DAGPtr {
  Var(NonNull<Var>),
  Lam(NonNull<Lam>),
  App(NonNull<App>),
  All(NonNull<All>),
  Slf(NonNull<Slf>),
  Dat(NonNull<Dat>),
  Cse(NonNull<Cse>),
  Ref(NonNull<Ref>),
  Let(NonNull<Let>),
  Typ(NonNull<Typ>),
  Ann(NonNull<Ann>),
  Lit(NonNull<Lit>),
  LTy(NonNull<LTy>),
  Opr(NonNull<Opr>),
}

// Doubly-linked list of parent nodes
pub type Parents = DLL<ParentPtr>;

// A bottom-up (parent) λ-DAG pointer. Keeps track of the relation between
// the child and the parent.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParentPtr {
  Root,
  LamBod(NonNull<Lam>),
  SlfBod(NonNull<Slf>),
  DatBod(NonNull<Dat>),
  CseBod(NonNull<Cse>),
  AppFun(NonNull<App>),
  AppArg(NonNull<App>),
  AllDom(NonNull<All>),
  AllImg(NonNull<All>),
  AnnTyp(NonNull<Ann>),
  AnnExp(NonNull<Ann>),
  LetTyp(NonNull<Let>),
  LetExp(NonNull<Let>),
  LetBod(NonNull<Let>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum BinderPtr {
  Free,
  Lam(NonNull<Lam>),
  All(NonNull<All>),
  Slf(NonNull<Slf>),
  Let(NonNull<Let>),
}

// The λ-DAG nodes
#[derive(Clone, Debug)]
#[repr(C)]
pub struct Var {
  pub nam: String,
  // The field `rec` is only used to preserve the Term <-> DAG isomorphism for
  // open Terms
  pub rec: bool,
  // The field `depth` is only used by the type checker to track free
  // variables. Otherwise it is irrelevant.
  pub dep: u64,
  pub binder: BinderPtr,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Lam {
  pub bod: DAGPtr,
  pub bod_ref: Parents,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct App {
  pub fun: DAGPtr,
  pub arg: DAGPtr,
  pub fun_ref: Parents,
  pub arg_ref: Parents,
  pub copy: Option<NonNull<App>>,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct All {
  pub uses: Uses,
  pub dom: DAGPtr,
  pub img: DAGPtr,
  pub dom_ref: Parents,
  pub img_ref: Parents,
  pub var: Var,
  pub copy: Option<NonNull<All>>,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Slf {
  pub bod: DAGPtr,
  pub bod_ref: Parents,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Dat {
  pub bod: DAGPtr,
  pub bod_ref: Parents,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Cse {
  pub bod: DAGPtr,
  pub bod_ref: Parents,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Ann {
  pub typ: DAGPtr,
  pub exp: DAGPtr,
  pub typ_ref: Parents,
  pub exp_ref: Parents,
  pub copy: Option<NonNull<Ann>>,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Let {
  pub uses: Uses,
  pub typ: DAGPtr,
  pub exp: DAGPtr,
  pub bod: DAGPtr,
  pub typ_ref: Parents,
  pub exp_ref: Parents,
  pub bod_ref: Parents,
  pub copy: Option<NonNull<Let>>,
  pub var: Var,
  pub parents: Option<NonNull<Parents>>,
}

#[repr(C)]
pub struct Ref {
  pub nam: String,
  pub rec: bool,
  pub exp: Cid,
  pub ast: Cid,
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
  pub opr: PrimOp,
  pub parents: Option<NonNull<Parents>>,
}

// Auxiliary allocation functions
#[inline]
pub fn alloc_val<T>(val: T) -> NonNull<T> {
  NonNull::new(Box::leak(Box::new(val))).unwrap()
}

#[inline]
pub fn alloc_lam(
  var_nam: String,
  var_dep: u64,
  var_parents: Option<NonNull<Parents>>,
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Lam> {
  unsafe {
    let lam = alloc_val(Lam {
      var: Var {
        nam: var_nam,
        rec: false,
        dep: var_dep,
        binder: mem::zeroed(),
        parents: var_parents,
      },
      bod,
      bod_ref: mem::zeroed(),
      parents,
    });
    (*lam.as_ptr()).var.binder = BinderPtr::Lam(lam);
    (*lam.as_ptr()).bod_ref = DLL::singleton(ParentPtr::LamBod(lam));
    lam
  }
}

#[inline]
pub fn alloc_slf(
  var_nam: String,
  var_dep: u64,
  var_parents: Option<NonNull<Parents>>,
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Slf> {
  unsafe {
    let slf = alloc_val(Slf {
      var: Var {
        nam: var_nam,
        rec: false,
        dep: var_dep,
        binder: mem::zeroed(),
        parents: var_parents,
      },
      bod,
      bod_ref: mem::zeroed(),
      parents,
    });
    (*slf.as_ptr()).var.binder = BinderPtr::Slf(slf);
    (*slf.as_ptr()).bod_ref = DLL::singleton(ParentPtr::SlfBod(slf));
    slf
  }
}

#[inline]
pub fn alloc_dat(
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Dat> {
  unsafe {
    let dat = alloc_val(Dat { bod, bod_ref: mem::zeroed(), parents });
    (*dat.as_ptr()).bod_ref = DLL::singleton(ParentPtr::DatBod(dat));
    dat
  }
}

#[inline]
pub fn alloc_cse(
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Cse> {
  unsafe {
    let cse = alloc_val(Cse { bod, bod_ref: mem::zeroed(), parents });
    (*cse.as_ptr()).bod_ref = DLL::singleton(ParentPtr::CseBod(cse));
    cse
  }
}

#[inline]
pub fn alloc_all(
  var_nam: String,
  var_dep: u64,
  var_parents: Option<NonNull<Parents>>,
  uses: Uses,
  dom: DAGPtr,
  img: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<All> {
  unsafe {
    let all = alloc_val(All {
      var: Var {
        nam: var_nam,
        rec: false,
        dep: var_dep,
        binder: mem::zeroed(),
        parents: var_parents,
      },
      uses,
      dom,
      img,
      copy: None,
      dom_ref: mem::zeroed(),
      img_ref: mem::zeroed(),
      parents,
    });
    (*all.as_ptr()).var.binder = BinderPtr::All(all);
    (*all.as_ptr()).dom_ref = DLL::singleton(ParentPtr::AllDom(all));
    (*all.as_ptr()).img_ref = DLL::singleton(ParentPtr::AllImg(all));
    all
  }
}

#[inline]
pub fn alloc_app(
  fun: DAGPtr,
  arg: DAGPtr,
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

#[inline]
pub fn alloc_ann(
  typ: DAGPtr,
  exp: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Ann> {
  unsafe {
    let ann = alloc_val(Ann {
      typ,
      exp,
      copy: None,
      typ_ref: mem::zeroed(),
      exp_ref: mem::zeroed(),
      parents,
    });
    (*ann.as_ptr()).typ_ref = DLL::singleton(ParentPtr::AnnTyp(ann));
    (*ann.as_ptr()).exp_ref = DLL::singleton(ParentPtr::AnnExp(ann));
    ann
  }
}

#[inline]
#[allow(clippy::too_many_arguments)]
pub fn alloc_let(
  var_nam: String,
  var_dep: u64,
  var_parents: Option<NonNull<Parents>>,
  uses: Uses,
  typ: DAGPtr,
  exp: DAGPtr,
  bod: DAGPtr,
  parents: Option<NonNull<Parents>>,
) -> NonNull<Let> {
  unsafe {
    let let_ = alloc_val(Let {
      var: Var {
        nam: var_nam,
        rec: false,
        dep: var_dep,
        binder: mem::zeroed(),
        parents: var_parents,
      },
      uses,
      typ,
      exp,
      bod,
      copy: None,
      typ_ref: mem::zeroed(),
      exp_ref: mem::zeroed(),
      bod_ref: mem::zeroed(),
      parents,
    });
    (*let_.as_ptr()).var.binder = BinderPtr::Let(let_);
    (*let_.as_ptr()).typ_ref = DLL::singleton(ParentPtr::LetTyp(let_));
    (*let_.as_ptr()).exp_ref = DLL::singleton(ParentPtr::LetExp(let_));
    (*let_.as_ptr()).bod_ref = DLL::singleton(ParentPtr::LetBod(let_));
    let_
  }
}

// Auxiliary parent functions
#[inline]
pub fn get_parents(term: DAGPtr) -> Option<NonNull<Parents>> {
  unsafe {
    match term {
      DAGPtr::Var(link) => (*link.as_ptr()).parents,
      DAGPtr::Lam(link) => (*link.as_ptr()).parents,
      DAGPtr::App(link) => (*link.as_ptr()).parents,
      DAGPtr::All(link) => (*link.as_ptr()).parents,
      DAGPtr::Slf(link) => (*link.as_ptr()).parents,
      DAGPtr::Dat(link) => (*link.as_ptr()).parents,
      DAGPtr::Cse(link) => (*link.as_ptr()).parents,
      DAGPtr::Ref(link) => (*link.as_ptr()).parents,
      DAGPtr::Let(link) => (*link.as_ptr()).parents,
      DAGPtr::Typ(link) => (*link.as_ptr()).parents,
      DAGPtr::Ann(link) => (*link.as_ptr()).parents,
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
      DAGPtr::Var(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Lam(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::App(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::All(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Slf(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Dat(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Cse(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Ref(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Let(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Typ(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Ann(link) => (*link.as_ptr()).parents = pref,
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
      ParentPtr::LamBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::SlfBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::DatBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::CseBod(parent) => (*parent.as_ptr()).bod = newchild,
      ParentPtr::AppFun(parent) => (*parent.as_ptr()).fun = newchild,
      ParentPtr::AppArg(parent) => (*parent.as_ptr()).arg = newchild,
      ParentPtr::AllDom(parent) => (*parent.as_ptr()).dom = newchild,
      ParentPtr::AllImg(parent) => (*parent.as_ptr()).img = newchild,
      ParentPtr::AnnExp(parent) => (*parent.as_ptr()).exp = newchild,
      ParentPtr::AnnTyp(parent) => (*parent.as_ptr()).typ = newchild,
      ParentPtr::LetExp(parent) => (*parent.as_ptr()).exp = newchild,
      ParentPtr::LetTyp(parent) => (*parent.as_ptr()).typ = newchild,
      ParentPtr::LetBod(parent) => (*parent.as_ptr()).bod = newchild,
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

// Free parentless nodes.
pub fn free_dead_node(node: DAGPtr) {
  unsafe {
    match node {
      DAGPtr::Lam(link) => {
        let Lam { bod, bod_ref, .. } = &link.as_ref();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::Slf(mut link) => {
        let Slf { bod, bod_ref, .. } = &link.as_mut();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::Cse(link) => {
        let Cse { bod, bod_ref, .. } = link.as_ref();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::Dat(link) => {
        let Dat { bod, bod_ref, .. } = &link.as_ref();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::All(link) => {
        let All { dom, img, dom_ref, img_ref, .. } = link.as_ref();
        let new_dom_parents = dom_ref.unlink_node();
        set_parents(*dom, new_dom_parents);
        if new_dom_parents.is_none() {
          free_dead_node(*dom)
        }
        let new_img_parents = img_ref.unlink_node();
        set_parents(*img, new_img_parents);
        if new_img_parents.is_none() {
          free_dead_node(*img)
        }
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::App(link) => {
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
      DAGPtr::Ann(link) => {
        let Ann { exp, typ, exp_ref, typ_ref, .. } = link.as_ref();
        let new_exp_parents = exp_ref.unlink_node();
        set_parents(*exp, new_exp_parents);
        if new_exp_parents.is_none() {
          free_dead_node(*exp)
        }
        let new_typ_parents = typ_ref.unlink_node();
        set_parents(*typ, new_typ_parents);
        if new_typ_parents.is_none() {
          free_dead_node(*typ)
        }
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::Let(link) => {
        let Let { exp, typ, exp_ref, typ_ref, bod, bod_ref, .. } =
          link.as_ref();
        let new_exp_parents = exp_ref.unlink_node();
        set_parents(*exp, new_exp_parents);
        if new_exp_parents.is_none() {
          free_dead_node(*exp)
        }
        let new_typ_parents = typ_ref.unlink_node();
        set_parents(*typ, new_typ_parents);
        if new_typ_parents.is_none() {
          free_dead_node(*typ)
        }
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::Var(link) => {
        let Var { binder, .. } = link.as_ref();
        // only free Free variables, bound variables are freed with their binder
        if let BinderPtr::Free = binder {
          Box::from_raw(link.as_ptr());
        }
      }
      DAGPtr::Ref(link) => {
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::Typ(link) => {
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::Lit(link) => {
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::LTy(link) => {
        Box::from_raw(link.as_ptr());
      }
      DAGPtr::Opr(link) => {
        Box::from_raw(link.as_ptr());
      }
    }
  }
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
    map: &mut HashMap<*mut Var, u64>,
    depth: u64,
  ) -> Term {
    match node {
      DAGPtr::Var(link) => {
        let Var { nam, dep: var_depth, rec, .. } = unsafe { link.as_ref() };
        if *rec {
          Term::Rec(Pos::None)
        }
        else if let Some(level) = map.get(&link.as_ptr()) {
          Term::Var(Pos::None, nam.clone(), depth - level - 1)
        }
        else {
          Term::Var(Pos::None, nam.clone(), *var_depth)
        }
      }
      DAGPtr::Typ(_) => Term::Typ(Pos::None),
      DAGPtr::LTy(link) => {
        let LTy { lty, .. } = unsafe { link.as_ref() };
        Term::LTy(Pos::None, *lty)
      }
      DAGPtr::Lit(link) => {
        let Lit { lit, .. } = unsafe { link.as_ref() };
        Term::Lit(Pos::None, lit.clone())
      }
      DAGPtr::Opr(link) => {
        let Opr { opr, .. } = unsafe { link.as_ref() };
        Term::Opr(Pos::None, *opr)
      }
      DAGPtr::Ref(link) => {
        let Ref { nam, exp, ast, rec, .. } = unsafe { link.as_ref() };
        if *rec {
          Term::Rec(Pos::None)
        }
        else {
          Term::Ref(Pos::None, nam.clone(), *exp, *ast)
        }
      }
      DAGPtr::Lam(link) => {
        let Lam { var, bod, .. } = unsafe { &mut *link.as_ptr() };
        let nam = var.nam.clone();
        map.insert(var, depth);
        let body = DAG::dag_ptr_to_term(bod, map, depth + 1);
        Term::Lam(Pos::None, nam, Box::new(body))
      }
      DAGPtr::Slf(link) => {
        let Slf { var, bod, .. } = unsafe { &mut *link.as_ptr() };
        let nam = var.nam.clone();
        map.insert(var, depth);
        let body = DAG::dag_ptr_to_term(bod, map, depth + 1);
        Term::Slf(Pos::None, nam, Box::new(body))
      }
      DAGPtr::Cse(link) => {
        let Cse { bod, .. } = unsafe { link.as_ref() };
        Term::Cse(Pos::None, Box::new(DAG::dag_ptr_to_term(bod, map, depth)))
      }
      DAGPtr::Dat(link) => {
        let Dat { bod, .. } = unsafe { link.as_ref() };
        Term::Dat(Pos::None, Box::new(DAG::dag_ptr_to_term(bod, map, depth)))
      }
      DAGPtr::App(link) => {
        let App { fun, arg, .. } = unsafe { link.as_ref() };
        let fun_map = &mut map.clone();
        Term::App(
          Pos::None,
          Box::new((
            DAG::dag_ptr_to_term(fun, fun_map, depth),
            DAG::dag_ptr_to_term(arg, map, depth),
          )),
        )
      }
      DAGPtr::Ann(link) => {
        let Ann { typ, exp, .. } = unsafe { link.as_ref() };
        let typ_map = &mut map.clone();
        Term::Ann(
          Pos::None,
          Box::new((
            DAG::dag_ptr_to_term(typ, typ_map, depth),
            DAG::dag_ptr_to_term(exp, map, depth),
          )),
        )
      }
      DAGPtr::All(link) => {
        let All { var, uses, dom, img, .. } = unsafe { &mut *link.as_ptr() };
        let nam = var.nam.clone();
        let dom_map = &mut map.clone();
        map.insert(var, depth);
        Term::All(
          Pos::None,
          *uses,
          nam,
          Box::new((
            DAG::dag_ptr_to_term(dom, dom_map, depth),
            DAG::dag_ptr_to_term(img, map, depth + 1),
          )),
        )
      }
      DAGPtr::Let(link) => {
        let Let { var, uses, typ, exp, bod, .. } =
          unsafe { &mut *link.as_ptr() };
        let nam = var.nam.clone();
        let typ_map = &mut map.clone();
        let exp_map = &mut map.clone();
        map.insert(var, depth);
        Term::Let(
          Pos::None,
          false,
          *uses,
          nam,
          Box::new((
            DAG::dag_ptr_to_term(typ, typ_map, depth),
            DAG::dag_ptr_to_term(exp, exp_map, depth),
            DAG::dag_ptr_to_term(bod, map, depth + 1),
          )),
        )
      }
    }
  }

  pub fn to_term(&self) -> Term {
    let mut map = HashMap::new();
    DAG::dag_ptr_to_term(&self.head, &mut map, 0)
  }

  pub fn from_term(tree: &Term) -> Self {
    let root = alloc_val(DLL::singleton(ParentPtr::Root));
    DAG::new(DAG::from_term_inner(tree, 0, Vector::new(), Some(root), None))
  }

  pub fn from_def(def: &Def, name: String) -> Self {
    let root = alloc_val(DLL::singleton(ParentPtr::Root));
    let (d, _, a) = def.embed();
    let def_cid = d.cid();
    let ast_cid = a.cid();
    DAG::new(DAG::from_term_inner(
      &def.term,
      0,
      Vector::new(),
      Some(root),
      Some((name, def_cid, ast_cid)),
    ))
  }

  pub fn from_ref(
    def: &Def,
    name: String,
    def_cid: Cid,
    ast_cid: Cid,
    parents: Option<NonNull<Parents>>,
  ) -> DAGPtr {
    DAG::from_term_inner(
      &def.term,
      0,
      Vector::new(),
      parents,
      Some((name, def_cid, ast_cid)),
    )
  }

  pub fn from_term_inner(
    tree: &Term,
    depth: u64,
    mut ctx: Vector<DAGPtr>,
    parents: Option<NonNull<Parents>>,
    rec_ref: Option<(String, Cid, Cid)>,
  ) -> DAGPtr {
    match tree {
      Term::Rec(_) => match rec_ref {
        Some((nam, exp, ast)) => {
          let ref_ = alloc_val(Ref { nam, rec: true, exp, ast, parents });
          DAGPtr::Ref(ref_)
        }
        None => {
          let var = alloc_val(Var {
            nam: "#^".to_owned(),
            rec: true,
            dep: depth,
            binder: BinderPtr::Free,
            parents,
          });
          DAGPtr::Var(var)
        }
      },
      Term::Var(_, name, idx) => match ctx.get(*idx as usize) {
        Some(val) => {
          if let Some(parents) = parents {
            DLL::concat(parents, get_parents(*val));
            set_parents(*val, Some(parents));
          }
          *val
        }
        None => {
          let var = alloc_val(Var {
            nam: name.clone(),
            rec: false,
            dep: depth - 1 - idx,
            binder: BinderPtr::Free,
            parents,
          });
          DAGPtr::Var(var)
        }
      },
      Term::Typ(_) => DAGPtr::Typ(alloc_val(Typ { parents })),
      Term::LTy(_, lty) => DAGPtr::LTy(alloc_val(LTy { lty: *lty, parents })),
      Term::Lit(_, lit) => {
        DAGPtr::Lit(alloc_val(Lit { lit: lit.clone(), parents }))
      }
      Term::Opr(_, opr) => DAGPtr::Opr(alloc_val(Opr { opr: *opr, parents })),
      Term::Ref(_, nam, exp, ast) => DAGPtr::Ref(alloc_val(Ref {
        nam: nam.clone(),
        rec: false,
        exp: *exp,
        ast: *ast,
        parents,
      })),
      Term::Lam(_, nam, bod) => unsafe {
        let lam = alloc_lam(nam.clone(), 0, None, mem::zeroed(), parents);
        let Lam { var, bod_ref, .. } = &mut *lam.as_ptr();
        ctx.push_front(DAGPtr::Var(NonNull::new(var).unwrap()));
        let bod = DAG::from_term_inner(
          &**bod,
          depth + 1,
          ctx,
          NonNull::new(bod_ref),
          rec_ref.clone(),
        );
        (*lam.as_ptr()).bod = bod;
        DAGPtr::Lam(lam)
      },
      Term::Slf(_, nam, bod) => unsafe {
        let slf = alloc_slf(nam.clone(), 0, None, mem::zeroed(), parents);
        let Slf { var, bod_ref, .. } = &mut *slf.as_ptr();
        ctx.push_front(DAGPtr::Var(NonNull::new(var).unwrap()));
        let bod = DAG::from_term_inner(
          &**bod,
          depth + 1,
          ctx,
          NonNull::new(bod_ref),
          rec_ref.clone(),
        );
        (*slf.as_ptr()).bod = bod;
        DAGPtr::Slf(slf)
      },
      Term::Dat(_, bod) => unsafe {
        let dat = alloc_dat(mem::zeroed(), parents);
        let Dat { bod_ref, .. } = &mut *dat.as_ptr();
        let bod = DAG::from_term_inner(
          &**bod,
          depth,
          ctx,
          NonNull::new(bod_ref),
          rec_ref.clone(),
        );
        (*dat.as_ptr()).bod = bod;
        DAGPtr::Dat(dat)
      },
      Term::Cse(_, bod) => unsafe {
        let cse = alloc_cse(mem::zeroed(), parents);
        let Cse { bod_ref, .. } = &mut *cse.as_ptr();
        let bod = DAG::from_term_inner(
          &**bod,
          depth,
          ctx,
          NonNull::new(bod_ref),
          rec_ref.clone(),
        );
        (*cse.as_ptr()).bod = bod;
        DAGPtr::Cse(cse)
      },
      Term::All(_, uses, nam, dom_img) => unsafe {
        let (dom, img) = (**dom_img).clone();
        let all = alloc_all(
          nam.clone(),
          0,
          None,
          *uses,
          mem::zeroed(),
          mem::zeroed(),
          parents,
        );
        let All { var, dom_ref, img_ref, .. } = &mut *all.as_ptr();
        let mut img_ctx = ctx.clone();
        let dom = DAG::from_term_inner(
          &dom,
          depth,
          ctx,
          NonNull::new(dom_ref),
          rec_ref.clone(),
        );
        img_ctx.push_front(DAGPtr::Var(NonNull::new(var).unwrap()));
        let img = DAG::from_term_inner(
          &img,
          depth + 1,
          img_ctx,
          NonNull::new(img_ref),
          rec_ref.clone(),
        );
        (*all.as_ptr()).dom = dom;
        (*all.as_ptr()).img = img;
        DAGPtr::All(all)
      },
      Term::App(_, fun_arg) => unsafe {
        let (fun, arg) = (**fun_arg).clone();
        let app = alloc_app(mem::zeroed(), mem::zeroed(), parents);
        let App { fun_ref, arg_ref, .. } = &mut *app.as_ptr();
        let fun = DAG::from_term_inner(
          &fun,
          depth,
          ctx.clone(),
          NonNull::new(fun_ref),
          rec_ref.clone(),
        );
        let arg = DAG::from_term_inner(
          &arg,
          depth,
          ctx,
          NonNull::new(arg_ref),
          rec_ref.clone(),
        );
        (*app.as_ptr()).fun = fun;
        (*app.as_ptr()).arg = arg;
        DAGPtr::App(app)
      },
      Term::Ann(_, typ_exp) => unsafe {
        let (typ, exp) = (**typ_exp).clone();
        let ann = alloc_ann(mem::zeroed(), mem::zeroed(), parents);
        let Ann { typ_ref, exp_ref, .. } = &mut *ann.as_ptr();
        let typ = DAG::from_term_inner(
          &typ,
          depth,
          ctx.clone(),
          NonNull::new(typ_ref),
          rec_ref.clone(),
        );
        let exp = DAG::from_term_inner(
          &exp,
          depth,
          ctx,
          NonNull::new(exp_ref),
          rec_ref.clone(),
        );
        (*ann.as_ptr()).typ = typ;
        (*ann.as_ptr()).exp = exp;
        DAGPtr::Ann(ann)
      },
      Term::Let(_, true, _uses, _name, _typ_exp_bod) => {
        panic!("letrec not implemented")
      }
      Term::Let(_, false, uses, nam, typ_exp_bod) => unsafe {
        let (typ, exp, bod) = (**typ_exp_bod).clone();
        let let_ = alloc_let(
          nam.clone(),
          0,
          None,
          *uses,
          mem::zeroed(),
          mem::zeroed(),
          mem::zeroed(),
          parents,
        );
        let Let { var, typ_ref, exp_ref, bod_ref, .. } = &mut *let_.as_ptr();
        let typ = DAG::from_term_inner(
          &typ,
          depth,
          ctx.clone(),
          NonNull::new(typ_ref),
          rec_ref.clone(),
        );
        let exp = DAG::from_term_inner(
          &exp,
          depth,
          ctx.clone(),
          NonNull::new(exp_ref),
          rec_ref.clone(),
        );
        ctx.push_front(DAGPtr::Var(NonNull::new(var).unwrap()));
        let bod = DAG::from_term_inner(
          &bod,
          depth + 1,
          ctx,
          NonNull::new(bod_ref),
          rec_ref.clone(),
        );
        (*let_.as_ptr()).typ = typ;
        (*let_.as_ptr()).exp = exp;
        (*let_.as_ptr()).bod = bod;
        DAGPtr::Let(let_)
      },
    }
  }

  pub fn from_subdag(
    node: DAGPtr,
    map: &mut HashMap<DAGPtr, DAGPtr>,
    parents: Option<NonNull<Parents>>,
  ) -> DAGPtr {
    // If the node is in the hash map then it was already copied,
    // so we update the parent list and return the copy
    if let Some(copy) = (*map).get(&node) {
      if let Some(parents) = parents {
        DLL::concat(parents, get_parents(*copy));
        set_parents(*copy, Some(parents));
      }
      return *copy;
    }
    // Otherwise create a new DAG node and add it to the map
    let new_node = match node {
      DAGPtr::Var(link) => unsafe {
        let Var { nam, dep, rec, .. } = &*link.as_ptr();
        let var = alloc_val(Var {
          nam: nam.clone(),
          rec: *rec,
          dep: *dep,
          binder: BinderPtr::Free,
          parents,
        });
        DAGPtr::Var(var)
      },
      DAGPtr::Ref(link) => unsafe {
        let Ref { nam, exp, ast, rec, .. } = &*link.as_ptr();
        let node = alloc_val(Ref {
          nam: nam.clone(),
          rec: *rec,
          exp: *exp,
          ast: *ast,
          parents,
        });
        DAGPtr::Ref(node)
      },
      DAGPtr::Lam(link) => unsafe {
        let Lam { var, bod, .. } = &mut *link.as_ptr();
        let lam =
          alloc_lam(var.nam.clone(), var.dep, None, mem::zeroed(), parents);
        let Lam { var: new_var, bod: new_bod, bod_ref, .. } =
          &mut *lam.as_ptr();
        map.insert(
          DAGPtr::Var(NonNull::new(var).unwrap()),
          DAGPtr::Var(NonNull::new(new_var).unwrap()),
        );
        *new_bod = DAG::from_subdag(*bod, map, NonNull::new(bod_ref));
        DAGPtr::Lam(lam)
      },
      DAGPtr::Slf(link) => unsafe {
        let Slf { var, bod, .. } = &mut *link.as_ptr();
        let slf =
          alloc_slf(var.nam.clone(), var.dep, None, mem::zeroed(), parents);
        let Slf { var: new_var, bod: new_bod, bod_ref, .. } =
          &mut *slf.as_ptr();
        map.insert(
          DAGPtr::Var(NonNull::new(var).unwrap()),
          DAGPtr::Var(NonNull::new(new_var).unwrap()),
        );
        *new_bod = DAG::from_subdag(*bod, map, NonNull::new(bod_ref));
        DAGPtr::Slf(slf)
      },
      DAGPtr::Cse(link) => unsafe {
        let Cse { bod, .. } = &mut *link.as_ptr();
        let cse = alloc_cse(mem::zeroed(), parents);
        let Cse { bod: new_bod, bod_ref, .. } = &mut *cse.as_ptr();
        *new_bod = DAG::from_subdag(*bod, map, NonNull::new(bod_ref));
        DAGPtr::Cse(cse)
      },
      DAGPtr::Dat(link) => unsafe {
        let Dat { bod, .. } = &mut *link.as_ptr();
        let dat = alloc_dat(mem::zeroed(), parents);
        let Dat { bod: new_bod, bod_ref, .. } = &mut *dat.as_ptr();
        *new_bod = DAG::from_subdag(*bod, map, NonNull::new(bod_ref));
        DAGPtr::Dat(dat)
      },
      DAGPtr::App(link) => unsafe {
        let App { fun, arg, .. } = &mut *link.as_ptr();
        let app = alloc_app(mem::zeroed(), mem::zeroed(), parents);
        let App { fun: new_fun, fun_ref, arg: new_arg, arg_ref, .. } =
          &mut *app.as_ptr();
        *new_fun = DAG::from_subdag(*fun, map, NonNull::new(fun_ref));
        *new_arg = DAG::from_subdag(*arg, map, NonNull::new(arg_ref));
        DAGPtr::App(app)
      },
      DAGPtr::All(link) => unsafe {
        let All { var, uses, dom, img, .. } = &mut *link.as_ptr();
        let all = alloc_all(
          var.nam.clone(),
          var.dep,
          None,
          *uses,
          mem::zeroed(),
          mem::zeroed(),
          parents,
        );
        let All {
          var: new_var,
          dom: new_dom,
          dom_ref,
          img: new_img,
          img_ref,
          ..
        } = &mut *all.as_ptr();
        map.insert(
          DAGPtr::Var(NonNull::new(var).unwrap()),
          DAGPtr::Var(NonNull::new(new_var).unwrap()),
        );
        *new_dom = DAG::from_subdag(*dom, map, NonNull::new(dom_ref));
        *new_img = DAG::from_subdag(*img, map, NonNull::new(img_ref));
        DAGPtr::All(all)
      },
      DAGPtr::Let(link) => unsafe {
        let Let { var, uses, typ, exp, bod, .. } = &mut *link.as_ptr();
        let let_ = alloc_let(
          var.nam.clone(),
          var.dep,
          None,
          *uses,
          mem::zeroed(),
          mem::zeroed(),
          mem::zeroed(),
          parents,
        );
        let Let {
          var: new_var,
          typ: new_typ,
          typ_ref,
          exp: new_exp,
          exp_ref,
          bod: new_bod,
          bod_ref,
          ..
        } = &mut *let_.as_ptr();
        map.insert(
          DAGPtr::Var(NonNull::new(var).unwrap()),
          DAGPtr::Var(NonNull::new(new_var).unwrap()),
        );
        *new_exp = DAG::from_subdag(*exp, map, NonNull::new(exp_ref));
        *new_typ = DAG::from_subdag(*typ, map, NonNull::new(typ_ref));
        *new_bod = DAG::from_subdag(*bod, map, NonNull::new(bod_ref));
        DAGPtr::Let(let_)
      },
      DAGPtr::Ann(link) => unsafe {
        let Ann { typ, exp, .. } = &mut *link.as_ptr();
        let ann = alloc_ann(mem::zeroed(), mem::zeroed(), parents);
        let Ann { typ: new_typ, typ_ref, exp: new_exp, exp_ref, .. } =
          &mut *ann.as_ptr();
        *new_typ = DAG::from_subdag(*typ, map, NonNull::new(typ_ref));
        *new_exp = DAG::from_subdag(*exp, map, NonNull::new(exp_ref));
        DAGPtr::Ann(ann)
      },
      DAGPtr::Lit(link) => unsafe {
        let Lit { lit, .. } = &*link.as_ptr();
        let node = alloc_val(Lit { lit: lit.clone(), parents });
        DAGPtr::Lit(node)
      },
      DAGPtr::LTy(link) => unsafe {
        let LTy { lty, .. } = *link.as_ptr();
        let node = alloc_val(LTy { lty, parents });
        DAGPtr::LTy(node)
      },
      DAGPtr::Opr(link) => unsafe {
        let Opr { opr, .. } = *link.as_ptr();
        let node = alloc_val(Opr { opr, parents });
        DAGPtr::Opr(node)
      },
      DAGPtr::Typ(_) => {
        let node = alloc_val(Typ { parents });
        DAGPtr::Typ(node)
      } // _ => panic!("TODO"),
    };
    // Map `node` to `new_node`
    map.insert(node, new_node);
    new_node
  }
}

impl Clone for DAG {
  fn clone(&self) -> Self {
    let mut map: HashMap<DAGPtr, DAGPtr> = HashMap::new();
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
        ParentPtr::LamBod(link) => format!("LamBod<{:?}>", link.as_ptr()),
        ParentPtr::SlfBod(link) => format!("SlfBod<{:?}>", link.as_ptr()),
        ParentPtr::DatBod(link) => format!("DatBod<{:?}>", link.as_ptr()),
        ParentPtr::CseBod(link) => format!("CseBod<{:?}>", link.as_ptr()),
        ParentPtr::AppFun(link) => format!("AppFun<{:?}>", link.as_ptr()),
        ParentPtr::AppArg(link) => format!("AppArg<{:?}>", link.as_ptr()),
        ParentPtr::AllDom(link) => format!("AllDom<{:?}>", link.as_ptr()),
        ParentPtr::AllImg(link) => format!("AllImg<{:?}>", link.as_ptr()),
        ParentPtr::AnnExp(link) => format!("AnnExp<{:?}>", link.as_ptr()),
        ParentPtr::AnnTyp(link) => format!("AnnTyp<{:?}>", link.as_ptr()),
        ParentPtr::LetExp(link) => format!("LetExp<{:?}>", link.as_ptr()),
        ParentPtr::LetTyp(link) => format!("LetTyp<{:?}>", link.as_ptr()),
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
    fn go(term: DAGPtr, set: &mut HashSet<usize>) -> String {
      match term {
        DAGPtr::Var(link) => {
          let Var { nam, parents, binder, dep, .. } = unsafe { link.as_ref() };
          let binder = match binder {
            BinderPtr::Free => format!("Free<{}>", *dep),
            BinderPtr::Lam(link) => format!("Lam<{:?}>", link.as_ptr()),
            BinderPtr::All(link) => format!("All<{:?}>", link.as_ptr()),
            BinderPtr::Slf(link) => format!("Slf<{:?}>", link.as_ptr()),
            BinderPtr::Let(link) => format!("Let<{:?}>", link.as_ptr()),
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
          let Typ { parents } = unsafe { link.as_ref() };
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
        DAGPtr::Lam(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            let Lam { var, parents, bod, .. } = unsafe { link.as_ref() };
            let name = var.nam.clone();
            set.insert(link.as_ptr() as usize);
            format!(
              "\nLam<{:?}> {} parents: {}{}",
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
        DAGPtr::Ann(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            set.insert(link.as_ptr() as usize);
            let Ann { typ, exp, parents, copy, .. } = unsafe { link.as_ref() };
            let copy = copy.map(|link| link.as_ptr() as usize);
            format!(
              "\nAnn<{:?}> parents: {} copy: {:?}{}{}",
              link.as_ptr(),
              format_parents(*parents),
              copy,
              go(*typ, set),
              go(*exp, set),
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::All(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            set.insert(link.as_ptr() as usize);
            let All { var, dom, img, parents, copy, .. } =
              unsafe { link.as_ref() };
            let name = var.nam.clone();
            let copy = copy.map(|link| link.as_ptr() as usize);
            format!(
              "\nAll<{:?}> {} parents: {} copy: {:?}{}{}",
              link.as_ptr(),
              name,
              format_parents(*parents),
              copy,
              go(*dom, set),
              go(*img, set),
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
        DAGPtr::Let(link) => {
          if set.get(&(link.as_ptr() as usize)).is_none() {
            set.insert(link.as_ptr() as usize);
            let Let { var, exp, typ, bod, parents, copy, .. } =
              unsafe { link.as_ref() };
            let name = var.nam.clone();
            let copy = copy.map(|link| link.as_ptr() as usize);
            format!(
              "\nLet<{:?}> {} parents: {} copy: {:?}{}{}{}",
              link.as_ptr(),
              name,
              format_parents(*parents),
              copy,
              go(*typ, set),
              go(*exp, set),
              go(*bod, set),
            )
          }
          else {
            format!("\nSHARE<{:?}>", link.as_ptr())
          }
        }
      }
    }
    write!(f, "{}", go(self.head, &mut HashSet::new()))
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

#[cfg(test)]
pub mod test {
  use super::*;
  // use crate::parse::term::parse;

  //#[test]
  // fn test_cases() {
  //  let (_, x) = parse("λ _z => Type").unwrap();
  //  assert_eq!(x, DAG::to_term(&DAG::from_term(&x)));
  //  let (_, x) = parse("λ z => z").unwrap();
  //  assert_eq!(x, DAG::to_term(&DAG::from_term(&x)));
  //  let (_, x) =
  //    parse("λ _z => (λ _a => ∀ (1 _x: _a) -> #Natural) Type").unwrap();
  //  assert_eq!(x, DAG::to_term(&DAG::from_term(&x)));
  //}

  #[quickcheck]
  fn dag_term_iso(x: Term) -> bool {
    // println!("x: {}", x);
    // println!("x: {:?}", x);
    let y = DAG::to_term(&DAG::from_term(&x));
    // println!("y: {}", y);
    // println!("y: {:?}", y);
    x == y
  }

  #[quickcheck]
  fn dag_def_iso(x: Def) -> bool {
    let y = DAG::to_term(&DAG::from_def(&x, "test".to_owned()));
    x.term == y
  }
}
