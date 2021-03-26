// Bottom-up reduction of lambda DAGs. Based on the paper by Olin Shivers and
// Mitchel Wand "Bottom-up β-reduction: uplinks and λ-DAGs" (https://www.brics.dk/RS/04/38/BRICS-RS-04-38.pdf)

use crate::{
  core::{
    dll::*,
    literal::{
      LitType,
      Literal,
    },
    primop::PrimOp,
    uses::Uses,
  },
  term::{
    Link,
    Term,
  },
};

use core::ptr::NonNull;
use im::{
  HashMap,
  Vector,
};
use std::{
  alloc::{
    alloc,
    dealloc,
    Layout,
  },
  collections::HashSet,
  fmt,
  mem,
};

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
  AnnExp(NonNull<Ann>),
  AnnTyp(NonNull<Ann>),
  LetExp(NonNull<Let>),
  LetTyp(NonNull<Let>),
  LetBod(NonNull<Let>),
}

// The λ-DAG nodes
#[derive(Clone)]
pub struct Var {
  pub nam: String,
  // The field `depth` is only used by the type checker to track free
  // variables. Otherwise it is irrelevant.
  pub dep: u64,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Lam {
  pub var: Var,
  pub bod: DAGPtr,
  pub bod_ref: Parents,
  pub parents: Option<NonNull<Parents>>,
}

pub struct App {
  pub fun: DAGPtr,
  pub arg: DAGPtr,
  pub fun_ref: Parents,
  pub arg_ref: Parents,
  pub copy: Option<NonNull<App>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct All {
  pub var: Var,
  pub uses: Uses,
  pub dom: DAGPtr,
  pub img: DAGPtr,
  pub dom_ref: Parents,
  pub img_ref: Parents,
  pub copy: Option<NonNull<All>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Slf {
  pub var: Var,
  pub bod: DAGPtr,
  pub bod_ref: Parents,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Dat {
  pub bod: DAGPtr,
  pub bod_ref: Parents,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Cse {
  pub bod: DAGPtr,
  pub bod_ref: Parents,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Ann {
  pub typ: DAGPtr,
  pub exp: DAGPtr,
  pub exp_ref: Parents,
  pub typ_ref: Parents,
  pub copy: Option<NonNull<Ann>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Let {
  pub var: Var,
  pub uses: Uses,
  pub exp: DAGPtr,
  pub typ: DAGPtr,
  pub bod: DAGPtr,
  pub exp_ref: Parents,
  pub typ_ref: Parents,
  pub bod_ref: Parents,
  pub copy: Option<NonNull<Let>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Ref {
  pub nam: String,
  pub exp: Link,
  pub ast: Link,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Typ {
  pub parents: Option<NonNull<Parents>>,
}

pub struct Lit {
  pub lit: Literal,
  pub parents: Option<NonNull<Parents>>,
}

pub struct LTy {
  pub lty: LitType,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Opr {
  pub opr: PrimOp,
  pub parents: Option<NonNull<Parents>>,
}

// Auxiliary allocation functions
#[inline]
pub fn alloc_val<T>(val: T) -> NonNull<T> {
  unsafe { NonNull::new_unchecked(Box::leak(Box::new(val))) }
}

#[inline]
pub fn alloc_uninit<T>() -> NonNull<T> {
  unsafe {
    let ptr = alloc(Layout::new::<T>()) as *mut T;
    NonNull::new_unchecked(ptr)
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
pub fn add_to_parents(node: DAGPtr, plink: NonNull<Parents>) {
  let parents = get_parents(node);
  match parents {
    Some(parents) => unsafe { (*parents.as_ptr()).merge(plink) },
    None => set_parents(node, Some(plink)),
  }
}

impl DAG {
  pub fn new(head: DAGPtr) -> DAG { DAG { head } }

  // Check whether a term is a root node
  pub fn is_root(&self) -> bool {
    match get_parents(self.head) {
      None => false,
      Some(pref) => unsafe { (*pref.as_ptr()).elem == ParentPtr::Root },
    }
  }

  // Remove the root parent
  pub fn uproot(&self) {
    match get_parents(self.head) {
      None => (),
      Some(pref) => unsafe {
        dealloc(pref.as_ptr() as *mut u8, Layout::new::<ParentPtr>());
        set_parents(self.head, None);
      },
    }
  }

  pub fn to_term(&self) -> Term {
    pub fn go(
      node: &DAGPtr,
      mut map: &mut HashMap<*mut Var, u64>,
      depth: u64,
    ) -> Term {
      match node {
        DAGPtr::Var(var) => {
          let Var { nam, dep: var_depth, .. } = unsafe { var.as_ref() };
          let ptr: *mut Var = var.as_ptr();
          if let Some(level) = map.get(&ptr) {
            Term::Var(None, nam.to_owned(), depth - level - 1)
          }
          else {
            Term::Var(None, nam.to_owned(), *var_depth)
          }
        }
        DAGPtr::Typ(_) => Term::Typ(None),
        DAGPtr::LTy(lty) => {
          let LTy { lty, .. } = unsafe { lty.as_ref() };
          Term::LTy(None, *lty)
        }
        DAGPtr::Lit(lit) => {
          let Lit { lit, .. } = unsafe { lit.as_ref() };
          Term::Lit(None, lit.clone())
        }
        DAGPtr::Opr(opr) => {
          let Opr { opr, .. } = unsafe { opr.as_ref() };
          Term::Opr(None, *opr)
        }
        DAGPtr::Ref(refr) => {
          let Ref { nam, exp, ast, .. } = unsafe { refr.as_ref() };
          Term::Ref(None, nam.to_owned(), *exp, *ast)
        }
        DAGPtr::Lam(lam) => {
          let Lam { var, bod, .. } = unsafe { &mut *lam.as_ptr() };
          let nam = var.nam.clone();
          let ptr: *mut Var = var;
          map.insert(ptr, depth);
          let body = go(bod, &mut map, depth + 1);
          Term::Lam(None, nam, Box::new(body))
        }
        DAGPtr::Slf(slf) => {
          let Slf { var, bod, .. } = unsafe { &mut *slf.as_ptr() };
          let nam = var.nam.clone();
          let ptr: *mut Var = var;
          map.insert(ptr, depth);
          let body = go(bod, &mut map, depth + 1);
          Term::Slf(None, nam, Box::new(body))
        }
        DAGPtr::Cse(cse) => {
          let Cse { bod, .. } = unsafe { cse.as_ref() };
          Term::Cse(None, Box::new(go(bod, &mut map, depth)))
        }
        DAGPtr::Dat(dat) => {
          let Dat { bod, .. } = unsafe { dat.as_ref() };
          Term::Dat(None, Box::new(go(bod, &mut map, depth)))
        }
        DAGPtr::App(app) => {
          let App { fun, arg, .. } = unsafe { app.as_ref() };
          Term::App(
            None,
            Box::new((go(fun, &mut map, depth), go(arg, &mut map, depth))),
          )
        }
        DAGPtr::Ann(ann) => {
          let Ann { typ, exp, .. } = unsafe { ann.as_ref() };
          Term::Ann(
            None,
            Box::new((go(typ, &mut map, depth), go(exp, &mut map, depth))),
          )
        }
        DAGPtr::All(all) => {
          let All { var, uses, dom, img, .. } = unsafe { &mut *all.as_ptr() };
          let nam = var.nam.clone();
          let ptr: *mut Var = var;
          map.insert(ptr, depth);
          Term::All(
            None,
            *uses,
            nam,
            Box::new((go(dom, &mut map, depth), go(img, &mut map, depth + 1))),
          )
        }
        _ => panic!("todo"),
      }
    }
    let mut map = HashMap::new();
    go(&self.head, &mut map, 0)
  }

  pub fn from_term(tree: &Term) -> Self {
    let root = alloc_val(DLL::singleton(ParentPtr::Root));
    DAG::from_subterm(tree, 0, Some(root))
  }

  pub fn from_subterm(
    tree: &Term,
    depth: u64,
    parents: Option<NonNull<Parents>>,
  ) -> Self {
    pub fn go(
      tree: &Term,
      depth: u64,
      mut ctx: Vector<NonNull<Var>>,
      parents: Option<NonNull<Parents>>,
    ) -> DAGPtr {
      match tree {
        Term::Var(_, name, idx) => match ctx.get(*idx as usize) {
          Some(var) => {
            let val = DAGPtr::Var(*var);
            if let Some(parents) = parents {
              DLL::concat(parents, get_parents(val));
              set_parents(val, Some(parents));
            }
            val
          }
          None => {
            let var = alloc_val(Var {
              nam: name.to_owned(),
              dep: depth - 1 - idx,
              parents,
            });
            DAGPtr::Var(var)
          }
        },
        Term::Typ(_) => DAGPtr::Typ(alloc_val(Typ { parents })),
        Term::LTy(_, lty) => DAGPtr::LTy(alloc_val(LTy { lty: *lty, parents })),
        Term::Lit(_, lit) => {
          DAGPtr::Lit(alloc_val(Lit { lit: lit.to_owned(), parents }))
        }
        Term::Opr(_, opr) => DAGPtr::Opr(alloc_val(Opr { opr: *opr, parents })),
        Term::Ref(_, nam, exp, ast) => DAGPtr::Ref(alloc_val(Ref {
          nam: nam.to_owned(),
          exp: *exp,
          ast: *ast,
          parents,
        })),
        Term::Lam(_, nam, bod) => unsafe {
          let var = Var { nam: nam.to_owned(), dep: 0, parents: None };
          let lam = alloc_val(Lam {
            var,
            bod: mem::zeroed(),
            bod_ref: mem::zeroed(),
            parents,
          });
          (*lam.as_ptr()).bod_ref = DLL::singleton(ParentPtr::LamBod(lam));
          let Lam { var, bod_ref, .. } = &mut *lam.as_ptr();
          ctx.push_front(NonNull::new_unchecked(var));
          let bod_ref = NonNull::new_unchecked(bod_ref);
          let bod = go(&**bod, depth + 1, ctx, Some(bod_ref));
          (*lam.as_ptr()).bod = bod;
          DAGPtr::Lam(lam)
        },
        Term::Slf(_, nam, bod) => unsafe {
          let var = Var { nam: nam.to_owned(), dep: 0, parents: None };
          let slf = alloc_val(Slf {
            var,
            bod: mem::zeroed(),
            bod_ref: mem::zeroed(),
            parents,
          });
          (*slf.as_ptr()).bod_ref = DLL::singleton(ParentPtr::SlfBod(slf));
          let Slf { var, bod_ref, .. } = &mut *slf.as_ptr();
          ctx.push_front(NonNull::new_unchecked(var));
          let bod_ref = NonNull::new_unchecked(bod_ref);
          let bod = go(&**bod, depth + 1, ctx, Some(bod_ref));
          (*slf.as_ptr()).bod = bod;
          DAGPtr::Slf(slf)
        },
        Term::Dat(_, bod) => unsafe {
          let dat = alloc_val(Dat {
            bod: mem::zeroed(),
            bod_ref: mem::zeroed(),
            parents,
          });
          (*dat.as_ptr()).bod_ref = DLL::singleton(ParentPtr::DatBod(dat));
          let Dat { bod_ref, .. } = &mut *dat.as_ptr();
          let bod_ref = NonNull::new_unchecked(bod_ref);
          let bod = go(&**bod, depth, ctx, Some(bod_ref));
          (*dat.as_ptr()).bod = bod;
          DAGPtr::Dat(dat)
        },
        Term::Cse(_, bod) => unsafe {
          let cse = alloc_val(Cse {
            bod: mem::zeroed(),
            bod_ref: mem::zeroed(),
            parents,
          });
          (*cse.as_ptr()).bod_ref = DLL::singleton(ParentPtr::CseBod(cse));
          let Cse { bod_ref, .. } = &mut *cse.as_ptr();
          let bod_ref = NonNull::new_unchecked(bod_ref);
          let bod = go(&**bod, depth, ctx, Some(bod_ref));
          (*cse.as_ptr()).bod = bod;
          DAGPtr::Cse(cse)
        },
        Term::All(_, uses, nam, dom_img) => unsafe {
          let (dom, img) = (**dom_img).to_owned();
          let var = Var { nam: nam.to_owned(), dep: 0, parents: None };
          let all = alloc_val(All {
            var,
            uses: *uses,
            dom: mem::zeroed(),
            img: mem::zeroed(),
            dom_ref: mem::zeroed(),
            img_ref: mem::zeroed(),
            copy: None,
            parents,
          });
          (*all.as_ptr()).dom_ref = DLL::singleton(ParentPtr::AllDom(all));
          (*all.as_ptr()).img_ref = DLL::singleton(ParentPtr::AllImg(all));
          let All { var, dom_ref, img_ref, .. } = &mut *all.as_ptr();
          let dom_ref = NonNull::new_unchecked(dom_ref);
          let img_ref = NonNull::new_unchecked(img_ref);
          let mut img_ctx = ctx.clone();
          let dom = go(&dom, depth, ctx, Some(dom_ref));
          img_ctx.push_front(NonNull::new_unchecked(var));
          let img = go(&img, depth + 1, img_ctx, Some(img_ref));
          (*all.as_ptr()).dom = dom;
          (*all.as_ptr()).img = img;
          DAGPtr::All(all)
        },
        Term::App(_, fun_arg) => unsafe {
          let (fun, arg) = (**fun_arg).to_owned();
          let app = alloc_val(App {
            fun: mem::zeroed(),
            arg: mem::zeroed(),
            fun_ref: mem::zeroed(),
            arg_ref: mem::zeroed(),
            copy: None,
            parents,
          });
          (*app.as_ptr()).fun_ref = DLL::singleton(ParentPtr::AppFun(app));
          (*app.as_ptr()).arg_ref = DLL::singleton(ParentPtr::AppArg(app));
          let App { fun_ref, arg_ref, .. } = &mut *app.as_ptr();
          let fun_ref = NonNull::new_unchecked(fun_ref);
          let arg_ref = NonNull::new_unchecked(arg_ref);
          let fun = go(&fun, depth, ctx.clone(), Some(fun_ref));
          let arg = go(&arg, depth, ctx, Some(arg_ref));
          (*app.as_ptr()).fun = fun;
          (*app.as_ptr()).arg = arg;
          DAGPtr::App(app)
        },
        Term::Ann(_, typ_exp) => unsafe {
          let (typ, exp) = (**typ_exp).to_owned();
          let ann = alloc_val(Ann {
            typ: mem::zeroed(),
            exp: mem::zeroed(),
            typ_ref: mem::zeroed(),
            exp_ref: mem::zeroed(),
            copy: None,
            parents,
          });
          (*ann.as_ptr()).typ_ref = DLL::singleton(ParentPtr::AnnTyp(ann));
          (*ann.as_ptr()).exp_ref = DLL::singleton(ParentPtr::AnnExp(ann));
          let Ann { typ_ref, exp_ref, .. } = &mut *ann.as_ptr();
          let typ_ref = NonNull::new_unchecked(typ_ref);
          let exp_ref = NonNull::new_unchecked(exp_ref);
          let typ = go(&typ, depth, ctx.clone(), Some(typ_ref));
          let exp = go(&exp, depth, ctx, Some(exp_ref));
          (*ann.as_ptr()).typ = typ;
          (*ann.as_ptr()).exp = exp;
          DAGPtr::Ann(ann)
        },
        Term::Let(_, rec, _, name, typ_exp_bod) => panic!("todo Let"),
        _ => panic!("todo"),
      }
    }
    DAG::new(go(tree, depth, Vector::new(), parents))
  }
}

impl fmt::Debug for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    #[inline]
    fn format_uplink(p: ParentPtr) -> String {
      match p {
        ParentPtr::Root => String::from("ROOT"),
        ParentPtr::LamBod(link) => format!("λB{}", link.as_ptr() as u64),
        ParentPtr::SlfBod(link) => format!("@B{}", link.as_ptr() as u64),
        ParentPtr::DatBod(link) => format!("DB{}", link.as_ptr() as u64),
        ParentPtr::CseBod(link) => format!("CB{}", link.as_ptr() as u64),
        ParentPtr::AppFun(link) => format!("AF{}", link.as_ptr() as u64),
        ParentPtr::AppArg(link) => format!("AA{}", link.as_ptr() as u64),
        ParentPtr::AllDom(link) => format!("∀D{}", link.as_ptr() as u64),
        ParentPtr::AllImg(link) => format!("∀I{}", link.as_ptr() as u64),
        ParentPtr::AnnExp(link) => format!(":E{}", link.as_ptr() as u64),
        ParentPtr::AnnTyp(link) => format!(":T{}", link.as_ptr() as u64),
        ParentPtr::LetExp(link) => format!("LE{}", link.as_ptr() as u64),
        ParentPtr::LetTyp(link) => format!("LT{}", link.as_ptr() as u64),
        ParentPtr::LetBod(link) => format!("LB{}", link.as_ptr() as u64),
      }
    }
    #[inline]
    fn format_parents(dll: Option<NonNull<Parents>>) -> String {
      match dll {
        Some(dll) => unsafe {
          let mut iter = (*dll.as_ptr()).iter();
          let head = &iter.next().map_or(String::from(""), |head| {
            format!("{}", format_uplink(*head))
          });
          let mut msg = String::from("[ ") + head;
          for val in iter {
            msg = msg + " <-> " + &format!("{}", format_uplink(*val));
          }
          msg + " ]"
        },
        _ => String::from("[]"),
      }
    }
    fn go(term: DAGPtr, set: &mut HashSet<u64>) -> String {
      match term {
        DAGPtr::Var(link) => {
          let Var { nam, parents, .. } = unsafe { link.as_ref() };
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            format!(
              "\nVar<{}> {} parents: {}",
              link.as_ptr() as u64,
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
          if set.get(&(link.as_ptr() as u64)).is_none() {
            let Lam { var, parents, bod, .. } = unsafe { link.as_ref() };
            let name = var.nam.clone();
            set.insert(link.as_ptr() as u64);
            format!(
              "\nLam<{:?}> {} parents: {}{}",
              link.as_ptr(),
              name,
              format_parents(*parents),
              go(*bod, set)
            )
          }
          else {
            format!("\nSHARE<{}>", link.as_ptr() as u64)
          }
        }
        DAGPtr::Slf(link) => {
          if set.get(&(link.as_ptr() as u64)).is_none() {
            let Slf { var, parents, bod, .. } = unsafe { link.as_ref() };
            let name = var.nam.clone();
            set.insert(link.as_ptr() as u64);
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
          if set.get(&(link.as_ptr() as u64)).is_none() {
            let Dat { parents, bod, .. } = unsafe { link.as_ref() };
            set.insert(link.as_ptr() as u64);
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
          if set.get(&(link.as_ptr() as u64)).is_none() {
            let Cse { parents, bod, .. } = unsafe { link.as_ref() };
            set.insert(link.as_ptr() as u64);
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
        // DAGPtr::Fix(link) => {
        //  if set.get(&(link.as_ptr() as u64)).is_none() {
        //    let Fix { parents, bod, .. } = unsafe { link.as_ref() };
        //    set.insert(link.as_ptr() as u64);
        //    format!(
        //      "\nFix<{:?}> parents: {}{}",
        //      link.as_ptr(),
        //      format_parents(*parents),
        //      go(*bod, set)
        //    )
        //  }
        //  else {
        //    format!("\nSHARE<{:?}>", link.as_ptr())
        //  }
        //}
        DAGPtr::App(link) => {
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            let App { fun, arg, parents, copy, .. } = unsafe { link.as_ref() };
            let copy = copy.map(|link| link.as_ptr() as u64);
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
            format!("\nSHARE<{}>", link.as_ptr() as u64)
          }
        }
        DAGPtr::Ann(link) => {
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            let Ann { typ, exp, parents, copy, .. } = unsafe { link.as_ref() };
            let copy = copy.map(|link| link.as_ptr() as u64);
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
            format!("\nSHARE<{}>", link.as_ptr() as u64)
          }
        }
        DAGPtr::All(link) => {
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            let All { var, dom, img, parents, copy, .. } =
              unsafe { link.as_ref() };
            let name = var.nam.clone();
            let copy = copy.map(|link| link.as_ptr() as u64);
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
            format!("\nSHARE<{}>", link.as_ptr() as u64)
          }
        }
        DAGPtr::Let(link) => {
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            let Let { var, exp, typ, bod, parents, copy, .. } =
              unsafe { link.as_ref() };
            let name = var.nam.clone();
            let copy = copy.map(|link| link.as_ptr() as u64);
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
            format!("\nSHARE<{}>", link.as_ptr() as u64)
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

#[cfg(test)]
mod test {
  use super::*;
  use crate::parse::term::parse;

  #[test]
  fn test_cases() {
    let (_, x) = parse("λ _z => Type").unwrap();
    assert_eq!(x, DAG::to_term(&DAG::from_term(&x)));
    let (_, x) = parse("λ z => z").unwrap();
    assert_eq!(x, DAG::to_term(&DAG::from_term(&x)));
    let (_, x) =
      parse("λ _z => (λ _a => ∀ (1 _x: _a) -> #Natural) Type").unwrap();
    assert_eq!(x, DAG::to_term(&DAG::from_term(&x)));
  }

  #[quickcheck]
  fn term_encode_decode(x: Term) -> bool {
    println!("x: {}", x);
    println!("x: {:?}", x);
    let y = DAG::to_term(&DAG::from_term(&x));
    println!("y: {}", y);
    println!("y: {:?}", y);
    x == y
  }
}
