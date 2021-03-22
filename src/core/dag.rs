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
type Parents = DLL<ParentPtr>;

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
  pub var: NonNull<Var>,
  pub bod: DAGPtr,
  pub bod_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct App {
  pub fun: DAGPtr,
  pub arg: DAGPtr,
  pub fun_ref: NonNull<Parents>,
  pub arg_ref: NonNull<Parents>,
  pub copy: Option<NonNull<App>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct All {
  pub var: NonNull<Var>,
  pub uses: Uses,
  pub dom: DAGPtr,
  pub img: DAGPtr,
  pub dom_ref: NonNull<Parents>,
  pub img_ref: NonNull<Parents>,
  pub copy: Option<NonNull<All>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Slf {
  pub var: NonNull<Var>,
  pub bod: DAGPtr,
  pub bod_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Dat {
  pub bod: DAGPtr,
  pub bod_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Cse {
  pub bod: DAGPtr,
  pub bod_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Ann {
  pub typ: DAGPtr,
  pub exp: DAGPtr,
  pub exp_ref: NonNull<Parents>,
  pub typ_ref: NonNull<Parents>,
  pub copy: Option<NonNull<Ann>>,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Let {
  pub var: NonNull<Var>,
  pub exp: DAGPtr,
  pub typ: DAGPtr,
  pub bod: DAGPtr,
  pub exp_ref: NonNull<Parents>,
  pub typ_ref: NonNull<Parents>,
  pub bod_ref: NonNull<Parents>,
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

  pub fn to_term(&self) -> Term {
    let mut map: HashMap<*mut Var, u64> = HashMap::new();

    pub fn go(
      node: &DAGPtr,
      mut map: &mut HashMap<*mut Var, u64>,
      depth: u64,
    ) -> Term {
      match node {
        DAGPtr::Var(var) => {
          let Var { nam, .. } = unsafe { var.as_ref() };
          let ptr: *mut Var = var.as_ptr();
          let level = map.get(&ptr).unwrap();
          Term::Var(None, nam.to_owned(), depth - level - 1)
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
          let Lam { var, bod, .. } = unsafe { lam.as_ref() };
          let nam = unsafe { var.as_ref().nam.clone() };
          let ptr: *mut Var = var.as_ptr();
          map.insert(ptr, depth);
          let body = go(bod, &mut map, depth + 1);
          Term::Lam(None, nam, Box::new(body))
        }
        DAGPtr::Slf(slf) => {
          let Slf { var, bod, .. } = unsafe { slf.as_ref() };
          let nam = unsafe { var.as_ref().nam.clone() };
          let ptr: *mut Var = var.as_ptr();
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
          let All { var, uses, dom, img, .. } = unsafe { all.as_ref() };
          let nam = unsafe { var.as_ref().nam.clone() };
          let ptr: *mut Var = var.as_ptr();
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
    go(&self.head, &mut map, 0)
  }

  pub fn from_term(tree: &Term) -> Self {
    let root = alloc_val(DLL::singleton(ParentPtr::Root));
    DAG::from_subterm(tree, 0, &Vector::new(), Vector::new(), Some(root))
  }

  pub fn from_subterm(
    tree: &Term,
    depth: u64,
    abs_ctx: &Vector<NonNull<Var>>,
    rel_ctx: Vector<NonNull<Var>>,
    parents: Option<NonNull<Parents>>,
  ) -> Self {
    pub fn go(
      tree: &Term,
      depth: u64,
      abs_ctx: &Vector<NonNull<Var>>,
      mut rel_ctx: Vector<NonNull<Var>>,
      parents: Option<NonNull<Parents>>,
    ) -> DAGPtr {
      match tree {
        Term::Var(_, name, idx) => match rel_ctx.get(*idx as usize) {
          Some(var) => {
            let val = DAGPtr::Var(*var);
            if let Some(parents) = parents {
              DLL::concat(parents, get_parents(val));
              set_parents(val, Some(parents));
            }
            val
          }
          None => match abs_ctx.get((depth - 1 - *idx) as usize) {
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
        Term::Lam(_, nam, bod) => {
          let var =
            alloc_val(Var { nam: nam.to_owned(), dep: 0, parents: None });
          rel_ctx.push_front(var);
          let bod_parents = alloc_uninit();
          let lam = alloc_val(Lam {
            var,
            bod: DAGPtr::Var(NonNull::dangling()),
            bod_ref: bod_parents,
            parents,
          });
          unsafe {
            *bod_parents.as_ptr() = DLL::singleton(ParentPtr::LamBod(lam));
          }
          let bod = go(&**bod, depth + 1, abs_ctx, rel_ctx, Some(bod_parents));
          unsafe {
            (*lam.as_ptr()).bod = bod;
          }
          DAGPtr::Lam(lam)
        }
        Term::Slf(_, nam, bod) => {
          let var =
            alloc_val(Var { nam: nam.to_owned(), dep: 0, parents: None });
          rel_ctx.push_front(var);
          let bod_parents = alloc_uninit();
          let slf = alloc_val(Slf {
            var,
            bod: DAGPtr::Var(NonNull::dangling()),
            bod_ref: bod_parents,
            parents,
          });
          unsafe {
            *bod_parents.as_ptr() = DLL::singleton(ParentPtr::SlfBod(slf));
          }
          let bod = go(&**bod, depth + 1, abs_ctx, rel_ctx, Some(bod_parents));
          unsafe {
            (*slf.as_ptr()).bod = bod;
          }
          DAGPtr::Slf(slf)
        }
        Term::Dat(_, bod) => {
          let bod_parents = alloc_uninit();
          let dat = alloc_val(Dat {
            bod: DAGPtr::Var(NonNull::dangling()),
            bod_ref: bod_parents,
            parents,
          });
          unsafe {
            *bod_parents.as_ptr() = DLL::singleton(ParentPtr::DatBod(dat));
          }
          let bod = go(&**bod, depth, abs_ctx, rel_ctx, Some(bod_parents));
          unsafe {
            (*dat.as_ptr()).bod = bod;
          }
          DAGPtr::Dat(dat)
        }
        Term::Cse(_, bod) => {
          let bod_parents = alloc_uninit();
          let cse = alloc_val(Cse {
            bod: DAGPtr::Var(NonNull::dangling()),
            bod_ref: bod_parents,
            parents,
          });
          unsafe {
            *bod_parents.as_ptr() = DLL::singleton(ParentPtr::CseBod(cse));
          }
          let bod = go(&**bod, depth, abs_ctx, rel_ctx, Some(bod_parents));
          unsafe {
            (*cse.as_ptr()).bod = bod;
          }
          DAGPtr::Cse(cse)
        }
        Term::All(_, uses, nam, dom_img) => {
          let (dom, img) = (**dom_img).to_owned();
          let var =
            alloc_val(Var { nam: nam.to_owned(), dep: 0, parents: None });
          let dom_parents = alloc_uninit();
          let img_parents = alloc_uninit();
          let all = alloc_val(All {
            var,
            uses: *uses,
            dom: DAGPtr::Var(NonNull::dangling()),
            img: DAGPtr::Var(NonNull::dangling()),
            dom_ref: dom_parents,
            img_ref: img_parents,
            copy: None,
            parents,
          });
          unsafe {
            *dom_parents.as_ptr() = DLL::singleton(ParentPtr::AllDom(all));
            *img_parents.as_ptr() = DLL::singleton(ParentPtr::AllImg(all));
          }
          let mut img_ctx = rel_ctx.clone();
          let dom = go(&dom, depth, abs_ctx, rel_ctx, Some(dom_parents));
          img_ctx.push_front(var);
          let img = go(&img, depth + 1, abs_ctx, img_ctx, Some(img_parents));
          unsafe {
            (*all.as_ptr()).dom = dom;
            (*all.as_ptr()).img = img;
          }
          DAGPtr::All(all)
        }
        Term::App(_, fun_arg) => {
          let (fun, arg) = (**fun_arg).to_owned();
          let fun_parents = alloc_uninit();
          let arg_parents = alloc_uninit();
          let app = alloc_val(App {
            fun: DAGPtr::Var(NonNull::dangling()),
            arg: DAGPtr::Var(NonNull::dangling()),
            fun_ref: fun_parents,
            arg_ref: arg_parents,
            copy: None,
            parents,
          });
          unsafe {
            *fun_parents.as_ptr() = DLL::singleton(ParentPtr::AppFun(app));
            *arg_parents.as_ptr() = DLL::singleton(ParentPtr::AppArg(app));
          }
          let fun =
            go(&fun, depth, abs_ctx, rel_ctx.clone(), Some(fun_parents));
          let arg = go(&arg, depth, abs_ctx, rel_ctx, Some(arg_parents));
          unsafe {
            (*app.as_ptr()).fun = fun;
            (*app.as_ptr()).arg = arg;
          }
          DAGPtr::App(app)
        }
        Term::Ann(_, typ_exp) => {
          let (typ, exp) = (**typ_exp).to_owned();
          let typ_parents = alloc_uninit();
          let exp_parents = alloc_uninit();
          let ann = alloc_val(Ann {
            typ: DAGPtr::Var(NonNull::dangling()),
            exp: DAGPtr::Var(NonNull::dangling()),
            typ_ref: typ_parents,
            exp_ref: exp_parents,
            copy: None,
            parents,
          });
          unsafe {
            *typ_parents.as_ptr() = DLL::singleton(ParentPtr::AnnTyp(ann));
            *exp_parents.as_ptr() = DLL::singleton(ParentPtr::AnnExp(ann));
          }
          let typ =
            go(&typ, depth, abs_ctx, rel_ctx.clone(), Some(typ_parents));
          let exp = go(&exp, depth, abs_ctx, rel_ctx, Some(exp_parents));
          unsafe {
            (*ann.as_ptr()).typ = typ;
            (*ann.as_ptr()).exp = exp;
          }
          DAGPtr::Ann(ann)
        }
        Term::Let(_, rec, _, name, typ_exp_bod) => panic!("todo Let"),
        _ => panic!("todo"),
      }
    }
    DAG::new(go(tree, depth, abs_ctx, rel_ctx, parents))
  }
}

// impl fmt::Debug for DAG {
//   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//     #[inline]
//     fn format_uplink(p: ParentPtr) -> String {
//       match p {
//         ParentPtr::Root => String::from("ROOT"),
//         ParentPtr::Body(link) => format!("B{}", link.as_ptr() as u64),
//         ParentPtr::Left(link) => format!("L{}", link.as_ptr() as u64),
//         ParentPtr::Right(link) => format!("R{}", link.as_ptr() as u64),
//       }
//     }
//     #[inline]
//     fn format_parents(dll: Option<NonNull<Parents>>) -> String {
//       match dll {
//         Some(dll) => unsafe {
//           let mut iter = (*dll.as_ptr()).iter();
//           let head = &iter.next().map_or(String::from(""), |head| {
//             format!("{}", format_uplink(*head))
//           });
//           let mut msg = String::from("[ ") + head;
//           for val in iter {
//             msg = msg + " <-> " + &format!("{}", format_uplink(*val));
//           }
//           msg + " ]"
//         },
//         _ => String::from("[]"),
//       }
//     }
//     fn go(term: DAGPtr, set: &mut HashSet<u64>) -> String {
//       match term {
//         DAGPtr::Branch(link) => unsafe {
//           if set.get(&(link.as_ptr() as u64)).is_none() {
//             set.insert(link.as_ptr() as u64);
//             let Branch { tag, var, parents, left, right, copy, .. } =
// *link.as_ptr();             let branch = match tag {
//               BranchTag::App => format!("App"),
//               BranchTag::All(_) => format!("All"),
//               BranchTag::Let => format!("Let"),
//             };
//             let name = match var {
//               Some(var_link) => format!("{} ",
// (*var_link.as_ptr()).name.clone()),               _ => String::from(""),
//             };
//             let copy = copy.map(|link| link.as_ptr() as u64);
//             format!(
//               "\n{}<{}> {}parents: {} copy: {:?}{}{}",
//               branch,
//               link.as_ptr() as u64,
//               name,
//               format_parents(parents),
//               copy,
//               go(left, set),
//               go(right, set)
//             )
//           }
//           else {
//             format!("\nSHARE<{}>", link.as_ptr() as u64)
//           }
//         },
//         DAGPtr::Single(link) => unsafe {
//           let Single { tag, var, parents, body, .. } = *link.as_ptr();
//           let single = match tag {
//             SingleTag::Lam => format!("Lam"),
//             SingleTag::Slf => format!("Slf"),
//             SingleTag::Cse => format!("Cse"),
//             SingleTag::Dat => format!("Dat"),
//             SingleTag::Fix => format!("Fix"),
//           };
//           let name = match var {
//             Some(var_link) => format!("{} ",
// (*var_link.as_ptr()).name.clone()),             _ => String::from(""),
//           };
//           if set.get(&(link.as_ptr() as u64)).is_none() {
//             set.insert(link.as_ptr() as u64);
//             format!(
//               "\n{}<{}> {}parents: {}{}",
//               single,
//               link.as_ptr() as u64,
//               name,
//               format_parents(parents),
//               go(body, set)
//             )
//           }
//           else {
//             format!("\nSHARE<{}>", link.as_ptr() as u64)
//           }
//         },
//         DAGPtr::Var(link) => unsafe {
//           let Var { parents, name, .. } = &*link.as_ptr();
//           if set.get(&(link.as_ptr() as u64)).is_none() {
//             set.insert(link.as_ptr() as u64);
//             format!(
//               "\nVar<{}> {} parents: {}",
//               link.as_ptr() as u64,
//               name,
//               format_parents(*parents)
//             )
//           }
//           else {
//             format!("\nSHARE<{}>", link.as_ptr() as u64)
//           }
//         },
//         DAGPtr::Leaf(link) => unsafe {
//           let Leaf { parents, tag, .. } = &*link.as_ptr();
//           let leaf = match tag {
//             LeafTag::Typ => format!("Typ<{}>", link.as_ptr() as u64),
//             LeafTag::LTy(lty) => format!("LTy<{}>", link.as_ptr() as u64),
//             LeafTag::Lit(lit) => format!("Lit<{}>", link.as_ptr() as u64),
//             LeafTag::Opr(opr) => format!("Opr<{}>", link.as_ptr() as u64),
//             LeafTag::Ref(nam, _def_link, _ast_link) => format!("Ref<{}> {}",
// link.as_ptr() as u64, nam),           };
//           format!("\n{} parents: {}", leaf, format_parents(*parents))
//         },
//       }
//     }
//     write!(f, "{}", go(self.head, &mut HashSet::new()))
//   }
// }

//   // Check whether a term is a root node
//   pub fn is_root(&self) -> bool {
//     match get_parents(self.head){
//       None => false,
//       Some(pref) => unsafe {
//         (*pref.as_ptr()).elem == ParentPtr::Root
//       },
//     }
//   }

//   // Remove the root parent
//   pub fn uproot(&self){
//     match get_parents(self.head) {
//       None => (),
//       Some(pref) => unsafe {
//         dealloc(pref.as_ptr() as *mut u8, Layout::new::<ParentPtr>());
//         set_parents(self.head, None);
//       },
//     }
//   }
// }

impl fmt::Display for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_term())
  }
}

// // DAG is not isomorphic to Term anymore
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
