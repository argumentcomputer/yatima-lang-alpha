#![allow(unused_variables)]

// Bottom-up reduction of lambda DAGs. Based on the paper by Olin Shivers and Mitchel Wand
// "Bottom-up β-reduction: uplinks and λ-DAGs" (https://www.brics.dk/RS/04/38/BRICS-RS-04-38.pdf)

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
  pub head: DAGPtr
}

// A top-down λ-DAG pointer. Keeps track of what kind of node it points to.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum DAGPtr {
  Var(NonNull<Var>),
  Leaf(NonNull<Leaf>),
  Single(NonNull<Single>),
  Branch(NonNull<Branch>),
}

// Doubly-linked list of parent nodes
type Parents = DLL<ParentPtr>;

// A bottom-up (parent) λ-DAG pointer. Keeps track of the relation between
// the child and the parent.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParentPtr {
  Root,
  Body(NonNull<Single>),
  Left(NonNull<Branch>),
  Right(NonNull<Branch>),
}

// The λ-DAG nodes
pub struct Var {
  pub name: String,
  // The field `depth` is only used by the type checker to track free variables.
  // Otherwise it is irrelevant.
  pub depth: u64,
  pub parents: Option<NonNull<Parents>>,
}

pub struct Leaf {
  pub tag: LeafTag,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Clone)]
pub enum LeafTag {
  Typ,
  LTy(LitType),
  Lit(Literal),
  Opr(PrimOp),
  Ref(String, Link, Link),
}

pub struct Single {
  pub var: Option<NonNull<Var>>,
  pub tag: SingleTag,
  pub body: DAGPtr,
  pub body_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SingleTag {
  Lam,
  Slf,
  Cse,
  Dat,
  Fix,
}

pub struct Branch {
  pub var: Option<NonNull<Var>>,
  pub tag: BranchTag,
  pub left: DAGPtr,
  pub right: DAGPtr,
  pub left_ref: NonNull<Parents>,
  pub right_ref: NonNull<Parents>,
  pub copy: Option<NonNull<Branch>>,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BranchTag {
  App,
  Let,
  All(Uses),
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

#[inline]
fn alloc_single(
  var: Option<NonNull<Var>>,
  tag: SingleTag,
  parents: Option<NonNull<Parents>>
) -> NonNull<Single> {
  // Allocate nodes
  let body_parents = alloc_uninit();
  let single = alloc_val(Single {
    var,
    tag,
    // Temporary, dangling DAG pointer
    body: DAGPtr::Leaf(NonNull::dangling()),
    body_ref: body_parents,
    parents,
  });
  // Update `body_parents` to refer to current node
  unsafe {
    *body_parents.as_ptr() = DLL::singleton(ParentPtr::Body(single));
  }
  single
}

#[inline]
fn alloc_branch(
  var: Option<NonNull<Var>>,
  tag: BranchTag,
  parents: Option<NonNull<Parents>>
) -> NonNull<Branch>{
  // Allocation and updates
  let left_parents = alloc_uninit();
  let right_parents = alloc_uninit();
  let branch = alloc_val(Branch {
    var,
    tag,
    // Temporary, dangling DAG pointers
    left: DAGPtr::Leaf(NonNull::dangling()),
    right: DAGPtr::Leaf(NonNull::dangling()),
    left_ref: left_parents,
    right_ref: right_parents,
    copy: None,
    parents,
  });
  unsafe {
    *left_parents.as_ptr() = DLL::singleton(ParentPtr::Left(branch));
    *right_parents.as_ptr() = DLL::singleton(ParentPtr::Right(branch));
  }
  branch
}

// Auxiliary parent functions
#[inline]
pub fn get_parents(term: DAGPtr) -> Option<NonNull<Parents>> {
  unsafe {
    match term {
      DAGPtr::Leaf(link) => (*link.as_ptr()).parents,
      DAGPtr::Var(link) => (*link.as_ptr()).parents,
      DAGPtr::Single(link) => (*link.as_ptr()).parents,
      DAGPtr::Branch(link) => (*link.as_ptr()).parents,
    }
  }
}

#[inline]
pub fn set_parents(term: DAGPtr, pref: Option<NonNull<Parents>>) {
  unsafe {
    match term {
      DAGPtr::Leaf(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Var(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Single(link) => (*link.as_ptr()).parents = pref,
      DAGPtr::Branch(link) => (*link.as_ptr()).parents = pref,
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

impl Clone for DAG {
  fn clone(&self) -> Self {
    fn go(
      node: DAGPtr,
      map: &mut HashMap<DAGPtr, DAGPtr>,
      parents: NonNull<Parents>,
    ) -> DAGPtr {
      // If the node is in the hash map then it was already copied,
      // so we update the parent list and return the copy
      match (*map).get(&node) {
        Some(copy) => {
          DLL::concat(parents, get_parents(*copy));
          set_parents(*copy, Some(parents));
          return *copy
        },
        None => (),
      }
      // Otherwise create a new DAG node and add it to the map
      let new_node = match node {
        DAGPtr::Leaf(link) => unsafe {
          let Leaf {tag, ..} = &(*link.as_ptr());
          let leaf = alloc_val(Leaf {
            tag: tag.clone(),
            parents: Some(parents)
          });
          DAGPtr::Leaf(leaf)
        }
        DAGPtr::Var(link) => unsafe {
          let Var {name, depth, ..} = &(*link.as_ptr());
          let var = alloc_val(Var {
            name: name.clone(),
            depth: *depth,
            parents: Some(parents)
          });
          DAGPtr::Var(var)
        }
        DAGPtr::Single(link) => unsafe {
          let Single {var, body, tag, ..} = *link.as_ptr();
          let var = var.map(|link| {
            let Var {name, depth, ..} = &*link.as_ptr();
            let new_var = alloc_val(Var { name: name.clone(), depth: *depth, parents: None });
            map.insert(DAGPtr::Var(link), DAGPtr::Var(new_var));
            new_var
          });
          let single = alloc_single(var, tag.clone(), Some(parents));
          // Update single's body to point to a valid node
          (*single.as_ptr()).body = go(body, map, (*single.as_ptr()).body_ref);

          DAGPtr::Single(single)
        }
        DAGPtr::Branch(link) => unsafe {
          let Branch {var, left, right, tag, ..} = *link.as_ptr();
          let var = var.map(|link| {
            let Var {name, depth, ..} = &*link.as_ptr();
            let new_var = alloc_val(Var { name: name.clone(), depth: *depth, parents: None });
            map.insert(DAGPtr::Var(link), DAGPtr::Var(new_var));
            new_var
          });
          let branch = alloc_branch(var, tag.clone(), Some(parents));
          (*branch.as_ptr()).left = go(left, map, (*branch.as_ptr()).left_ref);
          (*branch.as_ptr()).right = go(right, map, (*branch.as_ptr()).right_ref);
          DAGPtr::Branch(branch)
        }
      };
      // Map `node` to `new_node`
      map.insert(node, new_node);
      new_node
    }
    let mut map: HashMap<DAGPtr, DAGPtr> = HashMap::new();
    let root = alloc_val(DLL::singleton(ParentPtr::Root));
    DAG::new(go(self.head, &mut map, root))
  }
}

impl fmt::Debug for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    #[inline]
    fn format_uplink(p: ParentPtr) -> String {
      match p {
        ParentPtr::Root => String::from("ROOT"),
        ParentPtr::Body(link) => format!("B{}", link.as_ptr() as u64),
        ParentPtr::Left(link) => format!("L{}", link.as_ptr() as u64),
        ParentPtr::Right(link) => format!("R{}", link.as_ptr() as u64),
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
        DAGPtr::Branch(link) => unsafe {
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            let Branch { tag, var, parents, left, right, copy, .. } = *link.as_ptr();
            let branch = match tag {
              BranchTag::App => format!("App"),
              BranchTag::All(_) => format!("All"),
              BranchTag::Let => format!("Let"),
            };
            let name = match var {
              Some(var_link) => format!("{} ", (*var_link.as_ptr()).name.clone()),
              _ => String::from(""),
            };
            let copy = copy.map(|link| link.as_ptr() as u64);
            format!(
              "\n{}<{}> {}parents: {} copy: {:?}{}{}",
              branch,
              link.as_ptr() as u64,
              name,
              format_parents(parents),
              copy,
              go(left, set),
              go(right, set)
            )
          }
          else {
            format!("\nSHARE<{}>", link.as_ptr() as u64)
          }
        },
        DAGPtr::Single(link) => unsafe {
          let Single { tag, var, parents, body, .. } = *link.as_ptr();
          let single = match tag {
            SingleTag::Lam => format!("Lam"),
            SingleTag::Slf => format!("Slf"),
            SingleTag::Cse => format!("Cse"),
            SingleTag::Dat => format!("Dat"),
            SingleTag::Fix => format!("Fix"),
          };
          let name = match var {
            Some(var_link) => format!("{} ", (*var_link.as_ptr()).name.clone()),
            _ => String::from(""),
          };
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            format!(
              "\n{}<{}> {}parents: {}{}",
              single,
              link.as_ptr() as u64,
              name,
              format_parents(parents),
              go(body, set)
            )
          }
          else {
            format!("\nSHARE<{}>", link.as_ptr() as u64)
          }
        },
        DAGPtr::Var(link) => unsafe {
          let Var { parents, name, .. } = &*link.as_ptr();
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            format!(
              "\nVar<{}> {} parents: {}",
              link.as_ptr() as u64,
              name,
              format_parents(*parents)
            )
          }
          else {
            format!("\nSHARE<{}>", link.as_ptr() as u64)
          }
        },
        DAGPtr::Leaf(link) => unsafe {
          let Leaf { parents, tag, .. } = &*link.as_ptr();
          let leaf = match tag {
            LeafTag::Typ => format!("Typ<{}>", link.as_ptr() as u64),
            LeafTag::LTy(lty) => format!("LTy<{}>", link.as_ptr() as u64),
            LeafTag::Lit(lit) => format!("Lit<{}>", link.as_ptr() as u64),
            LeafTag::Opr(opr) => format!("Opr<{}>", link.as_ptr() as u64),
            LeafTag::Ref(nam, _def_link, _ast_link) => format!("Ref<{}> {}", link.as_ptr() as u64, nam),
          };
          format!("\n{} parents: {}", leaf, format_parents(*parents))
        },
      }
    }
    write!(f, "{}", go(self.head, &mut HashSet::new()))
  }
}

impl DAG {
  pub fn new(head: DAGPtr) -> DAG {
    DAG { head }
  }

  pub fn to_term(&self) -> Term {
    fn go(
      node: DAGPtr,
      mut map: &mut HashMap<*mut Var, u64>,
      depth: u64,
    ) -> Term {
      match node {
        DAGPtr::Leaf(link) => {
          let Leaf { tag, .. } = unsafe { &*link.as_ptr() };
          match tag {
            LeafTag::Typ => Term::Typ(None),
            LeafTag::LTy(lty) => Term::LTy(None, *lty),
            LeafTag::Lit(lit) => Term::Lit(None, lit.clone()),
            LeafTag::Opr(opr) => Term::Opr(None, *opr),
            LeafTag::Ref(nam, def_link, ast_link) => {
              Term::Ref(None, nam.to_owned(), *def_link, *ast_link)
            }
          }
        }
        DAGPtr::Var(link) => {
          let Var { name, depth: var_depth, .. } = unsafe { &*link.as_ptr() };
          if let Some(level) = map.get(&link.as_ptr()){
            Term::Var(None, name.to_owned(), depth - level - 1)
          }
          else {
            Term::Var(None, name.to_owned(), *var_depth)
          }
        }

        DAGPtr::Single(link) => {
          let Single { tag, body, var, .. } = unsafe { &*link.as_ptr() };
          match var {
            Some(var_link) => {
              let Var { name, .. } = unsafe { &*var_link.as_ptr() };
              match tag {
                SingleTag::Lam => {
                  map.insert(var_link.as_ptr(), depth);
                  let body = go(*body, &mut map, depth + 1);
                  Term::Lam(None, name.clone(), Box::new(body))
                }
                SingleTag::Slf => {
                  map.insert(var_link.as_ptr(), depth);
                  let body = go(*body, &mut map, depth + 1);
                  Term::Slf(None, name.clone(), Box::new(body))
                }
                SingleTag::Fix => panic!("Fix TODO"),
                _ => panic!("Malformed DAG."),
              }
            }
            None => match tag {
              SingleTag::Cse => {
                Term::Cse(None, Box::new(go(*body, &mut map, depth)))
              }
              SingleTag::Dat => {
                Term::Dat(None, Box::new(go(*body, &mut map, depth)))
              }
              _ => panic!("Malformed DAG."),
            },
          }
        }
        DAGPtr::Branch(link) => {
          let Branch { tag, left, right, var, .. } = unsafe { &*link.as_ptr() };
          match var {
            Some(var_link) => {
              let Var { name, .. } = unsafe { &*var_link.as_ptr() };
              match tag {
                BranchTag::All(uses) => {
                  map.insert(var_link.as_ptr(), depth);
                  let dom = go(*left, &mut map, depth);
                  let img = go(*right, &mut map, depth + 1);
                  Term::All(
                    None,
                    *uses,
                    name.clone(),
                    Box::new(dom),
                    Box::new(img),
                  )
                }
                _ => panic!("Malformed DAG."),
              }
            }
            None => match tag {
              BranchTag::App => {
                let fun = go(*left, &mut map, depth);
                let arg = go(*right, &mut map, depth);
                Term::App(None, Box::new(fun), Box::new(arg))
              }
              BranchTag::Let => panic!("Let TODO"),
              _ => panic!("Malformed DAG."),
            },
          }
        }
      }
    }
    let mut map: HashMap<*mut Var, u64> = HashMap::new();
    go(self.head, &mut map, 0)
  }

  pub fn from_term(tree: &Term) -> Self {
    let root = alloc_val(DLL::singleton(ParentPtr::Root));
    DAG::from_subterm(tree, 0, &Vector::new(), Vector::new(), Some(root))
  }

  // Converts an open subterm to DAG and add de Bruijn levels to free `Var` nodes. The argument `depth` is the
  // initial depth of the subterm. Takes initial relative and absolute contexts (assumes they do not overlap).
  pub fn from_subterm(
    tree: &Term,
    depth: u64,
    abs_ctx: &Vector<DAGPtr>,
    rel_ctx: Vector<DAGPtr>,
    parents: Option<NonNull<Parents>>,
  ) -> Self {
    pub fn go(
      tree: &Term,
      depth: u64,
      abs_ctx: &Vector<DAGPtr>,
      mut rel_ctx: Vector<DAGPtr>,
      parents: Option<NonNull<Parents>>,
    ) -> DAGPtr {
      match tree {
        Term::Lam(_, name, body) => unsafe {
          let var = alloc_val(Var { name: name.clone(), depth: 0, parents: None });
          let lam = alloc_single(Some(var), SingleTag::Lam, parents);
          let body_parents = (*lam.as_ptr()).body_ref;
          rel_ctx.push_front(DAGPtr::Var(var));
          let body = go(&**body, depth+1, abs_ctx, rel_ctx, Some(body_parents));
          // Update `lam` with the correct fields
          (*lam.as_ptr()).body = body;
          DAGPtr::Single(lam)
        }
        
        Term::Slf(_, name, body) => unsafe {
          let var = alloc_val(Var { name: name.clone(), depth: 0, parents: None });
          let slf = alloc_single(Some(var), SingleTag::Slf, parents);
          let body_parents = (*slf.as_ptr()).body_ref;
          rel_ctx.push_front(DAGPtr::Var(var));
          let body = go(&**body, depth+1, abs_ctx, rel_ctx, Some(body_parents));
          (*slf.as_ptr()).body = body;
          DAGPtr::Single(slf)
        }
        Term::Dat(_, body) => unsafe {
          let dat = alloc_single(None, SingleTag::Dat, parents);
          let body_parents = (*dat.as_ptr()).body_ref;
          let body = go(&**body, depth, abs_ctx, rel_ctx, Some(body_parents));
          (*dat.as_ptr()).body = body;
          DAGPtr::Single(dat)
        }
        Term::Cse(_, body) => unsafe {
          let cse = alloc_single(None, SingleTag::Cse, parents);
          let body_parents = (*cse.as_ptr()).body_ref;
          let body = go(&**body, depth, abs_ctx, rel_ctx, Some(body_parents));
          (*cse.as_ptr()).body = body;
          DAGPtr::Single(cse)
        }
        
        Term::All(_, uses, name, dom, img) => unsafe {
          let var = alloc_val(Var { name: name.clone(), depth: 0, parents: None });
          let all = alloc_branch(Some(var), BranchTag::All(*uses), parents);
          let mut img_ctx = rel_ctx.clone();
          let dom_parents = (*all.as_ptr()).left_ref;
          let dom = go(&**dom, depth, abs_ctx, rel_ctx, Some(dom_parents));
          img_ctx.push_front(DAGPtr::Var(var));
          let img_parents = (*all.as_ptr()).right_ref;
          let img = go(&**img, depth+1, abs_ctx, img_ctx, Some(img_parents));
          // Update `all` with the correct fields
          (*all.as_ptr()).left = dom;
          (*all.as_ptr()).right = img;
          DAGPtr::Branch(all)
        }
        
        Term::App(_, fun, arg) => unsafe {
          let app = alloc_branch(None, BranchTag::App, parents);
          let fun_parents = (*app.as_ptr()).left_ref;
          let fun = go(&**fun, depth, abs_ctx, rel_ctx.clone(), Some(fun_parents));
          let arg_parents = (*app.as_ptr()).right_ref;
          let arg = go(&**arg, depth, abs_ctx, rel_ctx, Some(arg_parents));
          (*app.as_ptr()).left = fun;
          (*app.as_ptr()).right = arg;
          DAGPtr::Branch(app)
        }
        Term::Let(_, rec, _, name, _, expr, body) => unsafe {
          if *rec {
            panic!("TODO: Add letrec")
          }
          let var = alloc_val(Var { name: name.clone(), depth: 0, parents: None });
          let let_node = alloc_branch(Some(var), BranchTag::Let, parents);
          let mut body_ctx = rel_ctx.clone();
          let expr_parents = (*let_node.as_ptr()).left_ref;
          let expr = go(&**expr, depth, abs_ctx, rel_ctx, Some(expr_parents));
          body_ctx.push_front(DAGPtr::Var(var));
          let body_parents = (*let_node.as_ptr()).right_ref;
          let body = go(&**body, depth+1, abs_ctx, body_ctx, Some(body_parents));
          (*let_node.as_ptr()).left = expr;
          (*let_node.as_ptr()).right = body;
          DAGPtr::Branch(let_node)
        }
        
        Term::Var(_, name, idx) => {
          match rel_ctx.get(*idx as usize) {
            Some(val) => {
              if let Some(parents) = parents {
                DLL::concat(parents, get_parents(*val));
                set_parents(*val, Some(parents));
              }
              *val
            },
            None => {
              let lvl = depth-1-*idx;
              match abs_ctx.get(lvl as usize) {
                Some(val) => {
                  if let Some(parents) = parents {
                    DLL::concat(parents, get_parents(*val));
                    set_parents(*val, Some(parents));
                  }
                  *val
                },
                None => {
                  let var = alloc_val(Var {
                    name: name.clone(),
                    // de Bruijn levels
                    depth: depth-1-idx,
                    parents,
                  });
                  DAGPtr::Var(var)
                }
              }
            }
          }
        }
        Term::Typ(_) => DAGPtr::Leaf(alloc_val(Leaf {
          tag: LeafTag::Typ,
          parents,
        })),
        Term::LTy(_, lty) => DAGPtr::Leaf(alloc_val(Leaf {
          tag: LeafTag::LTy(*lty),
          parents,
        })),
        Term::Lit(_, lit) => DAGPtr::Leaf(alloc_val(Leaf {
          tag: LeafTag::Lit(lit.clone()),
          parents,
        })),
        Term::Opr(_, opr) => DAGPtr::Leaf(alloc_val(Leaf {
          tag: LeafTag::Opr(*opr),
          parents,
        })),
        Term::Ref(_, name, def_link, ast_link) => DAGPtr::Leaf(alloc_val(Leaf {
          tag: LeafTag::Ref(name.to_string(), *def_link, *ast_link),
          parents,
        })),
        _ => panic!("TODO: implement Term::to_dag variants"),
      }
    }
    DAG::new(go(tree, depth, abs_ctx, rel_ctx, parents))
  }

  // Check whether a term is a root node
  pub fn is_root(&self) -> bool {
    match get_parents(self.head){
      None => false,
      Some(pref) => unsafe {
        (*pref.as_ptr()).elem == ParentPtr::Root
      },
    }
  }

  // Remove the root parent
  pub fn uproot(&self){
    match get_parents(self.head) {
      None => (),
      Some(pref) => unsafe {
        dealloc(pref.as_ptr() as *mut u8, Layout::new::<ParentPtr>());
        set_parents(self.head, None);
      },
    }
  }
}

impl fmt::Display for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_term())
  }
}

// DAG is not isomorphic to Term anymore
// #[cfg(test)]
// mod test {
//   use super::*;
//   use crate::parse::term::parse;
  
//   #[test]
//   fn test_cases() {
//     let (_, x) =
//       parse("(λ _z => (λ _a => ∀ (1 _x: _a) -> #Natural) Type)").unwrap();
//     println!("{:?}", parse("(λ _a => ∀ (1 _a: _a) -> #Natural)"));
//     // assert_eq!(true, false)
//     assert_eq!(x, DAG::to_term(&DAG::from_term(&x)));
//   }
  
//   #[quickcheck]
//   fn term_encode_decode(x: Term) -> bool {
//     println!("x: {}", x);
//     println!("x: {:?}", x);
//     let y = DAG::to_term(&DAG::from_term(&x));
//     println!("y: {}", y);
//     println!("y: {:?}", y);
//     x == y
//   }
// }
