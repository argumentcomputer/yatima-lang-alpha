#![allow(unused_variables)]

use crate::{
  core::{
    dll::*,
    eval,
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

// A top-down λ-DAG pointer. Keeps track of what kind of node it points to.
#[derive(Clone, Copy)]
pub enum DAG {
  Leaf(NonNull<Leaf>),
  Single(NonNull<Single>),
  Branch(NonNull<Branch>),
}

// Doubly-linked list of parent nodes
type Parents = DLL<ParentCell>;

// A bottom-up (parent) λ-DAG pointer. Keeps track of the relation between
// the child and the parent.
#[derive(Clone, Copy)]
pub enum ParentCell {
  Root,
  Body(NonNull<Single>),
  Left(NonNull<Branch>),
  Right(NonNull<Branch>),
}

// The λ-DAG nodes
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
  Var(String),
  Ref(String, Link, Link),
}

pub struct Single {
  pub var: Option<NonNull<Leaf>>,
  pub tag: SingleTag,
  pub body: DAG,
  pub body_ref: NonNull<Parents>,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Clone, Copy)]
pub enum SingleTag {
  Lam,
  Fix,
  Slf,
  Cse,
  Dat,
}

pub struct Branch {
  pub var: Option<NonNull<Leaf>>,
  pub tag: BranchTag,
  pub left: DAG,
  pub right: DAG,
  pub left_ref: NonNull<Parents>,
  pub right_ref: NonNull<Parents>,
  pub copy: Option<NonNull<Branch>>,
  pub parents: Option<NonNull<Parents>>,
}

#[derive(Clone, Copy)]
pub enum BranchTag {
  App,
  Ann,
  All(Uses),
}

// Get the parents of a term.
#[inline]
pub fn get_parents(term: DAG) -> Option<NonNull<Parents>> {
  unsafe {
    match term {
      DAG::Leaf(link) => (*link.as_ptr()).parents,
      DAG::Single(link) => (*link.as_ptr()).parents,
      DAG::Branch(link) => (*link.as_ptr()).parents,
    }
  }
}

// Set the parent slot of a term
#[inline]
pub fn set_parents(term: DAG, pref: Option<NonNull<Parents>>) {
  unsafe {
    match term {
      DAG::Leaf(link) => (*link.as_ptr()).parents = pref,
      DAG::Single(link) => (*link.as_ptr()).parents = pref,
      DAG::Branch(link) => (*link.as_ptr()).parents = pref,
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
pub fn clear_copies(mut spine: &Single, top_branch: &mut Branch) {
  #[inline]
  fn clean_up_var(var: Option<NonNull<Leaf>>) {
    match var {
      Some(var) => {
        let var = unsafe { &mut *var.as_ptr() };
        for var_parent in DLL::iter_option((*var).parents) {
          clean_up(var_parent);
        }
      }
      None => (),
    };
  }
  fn clean_up(cc: &ParentCell) {
    match cc {
      ParentCell::Left(parent) => unsafe {
        let parent = &mut *parent.as_ptr();
        parent.copy.map_or((), |branch| {
          parent.copy = None;
          let Branch { var, left, left_ref, right, right_ref, .. } =
            *branch.as_ptr();
          add_to_parents(left, left_ref);
          add_to_parents(right, right_ref);
          clean_up_var(var);
          for grandparent in DLL::iter_option(parent.parents) {
            clean_up(grandparent);
          }
        })
      },
      ParentCell::Right(parent) => unsafe {
        let parent = &mut *parent.as_ptr();
        parent.copy.map_or((), |branch| {
          parent.copy = None;
          let Branch { var, left, left_ref, right, right_ref, .. } =
            *branch.as_ptr();
          add_to_parents(left, left_ref);
          add_to_parents(right, right_ref);
          clean_up_var(var);
          for grandparent in DLL::iter_option(parent.parents) {
            clean_up(grandparent);
          }
        })
      },
      ParentCell::Body(parent) => unsafe {
        let Single { parents, var, .. } = &*parent.as_ptr();
        clean_up_var(*var);
        for grandparent in DLL::iter_option(*parents) {
          clean_up(grandparent);
        }
      },
      ParentCell::Root => (),
    }
  }
  // Clears the top app cache and adds itself to its children's list of parents
  top_branch.copy.map_or((), |ptr| unsafe {
    top_branch.copy = None;
    let Branch { var, left, left_ref, right, right_ref, .. } = *ptr.as_ptr();
    add_to_parents(left, left_ref);
    add_to_parents(right, right_ref);
    clean_up_var(var);
  });
  loop {
    clean_up_var(spine.var);
    match spine.body {
      DAG::Single(single) => unsafe { spine = &*single.as_ptr() },
      _ => break,
    }
  }
}

// // Free parentless nodes.
pub fn free_dead_node(node: DAG) {
  #[inline]
  fn free_var(var: Option<NonNull<Leaf>>) {
    match var {
      Some(var) => unsafe {
        if (*var.as_ptr()).parents.is_none() {
          free_dead_node(DAG::Leaf(var))
        }
      },
      None => (),
    };
  }
  unsafe {
    match node {
      DAG::Single(link) => {
        let Single { body, body_ref, var, .. } = &*link.as_ptr();
        free_var(*var);
        let new_body_parents = DLL::remove_node(*body_ref);
        set_parents(*body, new_body_parents);
        match new_body_parents {
          None => free_dead_node(*body),
          _ => (),
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Single>());
      }
      DAG::Branch(link) => {
        let Branch { left, right, left_ref, right_ref, var, .. } =
          &*link.as_ptr();
        free_var(*var);
        let new_left_parents = DLL::remove_node(*left_ref);
        set_parents(*left, new_left_parents);
        match new_left_parents {
          None => free_dead_node(*left),
          _ => (),
        }
        let new_right_parents = DLL::remove_node(*right_ref);
        set_parents(*right, new_right_parents);
        match new_right_parents {
          None => free_dead_node(*right),
          _ => (),
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Branch>());
      }
      DAG::Leaf(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Leaf>());
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
        ParentCell::Left(parent) => (*parent.as_ptr()).left = newchild,
        ParentCell::Right(parent) => (*parent.as_ptr()).right = newchild,
        ParentCell::Body(parent) => (*parent.as_ptr()).body = newchild,
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

// Allocate a fresh branch node, with the two given params as its children.
// Parent references are not added to its children.
#[inline]
pub fn new_branch(
  oldvar: Option<NonNull<Leaf>>,
  left: DAG,
  right: DAG,
  tag: BranchTag,
) -> NonNull<Branch> {
  unsafe {
    let left_ref = alloc_uninit();
    let right_ref = alloc_uninit();
    let new_branch = alloc_val(Branch {
      copy: None,
      tag,
      var: None,
      left,
      right,
      left_ref,
      right_ref,
      parents: None,
    });
    *left_ref.as_ptr() = DLL::singleton(ParentCell::Left(new_branch));
    *right_ref.as_ptr() = DLL::singleton(ParentCell::Right(new_branch));
    match oldvar {
      Some(oldvar) => {
        let Leaf { tag: var_tag, parents: var_parents } = &*oldvar.as_ptr();
        let var = alloc_val(Leaf { tag: var_tag.clone(), parents: None });
        (*new_branch.as_ptr()).var = Some(var);
        for parent in DLL::iter_option(*var_parents) {
          eval::upcopy(DAG::Leaf(var), *parent)
        }
      }
      None => (),
    };
    new_branch
  }
}

// Allocate a fresh single node
#[inline]
pub fn new_single(
  oldvar: Option<NonNull<Leaf>>,
  body: DAG,
  tag: SingleTag,
) -> NonNull<Single> {
  unsafe {
    let body_ref = alloc_uninit();
    let new_single =
      alloc_val(Single { tag, var: None, body, body_ref, parents: None });
    *body_ref.as_ptr() = DLL::singleton(ParentCell::Body(new_single));
    add_to_parents(body, body_ref);
    match oldvar {
      Some(oldvar) => {
        let Leaf { tag: var_tag, parents: var_parents } = &*oldvar.as_ptr();
        let var = alloc_val(Leaf { tag: var_tag.clone(), parents: None });
        (*new_single.as_ptr()).var = Some(var);
        for parent in DLL::iter_option(*var_parents) {
          eval::upcopy(DAG::Leaf(var), *parent)
        }
      }
      None => (),
    };
    new_single
  }
}

// Allocate a fresh leaf node
#[inline]
pub fn new_leaf(tag: LeafTag) -> NonNull<Leaf> {
  alloc_val(Leaf { tag, parents: None })
}

impl fmt::Debug for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    #[inline]
    fn stringify_parents(dll: Option<NonNull<Parents>>) -> String {
      #[inline]
      fn extract_link(p: ParentCell) -> String {
        match p {
          ParentCell::Root => String::from("ROOT"),
          ParentCell::Body(link) => format!("B{}", link.as_ptr() as u64),
          ParentCell::Left(link) => format!("L{}", link.as_ptr() as u64),
          ParentCell::Right(link) => format!("R{}", link.as_ptr() as u64),
        }
      }
      match dll {
        Some(dll) => unsafe {
          let mut iter = (*dll.as_ptr()).iter();
          let head = &iter.next().map_or(String::from(""), |head| {
            format!("{}", extract_link(*head))
          });
          let mut msg = String::from("[ ") + head;
          for val in iter {
            msg = msg + " <-> " + &format!("{}", extract_link(*val));
          }
          msg + " ]"
        },
        _ => String::from("[]"),
      }
    }
    fn go(term: DAG, set: &mut HashSet<u64>) -> String {
      match term {
        DAG::Branch(link) => unsafe {
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            let Branch { parents, left, right, copy, .. } = *link.as_ptr();
            let copy = copy.map(|link| format!("{}", link.as_ptr() as u64));
            format!(
              "\nApp<{}> parents: {} copy: {:?}{}{}",
              link.as_ptr() as u64,
              stringify_parents(parents),
              copy,
              go(left, set),
              go(right, set)
            )
          }
          else {
            format!("\nSHARE<{}>", link.as_ptr() as u64)
          }
        },
        DAG::Single(link) => unsafe {
          let Single { var, parents, body, .. } = *link.as_ptr();
          let name = match var {
            Some(var_link) => match &(*var_link.as_ptr()).tag {
              LeafTag::Var(name) => name.clone(),
              _ => panic!("TODO"),
            },
            _ => panic!("TODO"),
          };
          if set.get(&(link.as_ptr() as u64)).is_none() {
            set.insert(link.as_ptr() as u64);
            format!(
              "\nLam<{}> {} parents: {}{}",
              link.as_ptr() as u64,
              name,
              stringify_parents(parents),
              go(body, set)
            )
          }
          else {
            format!("\nSHARE<{}>", link.as_ptr() as u64)
          }
        },
        DAG::Leaf(link) => unsafe {
          let Leaf { parents, .. } = *link.as_ptr();
          match &(*link.as_ptr()).tag {
            LeafTag::Var(name) => {
              if set.get(&(link.as_ptr() as u64)).is_none() {
                set.insert(link.as_ptr() as u64);
                format!(
                  "\nVar<{}> {} parents: {}",
                  link.as_ptr() as u64,
                  name,
                  stringify_parents(parents)
                )
              }
              else {
                format!("\nSHARE<{}>", link.as_ptr() as u64)
              }
            }
            _ => panic!("TODO"),
          }
        },
      }
    }
    write!(f, "{}", go(*self, &mut HashSet::new()))
  }
}

impl fmt::Display for DAG {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_term())
  }
}

impl DAG {
  pub fn to_term(&self) -> Term {
    let mut map: HashMap<*mut Leaf, u64> = HashMap::new();

    pub fn go(
      node: &DAG,
      mut map: &mut HashMap<*mut Leaf, u64>,
      depth: u64,
    ) -> Term {
      match node {
        DAG::Leaf(link) => {
          let Leaf { tag, .. } = unsafe { &*link.as_ptr() };
          match tag {
            LeafTag::Typ => Term::Typ(None),
            LeafTag::LTy(lty) => Term::LTy(None, *lty),
            LeafTag::Lit(lit) => Term::Lit(None, lit.clone()),
            LeafTag::Opr(opr) => Term::Opr(None, *opr),
            LeafTag::Ref(nam, def_link, ast_link) => {
              Term::Ref(None, nam.to_owned(), *def_link, *ast_link)
            }
            LeafTag::Var(nam) => {
              let level = map.get(&link.as_ptr()).unwrap();
              Term::Var(None, nam.to_owned(), depth - level - 1)
            }
          }
        }

        DAG::Single(link) => {
          let Single { tag, body, var, .. } = unsafe { &*link.as_ptr() };
          match var {
            Some(var_link) => {
              let Leaf { tag: var_tag, .. } = unsafe { &*var_link.as_ptr() };
              let name = match var_tag {
                LeafTag::Var(name) => name,
                _ => panic!("Malformed DAG."),
              };
              match tag {
                SingleTag::Lam => {
                  map.insert(var_link.as_ptr(), depth);
                  let body = go(body, &mut map, depth + 1);
                  Term::Lam(None, name.clone(), Box::new(body))
                }
                SingleTag::Slf => {
                  map.insert(var_link.as_ptr(), depth);
                  let body = go(body, &mut map, depth + 1);
                  Term::Slf(None, name.clone(), Box::new(body))
                }
                SingleTag::Fix => panic!("TODO: Add Fix to Term."),
                _ => panic!("Malformed DAG."),
              }
            }
            None => match tag {
              SingleTag::Cse => {
                Term::Cse(None, Box::new(go(body, &mut map, depth)))
              }
              SingleTag::Dat => {
                Term::Dat(None, Box::new(go(body, &mut map, depth)))
              }
              _ => panic!("Malformed DAG."),
            },
          }
        }
        DAG::Branch(link) => {
          let Branch { tag, left, right, var, .. } = unsafe { &*link.as_ptr() };
          match var {
            Some(var_link) => {
              let Leaf { tag: var_tag, .. } = unsafe { &*var_link.as_ptr() };
              let name = match var_tag {
                LeafTag::Var(name) => name,
                _ => panic!("Malformed DAG."),
              };
              match tag {
                BranchTag::All(uses) => {
                  map.insert(var_link.as_ptr(), depth);
                  let dom = go(left, &mut map, depth);
                  let img = go(right, &mut map, depth + 1);
                  Term::All(
                    None,
                    *uses,
                    name.clone(),
                    Box::new((dom, img))
                  )
                }
                _ => panic!("Malformed DAG."),
              }
            }
            None => match tag {
              BranchTag::App => {
                let fun = go(left, &mut map, depth);
                let arg = go(right, &mut map, depth);
                Term::App(None, Box::new((fun, arg)))
              }
              BranchTag::Ann => {
                let typ = go(left, &mut map, depth);
                let trm = go(right, &mut map, depth);
                Term::Ann(None, Box::new((typ, trm)))
              }
              _ => panic!("Malformed DAG."),
            },
          }
        }
      }
    }
    go(&self, &mut map, 0)
  }

  pub fn from_term(tree: Term) -> DAG {
    pub fn go(
      tree: Term,
      mut ctx: Vector<NonNull<Leaf>>,
      parents: NonNull<DLL<ParentCell>>,
    ) -> DAG {
      match tree {
        Term::Lam(_, name, body) => {
          // Allocate nodes
          let var = new_leaf(LeafTag::Var(name.clone()));
          let sons_parents = alloc_uninit();
          let lam = alloc_val(Single {
            var: Some(var),
            tag: SingleTag::Lam,
            // Temporary, dangling DAG pointer
            body: DAG::Leaf(NonNull::dangling()),
            body_ref: sons_parents,
            parents: Some(parents),
          });

          // Update `sons_parents` to refer to current node
          unsafe {
            *sons_parents.as_ptr() = DLL::singleton(ParentCell::Body(lam));
          }

          ctx.push_front(var);
          let body = go((*body).clone(), ctx, sons_parents);

          // Update `lam` with the correct body
          unsafe {
            (*lam.as_ptr()).body = body;
          }
          DAG::Single(lam)
        }

        Term::Slf(_, name, body) => {
          let var = new_leaf(LeafTag::Var(name.clone()));
          let sons_parents = alloc_uninit();
          let lam = alloc_val(Single {
            var: Some(var),
            tag: SingleTag::Slf,
            body: DAG::Leaf(NonNull::dangling()),
            body_ref: sons_parents,
            parents: Some(parents),
          });
          unsafe {
            *sons_parents.as_ptr() = DLL::singleton(ParentCell::Body(lam));
          }
          ctx.push_front(var);
          let body = go((*body).clone(), ctx, sons_parents);
          unsafe {
            (*lam.as_ptr()).body = body;
          }
          DAG::Single(lam)
        }
        Term::Dat(_, body) => {
          let sons_parents = alloc_uninit();
          let lam = alloc_val(Single {
            var: None,
            tag: SingleTag::Dat,
            body: DAG::Leaf(NonNull::dangling()),
            body_ref: sons_parents,
            parents: Some(parents),
          });
          unsafe {
            *sons_parents.as_ptr() = DLL::singleton(ParentCell::Body(lam));
          }
          let body = go((*body).clone(), ctx, sons_parents);
          unsafe {
            (*lam.as_ptr()).body = body;
          }
          DAG::Single(lam)
        }
        Term::Cse(_, body) => {
          let sons_parents = alloc_uninit();
          let lam = alloc_val(Single {
            var: None,
            tag: SingleTag::Cse,
            body: DAG::Leaf(NonNull::dangling()),
            body_ref: sons_parents,
            parents: Some(parents),
          });
          unsafe {
            *sons_parents.as_ptr() = DLL::singleton(ParentCell::Body(lam));
          }
          let body = go((*body).clone(), ctx, sons_parents);
          unsafe {
            (*lam.as_ptr()).body = body;
          }
          DAG::Single(lam)
        }

        Term::All(_, uses, name, terms) => {
          // Allocation and updates
          let (dom, img) = *terms;
          let var = new_leaf(LeafTag::Var(name.clone()));
          let dom_parents = alloc_uninit();
          let img_parents = alloc_uninit();
          let all = alloc_val(Branch {
            var: Some(var),
            tag: BranchTag::All(uses),
            // Temporary, dangling DAG pointers
            left: DAG::Leaf(NonNull::dangling()),
            right: DAG::Leaf(NonNull::dangling()),
            left_ref: dom_parents,
            right_ref: img_parents,
            copy: None,
            parents: Some(parents),
          });
          unsafe {
            *dom_parents.as_ptr() = DLL::singleton(ParentCell::Left(all));
            *img_parents.as_ptr() = DLL::singleton(ParentCell::Right(all));
          }

          // Map `name` to `var` node
          let mut img_ctx = ctx.clone();
          let dom = go(dom, ctx, dom_parents);
          img_ctx.push_front(var);
          let img = go(img, img_ctx, img_parents);

          // Update `all` with the correct fields
          unsafe {
            (*all.as_ptr()).left = dom;
            (*all.as_ptr()).right = img;
          }
          DAG::Branch(all)
        }

        Term::App(_, terms) => {
          let (fun, arg) = *terms;
          let fun_parents = alloc_uninit();
          let arg_parents = alloc_uninit();
          let app = alloc_val(Branch {
            var: None,
            tag: BranchTag::App,
            left: DAG::Leaf(NonNull::dangling()),
            right: DAG::Leaf(NonNull::dangling()),
            left_ref: fun_parents,
            right_ref: arg_parents,
            copy: None,
            parents: Some(parents),
          });
          unsafe {
            *fun_parents.as_ptr() = DLL::singleton(ParentCell::Left(app));
            *arg_parents.as_ptr() = DLL::singleton(ParentCell::Right(app));
          }
          let fun = go(fun, ctx.clone(), fun_parents);
          let arg = go(arg, ctx, arg_parents);
          unsafe {
            (*app.as_ptr()).left = fun;
            (*app.as_ptr()).right = arg;
          }
          DAG::Branch(app)
        }
        Term::Ann(_, terms) => {
          let (typ, exp) = *terms;
          let typ_parents = alloc_uninit();
          let exp_parents = alloc_uninit();
          let ann = alloc_val(Branch {
            var: None,
            tag: BranchTag::Ann,
            left: DAG::Leaf(NonNull::dangling()),
            right: DAG::Leaf(NonNull::dangling()),
            left_ref: typ_parents,
            right_ref: exp_parents,
            copy: None,
            parents: Some(parents),
          });
          unsafe {
            *typ_parents.as_ptr() = DLL::singleton(ParentCell::Left(ann));
            *exp_parents.as_ptr() = DLL::singleton(ParentCell::Right(ann));
          }
          let typ = go(typ, ctx.clone(), typ_parents);
          let exp = go(exp, ctx, exp_parents);
          unsafe {
            (*ann.as_ptr()).left = typ;
            (*ann.as_ptr()).right = exp;
          }
          DAG::Branch(ann)
        }

        Term::Var(_, name, idx) => {
          let var = match ctx.get(idx as usize) {
            Some(var) => unsafe {
              DLL::concat(parents, (*var.as_ptr()).parents);
              (*var.as_ptr()).parents = Some(parents);
              *var
            },
            None => {
              let tag = LeafTag::Var(name.clone());
              alloc_val(Leaf { tag, parents: Some(parents) })
            }
          };
          DAG::Leaf(var)
        }
        Term::Typ(_) => DAG::Leaf(alloc_val(Leaf {
          tag: LeafTag::Typ,
          parents: Some(parents),
        })),
        Term::LTy(_, lty) => DAG::Leaf(alloc_val(Leaf {
          tag: LeafTag::LTy(lty),
          parents: Some(parents),
        })),
        Term::Lit(_, lit) => DAG::Leaf(alloc_val(Leaf {
          tag: LeafTag::Lit(lit),
          parents: Some(parents),
        })),
        Term::Opr(_, opr) => DAG::Leaf(alloc_val(Leaf {
          tag: LeafTag::Opr(opr),
          parents: Some(parents),
        })),
        Term::Ref(_, name, def_link, ast_link) => DAG::Leaf(alloc_val(Leaf {
          tag: LeafTag::Ref(name, def_link, ast_link),
          parents: Some(parents),
        })),
        _ => panic!("TODO: implement Term::to_dag variants"),
      }
    }
    let root = alloc_val(DLL::singleton(ParentCell::Root));
    go(tree, Vector::new(), root)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::parse::term::parse;

  #[test]
  fn test_cases() {
    let (_, x) =
      parse("(λ _z => (λ _a => ∀ (1 _x: _a) -> #Natural) Type)").unwrap();
    println!("{:?}", parse("(λ _a => ∀ (1 _a: _a) -> #Natural)"));
    // assert_eq!(true, false)
    assert_eq!(x, DAG::to_term(&DAG::from_term(x.clone())));
  }

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
