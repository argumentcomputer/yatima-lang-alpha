use crate::{
  core::{
    dag::*,
    dll::*,
  },
};

use core::ptr::NonNull;
use std::{
  alloc::{
    dealloc,
    Layout,
  },
};

// Resets the cache slots of the app nodes.
pub fn clear_copies(mut spine: &Single, top_branch: &mut Branch) {
  #[inline]
  fn clean_up_var(var: Option<NonNull<Var>>) {
    match var {
      Some(mut var) => {
        let var = unsafe { var.as_mut() };
        for var_parent in DLL::iter_option((*var).parents) {
          clean_up(var_parent);
        }
      }
      None => (),
    };
  }
  fn clean_up(cc: &ParentPtr) {
    match cc {
      ParentPtr::Left(mut parent) => unsafe {
        let parent = parent.as_mut();
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
      ParentPtr::Right(mut parent) => unsafe {
        let parent = parent.as_mut();
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
      ParentPtr::Body(parent) => unsafe {
        let Single { parents, var, .. } = &*parent.as_ptr();
        clean_up_var(*var);
        for grandparent in DLL::iter_option(*parents) {
          clean_up(grandparent);
        }
      },
      ParentPtr::Root => (),
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
      DAGPtr::Single(single) => unsafe { spine = &*single.as_ptr() },
      _ => break,
    }
  }
}

// // Free parentless nodes.
pub fn free_dead_node(node: DAGPtr) {
  #[inline]
  fn free_var(var: Option<NonNull<Var>>) {
    match var {
      Some(var) => unsafe {
        if (*var.as_ptr()).parents.is_none() {
          dealloc(var.as_ptr() as *mut u8, Layout::new::<Var>());
        }
      },
      None => (),
    };
  }
  unsafe {
    match node {
      DAGPtr::Single(link) => {
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
      DAGPtr::Branch(link) => {
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
      DAGPtr::Leaf(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Leaf>());
      }
      DAGPtr::Var(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Var>());
      }
    }
  }
}

// Replace one child w/another in the tree.
pub fn replace_child(oldchild: DAGPtr, newchild: DAGPtr) {
  #[inline]
  fn install_child(parent: &mut ParentPtr, newchild: DAGPtr) {
    unsafe {
      match parent {
        ParentPtr::Left(parent) => (*parent.as_ptr()).left = newchild,
        ParentPtr::Right(parent) => (*parent.as_ptr()).right = newchild,
        ParentPtr::Body(parent) => (*parent.as_ptr()).body = newchild,
        ParentPtr::Root => (),
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

// The core up-copy function.
pub fn upcopy(new_child: DAGPtr, cc: ParentPtr) {
  unsafe {
    match cc {
      ParentPtr::Body(parent) => {
        let Single { var, parents: grandparents, .. } = *parent.as_ptr();
        let tag = &(*parent.as_ptr()).tag;
        let new_single = upcopy_single(var, new_child, tag.clone());
        for grandparent in DLL::iter_option(grandparents) {
          upcopy(DAGPtr::Single(new_single), *grandparent)
        }
      }
      ParentPtr::Left(parent) => {
        let Branch { copy, right, parents: grandparents, .. } =
          *parent.as_ptr();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).left = new_child;
          }
          None => {
            let new_branch = upcopy_branch(parent, new_child, right);
            for grandparent in DLL::iter_option(grandparents) {
              upcopy(DAGPtr::Branch(new_branch), *grandparent)
            }
          }
        }
      }
      ParentPtr::Right(parent) => {
        let Branch { copy, left, parents: grandparents, .. } =
          *parent.as_ptr();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).right = new_child;
          }
          None => {
            let new_branch = upcopy_branch(parent, left, new_child);
            for grandparent in DLL::iter_option(grandparents) {
              upcopy(DAGPtr::Branch(new_branch), *grandparent)
            }
          }
        }
      }
      ParentPtr::Root => (),
    }
  }
}

// Allocate a fresh branch node, with the two given params as its children.
// Parent references are not added to its children.
#[inline]
pub fn upcopy_branch(
  oldbranch: NonNull<Branch>,
  left: DAGPtr,
  right: DAGPtr,
) -> NonNull<Branch> {
  unsafe {
    let left_ref = alloc_uninit();
    let right_ref = alloc_uninit();
    let new_branch = alloc_val(Branch {
      copy: None,
      tag: (*oldbranch.as_ptr()).tag.clone(),
      var: None,
      left,
      right,
      left_ref,
      right_ref,
      parents: None,
    });
    *left_ref.as_ptr() = DLL::singleton(ParentPtr::Left(new_branch));
    *right_ref.as_ptr() = DLL::singleton(ParentPtr::Right(new_branch));
    (*oldbranch.as_ptr()).copy = Some(new_branch);
    match (*oldbranch.as_ptr()).var {
      Some(oldvar) => {
        let Var { name, depth, parents: var_parents } = &*oldvar.as_ptr();
        let var = alloc_val(Var { name: name.clone(), depth: *depth, parents: None });
        (*new_branch.as_ptr()).var = Some(var);
        for parent in DLL::iter_option(*var_parents) {
          upcopy(DAGPtr::Var(var), *parent)
        }
      }
      None => (),
    };
    new_branch
  }
}

// Allocate a fresh single node
#[inline]
pub fn upcopy_single(
  oldvar: Option<NonNull<Var>>,
  body: DAGPtr,
  tag: SingleTag,
) -> NonNull<Single> {
  unsafe {
    let body_ref = alloc_uninit();
    let new_single =
      alloc_val(Single { tag, var: None, body, body_ref, parents: None });
    *body_ref.as_ptr() = DLL::singleton(ParentPtr::Body(new_single));
    add_to_parents(body, body_ref);
    match oldvar {
      Some(oldvar) => {
        let Var { name, depth, parents: var_parents } = &*oldvar.as_ptr();
        let var = alloc_val(Var { name: name.clone(), depth: *depth, parents: None });
        (*new_single.as_ptr()).var = Some(var);
        for parent in DLL::iter_option(*var_parents) {
          upcopy(DAGPtr::Var(var), *parent)
        }
      }
      None => (),
    };
    new_single
  }
}

// // Allocate a fresh leaf node
// #[inline]
// pub fn new_leaf(tag: LeafTag) -> NonNull<Leaf> {
//   alloc_val(Leaf { tag, parents: None })
// }

// // Allocate a fresh leaf node
// #[inline]
// pub fn new_var(name: String, depth: u64) -> NonNull<Var> {
//   alloc_val(Var { name, depth, parents: None })
// }


