use core::{
  marker::PhantomData,
  ptr::NonNull,
};

use sp_std::{
  boxed::Box,
  fmt,
};

use alloc::string::{
  String,
  ToString,
};

/// A doubly-linked list (DLL) node
/// Uses raw pointers in order to share mutable DAG references
#[derive(Debug)]
pub struct DLL<T> {
  pub next: Option<NonNull<DLL<T>>>,
  pub prev: Option<NonNull<DLL<T>>>,
  pub elem: T,
}

/// An forwards iterator over the DLL
/// Uses a PhantomData marker to simulate ownership of the DLL during its
/// lifetime of `'a`
pub struct Iter<'a, T> {
  next: Option<NonNull<DLL<T>>>,
  this: Option<NonNull<DLL<T>>>,
  marker: PhantomData<&'a mut DLL<T>>,
}

impl<'a, T> Iter<'a, T> {
  /// Checks if last element in the DLL
  #[inline]
  pub fn is_last(&self) -> bool { self.next.is_none() }

  /// Returns the inner DLL node
  #[inline]
  pub fn this(&self) -> Option<NonNull<DLL<T>>> { self.this }
}

impl<'a, T> Iterator for Iter<'a, T> {
  type Item = &'a mut T;

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    self.this = self.next;
    self.next.map(|node| {
      let deref = unsafe { &mut *node.as_ptr() };
      self.next = deref.next;
      &mut deref.elem
    })
  }
}

impl<T> DLL<T> {
  /// Creates a DLL with only one node
  #[inline]
  pub fn singleton(elem: T) -> Self { DLL { next: None, prev: None, elem } }

  /// Checks if the DLL has only one node
  #[inline]
  pub fn is_singleton(dll: Option<NonNull<Self>>) -> bool {
    dll.map_or(false, |dll| unsafe {
      let dll = &*dll.as_ptr();
      dll.prev.is_none() && dll.next.is_none()
    })
  }

  /// Inserts a new DLL node after the current node
  pub fn add_after(&mut self, elem: T) {
    let new_next = NonNull::new(Box::into_raw(Box::new(DLL {
      next: self.next,
      prev: NonNull::new(self),
      elem,
    })));
    self.next.map_or((), |ptr| unsafe { (*ptr.as_ptr()).prev = new_next });
    self.next = new_next;
  }

  /// Inserts a new DLL node before the current node
  pub fn add_before(&mut self, elem: T) {
    let new_prev = NonNull::new(Box::into_raw(Box::new(DLL {
      next: NonNull::new(self),
      prev: self.prev,
      elem,
    })));
    self.prev.map_or((), |ptr| unsafe { (*ptr.as_ptr()).next = new_prev });
    self.prev = new_prev;
  }

  /// Inserts a given node before the current node, dropping the former's
  /// previous links
  pub fn merge(&mut self, node: NonNull<Self>) {
    unsafe {
      (*node.as_ptr()).prev = self.prev;
      (*node.as_ptr()).next = NonNull::new(self);
      self.prev.map_or((), |ptr| (*ptr.as_ptr()).next = Some(node));
      self.prev = Some(node);
    }
  }

  /// Unlinks the given node and returns, if it exists, a neighboring node.
  /// Leaves the node to be deallocated afterwards.
  pub fn unlink_node(&self) -> Option<NonNull<Self>> {
    unsafe {
      let next = self.next;
      let prev = self.prev;
      if let Some(next) = next {
        (*next.as_ptr()).prev = prev;
      };
      if let Some(prev) = prev {
        (*prev.as_ptr()).next = next;
      }
      (prev).or(next)
    }
  }

  /// Returns the first node in the DLL
  pub fn first(mut node: NonNull<Self>) -> NonNull<Self> {
    loop {
      let prev = unsafe { (*node.as_ptr()).prev };
      match prev {
        None => break,
        Some(ptr) => node = ptr,
      }
    }
    node
  }

  /// Returns the last node in the DLL
  pub fn last(mut node: NonNull<Self>) -> NonNull<Self> {
    loop {
      let next = unsafe { (*node.as_ptr()).next };
      match next {
        None => break,
        Some(ptr) => node = ptr,
      }
    }
    node
  }

  /// Joins two DLLs together at the ends
  pub fn concat(dll: NonNull<Self>, rest: Option<NonNull<Self>>) {
    let last = DLL::last(dll);
    let first = rest.map(DLL::first);
    unsafe {
      (*last.as_ptr()).next = first;
    }
    first.map_or((), |first| unsafe {
      (*first.as_ptr()).prev = Some(last);
    });
  }

  /// Creates an iterator over the DLL if it exists
  #[inline]
  pub fn iter_option<'a>(dll: Option<NonNull<Self>>) -> Iter<'a, T> {
    Iter { next: dll.map(DLL::first), this: None, marker: PhantomData }
  }

  /// Creates a mutable iterator over the DLL
  #[inline]
  pub fn iter_mut(&mut self) -> Iter<'_, T> {
    let link: NonNull<Self> = NonNull::from(self);
    Iter { next: Some(DLL::first(link)), this: None, marker: PhantomData }
  }

  /// Creates an iterator over the DLL
  #[inline]
  pub fn iter(&self) -> Iter<'_, T> {
    let link: NonNull<Self> = NonNull::from(self);
    Iter { next: Some(DLL::first(link)), this: None, marker: PhantomData }
  }
}

impl<T: fmt::Display> fmt::Display for DLL<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut iter = self.iter();
    let head = &iter.next().map_or(String::from(""), |head| head.to_string());
    let mut msg = String::from("[ ") + head;
    for val in iter {
      msg = msg + " <-> " + &val.to_string();
    }
    write!(f, "{}", msg + " ]")
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use core::ptr::NonNull;

  #[test]
  pub fn dll() {
    unsafe {
      let dll = NonNull::new_unchecked(Box::leak(Box::new(DLL::singleton(3))));
      // Populate list
      let node = &mut *dll.as_ptr();
      node.add_after(6);
      node.add_after(5);
      node.add_after(4);
      node.add_before(0);
      node.add_before(1);
      node.add_before(2);
      assert_eq!(node.to_string(), "[ 0 <-> 1 <-> 2 <-> 3 <-> 4 <-> 5 <-> 6 ]");
      // Remove elements
      let dll = match DLL::unlink_node(dll.as_ref()) {
        Some(dll) => dll,
        None => return (),
      };
      assert_eq!(
        (*dll.as_ptr()).to_string(),
        "[ 0 <-> 1 <-> 2 <-> 4 <-> 5 <-> 6 ]"
      );
      let dll = match DLL::unlink_node(dll.as_ref()) {
        Some(dll) => dll,
        None => return (),
      };
      assert_eq!((*dll.as_ptr()).to_string(), "[ 0 <-> 1 <-> 4 <-> 5 <-> 6 ]");
      let dll = match DLL::unlink_node(dll.as_ref()) {
        Some(dll) => dll,
        None => return (),
      };
      let node = &mut *dll.as_ptr();
      let mut iter = node.iter();
      assert_eq!(iter.next(), Some(&mut 0));
      assert_eq!(iter.next(), Some(&mut 4));
      if let Some(dll) = iter.this() {
        let node = &*dll.as_ptr();
        assert_eq!(node.elem, 4);
      }
    }
  }
}
