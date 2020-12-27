use core::ptr::NonNull;
use core::marker::PhantomData;
use std::alloc::{dealloc, Layout};

// A doubly-linked list (DLL) node
pub struct DLL<T> {
  pub next: Option<NonNull<DLL<T>>>,
  pub prev: Option<NonNull<DLL<T>>>,
  pub elem: T,
}

pub struct Iter<'a, T> {
  next: Option<NonNull<DLL<T>>>,
  this: Option<NonNull<DLL<T>>>,
  marker: PhantomData<&'a mut DLL<T>>,
}

impl<'a, T> Iter<'a, T>{
  #[inline]
  pub fn is_last(&self) -> bool {
    self.next.is_none()
  }
  #[inline]
  pub fn this(&self) -> Option<NonNull<DLL<T>>> {
    self.this
  }
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

impl<T> DLL<T>{
  #[inline]
  pub fn singleton(elem: T) -> Self{
    DLL {
      next: None,
      prev: None,
      elem: elem,
    }
  }
  
  #[inline]
  pub fn is_singleton(dll: Option<NonNull<Self>>) -> bool {
    dll.map_or(false, |dll| unsafe {
      let dll = &*dll.as_ptr();
      dll.prev.is_none() && dll.next.is_none()
    })
  }
  
  pub fn add_after(&mut self, elem: T){
    let new_next = NonNull::new(Box::into_raw(Box::new(DLL {
      next: self.next,
      prev: NonNull::new(self),
      elem: elem,
    })));
    unsafe {
      self.next.map_or((), |ptr| (*ptr.as_ptr()).prev = new_next);
    }
    self.next = new_next;
  }
  
  pub fn add_before(&mut self, elem: T){
    let new_prev = NonNull::new(Box::into_raw(Box::new(DLL {
      next: NonNull::new(self),
      prev: self.prev,
      elem: elem,
    })));
    unsafe {
      self.prev.map_or((), |ptr| (*ptr.as_ptr()).next = new_prev);
    }
    self.prev = new_prev;
  }
  
  // Deallocates the given node and returns, if it exists, a neighboring node
  pub fn remove_node(node: NonNull<Self>) -> Option<NonNull<Self>>{
    unsafe {
      let node = node.as_ptr();
      let next = (*node).next;
      let prev = (*node).prev;
      next.map_or((), |next| {
        (*next.as_ptr()).prev = prev
      });
      prev.map_or((), |prev| {
        (*prev.as_ptr()).next = next
      });
      dealloc(node as *mut u8, Layout::new::<Self>());
      (prev).or(next)
    }
  }
  
  pub fn first(mut node: NonNull<Self>) -> NonNull<Self>{
    loop {
      let prev = unsafe { (*node.as_ptr()).prev };
      match prev {
        None => break,
        Some(ptr) => node = ptr,
      }
    };
    node
  }

  pub fn last(mut node: NonNull<Self>) -> NonNull<Self>{
    loop {
      let next = unsafe { (*node.as_ptr()).next };
      match next {
        None => break,
        Some(ptr) => node = ptr,
      }
    };
    node
  }

  pub fn concat(dll: NonNull<Self>, rest: Option<NonNull<Self>>) -> NonNull<Self>{
    let last = DLL::last(dll);
    let first = rest.map(|dll| DLL::first(dll));
    unsafe {
      (*last.as_ptr()).next = first;
    }
    first.map_or((), |first| unsafe {
      (*first.as_ptr()).prev = Some(last);
    });
    NonNull::dangling()
  }

  #[inline]
  pub fn iter_option<'a>(dll: Option<NonNull<Self>>) -> Iter<'a, T> {
    Iter {
      next: dll.map(|dll| DLL::first(dll)),
      this: None,
      marker: PhantomData,
    }
  }

  #[inline]
  pub fn iter(&mut self) -> Iter<'_, T> {
    let link: NonNull<Self> = NonNull::from(self);
    Iter {
      next: Some(DLL::first(link)),
      this: None,
      marker: PhantomData,
    }
  }
}

impl<T: ToString> DLL<T>{
  pub fn to_string(&mut self) -> String {
    let mut iter = self.iter();
    let head = &iter.next().map_or(String::from(""), |head| head.to_string());
    let mut msg = String::from("[ ") + head;
    for val in iter {
      msg = msg + " <-> " + &val.to_string();
    }
    msg + " ]"
  }
}
