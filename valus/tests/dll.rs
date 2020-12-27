#[cfg(test)]
mod test {
  use valus::dll::*;
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
      let dll = match DLL::remove_node(dll) {
        Some(dll) => dll,
        None => return (),
      };
      assert_eq!((*dll.as_ptr()).to_string(), "[ 0 <-> 1 <-> 2 <-> 4 <-> 5 <-> 6 ]");
      let dll = match DLL::remove_node(dll) {
        Some(dll) => dll,
        None => return (),
      };
      assert_eq!((*dll.as_ptr()).to_string(), "[ 0 <-> 1 <-> 4 <-> 5 <-> 6 ]");
      let dll = match DLL::remove_node(dll) {
        Some(dll) => dll,
        None => return (),
      };
      let node = &mut *dll.as_ptr();
      let mut iter = node.iter();
      assert_eq!(iter.next(), Some(&mut 0));
      assert_eq!(iter.next(), Some(&mut 4));
      if let Some(dll) = iter.this(){
        let node = &*dll.as_ptr();
        assert_eq!(node.elem, 4);
      }
    }
  }
}
