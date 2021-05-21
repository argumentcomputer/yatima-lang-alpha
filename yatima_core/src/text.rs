use ropey::Rope;

pub fn safe_head(mut x: Rope) -> Option<(char, Rope)> {
  if x.len_chars() == 0 {
    None
  }
  else {
    let tail = x.split_off(1);
    Some((x.char(0), tail))
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test_safe_head() {
    let rope: Rope = Rope::from_str("foo");
    let res = safe_head(rope);
    assert_eq!(res, Some(('f', Rope::from_str("oo"))));
  }
}
