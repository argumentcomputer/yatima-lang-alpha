use std::cmp::Ordering::*;

#[derive(Clone, Debug)]
pub struct FreeVars {
  list: Vec<u64>
}

impl FreeVars {
  pub fn singleton(x: u64) -> FreeVars {
    FreeVars {
      list: vec![x]
    }
  }

  pub fn union(x: &Self, y: &Self) -> Self {
    let mut i = 0;
    let mut j = 0;
    let mut list = vec![];
    while i < x.list.len() && j < y.list.len() {
      let cmp = x.list[i].cmp(&y.list[j]);
      match cmp {
        Greater => {
          list.push(x.list[i]);
          i = i+1;
        }
        Equal => {
          list.push(x.list[i]);
          i = i+1;
          j = j+1;
        }
        Less => {
          list.push(y.list[j]);
          j = j+1;
        }
      }
    };
    while i < x.list.len() {
      list.push(x.list[i]);
      i = i+1;
    }
    while j < y.list.len() {
      list.push(y.list[j]);
      j = j+1;
    }
    FreeVars { list }
  }

  pub fn bind(&mut self) {
    let mut i = 0;
    while i < self.list.len() {
      if self.list[i] == 0 {
        self.list.pop();
        break;
      }
      self.list[i] = self.list[i] - 1;
      i = i+1;
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn union_test() {
    let x = FreeVars {
      list: vec![23, 12, 8, 3, 1],
    };
    let y = FreeVars {
      list: vec![18, 14, 13, 12, 9, 3, 2, 1, 0],
    };
    let z = FreeVars::union(&x, &y);
    assert_eq!(format!("{:?}", z.list), format!("[23, 18, 14, 13, 12, 9, 8, 3, 2, 1, 0]"));
  }

  #[test]
  fn bind_test() {
    let mut x = FreeVars {
      list: vec![8, 5, 3, 2, 0],
    };
    x.bind();
    assert_eq!(format!("{:?}", x.list), format!("[7, 4, 2, 1]"));
    x.bind();
    assert_eq!(format!("{:?}", x.list), format!("[6, 3, 1, 0]"));
    x.bind();
    assert_eq!(format!("{:?}", x.list), format!("[5, 2, 0]"));
    x.bind();
    assert_eq!(format!("{:?}", x.list), format!("[4, 1]"));
    x.bind();
    assert_eq!(format!("{:?}", x.list), format!("[3, 0]"));
    x.bind();
    assert_eq!(format!("{:?}", x.list), format!("[2]"));
  }
}
