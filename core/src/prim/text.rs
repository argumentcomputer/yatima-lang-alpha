use libipld::ipld::Ipld;
use num_bigint::BigUint;
use ropey::Rope;
use std::fmt;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
  yatima,
};

use core::convert::TryFrom;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TextOp {
  Cons,
  LenChars,
  LenLines,
  LenBytes,
  Append,
  Insert,
  Remove,
  Take,
  Drop,
  Eql,
  Lte,
  Lth,
  Gte,
  Gth,
  Char,
  Byte,
  Line,
  CharAtByte,
  ByteAtChar,
  LineAtByte,
  LineAtChar,
  LineStartChar,
  LineStartByte,
  ToBytes,
}

impl TextOp {
  pub fn symbol(self) -> String {
    match self {
      Self::Cons => "cons".to_owned(),
      Self::Append => "append".to_owned(),
      Self::Insert => "insert".to_owned(),
      Self::Remove => "remove".to_owned(),
      Self::Take => "take".to_owned(),
      Self::Drop => "drop".to_owned(),
      Self::Eql => "eql".to_owned(),
      Self::Lte => "lte".to_owned(),
      Self::Lth => "lth".to_owned(),
      Self::Gte => "gte".to_owned(),
      Self::Gth => "gth".to_owned(),
      Self::LenChars => "len_chars".to_owned(),
      Self::LenBytes => "len_bytes".to_owned(),
      Self::LenLines => "len_lines".to_owned(),
      Self::Char => "char".to_owned(),
      Self::Byte => "byte".to_owned(),
      Self::Line => "line".to_owned(),
      Self::CharAtByte => "char_at_byte".to_owned(),
      Self::ByteAtChar => "byte_at_char".to_owned(),
      Self::LineAtByte => "line_at_byte".to_owned(),
      Self::LineAtChar => "line_at_char".to_owned(),
      Self::LineStartByte => "line_start_byte".to_owned(),
      Self::LineStartChar => "line_start_char".to_owned(),
      Self::ToBytes => "to_bytes".to_owned(),
    }
  }

  pub fn from_symbol(x: &str) -> Option<Self> {
    match x {
      "cons" => Some(Self::Cons),
      "append" => Some(Self::Append),
      "insert" => Some(Self::Insert),
      "remove" => Some(Self::Remove),
      "take" => Some(Self::Take),
      "drop" => Some(Self::Drop),
      "eql" => Some(Self::Eql),
      "lte" => Some(Self::Lte),
      "lth" => Some(Self::Lth),
      "gte" => Some(Self::Gte),
      "gth" => Some(Self::Gth),
      "len_chars" => Some(Self::LenChars),
      "len_bytes" => Some(Self::LenBytes),
      "len_lines" => Some(Self::LenLines),
      "char" => Some(Self::Char),
      "byte" => Some(Self::Byte),
      "line" => Some(Self::Line),
      "char_at_byte" => Some(Self::CharAtByte),
      "byte_at_char" => Some(Self::ByteAtChar),
      "line_at_byte" => Some(Self::LineAtByte),
      "line_at_char" => Some(Self::LineAtChar),
      "line_start_byte" => Some(Self::LineStartByte),
      "line_start_char" => Some(Self::LineStartChar),
      "to_bytes" => Some(Self::ToBytes),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Cons => yatima!("∀ #Char #Text -> #Text"),
      Self::LenChars => yatima!("∀ #Text -> #Nat"),
      Self::LenLines => yatima!("∀ #Text -> #Nat"),
      Self::LenBytes => yatima!("∀ #Text -> #Nat"),
      Self::Append => yatima!("∀ #Text #Text -> #Text"),
      Self::Insert => yatima!("∀ #Nat #Text #Text -> #Text"),
      Self::Remove => yatima!("∀ #Nat #Nat #Text -> #Text"),
      Self::Take => yatima!("∀ #Nat #Text -> #Text"),
      Self::Drop => yatima!("∀ #Nat #Text -> #Text"),
      Self::Eql => yatima!("∀ #Text #Text -> #Bool"),
      Self::Lte => yatima!("∀ #Text #Text -> #Bool"),
      Self::Char => yatima!("∀ #Nat #Text -> #Char"),
      Self::Byte => yatima!("∀ #Nat #Text -> #U8"),
      Self::Line => yatima!("∀ #Nat #Text -> #Text"),
      Self::CharAtByte => yatima!("∀ #Nat #Text -> #Nat"),
      Self::ByteAtChar => yatima!("∀ #Nat #Text -> #Nat"),
      Self::LineAtByte => yatima!("∀ #Nat #Text -> #Nat"),
      Self::LineAtChar => yatima!("∀ #Nat #Text -> #Nat"),
      Self::LineStartChar => yatima!("∀ #Nat #Text -> #Text"),
      Self::LineStartByte => yatima!("∀ #Nat #Text -> #Text"),
      _ => todo!(),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Cons => Ipld::Integer(0),
      Self::LenChars => Ipld::Integer(1),
      Self::LenLines => Ipld::Integer(2),
      Self::LenBytes => Ipld::Integer(3),
      Self::Append => Ipld::Integer(4),
      Self::Insert => Ipld::Integer(5),
      Self::Remove => Ipld::Integer(6),
      Self::Take => Ipld::Integer(7),
      Self::Drop => Ipld::Integer(8),
      Self::Eql => Ipld::Integer(9),
      Self::Lte => Ipld::Integer(10),
      Self::Lth => Ipld::Integer(11),
      Self::Gte => Ipld::Integer(12),
      Self::Gth => Ipld::Integer(13),
      Self::Char => Ipld::Integer(14),
      Self::Byte => Ipld::Integer(15),
      Self::Line => Ipld::Integer(16),
      Self::CharAtByte => Ipld::Integer(17),
      Self::ByteAtChar => Ipld::Integer(18),
      Self::LineAtByte => Ipld::Integer(19),
      Self::LineAtChar => Ipld::Integer(20),
      Self::LineStartChar => Ipld::Integer(21),
      Self::LineStartByte => Ipld::Integer(22),
      Self::ToBytes => Ipld::Integer(23),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::Cons),
      Ipld::Integer(1) => Ok(Self::LenChars),
      Ipld::Integer(2) => Ok(Self::LenLines),
      Ipld::Integer(3) => Ok(Self::LenBytes),
      Ipld::Integer(4) => Ok(Self::Append),
      Ipld::Integer(5) => Ok(Self::Insert),
      Ipld::Integer(6) => Ok(Self::Remove),
      Ipld::Integer(7) => Ok(Self::Take),
      Ipld::Integer(8) => Ok(Self::Drop),
      Ipld::Integer(9) => Ok(Self::Eql),
      Ipld::Integer(10) => Ok(Self::Lte),
      Ipld::Integer(11) => Ok(Self::Lth),
      Ipld::Integer(12) => Ok(Self::Gte),
      Ipld::Integer(13) => Ok(Self::Gth),
      Ipld::Integer(14) => Ok(Self::Char),
      Ipld::Integer(15) => Ok(Self::Byte),
      Ipld::Integer(16) => Ok(Self::Line),
      Ipld::Integer(17) => Ok(Self::CharAtByte),
      Ipld::Integer(18) => Ok(Self::ByteAtChar),
      Ipld::Integer(19) => Ok(Self::LineAtByte),
      Ipld::Integer(20) => Ok(Self::LineAtChar),
      Ipld::Integer(21) => Ok(Self::LineStartChar),
      Ipld::Integer(22) => Ok(Self::LineStartByte),
      Ipld::Integer(23) => Ok(Self::ToBytes),
      xs => Err(IpldError::TextOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::Cons => 2,
      Self::LenChars => 1,
      Self::LenLines => 1,
      Self::LenBytes => 1,
      Self::Append => 2,
      Self::Insert => 3,
      Self::Remove => 3,
      Self::Take => 2,
      Self::Drop => 2,
      Self::Eql => 2,
      Self::Lte => 2,
      Self::Lth => 2,
      Self::Gte => 2,
      Self::Gth => 2,
      Self::Char => 2,
      Self::Byte => 2,
      Self::Line => 2,
      Self::CharAtByte => 2,
      Self::ByteAtChar => 2,
      Self::LineAtByte => 2,
      Self::LineAtChar => 2,
      Self::LineStartChar => 2,
      Self::LineStartByte => 2,
      Self::ToBytes => 1,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::LenChars, Text(xs)) => Some(Nat(xs.len_chars().into())),
      (Self::LenBytes, Text(xs)) => Some(Nat(xs.len_bytes().into())),
      (Self::LenLines, Text(xs)) => Some(Nat(xs.len_lines().into())),
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Cons, Char(c), Text(cs)) => {
        let mut cs = cs.clone();
        cs.insert_char(0, *c);
        Some(Text(cs))
      }
      (Self::Append, Text(xs), Text(ys)) => {
        let mut xs = xs.clone();
        xs.append(ys.clone());
        Some(Text(xs))
      }
      (Self::Take, Nat(x), Text(xs)) => {
        let (xs, _) = safe_split(x, xs.clone());
        Some(Text(xs))
      }
      (Self::Drop, Nat(x), Text(xs)) => {
        let (_, ys) = safe_split(x, xs.clone());
        Some(Text(ys))
      }
      (Self::Eql, Text(xs), Text(ys)) => Some(Bool(xs == ys)),
      (Self::Lte, Text(xs), Text(ys)) => Some(Bool(xs <= ys)),
      (Self::Lth, Text(xs), Text(ys)) => Some(Bool(xs < ys)),
      (Self::Gte, Text(xs), Text(ys)) => Some(Bool(xs >= ys)),
      (Self::Gth, Text(xs), Text(ys)) => Some(Bool(xs > ys)),
      _ => None,
    }
  }

  pub fn apply3(
    self,
    x: &Literal,
    y: &Literal,
    z: &Literal,
  ) -> Option<Literal> {
    use Literal::*;
    match (self, x, y, z) {
      (Self::Insert, Nat(x), Text(y), Text(xs)) => {
        Some(Text(safe_insert(x, y.clone(), xs.clone())))
      }
      (Self::Remove, Nat(x), Nat(y), Text(xs)) => {
        Some(Text(safe_remove(x, y, xs.clone())))
      }
      _ => None,
    }
  }
}

impl fmt::Display for TextOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.symbol())
  }
}

pub fn safe_insert(idx: &BigUint, ys: Rope, mut xs: Rope) -> Rope {
  let idx = usize::try_from(idx);
  match idx {
    Ok(idx) if idx <= xs.len_chars() => {
      xs.insert(idx, &ys.to_string());
      xs
    }
    _ => xs,
  }
}

pub fn safe_remove(from: &BigUint, upto: &BigUint, mut xs: Rope) -> Rope {
  let from = usize::try_from(from);
  let upto = usize::try_from(upto);
  let len = xs.len_chars();
  match (from, upto) {
    (Ok(from), Ok(upto)) if (from <= len) && (upto <= len) => {
      xs.remove(from..upto);
      xs
    }
    _ => xs,
  }
}

pub fn safe_head(mut x: Rope) -> Option<(char, Rope)> {
  if x.len_chars() == 0 {
    None
  }
  else {
    let tail = x.split_off(1);
    Some((x.char(0), tail))
  }
}

pub fn safe_split(idx: &BigUint, mut xs: Rope) -> (Rope, Rope) {
  let idx = usize::try_from(idx);
  match idx {
    Ok(idx) if idx <= xs.len_chars() => {
      let ys = xs.split_off(idx);
      (xs, ys)
    }
    _ => (xs, Rope::from_str("")),
  }
}

pub fn safe_line(idx: BigUint, xs: Rope) -> Rope {
  if let Ok(i) = usize::try_from(idx) {
    if i > xs.len_chars() { Rope::from_str("") } else { Rope::from(xs.line(i)) }
  }
  else {
    Rope::from_str("")
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;
  impl Arbitrary for TextOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..11);
      match gen {
        0 => Self::Cons,
        1 => Self::LenChars,
        2 => Self::LenLines,
        3 => Self::LenBytes,
        4 => Self::Append,
        5 => Self::Insert,
        6 => Self::Remove,
        7 => Self::Take,
        8 => Self::Drop,
        9 => Self::Eql,
        10 => Self::Lte,
        11 => Self::Lth,
        12 => Self::Gte,
        13 => Self::Gth,
        14 => Self::Char,
        15 => Self::Byte,
        16 => Self::Line,
        17 => Self::CharAtByte,
        18 => Self::LineAtByte,
        19 => Self::LineAtChar,
        20 => Self::LineStartChar,
        21 => Self::LineStartByte,
        _ => Self::ToBytes,
      }
    }
  }

  #[quickcheck]
  fn text_op_ipld(x: TextOp) -> bool {
    match TextOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[test]
  fn test_safe_head() {
    let rope: Rope = Rope::from_str("foo");
    let res = safe_head(rope);
    assert_eq!(res, Some(('f', Rope::from_str("oo"))));
  }

  #[test]
  fn test_safe_split() {
    let rope: Rope = Rope::from_str("foo");
    let res = safe_split(&0u64.into(), rope.clone());
    assert_eq!(res, (Rope::from_str(""), Rope::from_str("foo")));
    let res = safe_split(&1u64.into(), rope.clone());
    assert_eq!(res, (Rope::from_str("f"), Rope::from_str("oo")));
    let res = safe_split(&4u64.into(), rope.clone());
    assert_eq!(res, (Rope::from_str("foo"), Rope::from_str("")));
    let res = safe_split(&u128::MAX.into(), rope.clone());
    assert_eq!(res, (Rope::from_str("foo"), Rope::from_str("")));
  }
}
