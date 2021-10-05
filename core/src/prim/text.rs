use num_bigint::BigUint;
use sp_ropey::Rope;
use sp_ipld::Ipld;

use sp_std::{
  borrow::ToOwned,
  fmt,
  vec::Vec,
};

use alloc::string::{
  String,
  ToString,
};

use crate::{
  defs,
  ipld_error::IpldError,
  literal::Literal,
  parse,
  term::Term,
  yatima,
};

use core::convert::{
  TryFrom,
  TryInto,
};

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
      Self::Lth => yatima!("∀ #Text #Text -> #Bool"),
      Self::Gte => yatima!("∀ #Text #Text -> #Bool"),
      Self::Gth => yatima!("∀ #Text #Text -> #Bool"),
      Self::Char => yatima!("∀ #Nat #Text -> #Char"),
      Self::Byte => yatima!("∀ #Nat #Text -> #U8"),
      Self::Line => yatima!("∀ #Nat #Text -> #Text"),
      Self::CharAtByte => yatima!("∀ #Nat #Text -> #Nat"),
      Self::ByteAtChar => yatima!("∀ #Nat #Text -> #Nat"),
      Self::LineAtByte => yatima!("∀ #Nat #Text -> #Nat"),
      Self::LineAtChar => yatima!("∀ #Nat #Text -> #Nat"),
      Self::LineStartChar => yatima!("∀ #Nat #Text -> #Nat"),
      Self::LineStartByte => yatima!("∀ #Nat #Text -> #Nat"),
      Self::ToBytes => yatima!("∀ #Text -> #Bytes"),
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
      (Self::ToBytes, Text(xs)) => Some(Bytes(xs.bytes().collect::<Vec<u8>>())),
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
      (Self::Char, Nat(idx), Text(ys)) => {
        let idx: usize = idx.clone().try_into().ok()?;
        if idx < ys.len_chars() { Some(Char(ys.char(idx))) } else { None }
      }
      (Self::Byte, Nat(idx), Text(ys)) => {
        let idx: usize = idx.clone().try_into().ok()?;
        if idx < ys.len_chars() { Some(U8(ys.byte(idx))) } else { None }
      }
      (Self::Line, Nat(idx), Text(ys)) => {
        let idx: usize = idx.clone().try_into().ok()?;
        if idx < ys.len_lines() {
          Some(Text(ys.line(idx).into()))
        }
        else {
          None
        }
      }
      (Self::CharAtByte, Nat(idx), Text(ys)) => {
        let idx: usize = idx.clone().try_into().ok()?;
        if idx < ys.len_bytes() {
          Some(Nat(ys.byte_to_char(idx).into()))
        }
        else {
          None
        }
      }
      (Self::ByteAtChar, Nat(idx), Text(ys)) => {
        let idx: usize = idx.clone().try_into().ok()?;
        if idx < ys.len_chars() {
          Some(Nat(ys.char_to_byte(idx).into()))
        }
        else {
          None
        }
      }
      (Self::LineAtChar, Nat(idx), Text(ys)) => {
        let idx: usize = idx.clone().try_into().ok()?;
        if idx < ys.len_chars() {
          Some(Nat(ys.char_to_line(idx).into()))
        }
        else {
          None
        }
      }
      (Self::LineAtByte, Nat(idx), Text(ys)) => {
        let idx: usize = idx.clone().try_into().ok()?;
        if idx < ys.len_bytes() {
          Some(Nat(ys.byte_to_line(idx).into()))
        }
        else {
          None
        }
      }
      (Self::LineStartChar, Nat(idx), Text(ys)) => {
        let idx: usize = idx.clone().try_into().ok()?;
        if idx < ys.len_lines() {
          Some(Nat(ys.line_to_char(idx).into()))
        }
        else {
          None
        }
      }
      (Self::LineStartByte, Nat(idx), Text(ys)) => {
        let idx: usize = idx.clone().try_into().ok()?;
        if idx < ys.len_lines() {
          Some(Nat(ys.line_to_byte(idx).into()))
        }
        else {
          None
        }
      }
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
    (Ok(from), Ok(upto))
      if (from <= len) && (upto <= len) && (upto >= from) =>
    {
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
  use crate::prim::tests::TestArg3;
  use core::fmt::Debug;
  use quickcheck::{
    Arbitrary,
    Gen,
    TestResult,
  };
  use rand::Rng;
  use sp_std::mem;
  use Literal::{
    Bool,
    Bytes,
    Char,
    Nat,
    Text,
    U8,
  };
  impl Arbitrary for TextOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..=23);
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
        18 => Self::ByteAtChar,
        19 => Self::LineAtByte,
        20 => Self::LineAtChar,
        21 => Self::LineStartChar,
        22 => Self::LineStartByte,
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

  #[quickcheck]
  fn test_apply(
    op: TextOp,
    a: String,
    b: char,
    c: String,
    d: u64,
    e: u64,
  ) -> TestResult {
    let a = Rope::from(a);
    let c = Rope::from(c);
    let big = BigUint::from;
    let apply1_text = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(TextOp::apply1(op, &Text(a.clone())) == expected)
    };

    let apply2_char_text = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        TextOp::apply2(op, &Char(b), &Text(a.clone())) == expected,
      )
    };

    let apply2_text_text = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        TextOp::apply2(op, &Text(a.clone()), &Text(c.clone())) == expected,
      )
    };

    let apply2_nat_text = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        TextOp::apply2(op, &Nat(big(d)), &Text(a.clone())) == expected,
      )
    };

    let apply3_nat_text_text = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        TextOp::apply3(op, &Nat(big(d)), &Text(a.clone()), &Text(c.clone()))
          == expected,
      )
    };

    let apply3_nat_nat_text = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        TextOp::apply3(op, &Nat(big(d)), &Nat(big(e)), &Text(a.clone()))
          == expected,
      )
    };

    match op {
      TextOp::Cons => {
        let mut cs = a.clone();
        cs.insert_char(0, b);
        apply2_char_text(Some(Text(cs)))
      }
      TextOp::LenChars => apply1_text(Some(Nat(a.len_chars().into()))),
      TextOp::LenLines => apply1_text(Some(Nat(a.len_lines().into()))),
      TextOp::LenBytes => apply1_text(Some(Nat(a.len_bytes().into()))),
      TextOp::Append => {
        let mut xs = a.clone();
        xs.append(c.clone());
        apply2_text_text(Some(Text(xs)))
      }
      TextOp::Insert => apply3_nat_text_text(Some(Text(safe_insert(
        &big(d),
        a.clone(),
        c.clone(),
      )))),
      TextOp::Remove => apply3_nat_nat_text(Some(Text(safe_remove(
        &big(d),
        &big(e),
        a.clone(),
      )))),
      TextOp::Take => {
        apply2_nat_text(Some(Text(safe_split(&big(d), a.clone()).0)))
      }
      TextOp::Drop => {
        apply2_nat_text(Some(Text(safe_split(&big(d), a.clone()).1)))
      }
      TextOp::Eql => apply2_text_text(Some(Bool(a == c))),
      TextOp::Lte => apply2_text_text(Some(Bool(a <= c))),
      TextOp::Lth => apply2_text_text(Some(Bool(a < c))),
      TextOp::Gte => apply2_text_text(Some(Bool(a >= c))),
      TextOp::Gth => apply2_text_text(Some(Bool(a > c))),
      TextOp::Char => {
        let idx: Option<usize> = big(d).clone().try_into().ok();
        match idx {
          None => apply2_nat_text(None),
          Some(idx) => {
            if idx < a.len_chars() {
              apply2_nat_text(Some(Char(a.char(idx))))
            }
            else {
              apply2_nat_text(None)
            }
          }
        }
      }
      TextOp::Byte => {
        let idx: Option<usize> = big(d).clone().try_into().ok();
        match idx {
          None => apply2_nat_text(None),
          Some(idx) => {
            if idx < a.len_chars() {
              apply2_nat_text(Some(U8(a.byte(idx))))
            }
            else {
              apply2_nat_text(None)
            }
          }
        }
      }
      TextOp::Line => {
        let idx: Option<usize> = big(d).clone().try_into().ok();
        match idx {
          None => apply2_nat_text(None),
          Some(idx) => {
            if idx < a.len_lines() {
              apply2_nat_text(Some(Text(a.line(idx).into())))
            }
            else {
              apply2_nat_text(None)
            }
          }
        }
      }
      TextOp::CharAtByte => {
        let idx: Option<usize> = big(d).clone().try_into().ok();
        match idx {
          None => apply2_nat_text(None),
          Some(idx) => {
            if idx < a.len_bytes() {
              apply2_nat_text(Some(Nat(a.byte_to_char(idx).into())))
            }
            else {
              apply2_nat_text(None)
            }
          }
        }
      }
      TextOp::ByteAtChar => {
        let idx: Option<usize> = big(d).clone().try_into().ok();
        match idx {
          None => apply2_nat_text(None),
          Some(idx) => {
            if idx < a.len_chars() {
              apply2_nat_text(Some(Nat(a.char_to_byte(idx).into())))
            }
            else {
              apply2_nat_text(None)
            }
          }
        }
      }
      TextOp::LineAtByte => {
        let idx: Option<usize> = big(d).clone().try_into().ok();
        match idx {
          None => apply2_nat_text(None),
          Some(idx) => {
            if idx < a.len_bytes() {
              apply2_nat_text(Some(Nat(a.byte_to_line(idx).into())))
            }
            else {
              apply2_nat_text(None)
            }
          }
        }
      }
      TextOp::LineAtChar => {
        let idx: Option<usize> = big(d).clone().try_into().ok();
        match idx {
          None => apply2_nat_text(None),
          Some(idx) => {
            if idx < a.len_chars() {
              apply2_nat_text(Some(Nat(a.char_to_line(idx).into())))
            }
            else {
              apply2_nat_text(None)
            }
          }
        }
      }
      TextOp::LineStartChar => {
        let idx: Option<usize> = big(d).clone().try_into().ok();
        match idx {
          None => apply2_nat_text(None),
          Some(idx) => {
            if idx < a.len_lines() {
              apply2_nat_text(Some(Nat(a.line_to_char(idx).into())))
            }
            else {
              apply2_nat_text(None)
            }
          }
        }
      }
      TextOp::LineStartByte => {
        let idx: Option<usize> = big(d).clone().try_into().ok();
        match idx {
          None => apply2_nat_text(None),
          Some(idx) => {
            if idx < a.len_lines() {
              apply2_nat_text(Some(Nat(a.line_to_byte(idx).into())))
            }
            else {
              apply2_nat_text(None)
            }
          }
        }
      }
      TextOp::ToBytes => {
        apply1_text(Some(Bytes(a.bytes().collect::<Vec<u8>>())))
      }
    }
  }

  #[derive(Debug, Clone)]
  struct ArgsApplyNoneOnInvalid(
    TextOp,
    Literal,
    String,
    char,
    String,
    u64,
    u64,
    bool,
    TestArg3,
  );

  impl Arbitrary for ArgsApplyNoneOnInvalid {
    fn arbitrary(g: &mut Gen) -> ArgsApplyNoneOnInvalid {
      ArgsApplyNoneOnInvalid(
        TextOp::arbitrary(g),
        Literal::arbitrary(g),
        String::arbitrary(g),
        char::arbitrary(g),
        String::arbitrary(g),
        u64::arbitrary(g),
        u64::arbitrary(g),
        bool::arbitrary(g),
        TestArg3::arbitrary(g),
      )
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(args: ArgsApplyNoneOnInvalid) -> TestResult {
    let ArgsApplyNoneOnInvalid(op, a, b, c, d, e, f, test_arg_2, test_arg_3) =
      args;
    let b = Rope::from(b);
    let d = Rope::from(d);
    let big = BigUint::from;
    let test_apply1_none_on_invalid = |valid_arg: Literal| -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      }
      else {
        TestResult::from_bool(TextOp::apply1(op, &a) == None)
      }
    };

    let test_apply2_none_on_invalid =
      |valid_arg: Literal, a_: Literal, b_: Literal| -> TestResult {
        let go = || TestResult::from_bool(TextOp::apply2(op, &a_, &b_) == None);
        if test_arg_2 {
          if mem::discriminant(&valid_arg) == mem::discriminant(&a_) {
            TestResult::discard()
          }
          else {
            go()
          }
        }
        else {
          if mem::discriminant(&valid_arg) == mem::discriminant(&b_) {
            TestResult::discard()
          }
          else {
            go()
          }
        }
      };

    let test_apply3_none_on_invalid = |valid_arg: Literal,
                                       a_: Literal,
                                       b_: Literal,
                                       c_: Literal|
     -> TestResult {
      let go =
        || TestResult::from_bool(TextOp::apply3(op, &a_, &b_, &c_) == None);
      match test_arg_3 {
        TestArg3::A => {
          if mem::discriminant(&valid_arg) == mem::discriminant(&a_) {
            TestResult::discard()
          }
          else {
            go()
          }
        }
        TestArg3::B => {
          if mem::discriminant(&valid_arg) == mem::discriminant(&b_) {
            TestResult::discard()
          }
          else {
            go()
          }
        }
        TestArg3::C => {
          if mem::discriminant(&valid_arg) == mem::discriminant(&c_) {
            TestResult::discard()
          }
          else {
            go()
          }
        }
      }
    };

    match op {
      // Arity 1, valid is Text.
      TextOp::LenChars
      | TextOp::LenBytes
      | TextOp::LenLines
      | TextOp::ToBytes => test_apply1_none_on_invalid(Text(b)),
      // Arity 2, valid are Char on a and Text on b.
      TextOp::Cons => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Char(c), a, Text(b.clone()))
        }
        else {
          test_apply2_none_on_invalid(Text(b.clone()), Char(c), a)
        }
      }
      // Arity 2, valid are Text on a and b.
      TextOp::Append
      | TextOp::Eql
      | TextOp::Lte
      | TextOp::Lth
      | TextOp::Gte
      | TextOp::Gth => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Text(b.clone()), a, Text(b.clone()))
        }
        else {
          test_apply2_none_on_invalid(Text(b.clone()), Text(b.clone()), a)
        }
      }
      // Arity 2, valid are Nat on a and Text on b.
      TextOp::Take
      | TextOp::Drop
      | TextOp::Char
      | TextOp::Byte
      | TextOp::Line
      | TextOp::CharAtByte
      | TextOp::ByteAtChar
      | TextOp::LineAtChar
      | TextOp::LineAtByte
      | TextOp::LineStartChar
      | TextOp::LineStartByte => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Nat(big(e)), a, Text(b.clone()))
        }
        else {
          test_apply2_none_on_invalid(Text(b.clone()), Nat(big(e)), a)
        }
      }
      // Arity 3, valid are Nat on a, Text on b and Text on c.
      TextOp::Insert => match test_arg_3 {
        TestArg3::A => test_apply3_none_on_invalid(
          Nat(big(e)),
          a,
          Text(b.clone()),
          Text(d.clone()),
        ),
        TestArg3::B => test_apply3_none_on_invalid(
          Text(b.clone()),
          Nat(big(e)),
          a,
          Text(b.clone()),
        ),
        TestArg3::C => test_apply3_none_on_invalid(
          Text(b.clone()),
          Nat(big(e)),
          Text(b.clone()),
          a,
        ),
      },
      // Arity 3, valid are Nat on a, Nat on b and Text on c.
      TextOp::Remove => match test_arg_3 {
        TestArg3::A => test_apply3_none_on_invalid(
          Nat(big(e)),
          a,
          Nat(big(e)),
          Text(d.clone()),
        ),
        TestArg3::B => test_apply3_none_on_invalid(
          Nat(big(e)),
          Nat(big(e)),
          a,
          Text(b.clone()),
        ),
        TestArg3::C => test_apply3_none_on_invalid(
          Text(b.clone()),
          Nat(big(e)),
          Nat(big(f)),
          a,
        ),
      },
    }
  }
}
