use num_bigint::BigUint;
use sp_ipld::Ipld;
use sp_std::{
  borrow::ToOwned,
  fmt,
  vec::Vec,
};

use alloc::string::String;

use crate::{
  defs,
  ipld_error::IpldError,
  literal::Literal,
  parse,
  term::Term,
  yatima,
};

use core::convert::TryFrom;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BitsOp {
  Cons,
  Len,
  Head,
  Tail,
  Take,
  Drop,
  Append,
  Insert,
  Remove,
  Index,
  ToBytes,
}

impl BitsOp {
  pub fn symbol(self) -> String {
    match self {
      Self::Cons => "cons".to_owned(),
      Self::Len => "len".to_owned(),
      Self::Head => "head".to_owned(),
      Self::Tail => "tail".to_owned(),
      Self::Take => "take".to_owned(),
      Self::Drop => "drop".to_owned(),
      Self::Append => "append".to_owned(),
      Self::Insert => "insert".to_owned(),
      Self::Remove => "remove".to_owned(),
      Self::Index => "index".to_owned(),
      Self::ToBytes => "to_Bytes".to_owned(),
    }
  }

  pub fn from_symbol(x: &str) -> Option<Self> {
    match x {
      "cons" => Some(Self::Cons),
      "len" => Some(Self::Len),
      "head" => Some(Self::Head),
      "tail" => Some(Self::Tail),
      "take" => Some(Self::Take),
      "drop" => Some(Self::Drop),
      "append" => Some(Self::Append),
      "insert" => Some(Self::Insert),
      "remove" => Some(Self::Remove),
      "index" => Some(Self::Index),
      "to_Bytes" => Some(Self::ToBytes),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Cons => yatima!("∀ #Bool #Bits -> #Bits"),
      Self::Len => yatima!("∀ #Bits -> #Nat"),
      Self::Head => yatima!("∀ #Bits -> #U8"),
      Self::Tail => yatima!("∀ #Bits -> #Bits"),
      Self::Take => yatima!("∀ #Nat #Bits -> #Bits"),
      Self::Drop => yatima!("∀ #Nat #Bits -> #Bits"),
      Self::Append => yatima!("∀ #Bits #Bits -> #Bits"),
      Self::Insert => yatima!("∀ #Bool #Nat #Bits -> #Bits"),
      Self::Remove => yatima!("∀ #Nat #Bits -> #Bits"),
      Self::Index => yatima!("∀ #Nat #Bits -> #U8"),
      Self::ToBytes => yatima!("∀ #Bits -> #Bytes"),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Cons => Ipld::Integer(0),
      Self::Len => Ipld::Integer(1),
      Self::Head => Ipld::Integer(2),
      Self::Tail => Ipld::Integer(3),
      Self::Take => Ipld::Integer(4),
      Self::Drop => Ipld::Integer(5),
      Self::Append => Ipld::Integer(6),
      Self::Insert => Ipld::Integer(7),
      Self::Remove => Ipld::Integer(8),
      Self::Index => Ipld::Integer(9),
      Self::ToBytes => Ipld::Integer(10),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::Cons),
      Ipld::Integer(1) => Ok(Self::Len),
      Ipld::Integer(2) => Ok(Self::Head),
      Ipld::Integer(3) => Ok(Self::Tail),
      Ipld::Integer(4) => Ok(Self::Take),
      Ipld::Integer(5) => Ok(Self::Drop),
      Ipld::Integer(6) => Ok(Self::Append),
      Ipld::Integer(7) => Ok(Self::Insert),
      Ipld::Integer(8) => Ok(Self::Remove),
      Ipld::Integer(9) => Ok(Self::Index),
      Ipld::Integer(10) => Ok(Self::ToBytes),
      xs => Err(IpldError::BitsOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::Cons => 2,
      Self::Len => 1,
      Self::Head => 1,
      Self::Tail => 1,
      Self::Take => 2,
      Self::Drop => 2,
      Self::Append => 2,
      Self::Insert => 3,
      Self::Remove => 2,
      Self::Index => 2,
      Self::ToBytes => 1,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::Len, Bits(xs)) => Some(Nat(xs.len().into())),
      (Self::Head, Bits(xs)) => {
        let x = xs.last();
        x.map(|x| Bool(*x))
      }
      (Self::Tail, Bits(xs)) => Some(Bits(if xs.is_empty() {
        vec![]
      }
      else {
        xs[0..xs.len() - 1].to_vec()
      })),
      (Self::ToBytes, Bits(xs)) => Some(Literal::Bytes(bits_to_bytes(xs).1)),
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Cons, Bool(x), Bits(xs)) => {
        let mut xs = xs.clone();
        xs.push(*x);
        Some(Bits(xs))
      }
      (Self::Drop, Nat(x), Bits(xs)) => {
        let (_, ys) = safe_split(x, xs);
        Some(Bits(ys))
      }
      (Self::Take, Nat(x), Bits(xs)) => {
        let (xs, _) = safe_split(x, xs);
        Some(Bits(xs))
      }
      (Self::Append, Bits(xs), Bits(ys)) => {
        let mut xs = xs.clone();
        xs.extend_from_slice(&ys);
        Some(Bits(xs))
      }
      (Self::Remove, Nat(idx), Bits(xs)) => {
        let idx = usize::try_from(idx);
        match idx {
          Ok(idx) if idx < xs.len() => {
            let mut xs = xs.clone();
            xs.remove(idx);
            Some(Bits(xs))
          }
          _ => Some(Bits(xs.clone())),
        }
      }
      (Self::Index, Nat(idx), Bits(xs)) => {
        let idx = usize::try_from(idx);
        match idx {
          Ok(idx) if idx < xs.len() => {
            let x = xs[idx];
            Some(Bool(x))
          }
          _ => None,
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
      (Self::Insert, Nat(idx), Bool(y), Bits(xs)) => {
        let idx = usize::try_from(idx);
        match idx {
          Ok(idx) if idx < xs.len() => {
            let mut xs = xs.clone();
            xs.insert(idx, *y);
            Some(Bits(xs))
          }
          _ => Some(Bits(xs.clone())),
        }
      }
      _ => None,
    }
  }
}

pub fn safe_split(idx: &BigUint, xs: &Vec<bool>) -> (Vec<bool>, Vec<bool>) {
  let idx = usize::try_from(idx);
  match idx {
    Ok(idx) if idx <= xs.len() => {
      let ys = xs[0..idx].to_vec();
      let zs = xs[idx..].to_vec();
      (ys, zs)
    }
    _ => (xs.clone(), vec![]),
  }
}

pub fn bits_to_byte(bits: &[bool]) -> u8 {
  let mut i = 0;
  for b in bits.iter().rev() {
    if *b {
      i = (i << 1) + 1;
    }
    else {
      i = i << 1;
    }
  }
  i
}

pub fn byte_to_bits(byte: u8) -> [bool; 8] {
  let mut x = byte;
  let mut res: [bool; 8] = [false; 8];
  for i in 0..=7 {
    res[i] = x % 2 == 1;
    x = x >> 1;
  }
  res
}

pub fn bits_to_bytes(bits: &Vec<bool>) -> (usize, Vec<u8>) {
  let len = bits.len();
  let mut res = Vec::new();
  let mut idx = 0;
  while len >= idx + 8 {
    let byte = bits_to_byte(&bits[idx..idx + 8]);
    res.push(byte);
    idx = idx + 8;
  }
  if len > idx {
    let byte = bits_to_byte(&bits[idx..]);
    res.push(byte);
  }
  (len, res)
}

pub fn bytes_to_bits(len: usize, bytes: &Vec<u8>) -> Vec<bool> {
  let mut res = Vec::new();
  for byte in bytes {
    let bits = byte_to_bits(*byte);
    for b in bits.iter() {
      res.push(*b)
    }
  }
  res.truncate(len);
  res
}

impl fmt::Display for BitsOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.symbol())
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::prim::{
    tests::TestArg3,
    BytesOp,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
    TestResult,
  };
  use rand::Rng;
  use sp_std::{
    convert::TryInto,
    mem,
  };
  use Literal::{
    Bool,
    Nat,
  };
  impl Arbitrary for BitsOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..=10);
      match gen {
        0 => Self::Cons,
        1 => Self::Len,
        2 => Self::Head,
        3 => Self::Tail,
        4 => Self::Take,
        5 => Self::Drop,
        6 => Self::Append,
        7 => Self::Insert,
        8 => Self::Remove,
        9 => Self::Index,
        _ => Self::ToBytes,
      }
    }
  }

  #[quickcheck]
  fn bytes_op_ipld(x: BitsOp) -> bool {
    match BitsOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
  #[derive(PartialEq, Eq, Clone, Copy, Debug)]
  struct Bits([bool; 8]);

  impl Arbitrary for Bits {
    fn arbitrary(g: &mut Gen) -> Self {
      let mut res: [bool; 8] =
        [false, false, false, false, false, false, false, false];
      for i in 0..7 {
        res[i] = Arbitrary::arbitrary(g)
      }
      Bits(res)
    }
  }

  #[quickcheck]
  fn test_bits_to_byte(x: Bits) -> bool {
    byte_to_bits(bits_to_byte(&x.0)) == x.0
  }

  #[quickcheck]
  fn test_byte_to_bits(x: u8) -> bool { bits_to_byte(&byte_to_bits(x)) == x }

  #[quickcheck]
  fn test_bits_to_bytes(x: Vec<bool>) -> bool {
    let (len, x1) = bits_to_bytes(&x);
    bytes_to_bits(len, &x1) == x
  }

  #[quickcheck]
  fn test_bytes_to_bits(x: Vec<u8>) -> bool {
    let len = x.len() * 8;
    bits_to_bytes(&bytes_to_bits(len, &x)) == (len, x)
  }

  #[quickcheck]
  fn test_apply(
    op: BitsOp,
    a: Vec<bool>,
    b: bool,
    c: u64,
    d: Vec<bool>,
  ) -> TestResult {
    let big = BigUint::from;
    let apply1_bits = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        BitsOp::apply1(op, &Literal::Bits(a.clone())) == expected,
      )
    };

    let apply2_bool_bits = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        BitsOp::apply2(op, &Bool(b), &Literal::Bits(a.clone())) == expected,
      )
    };

    let apply2_nat_bits = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        BitsOp::apply2(op, &Nat(big(c)), &Literal::Bits(a.clone())) == expected,
      )
    };

    let apply2_bits_bits = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        BitsOp::apply2(
          op,
          &Literal::Bits(a.clone()),
          &Literal::Bits(d.clone()),
        ) == expected,
      )
    };

    let apply3_nat_bool_bits = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        BitsOp::apply3(op, &Nat(big(c)), &Bool(b), &Literal::Bits(a.clone()))
          == expected,
      )
    };

    let from_bool = TestResult::from_bool;

    match op {
      BitsOp::Cons => {
        let mut a = a.clone();
        a.push(b);
        apply2_bool_bits(Some(Literal::Bits(a)))
      }
      BitsOp::Len => apply1_bits(Some(Nat(a.len().into()))),
      BitsOp::Head => apply1_bits(a.last().map(|z| Bool(*z))),
      BitsOp::Tail => apply1_bits(Some(Literal::Bits(if a.is_empty() {
        vec![]
      }
      else {
        a[0..a.len() - 1].to_vec()
      }))),
      BitsOp::Take => {
        apply2_nat_bits(Some(Literal::Bits(safe_split(&big(c), &a).0)))
      }
      BitsOp::Drop => {
        apply2_nat_bits(Some(Literal::Bits(safe_split(&big(c), &a).1)))
      }
      BitsOp::Append => {
        let mut a = a.clone();
        a.extend_from_slice(&d);
        apply2_bits_bits(Some(Literal::Bits(a)))
      }
      BitsOp::Insert => {
        let idx = usize::try_from(c);
        match idx {
          Ok(idx) if idx < a.len() => {
            let mut a = a.clone();
            a.insert(idx, b);
            apply3_nat_bool_bits(Some(Literal::Bits(a)))
          }
          _ => apply3_nat_bool_bits(Some(Literal::Bits(a.clone()))),
        }
      }
      BitsOp::Remove => {
        let idx = usize::try_from(c);
        match idx {
          Ok(idx) if idx < a.len() => {
            let mut a = a.clone();
            a.remove(idx);
            apply2_nat_bits(Some(Literal::Bits(a)))
          }
          _ => apply2_nat_bits(Some(Literal::Bits(a.clone()))),
        }
      }
      BitsOp::Index => {
        let idx = usize::try_from(c);
        match idx {
          Ok(idx) if idx < a.len() => apply2_nat_bits(Some(Bool(a[idx]))),
          _ => apply2_nat_bits(None),
        }
      }
      BitsOp::ToBytes => {
        let bytes = BitsOp::apply1(op, &Literal::Bits(a.clone()));
        match bytes {
          None => apply1_bits(None),
          Some(bytes_) => match bytes_.clone() {
            Literal::Bytes(_) => {
              let bits = BytesOp::apply2(
                BytesOp::ToBits,
                &Literal::Nat(big(a.len().try_into().unwrap())),
                &bytes_,
              );
              match bits {
                None => apply1_bits(None),
                Some(bits_) => from_bool(bits_ == Literal::Bits(a.clone())),
              }
            }
            _ => apply1_bits(None),
          },
        }
      }
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: BitsOp,
    a: Literal,
    b: Vec<bool>,
    c: bool,
    d: u64,
    test_arg_2: bool,
    test_arg_3: TestArg3,
  ) -> TestResult {
    let big = BigUint::from;
    let test_apply1_none_on_invalid = |valid_arg: Literal| -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      }
      else {
        TestResult::from_bool(BitsOp::apply1(op, &a) == None)
      }
    };

    let test_apply2_none_on_invalid =
      |valid_arg: Literal, a_: Literal, b_: Literal| -> TestResult {
        let go = || TestResult::from_bool(BitsOp::apply2(op, &a_, &b_) == None);
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
        || TestResult::from_bool(BitsOp::apply3(op, &a_, &b_, &c_) == None);
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
      // Arity 1, valid is Bits.
      BitsOp::Len | BitsOp::Head | BitsOp::Tail | BitsOp::ToBytes => {
        test_apply1_none_on_invalid(Literal::Bits(b))
      }
      // Arity 2, valid are Bool on a and Bits on b.
      BitsOp::Cons => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Bool(c), a, Literal::Bits(b))
        }
        else {
          test_apply2_none_on_invalid(Literal::Bits(b), Bool(c), a)
        }
      }
      // Arity 2, valid are Nat on a and Bits on b.
      BitsOp::Take | BitsOp::Drop | BitsOp::Remove | BitsOp::Index => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Nat(big(d)), a, Literal::Bits(b))
        }
        else {
          test_apply2_none_on_invalid(Literal::Bits(b), Nat(big(d)), a)
        }
      }
      // Arity 2, valid are Bits on a and b.
      BitsOp::Append => {
        if test_arg_2 {
          test_apply2_none_on_invalid(
            Literal::Bits(b.clone()),
            a,
            Literal::Bits(b),
          )
        }
        else {
          test_apply2_none_on_invalid(
            Literal::Bits(b.clone()),
            Literal::Bits(b),
            a,
          )
        }
      }
      // Arity 3, valid are Nat on a, Bool on b and Bits on c.
      BitsOp::Insert => match test_arg_3 {
        TestArg3::A => {
          test_apply3_none_on_invalid(Nat(big(d)), a, Bool(c), Literal::Bits(b))
        }
        TestArg3::B => {
          test_apply3_none_on_invalid(Bool(c), Nat(big(d)), a, Literal::Bits(b))
        }
        TestArg3::C => {
          test_apply3_none_on_invalid(Literal::Bits(b), Nat(big(d)), Bool(c), a)
        }
      },
    }
  }
}
