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
  prim::bits,
  term::Term,
  yatima,
};

use core::convert::TryFrom;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BytesOp {
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
  ToBits,
}

impl BytesOp {
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
      Self::ToBits => "to_Bits".to_owned(),
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
      "to_Bits" => Some(Self::ToBits),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Cons => yatima!("∀ #U8 #Bytes -> #Bytes"),
      Self::Len => yatima!("∀ #Bytes -> #Nat"),
      Self::Head => yatima!("∀ #Bytes -> #U8"),
      Self::Tail => yatima!("∀ #Bytes -> #Bytes"),
      Self::Take => yatima!("∀ #Nat #Bytes -> #Bytes"),
      Self::Drop => yatima!("∀ #Nat #Bytes -> #Bytes"),
      Self::Append => yatima!("∀ #Bytes #Bytes -> #Bytes"),
      Self::Insert => yatima!("∀ #U8 #Nat #Bytes -> #Bytes"),
      Self::Remove => yatima!("∀ #Nat #Bytes -> #Bytes"),
      Self::Index => yatima!("∀ #Nat #Bytes -> #U8"),
      Self::ToBits => yatima!("∀ #Nat #Bytes -> #Bits"),
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
      Self::ToBits => Ipld::Integer(10),
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
      Ipld::Integer(10) => Ok(Self::ToBits),
      xs => Err(IpldError::BytesOp(xs.to_owned())),
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
      Self::ToBits => 2,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::Len, Bytes(xs)) => Some(Nat(xs.len().into())),
      (Self::Head, Bytes(xs)) => {
        let x = xs.last();
        x.map(|x| U8(*x))
      }
      (Self::Tail, Bytes(xs)) => Some(Bytes(if xs.is_empty() {
        vec![]
      }
      else {
        xs[0..xs.len() - 1].to_vec()
      })),
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Cons, U8(x), Bytes(xs)) => {
        let mut xs = xs.clone();
        xs.push(*x);
        Some(Bytes(xs))
      }
      (Self::Drop, Nat(x), Bytes(xs)) => {
        let (_, ys) = safe_split(x, xs);
        Some(Bytes(ys))
      }
      (Self::Take, Nat(x), Bytes(xs)) => {
        let (xs, _) = safe_split(x, xs);
        Some(Bytes(xs))
      }
      (Self::Append, Bytes(xs), Bytes(ys)) => {
        let mut xs = xs.clone();
        xs.extend_from_slice(&ys);
        Some(Bytes(xs))
      }
      (Self::Remove, Nat(idx), Bytes(xs)) => {
        let idx = usize::try_from(idx);
        match idx {
          Ok(idx) if idx < xs.len() => {
            let mut xs = xs.clone();
            xs.remove(idx);
            Some(Bytes(xs))
          }
          _ => Some(Bytes(xs.clone())),
        }
      }
      (Self::Index, Nat(idx), Bytes(xs)) => {
        let idx = usize::try_from(idx);
        match idx {
          Ok(idx) if idx < xs.len() => {
            let x = xs[idx];
            Some(U8(x))
          }
          _ => None,
        }
      }
      (Self::ToBits, Nat(idx), Bytes(xs)) => match usize::try_from(idx) {
        Ok(x) => Some(Literal::Bits(bits::bytes_to_bits(x, xs))),
        _ => None,
      },
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
      (Self::Insert, Nat(idx), U8(y), Bytes(xs)) => {
        let idx = usize::try_from(idx);
        match idx {
          Ok(idx) if idx < xs.len() => {
            let mut xs = xs.clone();
            xs.insert(idx, *y);
            Some(Bytes(xs))
          }
          _ => Some(Bytes(xs.clone())),
        }
      }
      _ => None,
    }
  }
}

pub fn safe_split(idx: &BigUint, xs: &Vec<u8>) -> (Vec<u8>, Vec<u8>) {
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

impl fmt::Display for BytesOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.symbol())
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::prim::{
    tests::TestArg3,
    BitsOp,
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
    Bits,
    Bytes,
    Nat,
    U8,
  };
  impl Arbitrary for BytesOp {
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
        _ => Self::ToBits,
      }
    }
  }

  #[quickcheck]
  fn bytes_op_ipld(x: BytesOp) -> bool {
    match BytesOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn test_apply(
    op: BytesOp,
    a: Vec<u8>,
    b: u8,
    c: u64,
    d: Vec<u8>,
  ) -> TestResult {
    let big = BigUint::from;
    let apply1_bytes = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(BytesOp::apply1(op, &Bytes(a.clone())) == expected)
    };

    let apply2_u8_bytes = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        BytesOp::apply2(op, &U8(b), &Bytes(a.clone())) == expected,
      )
    };

    let apply2_nat_bytes = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        BytesOp::apply2(op, &Nat(big(c)), &Bytes(a.clone())) == expected,
      )
    };

    let apply2_bytes_bytes = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        BytesOp::apply2(op, &Bytes(a.clone()), &Bytes(d.clone())) == expected,
      )
    };

    let apply3_nat_u8_bytes = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        BytesOp::apply3(op, &Nat(big(c)), &U8(b), &Bytes(a.clone()))
          == expected,
      )
    };

    let from_bool = TestResult::from_bool;

    match op {
      BytesOp::Cons => {
        let mut a = a.clone();
        a.push(b);
        apply2_u8_bytes(Some(Bytes(a)))
      }
      BytesOp::Len => apply1_bytes(Some(Nat(a.len().into()))),
      BytesOp::Head => apply1_bytes(a.last().map(|z| U8(*z))),
      BytesOp::Tail => apply1_bytes(Some(Bytes(if a.is_empty() {
        vec![]
      }
      else {
        a[0..a.len() - 1].to_vec()
      }))),
      BytesOp::Take => apply2_nat_bytes(Some(Bytes(safe_split(&big(c), &a).0))),
      BytesOp::Drop => apply2_nat_bytes(Some(Bytes(safe_split(&big(c), &a).1))),
      BytesOp::Append => {
        let mut a = a.clone();
        a.extend_from_slice(&d);
        apply2_bytes_bytes(Some(Bytes(a)))
      }
      BytesOp::Insert => {
        let idx = usize::try_from(c);
        match idx {
          Ok(idx) if idx < a.len() => {
            let mut a = a.clone();
            a.insert(idx, b);
            apply3_nat_u8_bytes(Some(Bytes(a)))
          }
          _ => apply3_nat_u8_bytes(Some(Bytes(a.clone()))),
        }
      }
      BytesOp::Remove => {
        let idx = usize::try_from(c);
        match idx {
          Ok(idx) if idx < a.len() => {
            let mut a = a.clone();
            a.remove(idx);
            apply2_nat_bytes(Some(Bytes(a)))
          }
          _ => apply2_nat_bytes(Some(Bytes(a.clone()))),
        }
      }
      BytesOp::Index => {
        let idx = usize::try_from(c);
        match idx {
          Ok(idx) if idx < a.len() => apply2_nat_bytes(Some(U8(a[idx]))),
          _ => apply2_nat_bytes(None),
        }
      }
      BytesOp::ToBits => {
        let bits = BytesOp::apply1(op, &Bytes(a.clone()));
        match bits {
          None => apply1_bytes(None),
          Some(bits_) => match bits_.clone() {
            Bits(_) => {
              let bytes = BitsOp::apply2(
                BitsOp::ToBytes,
                &Nat(big(a.len().try_into().unwrap())),
                &bits_,
              );
              match bytes {
                None => apply1_bytes(None),
                Some(bytes_) => from_bool(bytes_ == Bytes(a.clone())),
              }
            }
            _ => apply1_bytes(None),
          },
        }
      }
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: BytesOp,
    a: Literal,
    b: Vec<u8>,
    c: u8,
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
        TestResult::from_bool(BytesOp::apply1(op, &a) == None)
      }
    };

    let test_apply2_none_on_invalid =
      |valid_arg: Literal, a_: Literal, b_: Literal| -> TestResult {
        let go =
          || TestResult::from_bool(BytesOp::apply2(op, &a_, &b_) == None);
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
        || TestResult::from_bool(BytesOp::apply3(op, &a_, &b_, &c_) == None);
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
      // Arity 1, valid is Bytes.
      BytesOp::Len | BytesOp::Head | BytesOp::Tail => {
        test_apply1_none_on_invalid(Bytes(b))
      }
      // Arity 2, valid are U8 on a and Bytes on b.
      BytesOp::Cons => {
        if test_arg_2 {
          test_apply2_none_on_invalid(U8(c), a, Bytes(b))
        }
        else {
          test_apply2_none_on_invalid(Bytes(b), U8(c), a)
        }
      }
      // Arity 2, valid are Nat on a and Bytes on b.
      BytesOp::Take
      | BytesOp::Drop
      | BytesOp::Remove
      | BytesOp::Index
      | BytesOp::ToBits => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Nat(big(d)), a, Bytes(b))
        }
        else {
          test_apply2_none_on_invalid(Bytes(b), Nat(big(d)), a)
        }
      }
      // Arity 2, valid are Bytes on a and b.
      BytesOp::Append => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Bytes(b.clone()), a, Bytes(b))
        }
        else {
          test_apply2_none_on_invalid(Bytes(b.clone()), Bytes(b), a)
        }
      }
      // Arity 3, valid are Nat on a, U8 on b and Bytes on c.
      BytesOp::Insert => match test_arg_3 {
        TestArg3::A => {
          test_apply3_none_on_invalid(Nat(big(d)), a, U8(c), Bytes(b))
        }
        TestArg3::B => {
          test_apply3_none_on_invalid(U8(c), Nat(big(d)), a, Bytes(b))
        }
        TestArg3::C => {
          test_apply3_none_on_invalid(Bytes(b), Nat(big(d)), U8(c), a)
        }
      },
    }
  }
}
