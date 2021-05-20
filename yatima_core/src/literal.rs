use crate::{
  ipld_error::IpldError,
  parse::base,
  position::Pos,
  prim::text,
  term::Term,
  yatima,
};

use libipld::ipld::Ipld;

use ropey::Rope;

use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use std::{
  convert::TryInto,
  fmt,
};

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
  Nat(BigUint),
  Int(BigInt),
  Bytes(Vec<u8>),
  Text(Rope),
  Char(char),
  Bool(bool),
  U8(u8),
  U16(u16),
  U32(u32),
  U64(u64),
  U128(u128),
  I8(i8),
  I16(i16),
  I32(i32),
  I64(i64),
  I128(i128),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum LitType {
  Nat,
  Int,
  Bytes,
  Text,
  Char,
  Bool,
  U8,
  U16,
  U32,
  U64,
  U128,
  I8,
  I16,
  I32,
  I64,
  I128,
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Literal::*;
    match self {
      Nat(x) => write!(f, "{}", x.to_str_radix(10)),
      Int(x) => match x.sign() {
        Sign::Minus => write!(f, "-{}", x.magnitude().to_str_radix(10)),
        _ => write!(f, "+{}", x.to_str_radix(10)),
      },
      Bytes(x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "x\'{}\'", base::LitBase::Hex.encode(x))
      }
      Text(x) => {
        write!(f, "\"{}\"", Rope::to_string(x).escape_default())
      }
      Char(x) => write!(f, "'{}'", x.escape_default()),
      Bool(true) => write!(f, "#Bool.true"),
      Bool(false) => write!(f, "#Bool.false"),
      U8(x) => write!(f, "{}u8", x),
      U16(x) => write!(f, "{}u16", x),
      U32(x) => write!(f, "{}u32", x),
      U64(x) => write!(f, "{}u64", x),
      U128(x) => write!(f, "{}u128", x),
      I8(x) => {
        if x.is_negative() {
          write!(f, "{}i8", x)
        }
        else {
          write!(f, "+{}i8", x)
        }
      }
      I16(x) => {
        if x.is_negative() {
          write!(f, "{}i16", x)
        }
        else {
          write!(f, "+{}i16", x)
        }
      }
      I32(x) => {
        if x.is_negative() {
          write!(f, "{}i32", x)
        }
        else {
          write!(f, "+{}i32", x)
        }
      }
      I64(x) => {
        if x.is_negative() {
          write!(f, "{}i64", x)
        }
        else {
          write!(f, "+{}i64", x)
        }
      }
      I128(x) => {
        if x.is_negative() {
          write!(f, "{}i128", x)
        }
        else {
          write!(f, "+{}i128", x)
        }
      }
    }
  }
}

impl Literal {
  pub fn expand(self) -> Term {
    match self {
      Self::Nat(n) => {
        if n == BigUint::from(0u64) {
          yatima!("λ P z s => z")
        }
        else {
          yatima!(
            "λ P z s => s #$0",
            Term::Lit(Pos::None, Literal::Nat(n - BigUint::from(1u64)))
          )
        }
      }
      Self::Int(_) => yatima!("λ P i => i"),
      // Self::Bytes(mut t) => {
      //  let c = t.pop_front();
      //  match c {
      //    None => yatima!("λ P n c => n"),
      //    Some(c) => {
      //      yatima!(
      //        "λ P n c => c #$0 #$1",
      //        Term::Lit(Pos::None, Literal::U8(c)),
      //        Term::Lit(Pos::None, Literal::Bytes(t))
      //      )
      //    }
      //  }
      //}
      Self::Text(t) => match text::safe_head(t) {
        None => yatima!("λ P n c => n"),
        Some((c, t)) => {
          yatima!(
            "λ P n c => c #$0 #$1",
            Term::Lit(Pos::None, Literal::Char(c)),
            Term::Lit(Pos::None, Literal::Text(t))
          )
        }
      },
      _ => todo!(),
    }
  }

  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Nat(x) => {
        Ipld::List(vec![Ipld::Integer(0), Ipld::Bytes(x.to_bytes_be())])
      }
      Self::Int(x) => {
        Ipld::List(vec![Ipld::Integer(1), Ipld::Bytes(x.to_signed_bytes_be())])
      }
      Self::Bytes(x) => {
        Ipld::List(vec![Ipld::Integer(2), Ipld::Bytes(x.to_owned())])
      }
      Self::Text(x) => Ipld::List(vec![
        Ipld::Integer(3),
        Ipld::Bytes(x.to_string().into_bytes()),
      ]),
      Self::Char(x) => Ipld::List(vec![
        Ipld::Integer(4),
        Ipld::Bytes((*x as u32).to_be_bytes().to_vec()),
      ]),
      Self::Bool(x) => Ipld::List(vec![
        Ipld::Integer(5),
        Ipld::Bytes(vec![if *x { 1 } else { 0 }]),
      ]),
      Self::U8(x) => Ipld::List(vec![
        Ipld::Integer(6),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
      Self::U16(x) => Ipld::List(vec![
        Ipld::Integer(7),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
      Self::U32(x) => Ipld::List(vec![
        Ipld::Integer(8),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
      Self::U64(x) => Ipld::List(vec![
        Ipld::Integer(9),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
      Self::U128(x) => Ipld::List(vec![
        Ipld::Integer(10),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
      Self::I8(x) => Ipld::List(vec![
        Ipld::Integer(11),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
      Self::I16(x) => Ipld::List(vec![
        Ipld::Integer(12),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
      Self::I32(x) => Ipld::List(vec![
        Ipld::Integer(13),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
      Self::I64(x) => Ipld::List(vec![
        Ipld::Integer(14),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
      Self::I128(x) => Ipld::List(vec![
        Ipld::Integer(15),
        Ipld::Bytes(x.to_be_bytes().to_vec()),
      ]),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(0), Ipld::Bytes(x)] => {
          Ok(Self::Nat(BigUint::from_bytes_be(x)))
        }
        [Ipld::Integer(1), Ipld::Bytes(x)] => {
          Ok(Self::Int(BigInt::from_signed_bytes_be(x)))
        }
        [Ipld::Integer(2), Ipld::Bytes(x)] => {
          Ok(Self::Bytes(x.to_owned().into()))
        }
        [Ipld::Integer(3), Ipld::Bytes(x)] => String::from_utf8(x.to_owned())
          .map_or_else(
            |e| Err(IpldError::Utf8(x.clone(), e)),
            |x| Ok(Self::Text(x.into())),
          ),
        [Ipld::Integer(4), Ipld::Bytes(x)] => {
          let x: [u8; 4] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 4)), Ok)?;
          let x: u32 = u32::from_be_bytes(x);
          match std::char::from_u32(x) {
            Some(c) => Ok(Self::Char(c)),
            None => Err(IpldError::UnicodeChar(x)),
          }
        }
        [Ipld::Integer(5), Ipld::Bytes(x)] => match x.as_slice() {
          [1] => Ok(Self::Bool(true)),
          [0] => Ok(Self::Bool(false)),
          xs => Err(IpldError::Bool(Ipld::Bytes(xs.to_owned()))),
        },
        [Ipld::Integer(6), Ipld::Bytes(x)] => {
          let x: [u8; 1] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 1)), Ok)?;
          Ok(Self::U8(u8::from_be_bytes(x)))
        }
        [Ipld::Integer(7), Ipld::Bytes(x)] => {
          let x: [u8; 2] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 2)), Ok)?;
          Ok(Self::U16(u16::from_be_bytes(x)))
        }
        [Ipld::Integer(8), Ipld::Bytes(x)] => {
          let x: [u8; 4] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 4)), Ok)?;
          Ok(Self::U32(u32::from_be_bytes(x)))
        }
        [Ipld::Integer(9), Ipld::Bytes(x)] => {
          let x: [u8; 8] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 8)), Ok)?;
          Ok(Self::U64(u64::from_be_bytes(x)))
        }
        [Ipld::Integer(10), Ipld::Bytes(x)] => {
          let x: [u8; 16] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 16)), Ok)?;
          Ok(Self::U128(u128::from_be_bytes(x)))
        }
        [Ipld::Integer(11), Ipld::Bytes(x)] => {
          let x: [u8; 1] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 1)), Ok)?;
          Ok(Self::I8(i8::from_be_bytes(x)))
        }
        [Ipld::Integer(12), Ipld::Bytes(x)] => {
          let x: [u8; 2] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 2)), Ok)?;
          Ok(Self::I16(i16::from_be_bytes(x)))
        }
        [Ipld::Integer(13), Ipld::Bytes(x)] => {
          let x: [u8; 4] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 4)), Ok)?;
          Ok(Self::I32(i32::from_be_bytes(x)))
        }
        [Ipld::Integer(14), Ipld::Bytes(x)] => {
          let x: [u8; 8] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 8)), Ok)?;
          Ok(Self::I64(i64::from_be_bytes(x)))
        }
        [Ipld::Integer(15), Ipld::Bytes(x)] => {
          let x: [u8; 16] = x
            .to_owned()
            .try_into()
            .map_or_else(|e| Err(IpldError::ByteCount(e, 16)), Ok)?;
          Ok(Self::I128(i128::from_be_bytes(x)))
        }
        xs => Err(IpldError::Literal(Ipld::List(xs.to_owned()))),
      },
      _ => Err(IpldError::Literal(ipld.clone())),
    }
  }
}

impl LitType {
  pub fn induction(self, val: Term) -> Term {
    match self {
      Self::Nat => {
        yatima!(
          "∀ (0 P: ∀ #Nat -> Type)
             (& zero: P 0)
             (& succ: ∀ (pred: #Nat) -> (#Nat.suc pred))
           -> P #$0
          ",
          val
        )
      }
      Self::Int => {
        yatima!(
          "∀ (0 P: ∀ #Int -> Type)
             (& int: ∀ (sign: #Bool) (abs: #Nat) -> P (#Int.new sign abs))
           -> P #$0
          ",
          val
        )
      }
      Self::Bytes => {
        yatima!(
          "∀ (0 P: ∀ #Bytes -> Type)
             (& nil: P \"\")
             (& cons: ∀ (x: #U8) (xs: #Bytes) -> (#Bytes.cons x xs))
           -> P #$0
          ",
          val
        )
      }
      Self::Text => {
        yatima!(
          "∀ (0 P: ∀ #Text -> Type)
             (& nil: P \"\")
             (& cons: ∀ (x: #Char) (xs: #Text) -> (#Text.cons x xs))
           -> P #$0
          ",
          val
        )
      }
      _ => todo!(),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Nat => Ipld::List(vec![Ipld::Integer(0)]),
      Self::Int => Ipld::List(vec![Ipld::Integer(1)]),
      Self::Bytes => Ipld::List(vec![Ipld::Integer(2)]),
      Self::Text => Ipld::List(vec![Ipld::Integer(3)]),
      Self::Char => Ipld::List(vec![Ipld::Integer(4)]),
      Self::Bool => Ipld::List(vec![Ipld::Integer(5)]),
      Self::U8 => Ipld::List(vec![Ipld::Integer(6)]),
      Self::U16 => Ipld::List(vec![Ipld::Integer(7)]),
      Self::U32 => Ipld::List(vec![Ipld::Integer(8)]),
      Self::U64 => Ipld::List(vec![Ipld::Integer(9)]),
      Self::U128 => Ipld::List(vec![Ipld::Integer(10)]),
      Self::I8 => Ipld::List(vec![Ipld::Integer(11)]),
      Self::I16 => Ipld::List(vec![Ipld::Integer(12)]),
      Self::I32 => Ipld::List(vec![Ipld::Integer(13)]),
      Self::I64 => Ipld::List(vec![Ipld::Integer(14)]),
      Self::I128 => Ipld::List(vec![Ipld::Integer(15)]),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(0)] => Ok(Self::Nat),
        [Ipld::Integer(1)] => Ok(Self::Int),
        [Ipld::Integer(2)] => Ok(Self::Bytes),
        [Ipld::Integer(3)] => Ok(Self::Text),
        [Ipld::Integer(4)] => Ok(Self::Char),
        [Ipld::Integer(5)] => Ok(Self::Bool),
        [Ipld::Integer(6)] => Ok(Self::U8),
        [Ipld::Integer(7)] => Ok(Self::U16),
        [Ipld::Integer(8)] => Ok(Self::U32),
        [Ipld::Integer(9)] => Ok(Self::U64),
        [Ipld::Integer(10)] => Ok(Self::U128),
        [Ipld::Integer(11)] => Ok(Self::I8),
        [Ipld::Integer(12)] => Ok(Self::I16),
        [Ipld::Integer(13)] => Ok(Self::I32),
        [Ipld::Integer(14)] => Ok(Self::I64),
        [Ipld::Integer(15)] => Ok(Self::I128),
        xs => Err(IpldError::LitType(Ipld::List(xs.to_owned()))),
      },
      _ => Err(IpldError::LitType(ipld.clone())),
    }
  }
}

impl fmt::Display for LitType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Nat => write!(f, "#Nat"),
      Self::Int => write!(f, "#Int"),
      Self::Bytes => write!(f, "#Bytes"),
      Self::Text => write!(f, "#Text"),
      Self::Char => write!(f, "#Char"),
      Self::Bool => write!(f, "#Bool"),
      Self::U8 => write!(f, "#U8"),
      Self::U16 => write!(f, "#U16"),
      Self::U32 => write!(f, "#U32"),
      Self::U64 => write!(f, "#U64"),
      Self::I8 => write!(f, "#I8"),
      Self::I16 => write!(f, "#I16"),
      Self::I32 => write!(f, "#I32"),
      Self::I64 => write!(f, "#I64"),
      _ => todo!(),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use crate::tests::frequency;

  pub fn arbitrary_nat() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let v: Vec<u8> = Arbitrary::arbitrary(g);
      let x: BigUint = BigUint::from_bytes_be(&v);
      Literal::Nat(x)
    })
  }

  pub fn arbitrary_int() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let v: Vec<u8> = Arbitrary::arbitrary(g);
      let x: BigInt = BigInt::from_signed_bytes_be(&v);
      Literal::Int(x)
    })
  }

  pub fn arbitrary_bytes() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: Vec<u8> = Arbitrary::arbitrary(g);
      Literal::Bytes(x.into())
    })
  }

  pub fn arbitrary_text() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: String = Arbitrary::arbitrary(g);
      Literal::Text(x.into())
    })
  }

  pub fn arbitrary_char() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: char = Arbitrary::arbitrary(g);
      Literal::Char(x)
    })
  }

  pub fn arbitrary_bool() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: bool = Arbitrary::arbitrary(g);
      Literal::Bool(x)
    })
  }

  pub fn arbitrary_u8() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: u8 = Arbitrary::arbitrary(g);
      Literal::U8(x)
    })
  }
  pub fn arbitrary_u16() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: u16 = Arbitrary::arbitrary(g);
      Literal::U16(x)
    })
  }
  pub fn arbitrary_u32() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: u32 = Arbitrary::arbitrary(g);
      Literal::U32(x)
    })
  }
  pub fn arbitrary_u64() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: u64 = Arbitrary::arbitrary(g);
      Literal::U64(x)
    })
  }
  pub fn arbitrary_u128() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: u128 = Arbitrary::arbitrary(g);
      Literal::U128(x)
    })
  }

  pub fn arbitrary_i8() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: i8 = Arbitrary::arbitrary(g);
      Literal::I8(x)
    })
  }
  pub fn arbitrary_i16() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: i16 = Arbitrary::arbitrary(g);
      Literal::I16(x)
    })
  }
  pub fn arbitrary_i32() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: i32 = Arbitrary::arbitrary(g);
      Literal::I32(x)
    })
  }
  pub fn arbitrary_i64() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: i64 = Arbitrary::arbitrary(g);
      Literal::I64(x)
    })
  }
  pub fn arbitrary_i128() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: i128 = Arbitrary::arbitrary(g);
      Literal::I128(x)
    })
  }

  impl Arbitrary for Literal {
    fn arbitrary(g: &mut Gen) -> Self {
      frequency(g, vec![
        (1, arbitrary_nat()),
        (1, arbitrary_int()),
        (1, arbitrary_bytes()),
        (1, arbitrary_text()),
        (1, arbitrary_char()),
        //(1, arbitrary_bool()),
        (1, arbitrary_u8()),
        (1, arbitrary_u16()),
        (1, arbitrary_u32()),
        (1, arbitrary_u64()),
        (1, arbitrary_u128()),
        (1, arbitrary_i8()),
        //(1, arbitrary_i16()),
        //(1, arbitrary_i32()),
        //(1, arbitrary_i64()),
        //(1, arbitrary_i128()),
        (1, Box::new(|g| Self::Char(Arbitrary::arbitrary(g)))),
      ])
    }
  }

  #[quickcheck]
  fn literal_ipld(x: Literal) -> bool {
    match Literal::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  impl Arbitrary for LitType {
    fn arbitrary(g: &mut Gen) -> Self {
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> LitType>)> = vec![
        (1, Box::new(|_| Self::Nat)),
        (1, Box::new(|_| Self::Int)),
        (1, Box::new(|_| Self::Bytes)),
        (1, Box::new(|_| Self::Text)),
        (1, Box::new(|_| Self::Char)),
      ];
      frequency(g, input)
    }
  }

  #[quickcheck]
  fn lit_type_ipld(x: LitType) -> bool {
    match LitType::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[test]
  fn test_expand() {
    assert_eq!(
      Literal::Nat(BigUint::from(1u64)).expand(),
      yatima!("λ P z s => s 0")
    )
  }
}
