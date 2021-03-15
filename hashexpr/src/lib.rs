#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
extern crate rand;

use std::{
  convert::TryInto,
  fmt,
};

use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use nom::{
  branch::alt,
  bytes::complete::{
    tag,
    take,
    take_till1,
  },
  character::complete::multispace1,
  combinator::{
    opt,
    value,
  },
  multi::{
    count,
    separated_list0,
  },
  sequence::{
    delimited,
    preceded,
    terminated,
  },
  Err,
  Err::Error,
  IResult,
};

pub mod atom;
pub mod base;
pub mod bytevec;
pub mod error;
pub mod link;
pub mod position;
pub mod span;
pub mod string;

use base::Base;
use error::{
  DeserialError,
  DeserialErrorKind,
  ParseError,
  ParseErrorKind,
};
pub use link::Link;
use string::parse_string;

use position::Pos;
use span::Span;

#[derive(Clone, Debug)]
pub enum Expr {
  Atom(Option<Pos>, atom::Atom),
  Cons(Option<Pos>, Vec<Expr>),
}

impl PartialEq for Expr {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Atom(_, a), Self::Atom(_, b)) => a == b,
      (Self::Cons(_, a), Self::Cons(_, b)) => a == b,
      _ => false,
    }
  }
}

impl Eq for Expr {}

pub fn number_of_bytes(x: u64) -> u8 {
  let mut n: u32 = 1;
  let base: u64 = 256;
  while base.pow(n) <= x {
    n += 1;
  }
  return n as u8;
}

pub fn pack_u64(x: u64) -> (u8, Vec<u8>) {
  let x_len = number_of_bytes(x);
  (
    x_len,
    x.to_le_bytes()[0..(x_len as usize)].to_vec().into_iter().rev().collect(),
  )
}

pub fn bytelen_from_bitlen(bits: u64) -> u64 {
  if bits % 8 == 0 { bits / 8 } else { bits / 8 + 1 }
}

impl Expr {
  pub fn from_bits<A: AsRef<[u8]>>(x: A) -> Expr { bits!(x.as_ref().to_vec()) }

  pub fn from_string(x: &str) -> Expr { text!(x.to_owned()) }

  pub fn set_postion(&self, p: Pos) -> Expr {
    match self {
      Self::Atom(_, x) => Self::Atom(Some(p), x.clone()),
      Self::Cons(_, x) => Self::Cons(Some(p), x.clone()),
    }
  }

  pub fn position(&self) -> Option<Pos> {
    match self {
      Self::Atom(p, _) => *p,
      Self::Cons(p, _) => *p,
    }
  }

  pub fn serialize(&self) -> Vec<u8> {
    match self {
      Self::Atom(_, atom) => {
        let type_code = atom.type_code();
        let data = atom.data_bytes();
        let type_len = type_code.len() as u8;
        let data_len = (data.len() as u64) * 8;
        let (data_len_len, data_len_bytes) = pack_u64(data_len);

        let size_byte: u8 = type_len - 1 << 3 | data_len_len - 1;

        let mut ret = vec![];
        ret.extend(vec![size_byte]);
        ret.extend(type_code);
        ret.extend(data_len_bytes);
        ret.extend(data);
        ret
      }
      Self::Cons(_, xs) => {
        let (xs_count_len, xs_count) = pack_u64(xs.len() as u64);
        let size_byte: u8 = 0b1000_0000 | (xs_count_len - 1);
        let mut ret: Vec<u8> = vec![];
        ret.extend(vec![size_byte]);
        ret.extend(xs_count);
        ret.extend(xs.iter().fold(vec![], |mut acc, x| {
          acc.extend(Expr::serialize(x));
          acc
        }));
        ret
      }
    }
  }

  pub fn link(&self) -> Link { Link::make(&self.serialize()) }

  pub fn hash(&self) -> Expr { link!(self.position(), self.link()) }

  pub fn deserialize(i: &[u8]) -> IResult<&[u8], Expr, DeserialError<&[u8]>> {
    let (i, size) = take(1 as usize)(i)?;
    let (is_atom, type_len, data_len_len) = (
      ((size[0] & 0b1000_0000) >> 7) == 0,
      ((size[0] & 0b0011_1000) >> 3) + 1,
      (size[0] & 0b111) + 1,
    );
    if is_atom {
      let (i_type, type_code) = take(type_len)(i)?;
      let (i, data_len) = take(data_len_len)(i_type)?;
      let data_bitlen =
        data_len.iter().fold(0, |acc, &x| (acc * 256) + x as u64);
      let data_bytelen = bytelen_from_bitlen(data_bitlen);
      let (i, data) = take(data_bytelen)(i)?;
      match type_code {
        [0x00] => {
          let data: [u8; 32] = data.try_into().map_err(|_| {
            Error(DeserialError::new(
              i_type,
              DeserialErrorKind::BadLinkLength(data_bitlen),
            ))
          })?;
          Ok((i, link!(Link::from(data))))
        }
        [0x01] => Ok((i, bits!(data.to_owned()))),
        [0x02] => match String::from_utf8(data.to_owned()) {
          Ok(s) => Ok((i, text!(s))),
          Err(e) => {
            Err(Error(DeserialError::new(i, DeserialErrorKind::Utf8Error(e))))
          }
        },
        [0x03] => {
          let data: [u8; 4] = data.try_into().map_err(|_| {
            Error(DeserialError::new(
              i_type,
              DeserialErrorKind::BadCharLength(data_bitlen),
            ))
          })?;
          let data: u32 = u32::from_be_bytes(data);
          match std::char::from_u32(data) {
            Some(c) => Ok((i, char!(c))),
            None => Err(Error(DeserialError::new(
              i_type,
              DeserialErrorKind::InvalidUnicodeCodepoint(data),
            ))),
          }
        }
        [0x04] => Ok((i, nat!(BigUint::from_bytes_be(data)))),
        [0x05] => Ok((i, int!(BigInt::from_signed_bytes_be(data)))),
        _ => Err(Error(DeserialError::new(
          i_type,
          DeserialErrorKind::UnknownTypeCode(type_code.to_owned()),
        ))),
      }
    }
    else {
      // println!("de is_atom {}", is_atom);
      let (i, xs_len) = take(data_len_len)(i)?;
      // println!("de xs_len {:?}", xs_len);
      let xs_len = xs_len.iter().fold(0, |acc, &x| (acc * 256) + x as u64);
      let (i, xs) = count(Expr::deserialize, xs_len as usize)(i)?;
      Ok((i, Expr::Cons(None, xs)))
    }
  }

  pub fn to_desugared_string(&self) -> String {
    match self {
      Self::Atom(..) => {
        format!("{}", base::encode(Base::_64, self.serialize()))
      }
      Self::Cons(_, xs) => {
        let xs = xs
          .iter()
          .map(|x| x.to_desugared_string())
          .collect::<Vec<String>>()
          .join(" ");
        format!("({})", xs)
      }
    }
  }
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Atom(_, atom) => write!(f, "{}", atom),
      Self::Cons(_, xs) => {
        let x = xs
          .iter()
          .map(|x| format!("{}", x))
          .collect::<Vec<String>>()
          .join(" ");
        write!(f, "({})", x)
      }
    }
  }
}
pub fn parse_raw(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let (upto, (_, raw)) = base::parse(from)?;
  let (_, x) = Expr::deserialize(&raw).map_err(|e| match e {
    Err::Incomplete(n) => Err::Incomplete(n),
    Err::Error(e) => Err::Error(ParseError::new(
      from,
      ParseErrorKind::DeserialErr(e.to_owned().input_as_bytes()),
    )),
    Err::Failure(e) => Err::Error(ParseError::new(
      from,
      ParseErrorKind::DeserialErr(e.to_owned().input_as_bytes()),
    )),
  })?;
  Ok((upto, x.set_postion(Pos::from_upto(from, upto))))
}

pub fn parse_char(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let p = |i| parse_string("'", i);
  let (upto, c) = delimited(tag("'"), p, tag("'"))(from)?;
  let s: Vec<char> = c.chars().collect();
  if s.len() != 1 {
    Err(Err::Error(ParseError::new(
      upto,
      ParseErrorKind::ExpectedSingleChar(s),
    )))
  }
  else {
    Ok((upto, char!(Some(Pos::from_upto(from, upto)), s[0])))
  }
}

pub fn parse_text(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let p = |i| parse_string("\"", i);
  let (upto, val) = delimited(tag("\""), p, tag("\""))(from)?;
  Ok((upto, text!(Some(Pos::from_upto(from, upto)), val)))
}

pub fn parse_nat(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let (i, base) = opt(preceded(tag("0"), base::Base::parse_code))(from)?;
  let base = base.unwrap_or(Base::_10);
  let (upto, val) = base.decode1(i).map_err(|e| nom::Err::convert(e))?;
  Ok((
    upto,
    nat!(Some(Pos::from_upto(from, upto)), BigUint::from_bytes_be(&val)),
  ))
}

pub fn parse_int(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let (i, s) =
    alt((value(Sign::Minus, tag("-")), value(Sign::Plus, tag("+"))))(from)?;
  let (i, base) = opt(preceded(tag("0"), base::Base::parse_code))(i)?;
  let base = base.unwrap_or(Base::_10);
  let (upto, val) = base.decode1(i).map_err(|e| nom::Err::convert(e))?;
  Ok((
    upto,
    int!(Some(Pos::from_upto(from, upto)), BigInt::from_bytes_be(s, &val)),
  ))
}

pub fn parse_bits(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let (i, base) = terminated(Base::parse_code, tag("\""))(from)?;
  let (i, bytes) = Base::decode(&base, i)?;
  let (upto, _) = tag("\"")(i)?;
  Ok((upto, bits!(Some(Pos::from_upto(from, upto)), bytes)))
}

pub fn parse_cons(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let (upto, xs) =
    delimited(tag("("), separated_list0(multispace1, parse_expr), tag(")"))(
      from,
    )?;
  Ok((upto, Expr::Cons(Some(Pos::from_upto(from, upto)), xs)))
}
pub fn parse_expr(i: Span) -> IResult<Span, Expr, ParseError<Span>> {
  alt((
    parse_bits, parse_raw, parse_text, parse_char, parse_int, parse_nat,
    parse_cons,
  ))(i)
}

pub fn parse(i: &str) -> IResult<Span, Expr, ParseError<Span>> {
  parse_expr(Span::new(i))
}

#[macro_export]
macro_rules! atom {
  ($x:expr) => {
    Expr::Atom(None, $x)
  };
  ($p:expr, $x:expr) => {
    Expr::Atom($p, $x)
  };
}

#[macro_export]
macro_rules! link {
  ($x:expr) => {
    Expr::Atom(None, atom::Atom::Link($x))
  };
  ($p:expr, $x:expr) => {
    Expr::Atom($p, atom::Atom::Link($x))
  };
}

#[macro_export]
macro_rules! bits {
  ($x:expr) => {
    Expr::Atom(None, atom::Atom::Bits($x))
  };
  ($p:expr, $x:expr) => {
    Expr::Atom($p, atom::Atom::Bits($x))
  };
}

#[macro_export]
macro_rules! text {
  ($i:literal) => {
    Expr::Atom(None, atom::Atom::Text(String::from($i)))
  };
  ($i:expr) => {
    Expr::Atom(None, atom::Atom::Text($i))
  };
  ($p:expr, $x:literal) => {
    Expr::Atom($p, atom::Atom::Text(String::from($x)))
  };
  ($p:expr, $x:expr) => {
    Expr::Atom($p, atom::Atom::Text($x))
  };
}

#[macro_export]
macro_rules! char {
  ($i:expr) => {
    Expr::Atom(None, atom::Atom::Char($i))
  };
  ($i:literal) => {
    Expr::Atom(None, atom::Atom::Char($i))
  };
  ($p:expr, $x:literal) => {
    Expr::Atom($p, atom::Atom::Char($x))
  };
  ($p:expr, $x:expr) => {
    Expr::Atom($p, atom::Atom::Char($x))
  };
}
#[macro_export]
macro_rules! int {
  ($i:expr) => {
    Expr::Atom(None, atom::Atom::Int($i))
  };
  ($i:literal) => {
    Expr::Atom(None, atom::Atom::Int(BigInt::from($i)))
  };
  ($p:expr, $i:expr) => {
    Expr::Atom($p, atom::Atom::Int($i))
  };
  ($p:expr, $i:literal) => {
    Expr::Atom($p, atom::Atom::Int(BigInt::from($i)))
  };
}

#[macro_export]
macro_rules! nat {
  ($i:expr) => {
    Expr::Atom(None, atom::Atom::Nat($i))
  };
  ($i:literal) => {
    Expr::Atom(None, atom::Atom::Nat(BigUint::from($i)))
  };
  ($p:expr, $i:expr) => {
    Expr::Atom($p, atom::Atom::Nat($i))
  };
  ($p:expr, $i:literal) => {
    Expr::Atom($p, atom::Atom::Nat(BigUint::from($i)))
  };
}

#[macro_export]
macro_rules! cons {
  ($p:expr, $($x: expr),*) => {{
    let mut vector = Vec::new();
    $(vector.push($x);)*
    Expr::Cons($p, vector)
  }}
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
    StdThreadGen,
  };
  use rand::Rng;

  pub fn arbitrary_bits<G: Gen>(g: &mut G) -> atom::Atom {
    let x: Vec<u8> = Arbitrary::arbitrary(g);
    atom::Atom::Bits(x)
  }

  pub fn arbitrary_text<G: Gen>(g: &mut G) -> atom::Atom {
    let x: String = Arbitrary::arbitrary(g);
    atom::Atom::Text(x)
  }
  pub fn arbitrary_nat<G: Gen>(g: &mut G) -> atom::Atom {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigUint = BigUint::from_bytes_be(&v);
    atom::Atom::Nat(x)
  }
  pub fn arbitrary_int<G: Gen>(g: &mut G) -> atom::Atom {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigInt = BigInt::from_signed_bytes_be(&v);
    atom::Atom::Int(x)
  }

  impl Arbitrary for atom::Atom {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let gen = g.gen_range(0, 6);
      match gen {
        0 => arbitrary_bits(g),
        1 => arbitrary_text(g),
        2 => atom::Atom::Char(Arbitrary::arbitrary(g)),
        3 => arbitrary_nat(g),
        4 => arbitrary_int(g),
        _ => atom::Atom::Link(Arbitrary::arbitrary(g)),
      }
    }
  }

  impl Arbitrary for Expr {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let gen_atom = g.gen_ratio(2, 3);
      if gen_atom {
        Expr::Atom(None, Arbitrary::arbitrary(g))
      }
      else {
        let size = g.gen_range(0, 5);
        let mut xs: Vec<Expr> = Vec::new();
        for _ in 0..size {
          xs.push(Arbitrary::arbitrary(g));
        }
        Expr::Cons(None, xs)
      }
    }
  }

  #[test]
  fn expr_test_cases() {
    let a = parse("(())").unwrap().1;
    assert_eq!(Expr::deserialize(&a.serialize()).unwrap().1, a);
    let a = parse("+0d0").unwrap().1;
    // let a_val = vec![64, 5, 16, 0];
    // assert_eq!(a.serialize(), a_val);
    assert_eq!(Expr::deserialize(&a.serialize()).unwrap().1, a);
    // let a = parse("(lam ((var 0d0:nat64)))").unwrap().1;
    // let a_val = vec![
    //  128, 2, 0, 2, 24, 108, 97, 109, 128, 1, 128, 2, 0, 2, 24, 118, 97, 114,
    //  64, 5, 64, 0,
    //];
    // assert_eq!(a.serialize(), a_val);
    // assert_eq!(Expr::deserialize(&a_val).unwrap().1, a);
    let a_val = vec![
      52, 78, 86, 90, 12, 9, 43, 13, 0, 56, 30, 55, 26, 51, 91, 44, 23, 30, 91,
      52, 86, 33, 72, 41, 55, 56, 45, 98, 53, 40, 84, 60, 93, 84, 94, 48, 28,
      30, 19, 63, 49, 16, 16, 95, 50, 59, 49, 59, 52, 89, 27, 49, 90, 73, 38,
      77, 12, 51, 61, 38, 28, 8, 11, 26, 58, 35, 70, 37, 97, 90, 4, 64, 90, 20,
      56, 79, 53, 73, 40, 52, 95, 43, 97, 73, 37,
    ];
    let a = bits!(a_val.clone());
    assert_eq!(format!("{}", a), "~\"0TlZaDAkrDQA4HjcaM1ssFx5bNFYhSCk3OC1iNShUPF1UXjAcHhM_MRAQXzI7MTs0WRsxWkkmTQwzPSYcCAsaOiNGJWFaBEBaFDhPNUkoNF8rYUkl\"");
    assert_eq!(parse(&format!("{}", a)).unwrap().1, a.clone());
    let a_ser = vec![
      1, 1, 2, 168, 52, 78, 86, 90, 12, 9, 43, 13, 0, 56, 30, 55, 26, 51, 91,
      44, 23, 30, 91, 52, 86, 33, 72, 41, 55, 56, 45, 98, 53, 40, 84, 60, 93,
      84, 94, 48, 28, 30, 19, 63, 49, 16, 16, 95, 50, 59, 49, 59, 52, 89, 27,
      49, 90, 73, 38, 77, 12, 51, 61, 38, 28, 8, 11, 26, 58, 35, 70, 37, 97,
      90, 4, 64, 90, 20, 56, 79, 53, 73, 40, 52, 95, 43, 97, 73, 37,
    ];
    assert_eq!(a.serialize(), a_ser);
    assert_eq!(Expr::deserialize(&a_ser).unwrap(), (b"".as_ref(), a.clone()));
    let a = bits!(vec![]);
    assert_eq!(format!("{}", a), "~\"\"");
    assert_eq!(parse(&format!("{}", a)).unwrap().1, a.clone());
    assert_eq!(a.serialize(), vec![0, 1, 0]);
    assert_eq!(
      Expr::deserialize(&vec![0, 1, 0]).unwrap(),
      (b"".as_ref(), a.clone())
    );
    let a = bits!(vec![0]);
    assert_eq!(format!("{}", a), "~\"A\"");
    assert_eq!(parse(&format!("{}", a)).unwrap().1, a.clone());
    assert_eq!(a.serialize(), vec![0, 1, 8, 0]);
    assert_eq!(
      Expr::deserialize(&vec![0, 1, 8, 0]).unwrap(),
      (b"".as_ref(), a.clone())
    );
    let a1 = bits!(vec![0]);
    assert_eq!(format!("{}", a1), "~\"A\"");
    assert_eq!(parse(&format!("{}", a1)).unwrap().1, a1.clone());
    assert_eq!(a1.serialize(), vec![0, 1, 8, 0]);
    assert_eq!(
      Expr::deserialize(&vec![0, 1, 8, 0]).unwrap(),
      (b"".as_ref(), a1.clone())
    );
    let b = bits!(vec![42, 42, 42]);
    assert_eq!(format!("{}", b), "~\"Kioq\"");
    assert_eq!(b.serialize(), vec![0, 1, 24, 42, 42, 42]);
    assert_eq!(parse(&format!("{}", b)).unwrap().1, b.clone());
    assert_eq!(
      Expr::deserialize(&vec![0, 1, 24, 42, 42, 42]).unwrap(),
      (b"".as_ref(), b.clone())
    );
    let b = text!("foobar");
    assert_eq!(format!("{}", b), "\"foobar\"");
    assert_eq!(parse(&format!("{}", b)).unwrap().1, b.clone());
    assert_eq!(b.serialize(), vec![0, 2, 48, 102, 111, 111, 98, 97, 114]);
    assert_eq!(
      Expr::deserialize(&vec![0, 2, 48, 102, 111, 111, 98, 97, 114]).unwrap(),
      (b"".as_ref(), b.clone())
    );

    let c = Expr::Cons(None, vec![a.clone(), a.clone(), a.clone()]);
    assert_eq!(format!("{}", c), "(~\"A\" ~\"A\" ~\"A\")");
    assert_eq!(c.serialize(), vec![128, 3, 0, 1, 8, 0, 0, 1, 8, 0, 0, 1, 8, 0]);
    assert_eq!(
      Expr::deserialize(&vec![128, 3, 0, 1, 8, 0, 0, 1, 8, 0, 0, 1, 8, 0])
        .unwrap(),
      (b"".as_ref(), c.clone())
    );
    assert_eq!("-23", format!("{}", int!(BigInt::from(-23))));
    assert_eq!("+23", format!("{}", int!(BigInt::from(23))));
  }

  #[quickcheck]
  fn expr_deserial_serial(x: Expr) -> bool {
    println!("expr {}", x);
    match Expr::deserialize(&Expr::serialize(&x)) {
      Ok((_, y)) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn expr_print_parse_desugared_string(x: Expr) -> bool {
    match parse(&x.to_desugared_string()) {
      Ok((_, y)) => x == y,
      _ => false,
    }
  }
  #[quickcheck]
  fn expr_print_parse(x: Expr) -> bool {
    match parse(&format!("{}", x)) {
      Ok((i, y)) => {
        if x == y {
          true
        }
        else {
          println!("x: {}", x);
          println!("x: {:?}", x);
          println!("y: {}", y);
          println!("y: {:?}", y);
          println!("i: {}", i);
          false
        }
      }
      e => {
        println!("x: {}", x);
        println!("x: {:?}", x);
        println!("e: {:?}", e);
        false
      }
    }
  }
}
