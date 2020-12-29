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
  character::complete::{
    digit0,
    multispace1,
  },
  combinator::opt,
  multi::{
    count,
    separated_list0,
  },
  sequence::delimited,
  Err,
  Err::Error,
  IResult,
};

pub mod atom;
pub mod base;
pub mod bytevec;
pub mod error;
pub mod position;
pub mod span;
pub mod string;

use atom::{
  Atom,
  Atom::*,
  Link,
};
use base::Base;
use error::{
  DeserialError,
  ParseError,
};
use string::parse_string;

use position::Pos;
use span::Span;

#[derive(Clone, Debug)]
pub enum Expr {
  Atom(Option<Pos>, Atom),
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
  pub fn from_bits<A: AsRef<[u8]>>(x: A) -> Expr {
    Self::Atom(None, Bits(x.as_ref().to_vec(), None))
  }

  pub fn from_string(x: &str) -> Expr {
    Self::Atom(None, Text(x.to_owned(), None))
  }

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
        let (type_code, len_sig) = atom.type_code();
        let data = atom.data_bytes();
        let type_len = type_code.len() as u8;
        let data_len = len_sig.unwrap_or((data.len() as u64) * 8);
        let (data_len_len, data_len_bytes) = pack_u64(data_len);

        let size_byte: u8 = if len_sig.is_some() {
          1 << 6 | type_len - 1 << 3 | data_len_len - 1
        }
        else {
          type_len - 1 << 3 | data_len_len - 1
        };

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

  pub fn hash(&self) -> Expr {
    Expr::Atom(self.position(), Link(Link::make(&self.serialize())))
  }

  pub fn deserialize(i: &[u8]) -> IResult<&[u8], Expr, DeserialError<&[u8]>> {
    let (i, size) = take(1 as usize)(i)?;
    let (is_atom, sig, type_len, data_len_len) = (
      ((size[0] & 0b1000_0000) >> 7) == 0,
      ((size[0] & 0b0100_0000) >> 6) == 1,
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
      let len_sig = if sig { Some(data_bitlen) } else { None };
      match type_code {
        [0x00] => {
          let data: [u8; 32] = data.try_into().map_err(|_| {
            Error(DeserialError::BadLinkLength(i_type, data_bitlen))
          })?;
          Ok((i, Expr::Atom(None, Link(Link::from(data)))))
        }
        [0x01] => Ok((i, Expr::Atom(None, Bits(data.to_owned(), len_sig)))),
        [0x02] => match String::from_utf8(data.to_owned()) {
          Ok(s) => Ok((i, Expr::Atom(None, Symbol(s)))),
          Err(e) => Err(Error(DeserialError::Utf8Error(i, e))),
        },
        [0x03] => match String::from_utf8(data.to_owned()) {
          Ok(s) => Ok((i, Expr::Atom(None, Text(s, len_sig)))),
          Err(e) => Err(Error(DeserialError::Utf8Error(i, e))),
        },
        [0x04] => {
          let data: [u8; 4] = data.try_into().map_err(|_| {
            Error(DeserialError::BadCharLength(i_type, data_bitlen))
          })?;
          let data: u32 = u32::from_be_bytes(data);
          match std::char::from_u32(data) {
            Some(c) => Ok((i, Expr::Atom(None, Char(c)))),
            None => Err(Error(DeserialError::InvalidUnicodeCodepoint(i, data))),
          }
        }
        [0x05] => {
          Ok((i, Expr::Atom(None, Nat(BigUint::from_bytes_be(data), len_sig))))
        }
        [0x06] => Ok((
          i,
          Expr::Atom(None, Int(BigInt::from_signed_bytes_be(data), len_sig)),
        )),
        _ => Err(Error(DeserialError::UnknownTypeCode(
          i_type,
          type_code.to_owned(),
          len_sig,
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
        format!("#{}", base::encode(Base::_64, self.serialize()))
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

impl Link {
  pub fn serialize(&self) -> Vec<u8> {
    Expr::Atom(None, Link(self.clone())).serialize()
  }

  pub fn deserialize(i: &[u8]) -> Option<Link> {
    match Expr::deserialize(i) {
      Ok((_, Expr::Atom(_, Link(x)))) => Some(x),
      _ => None,
    }
  }
}

impl fmt::Display for Link {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", base::encode(Base::_58, self.serialize()))
  }
}

impl fmt::Display for Atom {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Bits(x, Some(l)) => {
        let x: &[u8] = x.as_ref();
        write!(f, "{}:bits{}", base::encode(Base::_64, x), l)
      }
      Self::Bits(x, None) => {
        let x: &[u8] = x.as_ref();
        write!(f, "{}:bits", base::encode(Base::_64, x))
      }
      Self::Link(l) => write!(f, "#{}", l),
      Self::Symbol(x) => write!(f, "{}", x),
      Self::Nat(x, Some(l)) => write!(f, "0d{}:nat{}", x.to_str_radix(10), l),
      Self::Nat(x, None) => {
        write!(f, "0d{}:nat", x.to_str_radix(10))
      }
      Self::Int(x, Some(l)) => match x.sign() {
        Sign::Minus => write!(f, "-0d{}:int{}", x.to_str_radix(10), l),
        _ => write!(f, "0d{}:int{}", x.to_str_radix(10), l),
      },
      Self::Int(x, None) => match x.sign() {
        Sign::Minus => write!(f, "-0d{}:int", x.to_str_radix(10)),
        _ => write!(f, "0d{}:int", x.to_str_radix(10)),
      },
      Self::Text(x, Some(l)) => {
        write!(f, "\"{}\":text{}", x.escape_default(), l)
      }
      Self::Text(x, None) => write!(f, "\"{}\":text", x.escape_default()),
      Self::Char(x) => write!(f, "'{}':char", x.escape_default()),
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
  let (i, _) = tag("#")(from)?;
  let (upto, (_, raw)) = base::parse(i).map_err(|e| nom::Err::convert(e))?;
  let (_, x) = Expr::deserialize(&raw).map_err(|e| match e {
    Err::Incomplete(n) => Err::Incomplete(n),
    Err::Error(e) => {
      Err::Error(ParseError::DeserialErr(i, e.to_owned().input_as_bytes()))
    }
    Err::Failure(e) => {
      Err::Failure(ParseError::DeserialErr(i, e.to_owned().input_as_bytes()))
    }
  })?;
  Ok((upto, x.set_postion(Pos::from_upto(from, upto))))
}

pub fn parse_symbol(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let (upto, s) =
    take_till1(|x| char::is_whitespace(x) | (x == ')') | (x == '('))(from)?;
  Ok((
    upto,
    Expr::Atom(
      Some(Pos::from_upto(from, upto)),
      Symbol(String::from(s.fragment().to_owned())),
    ),
  ))
}

pub fn parse_char(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let p = |i| parse_string("'", i);
  let (i_err, c) = delimited(tag("'"), p, tag("'"))(from)?;
  let (upto, _) = tag(":char")(i_err)?;
  let s: Vec<char> = c.chars().collect();
  if s.len() != 1 {
    Err(Err::Error(ParseError::ExpectedSingleChar(i_err, s)))
  }
  else {
    Ok((upto, Expr::Atom(Some(Pos::from_upto(from, upto)), Char(s[0]))))
  }
}

pub fn parse_text(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let p = |i| parse_string("\"", i);
  let (i_err, val) = delimited(tag("\""), p, tag("\""))(from)?;
  let (i, _) = tag(":text")(i_err)?;
  let (upto, ds) = digit0(i)?;
  if ds.len() == 0 {
    Ok((upto, Expr::Atom(Some(Pos::from_upto(from, upto)), Text(val, None))))
  }
  else {
    let len = ds.parse::<u64>().unwrap();
    if bytelen_from_bitlen(len) < (val.len() as u64) {
      Err(Error(ParseError::LengthTooSmall(
        i_err,
        val.as_bytes().to_owned(),
        len,
      )))
    }
    else {
      Ok((
        upto,
        Expr::Atom(Some(Pos::from_upto(from, upto)), Text(val, Some(len))),
      ))
    }
  }
}
pub fn parse_nat(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let (i, _) = tag("0")(from)?;
  let (i, (_, val)) = base::parse(i).map_err(|e| nom::Err::convert(e))?;
  let (i, _) = tag(":nat")(i)?;
  let (upto, ds) = digit0(i)?;
  if ds.len() == 0 {
    Ok((
      upto,
      Expr::Atom(
        Some(Pos::from_upto(from, upto)),
        Nat(BigUint::from_bytes_be(&val), None),
      ),
    ))
  }
  else {
    let len = ds.parse::<u64>().unwrap();
    if bytelen_from_bitlen(len) < (val.len() as u64) {
      Err(Error(ParseError::LengthTooSmall(i, val, len)))
    }
    else {
      Ok((
        upto,
        Expr::Atom(
          Some(Pos::from_upto(from, upto)),
          Nat(BigUint::from_bytes_be(&val), Some(len)),
        ),
      ))
    }
  }
}
pub fn parse_int(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let (i, s) = opt(tag("-"))(from)?;
  let s = s.map_or(Sign::Plus, |_| Sign::Minus);
  let (i, _) = tag("0")(i)?;
  let (i, (_, val)) = base::parse(i).map_err(|e| nom::Err::convert(e))?;
  let (i, _) = tag(":int")(i)?;
  let (upto, ds) = digit0(i)?;
  if ds.len() == 0 {
    Ok((
      upto,
      Expr::Atom(
        Some(Pos::from_upto(from, upto)),
        Int(BigInt::from_bytes_be(s, &val), None),
      ),
    ))
  }
  else {
    let len = ds.parse::<u64>().unwrap();
    if bytelen_from_bitlen(len) < (val.len() as u64) {
      Err(Error(ParseError::LengthTooSmall(i, val, len)))
    }
    else {
      Ok((
        upto,
        Expr::Atom(
          Some(Pos::from_upto(from, upto)),
          Int(BigInt::from_bytes_be(s, &val), Some(len)),
        ),
      ))
    }
  }
}
pub fn parse_bits(from: Span) -> IResult<Span, Expr, ParseError<Span>> {
  let (i, (_, val)) = base::parse(from).map_err(|e| nom::Err::convert(e))?;
  let (i, _) = tag(":bits")(i)?;
  let (upto, ds) = digit0(i)?;
  if ds.len() == 0 {
    Ok((i, Expr::Atom(Some(Pos::from_upto(from, upto)), Bits(val, None))))
  }
  else {
    let len = ds.parse::<u64>().unwrap();
    if bytelen_from_bitlen(len) < (val.len() as u64) {
      Err(Error(ParseError::LengthTooSmall(i, val, len)))
    }
    else {
      Ok((
        upto,
        Expr::Atom(Some(Pos::from_upto(from, upto)), Bits(val, Some(len))),
      ))
    }
  }
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
    parse_bits,
    parse_raw,
    parse_nat,
    parse_int,
    parse_text,
    parse_char,
    parse_symbol,
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
macro_rules! cons {
  ($p:expr, $($x: expr),*) => {{
    let mut vector = Vec::new();
    $(vector.push($x);)*
    Expr::Cons($p, vector)
  }}
}

#[cfg(test)]
mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
    StdThreadGen,
  };
  use rand::Rng;

  fn gen_symbol<G: Gen>(g: &mut G) -> String {
    let s: String = Arbitrary::arbitrary(g);
    let s: String = s
      .chars()
      .filter(|x| {
        !char::is_whitespace(*x)
          && (*x != ')')
          && (*x != '"')
          && (*x != '\'')
          && (*x != '(')
      })
      .collect();
    let other_literal = alt((
      parse_bits, parse_raw, parse_nat, parse_int, parse_text, parse_char,
    ))(Span::new(&s))
    .is_ok();

    if (s.len() == 0) | other_literal { gen_symbol(g) } else { s }
  }

  impl Arbitrary for Atom {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let gen = g.gen_range(0, 7);
      match gen {
        0 => {
          let x: Vec<u8> = Arbitrary::arbitrary(g);
          let b: bool = Arbitrary::arbitrary(g);
          let l = if b { Some((x.len() as u64) * 8) } else { None };
          Bits(x, l)
        }
        1 => Symbol(gen_symbol(g)),
        2 => {
          let x: String = Arbitrary::arbitrary(g);
          let b: bool = Arbitrary::arbitrary(g);
          let l = if b { Some((x.len() as u64) * 8) } else { None };
          Text(x, l)
        }
        3 => Char(Arbitrary::arbitrary(g)),
        4 => {
          let v: Vec<u8> = Arbitrary::arbitrary(g);
          let x: BigUint = BigUint::from_bytes_be(&v);
          let b: bool = Arbitrary::arbitrary(g);
          let l = if b { Some(x.to_bytes_be().len() as u64 * 8) } else { None };
          Nat(x, l)
        }
        5 => {
          let v: Vec<u8> = Arbitrary::arbitrary(g);
          let x: BigInt = BigInt::from_signed_bytes_be(&v);
          let b: bool = Arbitrary::arbitrary(g);
          let l = if b {
            Some(x.to_signed_bytes_be().len() as u64 * 8)
          }
          else {
            None
          };
          Int(x, l)
        }
        _ => {
          let bytes: [u8; 32] = [
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
            Arbitrary::arbitrary(g),
          ];
          Link(Link::from(bytes))
        }
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
        let size = g.size();
        Expr::Cons(None, Arbitrary::arbitrary(&mut StdThreadGen::new(size / 2)))
      }
    }
  }

  #[test]
  fn expr_test_cases() {
    let a_val = vec![
      52, 78, 86, 90, 12, 9, 43, 13, 0, 56, 30, 55, 26, 51, 91, 44, 23, 30, 91,
      52, 86, 33, 72, 41, 55, 56, 45, 98, 53, 40, 84, 60, 93, 84, 94, 48, 28,
      30, 19, 63, 49, 16, 16, 95, 50, 59, 49, 59, 52, 89, 27, 49, 90, 73, 38,
      77, 12, 51, 61, 38, 28, 8, 11, 26, 58, 35, 70, 37, 97, 90, 4, 64, 90, 20,
      56, 79, 53, 73, 40, 52, 95, 43, 97, 73, 37,
    ];
    let a = Expr::Atom(None, Bits(a_val.clone(), Some(680)));
    assert_eq!(format!("{}", a), "~0TlZaDAkrDQA4HjcaM1ssFx5bNFYhSCk3OC1iNShUPF1UXjAcHhM_MRAQXzI7MTs0WRsxWkkmTQwzPSYcCAsaOiNGJWFaBEBaFDhPNUkoNF8rYUkl:bits680");
    assert_eq!(parse(&format!("{}", a)).unwrap().1, a.clone());
    let a_ser = vec![
      65, 1, 2, 168, 52, 78, 86, 90, 12, 9, 43, 13, 0, 56, 30, 55, 26, 51, 91,
      44, 23, 30, 91, 52, 86, 33, 72, 41, 55, 56, 45, 98, 53, 40, 84, 60, 93,
      84, 94, 48, 28, 30, 19, 63, 49, 16, 16, 95, 50, 59, 49, 59, 52, 89, 27,
      49, 90, 73, 38, 77, 12, 51, 61, 38, 28, 8, 11, 26, 58, 35, 70, 37, 97,
      90, 4, 64, 90, 20, 56, 79, 53, 73, 40, 52, 95, 43, 97, 73, 37,
    ];
    assert_eq!(a.serialize(), a_ser);
    assert_eq!(Expr::deserialize(&a_ser).unwrap(), (b"".as_ref(), a.clone()));
    let a = Expr::Atom(None, Bits(vec![], None));
    assert_eq!(format!("{}", a), "~:bits");
    assert_eq!(parse(&format!("{}", a)).unwrap().1, a.clone());
    assert_eq!(a.serialize(), vec![0, 1, 0]);
    assert_eq!(
      Expr::deserialize(&vec![0, 1, 0]).unwrap(),
      (b"".as_ref(), a.clone())
    );
    let a = Expr::Atom(None, Bits(vec![0], None));
    assert_eq!(format!("{}", a), "~A:bits");
    assert_eq!(parse(&format!("{}", a)).unwrap().1, a.clone());
    assert_eq!(a.serialize(), vec![0, 1, 8, 0]);
    assert_eq!(
      Expr::deserialize(&vec![0, 1, 8, 0]).unwrap(),
      (b"".as_ref(), a.clone())
    );
    let a1 = Expr::Atom(None, Bits(vec![0], Some(1)));
    assert_eq!(format!("{}", a1), "~A:bits1");
    assert_eq!(parse(&format!("{}", a1)).unwrap().1, a1.clone());
    assert_eq!(a1.serialize(), vec![64, 1, 1, 0]);
    assert_eq!(
      Expr::deserialize(&vec![64, 1, 1, 0]).unwrap(),
      (b"".as_ref(), a1.clone())
    );
    let b = Expr::Atom(None, Bits(vec![42, 42, 42], None));
    assert_eq!(format!("{}", b), "~Kioq:bits");
    assert_eq!(b.serialize(), vec![0, 1, 24, 42, 42, 42]);
    assert_eq!(parse(&format!("{}", b)).unwrap().1, b.clone());
    assert_eq!(
      Expr::deserialize(&vec![0, 1, 24, 42, 42, 42]).unwrap(),
      (b"".as_ref(), b.clone())
    );
    assert_eq!(
      parse_symbol(Span::new("foo")).unwrap().1,
      Expr::Atom(None, Symbol(String::from("foo")))
    );
    let b = Expr::Atom(None, Symbol(String::from("foobar")));
    assert_eq!(
      parse_symbol(Span::new(&format!("{}", b))).unwrap().1,
      b.clone()
    );
    assert_eq!(format!("{}", b), "foobar");
    assert_eq!(parse(&format!("{}", b)).unwrap().1, b.clone());
    assert_eq!(b.serialize(), vec![0, 2, 48, 102, 111, 111, 98, 97, 114]);
    assert_eq!(
      Expr::deserialize(&vec![0, 2, 48, 102, 111, 111, 98, 97, 114]).unwrap(),
      (b"".as_ref(), b.clone())
    );

    let c = Expr::Cons(None, vec![a.clone(), a.clone(), a.clone()]);
    assert_eq!(format!("{}", c), "(~A:bits ~A:bits ~A:bits)");
    assert_eq!(c.serialize(), vec![128, 3, 0, 1, 8, 0, 0, 1, 8, 0, 0, 1, 8, 0]);
    assert_eq!(
      Expr::deserialize(&vec![128, 3, 0, 1, 8, 0, 0, 1, 8, 0, 0, 1, 8, 0])
        .unwrap(),
      (b"".as_ref(), c.clone())
    );
    // assert!(format!(, x);
  }

  #[quickcheck]
  fn prop_deserial_serial_identity(x: Expr) -> bool {
    println!("expr {}", x);
    match Expr::deserialize(&Expr::serialize(&x)) {
      Ok((_, y)) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn prop_print_parse_desugared_string(x: Expr) -> bool {
    match parse(&x.to_desugared_string()) {
      Ok((_, y)) => x == y,
      _ => false,
    }
  }
  #[quickcheck]
  fn prop_print_parse_expr(x: Expr) -> bool {
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
