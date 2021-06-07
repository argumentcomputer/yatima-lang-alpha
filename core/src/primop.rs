// use cid::Cid;
// use std::convert::TryInto;

use libipld::ipld::Ipld;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
  text,
  yatima,
};

use ropey;

use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use std::fmt;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum PrimOp {
  NatSuc,
  NatPre,
  NatEql,
  NatLth,
  NatLte,
  NatGth,
  NatGte,
  NatAdd,
  NatSub,
  NatMul,
  NatDiv,
  NatMod,
  IntNew,
  IntSgn,
  IntAbs,
  IntEql,
  IntLth,
  IntLte,
  IntGth,
  IntGte,
  IntAdd,
  IntSub,
  IntMul,
  IntDiv,
  IntMod,
  BytesCons,
  BytesSnoc,
  BytesAppend,
  BytesHead,
  BytesLast,
  BytesTail,
  BytesInit,
  BytesNull,
  BytesLen,
  BytesIndex,
  BytesTake,
  BytesDrop,
  BytesEql,
  BytesLth,
  BytesLte,
  BytesGth,
  BytesGte,
  TextCons,
  TextSnoc,
  TextAppend,
  TextHead,
  TextLast,
  TextTail,
  TextInit,
  TextNull,
  TextLen,
  TextIndex,
  TextTake,
  TextDrop,
  TextEql,
  TextLth,
  TextLte,
  TextGth,
  TextGte,
}

impl PrimOp {
  pub fn symbol(self) -> String {
    match self {
      Self::NatSuc => "#Nat.suc".to_owned(),
      Self::NatPre => "#Nat.pre".to_owned(),
      Self::NatEql => "#Nat.eql".to_owned(),
      Self::NatLth => "#Nat.lth".to_owned(),
      Self::NatLte => "#Nat.lte".to_owned(),
      Self::NatGth => "#Nat.gth".to_owned(),
      Self::NatGte => "#Nat.gte".to_owned(),
      Self::NatAdd => "#Nat.add".to_owned(),
      Self::NatSub => "#Nat.sub".to_owned(),
      Self::NatMul => "#Nat.mul".to_owned(),
      Self::NatDiv => "#Nat.div".to_owned(),
      Self::NatMod => "#Nat.mod".to_owned(),
      Self::IntNew => "#Int.new".to_owned(),
      Self::IntSgn => "#Int.sgn".to_owned(),
      Self::IntAbs => "#Int.abs".to_owned(),
      Self::IntEql => "#Int.eql".to_owned(),
      Self::IntLth => "#Int.lth".to_owned(),
      Self::IntLte => "#Int.lte".to_owned(),
      Self::IntGth => "#Int.gth".to_owned(),
      Self::IntGte => "#Int.gte".to_owned(),
      Self::IntAdd => "#Int.add".to_owned(),
      Self::IntSub => "#Int.sub".to_owned(),
      Self::IntMul => "#Int.mul".to_owned(),
      Self::IntDiv => "#Int.div".to_owned(),
      Self::IntMod => "#Int.mod".to_owned(),
      Self::BytesCons => "#Bytes.cons".to_owned(),
      Self::BytesSnoc => "#Bytes.snoc".to_owned(),
      Self::BytesAppend => "#Bytes.append".to_owned(),
      Self::BytesHead => "#Bytes.head".to_owned(),
      Self::BytesLast => "#Bytes.last".to_owned(),
      Self::BytesTail => "#Bytes.tail".to_owned(),
      Self::BytesInit => "#Bytes.init".to_owned(),
      Self::BytesNull => "#Bytes.null".to_owned(),
      Self::BytesLen => "#Bytes.len".to_owned(),
      Self::BytesIndex => "#Bytes.index".to_owned(),
      Self::BytesTake => "#Bytes.take".to_owned(),
      Self::BytesDrop => "#Bytes.drop".to_owned(),
      Self::BytesEql => "#Bytes.eql".to_owned(),
      Self::BytesLth => "#Bytes.lth".to_owned(),
      Self::BytesLte => "#Bytes.lte".to_owned(),
      Self::BytesGth => "#Bytes.gth".to_owned(),
      Self::BytesGte => "#Bytes.gte".to_owned(),
      Self::TextCons => "#Text.cons".to_owned(),
      Self::TextSnoc => "#Text.snoc".to_owned(),
      Self::TextAppend => "#Text.append".to_owned(),
      Self::TextHead => "#Text.head".to_owned(),
      Self::TextLast => "#Text.last".to_owned(),
      Self::TextTail => "#Text.tail".to_owned(),
      Self::TextInit => "#Text.init".to_owned(),
      Self::TextNull => "#Text.null".to_owned(),
      Self::TextLen => "#Text.len".to_owned(),
      Self::TextIndex => "#Text.index".to_owned(),
      Self::TextTake => "#Text.take".to_owned(),
      Self::TextDrop => "#Text.drop".to_owned(),
      Self::TextEql => "#Text.eql".to_owned(),
      Self::TextLth => "#Text.lth".to_owned(),
      Self::TextLte => "#Text.lte".to_owned(),
      Self::TextGth => "#Text.gth".to_owned(),
      Self::TextGte => "#Text.gte".to_owned(),
    }
  }

  pub fn from_symbol(s: String) -> Option<Self> {
    match s.as_str() {
      "#Nat.suc" => Some(Self::NatSuc),
      "#Nat.pre" => Some(Self::NatPre),
      "#Nat.eql" => Some(Self::NatEql),
      "#Nat.lth" => Some(Self::NatLth),
      "#Nat.lte" => Some(Self::NatLte),
      "#Nat.gth" => Some(Self::NatGth),
      "#Nat.gte" => Some(Self::NatGte),
      "#Nat.add" => Some(Self::NatAdd),
      "#Nat.sub" => Some(Self::NatSub),
      "#Nat.mul" => Some(Self::NatMul),
      "#Nat.div" => Some(Self::NatDiv),
      "#Nat.mod" => Some(Self::NatMod),
      "#Int.new" => Some(Self::IntNew),
      "#Int.sgn" => Some(Self::IntSgn),
      "#Int.abs" => Some(Self::IntAbs),
      "#Int.eql" => Some(Self::IntEql),
      "#Int.lth" => Some(Self::IntLth),
      "#Int.lte" => Some(Self::IntLte),
      "#Int.gth" => Some(Self::IntGth),
      "#Int.gte" => Some(Self::IntGte),
      "#Int.add" => Some(Self::IntAdd),
      "#Int.sub" => Some(Self::IntSub),
      "#Int.mul" => Some(Self::IntMul),
      "#Int.div" => Some(Self::IntDiv),
      "#Int.mod" => Some(Self::IntMod),
      "#Bytes.cons" => Some(Self::BytesCons),
      "#Bytes.snoc" => Some(Self::BytesSnoc),
      "#Bytes.append" => Some(Self::BytesAppend),
      "#Bytes.head" => Some(Self::BytesHead),
      "#Bytes.last" => Some(Self::BytesLast),
      "#Bytes.tail" => Some(Self::BytesTail),
      "#Bytes.init" => Some(Self::BytesInit),
      "#Bytes.null" => Some(Self::BytesNull),
      "#Bytes.len" => Some(Self::BytesLen),
      "#Bytes.index" => Some(Self::BytesIndex),
      "#Bytes.take" => Some(Self::BytesTake),
      "#Bytes.drop" => Some(Self::BytesDrop),
      "#Bytes.eql" => Some(Self::BytesEql),
      "#Bytes.lth" => Some(Self::BytesLth),
      "#Bytes.lte" => Some(Self::BytesLte),
      "#Bytes.gth" => Some(Self::BytesGth),
      "#Bytes.gte" => Some(Self::BytesGte),
      "#Text.cons" => Some(Self::TextCons),
      "#Text.snoc" => Some(Self::TextSnoc),
      "#Text.append" => Some(Self::TextAppend),
      "#Text.head" => Some(Self::TextHead),
      "#Text.last" => Some(Self::TextLast),
      "#Text.tail" => Some(Self::TextTail),
      "#Text.init" => Some(Self::TextInit),
      "#Text.null" => Some(Self::TextNull),
      "#Text.len" => Some(Self::TextLen),
      "#Text.index" => Some(Self::TextIndex),
      "#Text.take" => Some(Self::TextTake),
      "#Text.drop" => Some(Self::TextDrop),
      "#Text.eql" => Some(Self::TextEql),
      "#Text.lth" => Some(Self::TextLth),
      "#Text.lte" => Some(Self::TextLte),
      "#Text.gth" => Some(Self::TextGth),
      "#Text.gte" => Some(Self::TextGte),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::NatSuc => yatima!("∀ #Nat -> #Nat"),
      Self::NatPre => yatima!("∀ #Nat -> #Nat"),
      Self::NatEql => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatLth => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatLte => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatGth => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatGte => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatAdd => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::NatSub => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::NatMul => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::NatDiv => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::NatMod => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::IntNew => yatima!("∀ #Bool #Nat -> #Int"),
      Self::IntSgn => yatima!("∀ #Int -> #Bool"),
      Self::IntAbs => yatima!("∀ #Int -> #Nat"),
      Self::IntEql => yatima!("∀ #Int #Int -> #Bool"),
      Self::IntLth => yatima!("∀ #Int #Int -> #Bool"),
      Self::IntLte => yatima!("∀ #Int #Int -> #Bool"),
      Self::IntGth => yatima!("∀ #Int #Int -> #Bool"),
      Self::IntGte => yatima!("∀ #Int #Int -> #Bool"),
      Self::IntAdd => yatima!("∀ #Int #Int -> #Int"),
      Self::IntSub => yatima!("∀ #Int #Int -> #Int"),
      Self::IntMul => yatima!("∀ #Int #Int -> #Int"),
      Self::IntDiv => yatima!("∀ #Int #Int -> #Int"),
      Self::IntMod => yatima!("∀ #Int #Int -> #Int"),
      Self::BytesCons => yatima!("∀ #Char #Bytes -> #Bytes"),
      Self::BytesSnoc => yatima!("∀ #Char #Bytes -> #Bytes"),
      Self::BytesAppend => yatima!("∀ #Bytes #Bytes -> #Bytes"),
      Self::BytesHead => yatima!("∀ #Bytes -> #Char"),
      Self::BytesLast => yatima!("∀ #Bytes -> #Char"),
      Self::BytesTail => yatima!("∀ #Bytes -> #Bytes"),
      Self::BytesInit => yatima!("∀ #Bytes -> #Bytes"),
      Self::BytesNull => yatima!("∀ #Bytes -> #Bool"),
      Self::BytesLen => yatima!("∀ #Bytes -> #Nat"),
      Self::BytesIndex => yatima!("∀ #Nat #Bytes -> #Char"),
      Self::BytesTake => yatima!("∀ #Nat #Bytes -> #Bytes"),
      Self::BytesDrop => yatima!("∀ #Nat #Bytes -> #Bytes"),
      Self::BytesEql => yatima!("∀ #Bytes #Bytes -> #Bool"),
      Self::BytesLth => yatima!("∀ #Bytes #Bytes -> #Bool"),
      Self::BytesLte => yatima!("∀ #Bytes #Bytes -> #Bool"),
      Self::BytesGth => yatima!("∀ #Bytes #Bytes -> #Bool"),
      Self::BytesGte => yatima!("∀ #Bytes #Bytes -> #Bool"),
      Self::TextCons => yatima!("∀ #Char #Text -> #Text"),
      Self::TextSnoc => yatima!("∀ #Char #Text -> #Text"),
      Self::TextAppend => yatima!("∀ #Text #Text -> #Text"),
      Self::TextHead => yatima!("∀ #Text -> #Char"),
      Self::TextLast => yatima!("∀ #Text -> #Char"),
      Self::TextTail => yatima!("∀ #Text -> #Text"),
      Self::TextInit => yatima!("∀ #Text -> #Text"),
      Self::TextNull => yatima!("∀ #Text -> #Bool"),
      Self::TextLen => yatima!("∀ #Text -> #Nat"),
      Self::TextIndex => yatima!("∀ #Nat #Text -> #Char"),
      Self::TextTake => yatima!("∀ #Nat #Text -> #Text"),
      Self::TextDrop => yatima!("∀ #Nat #Text -> #Text"),
      Self::TextEql => yatima!("∀ #Text #Text -> #Bool"),
      Self::TextLth => yatima!("∀ #Text #Text -> #Bool"),
      Self::TextLte => yatima!("∀ #Text #Text -> #Bool"),
      Self::TextGth => yatima!("∀ #Text #Text -> #Bool"),
      Self::TextGte => yatima!("∀ #Text #Text -> #Bool"),
      _ => todo!(),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::NatSuc => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(0)]),
      Self::NatPre => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(1)]),
      Self::NatEql => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(2)]),
      Self::NatLth => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(3)]),
      Self::NatLte => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(4)]),
      Self::NatGth => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(5)]),
      Self::NatGte => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(6)]),
      Self::NatAdd => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(7)]),
      Self::NatSub => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(8)]),
      Self::NatMul => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(9)]),
      Self::NatDiv => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(10)]),
      Self::NatMod => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(11)]),
      Self::IntNew => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(0)]),
      Self::IntSgn => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(1)]),
      Self::IntAbs => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(2)]),
      Self::IntEql => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(3)]),
      Self::IntLth => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(4)]),
      Self::IntLte => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(5)]),
      Self::IntGth => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(6)]),
      Self::IntGte => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(7)]),
      Self::IntAdd => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(8)]),
      Self::IntSub => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(9)]),
      Self::IntMul => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(10)]),
      Self::IntDiv => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(11)]),
      Self::IntMod => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(12)]),
      Self::BytesCons => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(0)]),
      Self::BytesSnoc => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(1)]),
      Self::BytesAppend => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(2)]),
      Self::BytesHead => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(3)]),
      Self::BytesLast => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(4)]),
      Self::BytesTail => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(5)]),
      Self::BytesInit => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(6)]),
      Self::BytesNull => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(7)]),
      Self::BytesLen => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(8)]),
      Self::BytesIndex => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(9)]),
      Self::BytesTake => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(10)]),
      Self::BytesDrop => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(11)]),
      Self::BytesEql => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(12)]),
      Self::BytesLth => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(12)]),
      Self::BytesLte => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(14)]),
      Self::BytesGth => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(15)]),
      Self::BytesGte => Ipld::List(vec![Ipld::Integer(2), Ipld::Integer(16)]),
      Self::TextCons => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(0)]),
      Self::TextSnoc => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(1)]),
      Self::TextAppend => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(2)]),
      Self::TextHead => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(3)]),
      Self::TextLast => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(4)]),
      Self::TextTail => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(5)]),
      Self::TextInit => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(6)]),
      Self::TextNull => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(7)]),
      Self::TextLen => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(8)]),
      Self::TextIndex => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(9)]),
      Self::TextTake => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(10)]),
      Self::TextDrop => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(11)]),
      Self::TextEql => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(12)]),
      Self::TextLth => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(13)]),
      Self::TextLte => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(14)]),
      Self::TextGth => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(15)]),
      Self::TextGte => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(16)]),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(0), Ipld::Integer(0)] => Ok(Self::NatSuc),
        [Ipld::Integer(0), Ipld::Integer(1)] => Ok(Self::NatPre),
        [Ipld::Integer(0), Ipld::Integer(2)] => Ok(Self::NatEql),
        [Ipld::Integer(0), Ipld::Integer(3)] => Ok(Self::NatLth),
        [Ipld::Integer(0), Ipld::Integer(4)] => Ok(Self::NatLte),
        [Ipld::Integer(0), Ipld::Integer(5)] => Ok(Self::NatGth),
        [Ipld::Integer(0), Ipld::Integer(6)] => Ok(Self::NatGte),
        [Ipld::Integer(0), Ipld::Integer(7)] => Ok(Self::NatAdd),
        [Ipld::Integer(0), Ipld::Integer(8)] => Ok(Self::NatSub),
        [Ipld::Integer(0), Ipld::Integer(9)] => Ok(Self::NatMul),
        [Ipld::Integer(0), Ipld::Integer(10)] => Ok(Self::NatDiv),
        [Ipld::Integer(0), Ipld::Integer(11)] => Ok(Self::NatMod),
        [Ipld::Integer(1), Ipld::Integer(0)] => Ok(Self::IntNew),
        [Ipld::Integer(1), Ipld::Integer(1)] => Ok(Self::IntSgn),
        [Ipld::Integer(1), Ipld::Integer(2)] => Ok(Self::IntAbs),
        [Ipld::Integer(1), Ipld::Integer(3)] => Ok(Self::IntEql),
        [Ipld::Integer(1), Ipld::Integer(4)] => Ok(Self::IntLth),
        [Ipld::Integer(1), Ipld::Integer(5)] => Ok(Self::IntLte),
        [Ipld::Integer(1), Ipld::Integer(6)] => Ok(Self::IntGth),
        [Ipld::Integer(1), Ipld::Integer(7)] => Ok(Self::IntGte),
        [Ipld::Integer(1), Ipld::Integer(8)] => Ok(Self::IntAdd),
        [Ipld::Integer(1), Ipld::Integer(9)] => Ok(Self::IntSub),
        [Ipld::Integer(1), Ipld::Integer(10)] => Ok(Self::IntMul),
        [Ipld::Integer(1), Ipld::Integer(11)] => Ok(Self::IntDiv),
        [Ipld::Integer(1), Ipld::Integer(12)] => Ok(Self::IntMod),
        [Ipld::Integer(2), Ipld::Integer(0)] => Ok(Self::BytesCons),
        [Ipld::Integer(2), Ipld::Integer(1)] => Ok(Self::BytesSnoc),
        [Ipld::Integer(2), Ipld::Integer(2)] => Ok(Self::BytesAppend),
        [Ipld::Integer(2), Ipld::Integer(3)] => Ok(Self::BytesHead),
        [Ipld::Integer(2), Ipld::Integer(4)] => Ok(Self::BytesLast),
        [Ipld::Integer(2), Ipld::Integer(5)] => Ok(Self::BytesTail),
        [Ipld::Integer(2), Ipld::Integer(6)] => Ok(Self::BytesInit),
        [Ipld::Integer(2), Ipld::Integer(7)] => Ok(Self::BytesNull),
        [Ipld::Integer(2), Ipld::Integer(8)] => Ok(Self::BytesLen),
        [Ipld::Integer(2), Ipld::Integer(9)] => Ok(Self::BytesIndex),
        [Ipld::Integer(2), Ipld::Integer(10)] => Ok(Self::BytesTake),
        [Ipld::Integer(2), Ipld::Integer(11)] => Ok(Self::BytesDrop),
        [Ipld::Integer(2), Ipld::Integer(12)] => Ok(Self::BytesEql),
        [Ipld::Integer(2), Ipld::Integer(13)] => Ok(Self::BytesLth),
        [Ipld::Integer(2), Ipld::Integer(14)] => Ok(Self::BytesLte),
        [Ipld::Integer(2), Ipld::Integer(15)] => Ok(Self::BytesGth),
        [Ipld::Integer(2), Ipld::Integer(16)] => Ok(Self::BytesGte),
        [Ipld::Integer(3), Ipld::Integer(0)] => Ok(Self::TextCons),
        [Ipld::Integer(3), Ipld::Integer(1)] => Ok(Self::TextSnoc),
        [Ipld::Integer(3), Ipld::Integer(2)] => Ok(Self::TextAppend),
        [Ipld::Integer(3), Ipld::Integer(3)] => Ok(Self::TextHead),
        [Ipld::Integer(3), Ipld::Integer(4)] => Ok(Self::TextLast),
        [Ipld::Integer(3), Ipld::Integer(5)] => Ok(Self::TextTail),
        [Ipld::Integer(3), Ipld::Integer(6)] => Ok(Self::TextInit),
        [Ipld::Integer(3), Ipld::Integer(7)] => Ok(Self::TextNull),
        [Ipld::Integer(3), Ipld::Integer(8)] => Ok(Self::TextLen),
        [Ipld::Integer(3), Ipld::Integer(9)] => Ok(Self::TextIndex),
        [Ipld::Integer(3), Ipld::Integer(10)] => Ok(Self::TextTake),
        [Ipld::Integer(3), Ipld::Integer(11)] => Ok(Self::TextDrop),
        [Ipld::Integer(3), Ipld::Integer(12)] => Ok(Self::TextEql),
        [Ipld::Integer(3), Ipld::Integer(13)] => Ok(Self::TextLth),
        [Ipld::Integer(3), Ipld::Integer(14)] => Ok(Self::TextLte),
        [Ipld::Integer(3), Ipld::Integer(15)] => Ok(Self::TextGth),
        [Ipld::Integer(3), Ipld::Integer(16)] => Ok(Self::TextGte),
        xs => Err(IpldError::PrimOp(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::PrimOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::NatEql => 2,
      Self::NatLth => 2,
      Self::NatLte => 2,
      Self::NatGth => 2,
      Self::NatGte => 2,
      Self::NatSuc => 1,
      Self::NatPre => 1,
      Self::NatAdd => 2,
      Self::NatSub => 2,
      Self::NatMul => 2,
      Self::NatDiv => 2,
      Self::NatMod => 2,
      Self::IntNew => 2,
      Self::IntSgn => 1,
      Self::IntAbs => 1,
      Self::IntEql => 2,
      Self::IntLth => 2,
      Self::IntLte => 2,
      Self::IntGth => 2,
      Self::IntGte => 2,
      Self::IntAdd => 2,
      Self::IntSub => 2,
      Self::IntMul => 2,
      Self::IntDiv => 2,
      Self::IntMod => 2,
      Self::BytesCons => 2,
      Self::BytesSnoc => 2,
      Self::BytesAppend => 2,
      Self::BytesHead => 1,
      Self::BytesLast => 1,
      Self::BytesTail => 1,
      Self::BytesInit => 1,
      Self::BytesNull => 1,
      Self::BytesLen => 1,
      Self::BytesIndex => 2,
      Self::BytesTake => 1,
      Self::BytesDrop => 1,
      Self::BytesEql => 2,
      Self::BytesLth => 2,
      Self::BytesLte => 2,
      Self::BytesGth => 2,
      Self::BytesGte => 2,
      Self::TextCons => 2,
      Self::TextSnoc => 2,
      Self::TextAppend => 2,
      Self::TextHead => 1,
      Self::TextLast => 1,
      Self::TextTail => 1,
      Self::TextInit => 1,
      Self::TextNull => 1,
      Self::TextLen => 1,
      Self::TextIndex => 2,
      Self::TextTake => 1,
      Self::TextDrop => 1,
      Self::TextEql => 2,
      Self::TextLth => 2,
      Self::TextLte => 2,
      Self::TextGth => 2,
      Self::TextGte => 2,
    }
  }
}

pub fn apply_una_op(opr: PrimOp, x: Literal) -> Option<Literal> {
  use Literal::*;
  use PrimOp::*;
  match (opr, x) {
    (NatSuc, Nat(x)) => Some(Nat(x + BigUint::from(1u64))),
    (NatPre, Nat(x)) if x != 0u64.into() => Some(Nat(x - BigUint::from(1u64))),
    (IntSgn, Int(x)) => match x.sign() {
      Sign::NoSign => Some(Int(BigInt::from(0i64))),
      Sign::Plus => Some(Int(BigInt::from(1i64))),
      Sign::Minus => Some(Int(BigInt::from(-1i64))),
    },
    (IntAbs, Int(x)) => Some(Nat(x.into_parts().1)),
    _ => None,
  }
}

pub fn apply_bin_op(opr: PrimOp, x: Literal, y: Literal) -> Option<Literal> {
  use Literal::*;
  use PrimOp::*;
  let tt = Bool(true);
  let ff = Bool(false);
  let ite = |c| if c { tt } else { ff };
  match (opr, x, y) {
    // Construction
    (IntNew, Bool(x), Nat(y)) => {
      if y == 0u64.into() {
        Some(Int(BigInt::from_biguint(Sign::NoSign, y)))
      }
      else if x {
        Some(Int(BigInt::from_biguint(Sign::Plus, y)))
      }
      else {
        Some(Int(BigInt::from_biguint(Sign::Minus, y)))
      }
    }
    // Comparison
    (NatEql, Nat(x), Nat(y)) => Some(ite(x == y)),
    (IntEql, Int(x), Int(y)) => Some(ite(x == y)),
    (NatLth, Nat(x), Nat(y)) => Some(ite(x < y)),
    (IntLth, Int(x), Int(y)) => Some(ite(x < y)),
    (NatLte, Nat(x), Nat(y)) => Some(ite(x <= y)),
    (IntLte, Int(x), Int(y)) => Some(ite(x <= y)),
    (NatGth, Nat(x), Nat(y)) => Some(ite(x > y)),
    (IntGth, Int(x), Int(y)) => Some(ite(x > y)),
    // Arithmetic
    (NatAdd, Nat(x), Nat(y)) => Some(Nat(x + y)),
    (IntAdd, Int(x), Int(y)) => Some(Int(x + y)),
    (NatSub, Nat(x), Nat(y)) if x >= y => Some(Nat(x - y)),
    (IntSub, Int(x), Int(y)) => Some(Int(x - y)),
    (NatMul, Nat(x), Nat(y)) => Some(Nat(x * y)),
    (IntMul, Int(x), Int(y)) => Some(Int(x * y)),
    (NatDiv, Nat(x), Nat(y)) if y != (0u64).into() => Some(Nat(x * y)),
    (IntDiv, Int(x), Int(y)) if y != 0.into() => Some(Int(x / y)),
    (NatMod, Nat(x), Nat(y)) if y != (0u64).into() => Some(Nat(x * y)),
    (IntMod, Int(x), Int(y)) if y != 0.into() => Some(Int(x % y)),
    (TextCons, Char(c), Text(mut cs)) => {
      cs.insert_char(0, c);
      Some(Text(cs))
    }
    (BytesCons, U8(c), Bytes(mut cs)) => {
      cs.push_front(c);
      Some(Bytes(cs))
    }
    _ => None,
  }
}

impl fmt::Display for PrimOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.symbol())
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
  impl Arbitrary for PrimOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..24);
      match gen {
        0 => Self::NatEql,
        1 => Self::NatLth,
        2 => Self::NatLte,
        3 => Self::NatGth,
        4 => Self::NatGte,
        5 => Self::NatSuc,
        6 => Self::NatPre,
        7 => Self::NatAdd,
        8 => Self::NatSub,
        9 => Self::NatMul,
        10 => Self::NatDiv,
        11 => Self::NatMod,
        12 => Self::IntNew,
        13 => Self::IntSgn,
        14 => Self::IntAbs,
        15 => Self::IntEql,
        16 => Self::IntLth,
        17 => Self::IntLte,
        18 => Self::IntGth,
        19 => Self::IntGte,
        20 => Self::IntAdd,
        21 => Self::IntSub,
        22 => Self::IntMul,
        23 => Self::IntDiv,
        _ => Self::IntMod,
      }
    }
  }

  #[quickcheck]
  fn primop_ipld(x: PrimOp) -> bool {
    match PrimOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[test]
  fn test_apply_bin_op() {
    assert_eq!(
      Some(Literal::Text(ropey::Rope::from_str("foo"))),
      apply_bin_op(
        PrimOp::TextCons,
        Literal::Char('f'),
        Literal::Text(ropey::Rope::from_str("oo"))
      )
    )
  }
}
