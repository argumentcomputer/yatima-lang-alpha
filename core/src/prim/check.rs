use crate::{
  literal::{
    LitType,
    Literal,
  },
  position::Pos,
  term::Term,
  yatima,
};

use num_bigint::BigUint;

use crate::prim::{
  bits::BitsOp,
  bool::BoolOp,
  bytes::BytesOp,
  char::CharOp,
  i16::I16Op,
  i32::I32Op,
  i64::I64Op,
  i8::I8Op,
  int::IntOp,
  nat::NatOp,
  text::TextOp,
  u16::U16Op,
  u32::U32Op,
  u64::U64Op,
  u8::U8Op,
  Op,
};

pub fn type_of_op(x: &Op) -> Term {
  match x {
    Op::Nat(op) => type_of_nat_op(op),
    Op::Int(op) => type_of_int_op(op),
    Op::Bits(op) => type_of_bits_op(op),
    Op::Bytes(op) => type_of_bytes_op(op),
    Op::Bool(op) => type_of_bool_op(op),
    Op::Char(op) => type_of_char_op(op),
    Op::Text(op) => type_of_text_op(op),
    Op::I8(op) => type_of_i8_op(op),
    Op::I16(op) => type_of_i16_op(op),
    Op::I32(op) => type_of_i32_op(op),
    Op::I64(op) => type_of_i64_op(op),
    Op::U8(op) => type_of_u8_op(op),
    Op::U16(op) => type_of_u16_op(op),
    Op::U32(op) => type_of_u32_op(op),
    Op::U64(op) => type_of_u64_op(op),
  }
}

pub fn type_of_bits_op(x: &BitsOp) -> Term {
  match x {
    BitsOp::Cons => yatima!("(∀ (ω x: #Bool) (ω y: #Bits) -> #Bits)"),
    BitsOp::Len => yatima!("(∀ (ω x: #Bits) -> #Nat)"),
    BitsOp::Head => yatima!("(∀ (ω x: #Bits) -> #U8)"),
    BitsOp::Tail => yatima!("(∀ (ω x: #Bits) -> #Bits)"),
    BitsOp::Take => yatima!("(∀ (ω x: #Nat) (ω y: #Bits) -> #Bits)"),
    BitsOp::Drop => yatima!("(∀ (ω x: #Nat) (ω y: #Bits) -> #Bits)"),
    BitsOp::Append => yatima!("(∀ (ω x: #Bits) (ω y: #Bits) -> #Bits)"),
    BitsOp::Insert => {
      yatima!("(∀ (ω x: #Bool) (ω y: #Nat) (ω z: #Bits) -> #Bits)")
    }
    BitsOp::Remove => yatima!("(∀ (ω x: #Nat) (ω y: #Bits) -> #Bits)"),
    BitsOp::Index => yatima!("(∀ (ω x: #Nat) (ω y: #Bits) -> #U8)"),
    BitsOp::ToBytes => yatima!("(∀ (ω x: #Bits) -> #Bytes)"),
  }
}

pub fn type_of_bool_op(x: &BoolOp) -> Term {
  match x {
    BoolOp::Eql => yatima!("(∀ (ω x: &#Bool) (ω y: #Bool) -> #Bool)"),
    BoolOp::Lte => yatima!("(∀ (ω x: &#Bool) (ω y: #Bool) -> #Bool)"),
    BoolOp::Lth => yatima!("(∀ (ω x: &#Bool) (ω y: #Bool) -> #Bool)"),
    BoolOp::Gte => yatima!("(∀ (ω x: &#Bool) (ω y: #Bool) -> #Bool)"),
    BoolOp::Gth => yatima!("(∀ (ω x: &#Bool) (ω y: #Bool) -> #Bool)"),
    BoolOp::And => yatima!("(∀ (ω x: &#Bool) (ω y: #Bool) -> #Bool)"),
    BoolOp::Or => yatima!("(∀ (ω x: &#Bool) (ω y: #Bool) -> #Bool)"),
    BoolOp::Xor => yatima!("(∀ (ω x: &#Bool) (ω y: #Bool) -> #Bool)"),
    BoolOp::Not => yatima!("(∀ (ω x: &#Bool) -> #Bool)"),
  }
}

pub fn type_of_bytes_op(x: &BytesOp) -> Term {
  match x {
    BytesOp::Cons => yatima!("(∀ (ω x: &#U8) (ω y: #Bytes) -> #Bytes)"),
    BytesOp::Len => yatima!("(∀ (ω x: &#Bytes) -> #Nat)"),
    BytesOp::Head => yatima!("(∀ (ω x: &#Bytes) -> #U8)"),
    BytesOp::Tail => yatima!("(∀ (ω x: &#Bytes) -> #Bytes)"),
    BytesOp::Take => yatima!("(∀ (ω x: &#Nat) (ω y: #Bytes) -> #Bytes)"),
    BytesOp::Drop => yatima!("(∀ (ω x: &#Nat) (ω y: #Bytes) -> #Bytes)"),
    BytesOp::Append => {
      yatima!("(∀ (ω x: &#Bytes) (ω y: #Bytes) -> #Bytes)")
    }
    BytesOp::Insert => {
      yatima!("(∀ (ω x: &#U8) (ω y: #Nat) (ω z: #Bytes) -> #Bytes)")
    }
    BytesOp::Remove => {
      yatima!("(∀ (ω x: &#Nat) (ω y: #Bytes) -> #Bytes)")
    }
    BytesOp::Index => yatima!("(∀ (ω x: &#Nat) (ω y: #Bytes) -> #U8)"),
    BytesOp::ToBits => yatima!("(∀ (ω x: &#Nat) (ω y: #Bytes) -> #Bits)"),
  }
}
pub fn type_of_char_op(x: &CharOp) -> Term {
  match x {
    CharOp::FromU32 => yatima!("(∀ (ω x: &#U32) -> #Char)"),
    CharOp::ToU32 => yatima!("(∀ (ω x: &#Char) -> #U32)"),
    CharOp::IsAlphabetic => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAlphanumeric => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAscii => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiAlphabetic => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiAlphanumeric => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiControl => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiDigit => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiGraphic => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiHexDigit => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiLowerCase => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiPunctuation => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiUpperCase => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsAsciiWhitespace => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsControl => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsDigit => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsLowercase => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsNumeric => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsUppercase => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::IsWhitespace => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::LenUTF8 => yatima!("(∀ (ω x: &#Char) -> #Nat)"),
    CharOp::LenUTF16 => yatima!("(∀ (ω x: &#Char) -> #Nat)"),
    CharOp::ToAsciiLowercase => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::ToAsciiUppercase => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::ToLowercase => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::ToUppercase => yatima!("(∀ (ω x: &#Char) -> #Bool)"),
    CharOp::Eql => yatima!("(∀ (ω x: &#Char) (ω y: #Char) -> #Bool)"),
    CharOp::Lte => yatima!("(∀ (ω x: &#Char) (ω y: #Char) -> #Bool)"),
    CharOp::Lth => yatima!("(∀ (ω x: &#Char) (ω y: #Char) -> #Bool)"),
    CharOp::Gth => yatima!("(∀ (ω x: &#Char) (ω y: #Char) -> #Bool)"),
    CharOp::Gte => yatima!("(∀ (ω x: &#Char) (ω y: #Char) -> #Bool)"),
  }
}

pub fn type_of_i8_op(x: &I8Op) -> Term {
  match x {
    I8Op::Abs => yatima!("(∀ (ω x: &#I8) -> #U8)"),
    I8Op::Sgn => yatima!("(∀ (ω x: &#I8) -> #Bool)"),
    I8Op::Max => yatima!("#I8)"),
    I8Op::Min => yatima!("#I8)"),
    I8Op::Eql => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #Bool)"),
    I8Op::Lte => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #Bool)"),
    I8Op::Lth => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #Bool)"),
    I8Op::Gth => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #Bool)"),
    I8Op::Gte => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #Bool)"),
    I8Op::Not => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #Bool)"),
    I8Op::And => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #Bool)"),
    I8Op::Or => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #Bool)"),
    I8Op::Xor => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #Bool)"),
    I8Op::Add => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #I8)"),
    I8Op::Sub => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #I8)"),
    I8Op::Mul => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #I8)"),
    I8Op::Div => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #I8)"),
    I8Op::Mod => yatima!("(∀ (ω x: &#I8) (ω y: #I8) -> #I8)"),
    I8Op::Pow => yatima!("(∀ (ω x: &#I8) (ω y: #U32) -> #I8)"),
    I8Op::Shl => yatima!("(∀ (ω x: &#U32) (ω y: #I8) -> #I8)"),
    I8Op::Shr => yatima!("(∀ (ω x: &#U32) (ω y: #I8) -> #I8)"),
    I8Op::Rol => yatima!("(∀ (ω x: &#U32) (ω y: #I8) -> #I8)"),
    I8Op::Ror => yatima!("(∀ (ω x: &#U32) (ω y: #I8) -> #I8)"),
    I8Op::CountZeros => yatima!("(∀ (ω x: &#I8) -> #U32)"),
    I8Op::CountOnes => yatima!("(∀ (ω x: &#I8) -> #U32)"),
    I8Op::ToU8 => yatima!("(∀ (ω x: &#I8) -> #U8)"),
    I8Op::ToU16 => yatima!("(∀ (ω x: &#I8) -> #U16)"),
    I8Op::ToU32 => yatima!("(∀ (ω x: &#I8) -> #U32)"),
    I8Op::ToU64 => yatima!("(∀ (ω x: &#I8) -> #U64)"),
    I8Op::ToNat => yatima!("(∀ (ω x: &#I8) -> #Nat)"),
    I8Op::ToI16 => yatima!("(∀ (ω x: &#I8) -> #I16)"),
    I8Op::ToI32 => yatima!("(∀ (ω x: &#I8) -> #I32)"),
    I8Op::ToI64 => yatima!("(∀ (ω x: &#I8) -> #I64)"),
    I8Op::ToInt => yatima!("(∀ (ω x: &#I8) -> #Int)"),
    I8Op::ToBits => yatima!("(∀ (ω x: &#I8) -> #Bits)"),
    I8Op::ToBytes => yatima!("(∀ (ω x: &#I8) -> #Bytes)"),
  }
}
pub fn type_of_i16_op(x: &I16Op) -> Term {
  match x {
    I16Op::Abs => yatima!("(∀ (ω x: &#I16) -> #U16)"),
    I16Op::Sgn => yatima!("(∀ (ω x: &#I16) -> #Bool)"),
    I16Op::Max => yatima!("#I16"),
    I16Op::Min => yatima!("#I16"),
    I16Op::Eql => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #Bool)"),
    I16Op::Lte => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #Bool)"),
    I16Op::Lth => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #Bool)"),
    I16Op::Gth => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #Bool)"),
    I16Op::Gte => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #Bool)"),
    I16Op::Not => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #Bool)"),
    I16Op::And => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #Bool)"),
    I16Op::Or => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #Bool)"),
    I16Op::Xor => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #Bool)"),
    I16Op::Add => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #I16)"),
    I16Op::Sub => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #I16)"),
    I16Op::Mul => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #I16)"),
    I16Op::Div => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #I16)"),
    I16Op::Mod => yatima!("(∀ (ω x: &#I16) (ω y: #I16) -> #I16)"),
    I16Op::Pow => yatima!("(∀ (ω x: &#I16) (ω y: #U32) -> #I16)"),
    I16Op::Shl => yatima!("(∀ (ω x: &#U32) (ω y: #I16) -> #I16)"),
    I16Op::Shr => yatima!("(∀ (ω x: &#U32) (ω y: #I16) -> #I16)"),
    I16Op::Rol => yatima!("(∀ (ω x: &#U32) (ω y: #I16) -> #I16)"),
    I16Op::Ror => yatima!("(∀ (ω x: &#U32) (ω y: #I16) -> #I16)"),
    I16Op::CountZeros => yatima!("(∀ (ω x: &#I16) -> #U32)"),
    I16Op::CountOnes => yatima!("(∀ (ω x: &#I16) -> #U32)"),
    I16Op::ToU8 => yatima!("(∀ (ω x: &#I16) -> #U8)"),
    I16Op::ToU16 => yatima!("(∀ (ω x: &#I16) -> #U16)"),
    I16Op::ToU32 => yatima!("(∀ (ω x: &#I16) -> #U32)"),
    I16Op::ToU64 => yatima!("(∀ (ω x: &#I16) -> #U64)"),
    I16Op::ToNat => yatima!("(∀ (ω x: &#I16) -> #Nat)"),
    I16Op::ToI8 => yatima!("(∀ (ω x: &#I16) -> #I8)"),
    I16Op::ToI32 => yatima!("(∀ (ω x: &#I16) -> #I32)"),
    I16Op::ToI64 => yatima!("(∀ (ω x: &#I16) -> #I64)"),
    I16Op::ToInt => yatima!("(∀ (ω x: &#I16) -> #Int)"),
    I16Op::ToBits => yatima!("(∀ (ω x: &#I16) -> #Bits)"),
    I16Op::ToBytes => yatima!("(∀ (ω x: &#I16) -> #Bytes)"),
  }
}

pub fn type_of_i32_op(x: &I32Op) -> Term {
  match x {
    I32Op::Abs => yatima!("(∀ (ω x: &#I32) -> #U32)"),
    I32Op::Sgn => yatima!("(∀ (ω x: &#I32) -> #Bool)"),
    I32Op::Max => yatima!("#I32"),
    I32Op::Min => yatima!("#I32"),
    I32Op::Eql => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #Bool)"),
    I32Op::Lte => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #Bool)"),
    I32Op::Lth => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #Bool)"),
    I32Op::Gth => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #Bool)"),
    I32Op::Gte => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #Bool)"),
    I32Op::Not => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #Bool)"),
    I32Op::And => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #Bool)"),
    I32Op::Or => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #Bool)"),
    I32Op::Xor => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #Bool)"),
    I32Op::Add => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #I32)"),
    I32Op::Sub => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #I32)"),
    I32Op::Mul => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #I32)"),
    I32Op::Div => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #I32)"),
    I32Op::Mod => yatima!("(∀ (ω x: &#I32) (ω y: #I32) -> #I32)"),
    I32Op::Pow => yatima!("(∀ (ω x: &#I32) (ω y: #U32) -> #I32)"),
    I32Op::Shl => yatima!("(∀ (ω x: &#U32) (ω y: #I32) -> #I32)"),
    I32Op::Shr => yatima!("(∀ (ω x: &#U32) (ω y: #I32) -> #I32)"),
    I32Op::Rol => yatima!("(∀ (ω x: &#U32) (ω y: #I32) -> #I32)"),
    I32Op::Ror => yatima!("(∀ (ω x: &#U32) (ω y: #I32) -> #I32)"),
    I32Op::CountZeros => yatima!("(∀ (ω x: &#I32) -> #U32)"),
    I32Op::CountOnes => yatima!("(∀ (ω x: &#I32) -> #U32)"),
    I32Op::ToU8 => yatima!("(∀ (ω x: &#I32) -> #U8)"),
    I32Op::ToU16 => yatima!("(∀ (ω x: &#I32) -> #U16)"),
    I32Op::ToU32 => yatima!("(∀ (ω x: &#I32) -> #U32)"),
    I32Op::ToU64 => yatima!("(∀ (ω x: &#I32) -> #U64)"),
    I32Op::ToNat => yatima!("(∀ (ω x: &#I32) -> #Nat)"),
    I32Op::ToI8 => yatima!("(∀ (ω x: &#I32) -> #I8)"),
    I32Op::ToI16 => yatima!("(∀ (ω x: &#I32) -> #I16)"),
    I32Op::ToI64 => yatima!("(∀ (ω x: &#I32) -> #I64)"),
    I32Op::ToInt => yatima!("(∀ (ω x: &#I32) -> #Int)"),
    I32Op::ToBytes => yatima!("(∀ (ω x: &#I32) -> #Bytes)"),
    I32Op::ToBits => yatima!("(∀ (ω x: &#I32) -> #Bits)"),
  }
}
pub fn type_of_i64_op(x: &I64Op) -> Term {
  match x {
    I64Op::Abs => yatima!("(∀ (ω x: &#I64) -> #U64)"),
    I64Op::Sgn => yatima!("(∀ (ω x: &#I64) -> #Bool)"),
    I64Op::Max => yatima!("#I64)"),
    I64Op::Min => yatima!("#I64)"),
    I64Op::Eql => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #Bool)"),
    I64Op::Lte => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #Bool)"),
    I64Op::Lth => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #Bool)"),
    I64Op::Gth => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #Bool)"),
    I64Op::Gte => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #Bool)"),
    I64Op::Not => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #Bool)"),
    I64Op::And => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #Bool)"),
    I64Op::Or => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #Bool)"),
    I64Op::Xor => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #Bool)"),
    I64Op::Add => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #I64)"),
    I64Op::Sub => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #I64)"),
    I64Op::Mul => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #I64)"),
    I64Op::Div => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #I64)"),
    I64Op::Mod => yatima!("(∀ (ω x: &#I64) (ω y: #I64) -> #I64)"),
    I64Op::Pow => yatima!("(∀ (ω x: &#I64) (ω y: #U32) -> #I64)"),
    I64Op::Shl => yatima!("(∀ (ω x: &#U32) (ω y: #I64) -> #I64)"),
    I64Op::Shr => yatima!("(∀ (ω x: &#U32) (ω y: #I64) -> #I64)"),
    I64Op::Rol => yatima!("(∀ (ω x: &#U32) (ω y: #I64) -> #I64)"),
    I64Op::Ror => yatima!("(∀ (ω x: &#U32) (ω y: #I64) -> #I64)"),
    I64Op::CountZeros => yatima!("(∀ (ω x: &#I64) -> #U32)"),
    I64Op::CountOnes => yatima!("(∀ (ω x: &#I64) -> #U32)"),
    I64Op::ToU8 => yatima!("(∀ (ω x: &#I64) -> #U8)"),
    I64Op::ToU16 => yatima!("(∀ (ω x: &#I64) -> #U16)"),
    I64Op::ToU32 => yatima!("(∀ (ω x: &#I64) -> #U32)"),
    I64Op::ToU64 => yatima!("(∀ (ω x: &#I64) -> #U64)"),
    I64Op::ToNat => yatima!("(∀ (ω x: &#I64) -> #Nat)"),
    I64Op::ToI8 => yatima!("(∀ (ω x: &#I64) -> #I8)"),
    I64Op::ToI16 => yatima!("(∀ (ω x: &#I64) -> #I16)"),
    I64Op::ToI32 => yatima!("(∀ (ω x: &#I64) -> #I32)"),
    I64Op::ToInt => yatima!("(∀ (ω x: &#I64) -> #Int)"),
    I64Op::ToBytes => yatima!("(∀ (ω x: &#I64) -> #Bytes)"),
    I64Op::ToBits => yatima!("(∀ (ω x: &#I64) -> #Bits)"),
  }
}
pub fn type_of_int_op(x: &IntOp) -> Term {
  match x {
    IntOp::New => yatima!("(∀ (ω x: &#Bool) (ω y: #Nat) -> #Int)"),
    IntOp::Sgn => yatima!("(∀ (ω x: &#Int) -> #Bool)"),
    IntOp::Abs => yatima!("(∀ (ω x: &#Int) -> #Nat)"),
    IntOp::Eql => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Bool)"),
    IntOp::Lte => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Bool)"),
    IntOp::Lth => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Bool)"),
    IntOp::Gte => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Bool)"),
    IntOp::Gth => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Bool)"),
    IntOp::Add => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Int)"),
    IntOp::Sub => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Int)"),
    IntOp::Mul => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Int)"),
    IntOp::Div => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Int)"),
    IntOp::Mod => yatima!("(∀ (ω x: &#Int) (ω y: #Int) -> #Int)"),
  }
}

pub fn type_of_nat_op(x: &NatOp) -> Term {
  match x {
    NatOp::Suc => yatima!("(∀ (ω x: &#Nat) -> #Nat)"),
    NatOp::Pre => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Nat)"),
    NatOp::Eql => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Bool)"),
    NatOp::Lte => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Bool)"),
    NatOp::Lth => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Bool)"),
    NatOp::Gte => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Bool)"),
    NatOp::Gth => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Bool)"),
    NatOp::Add => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Nat)"),
    NatOp::Sub => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Nat)"),
    NatOp::Mul => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Nat)"),
    NatOp::Div => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Nat)"),
    NatOp::Mod => yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) -> #Nat)"),
  }
}

pub fn type_of_text_op(x: &TextOp) -> Term {
  match x {
    TextOp::Cons => yatima!("(∀ (ω x: &#Char) (ω y: #Text) -> #Text)"),
    TextOp::LenChars => yatima!("(∀ (ω x: &#Text) -> #Nat)"),
    TextOp::LenLines => yatima!("(∀ (ω x: &#Text) -> #Nat)"),
    TextOp::LenBytes => yatima!("(∀ (ω x: &#Text) -> #Nat)"),
    TextOp::Append => yatima!("(∀ (ω x: &#Text) (ω y: #Text) -> #Text)"),
    TextOp::Insert => {
      yatima!("(∀ (ω x: &#Nat) (ω y: #Text) (ω z: #Text) -> #Text)")
    }
    TextOp::Remove => {
      yatima!("(∀ (ω x: &#Nat) (ω y: #Nat) (ω z: #Text) -> #Text)")
    }
    TextOp::Take => yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Text)"),
    TextOp::Drop => yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Text)"),
    TextOp::Eql => yatima!("(∀ (ω x: &#Text) (ω y: #Text) -> #Bool)"),
    TextOp::Lte => yatima!("(∀ (ω x: &#Text) (ω y: #Text) -> #Bool)"),
    TextOp::Lth => yatima!("(∀ (ω x: &#Text) (ω y: #Text) -> #Bool)"),
    TextOp::Gte => yatima!("(∀ (ω x: &#Text) (ω y: #Text) -> #Bool)"),
    TextOp::Gth => yatima!("(∀ (ω x: &#Text) (ω y: #Text) -> #Bool)"),
    TextOp::Char => yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Char)"),
    TextOp::Byte => yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #U8)"),
    TextOp::Line => yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Text)"),
    TextOp::CharAtByte => {
      yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Nat)")
    }
    TextOp::ByteAtChar => {
      yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Nat)")
    }
    TextOp::LineAtByte => {
      yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Nat)")
    }
    TextOp::LineAtChar => {
      yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Nat)")
    }
    TextOp::LineStartChar => {
      yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Nat)")
    }
    TextOp::LineStartByte => {
      yatima!("(∀ (ω x: &#Nat) (ω y: #Text) -> #Nat)")
    }
    TextOp::ToBytes => yatima!("(∀ (ω x: &#Text) -> #Bytes)"),
  }
}

pub fn type_of_u8_op(x: &U8Op) -> Term {
  match x {
    U8Op::Max => yatima!("#U8"),
    U8Op::Min => yatima!("#U8"),
    U8Op::Eql => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #Bool)"),
    U8Op::Lte => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #Bool)"),
    U8Op::Lth => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #Bool)"),
    U8Op::Gth => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #Bool)"),
    U8Op::Gte => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #Bool)"),
    U8Op::Not => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #Bool)"),
    U8Op::And => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #Bool)"),
    U8Op::Or => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #Bool)"),
    U8Op::Xor => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #Bool)"),
    U8Op::Add => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #U8)"),
    U8Op::Sub => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #U8)"),
    U8Op::Mul => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #U8)"),
    U8Op::Div => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #U8)"),
    U8Op::Mod => yatima!("(∀ (ω x: &#U8) (ω y: #U8) -> #U8)"),
    U8Op::Pow => yatima!("(∀ (ω x: &#U8) (ω y: #U32) -> #U8)"),
    U8Op::Shl => yatima!("(∀ (ω x: &#U32) (ω y: #U8) -> #U8)"),
    U8Op::Shr => yatima!("(∀ (ω x: &#U32) (ω y: #U8) -> #U8)"),
    U8Op::Rol => yatima!("(∀ (ω x: &#U32) (ω y: #U8) -> #U8)"),
    U8Op::Ror => yatima!("(∀ (ω x: &#U32) (ω y: #U8) -> #U8)"),
    U8Op::CountZeros => yatima!("(∀ (ω x: &#U8) -> #U32)"),
    U8Op::CountOnes => yatima!("(∀ (ω x: &#U8) -> #U32)"),
    U8Op::ToU16 => yatima!("(∀ (ω x: &#U8) -> #U16)"),
    U8Op::ToU32 => yatima!("(∀ (ω x: &#U8) -> #U32)"),
    U8Op::ToU64 => yatima!("(∀ (ω x: &#U8) -> #U64)"),
    U8Op::ToNat => yatima!("(∀ (ω x: &#U8) -> #Nat)"),
    U8Op::ToI8 => yatima!("(∀ (ω x: &#U8) -> #I8)"),
    U8Op::ToI16 => yatima!("(∀ (ω x: &#U8) -> #I16)"),
    U8Op::ToI32 => yatima!("(∀ (ω x: &#U8) -> #I32)"),
    U8Op::ToI64 => yatima!("(∀ (ω x: &#U8) -> #I64)"),
    U8Op::ToInt => yatima!("(∀ (ω x: &#U8) -> #Int)"),
    U8Op::ToBits => yatima!("(∀ (ω x: &#U8) -> #Bits)"),
    U8Op::ToBytes => yatima!("(∀ (ω x: &#U8) -> #Bytes)"),
    U8Op::ToChar => yatima!("(∀ (ω x: &#U8) -> #Char)"),
  }
}
pub fn type_of_u16_op(x: &U16Op) -> Term {
  match x {
    U16Op::Max => yatima!("#U16"),
    U16Op::Min => yatima!("#U16"),
    U16Op::Eql => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #Bool)"),
    U16Op::Lte => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #Bool)"),
    U16Op::Lth => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #Bool)"),
    U16Op::Gth => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #Bool)"),
    U16Op::Gte => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #Bool)"),
    U16Op::Not => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #Bool)"),
    U16Op::And => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #Bool)"),
    U16Op::Or => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #Bool)"),
    U16Op::Xor => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #Bool)"),
    U16Op::Add => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #U16)"),
    U16Op::Sub => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #U16)"),
    U16Op::Mul => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #U16)"),
    U16Op::Div => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #U16)"),
    U16Op::Mod => yatima!("(∀ (ω x: &#U16) (ω y: #U16) -> #U16)"),
    U16Op::Pow => yatima!("(∀ (ω x: &#U16) (ω y: #U32) -> #U16)"),
    U16Op::Shl => yatima!("(∀ (ω x: &#U32) (ω y: #U16) -> #U16)"),
    U16Op::Shr => yatima!("(∀ (ω x: &#U32) (ω y: #U16) -> #U16)"),
    U16Op::Rol => yatima!("(∀ (ω x: &#U32) (ω y: #U16) -> #U16)"),
    U16Op::Ror => yatima!("(∀ (ω x: &#U32) (ω y: #U16) -> #U16)"),
    U16Op::CountZeros => yatima!("(∀ (ω x: &#U16) -> #U32)"),
    U16Op::CountOnes => yatima!("(∀ (ω x: &#U16) -> #U32)"),
    U16Op::ToU8 => yatima!("(∀ (ω x: &#U16) -> #U8)"),
    U16Op::ToU32 => yatima!("(∀ (ω x: &#U16) -> #U32)"),
    U16Op::ToU64 => yatima!("(∀ (ω x: &#U16) -> #U64)"),
    U16Op::ToNat => yatima!("(∀ (ω x: &#U16) -> #Nat)"),
    U16Op::ToI8 => yatima!("(∀ (ω x: &#U16) -> #I8)"),
    U16Op::ToI16 => yatima!("(∀ (ω x: &#U16) -> #I16)"),
    U16Op::ToI32 => yatima!("(∀ (ω x: &#U16) -> #I32)"),
    U16Op::ToI64 => yatima!("(∀ (ω x: &#U16) -> #I64)"),
    U16Op::ToInt => yatima!("(∀ (ω x: &#U16) -> #Int)"),
    U16Op::ToBits => yatima!("(∀ (ω x: &#U8) -> #Bits)"),
    U16Op::ToBytes => yatima!("(∀ (ω x: &#U16) -> #Bytes)"),
  }
}
pub fn type_of_u32_op(x: &U32Op) -> Term {
  match x {
    U32Op::Max => yatima!("#U32"),
    U32Op::Min => yatima!("#U32"),
    U32Op::Eql => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #Bool)"),
    U32Op::Lte => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #Bool)"),
    U32Op::Lth => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #Bool)"),
    U32Op::Gth => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #Bool)"),
    U32Op::Gte => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #Bool)"),
    U32Op::Not => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #Bool)"),
    U32Op::And => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #Bool)"),
    U32Op::Or => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #Bool)"),
    U32Op::Xor => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #Bool)"),
    U32Op::Add => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::Sub => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::Mul => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::Div => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::Mod => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::Pow => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::Shl => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::Shr => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::Rol => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::Ror => yatima!("(∀ (ω x: &#U32) (ω y: #U32) -> #U32)"),
    U32Op::CountZeros => yatima!("(∀ (ω x: &#U32) -> #U32)"),
    U32Op::CountOnes => yatima!("(∀ (ω x: &#U32) -> #U32)"),
    U32Op::ToU8 => yatima!("(∀ (ω x: &#U32) -> #U8)"),
    U32Op::ToU16 => yatima!("(∀ (ω x: &#U32) -> #U16)"),
    U32Op::ToU64 => yatima!("(∀ (ω x: &#U32) -> #U64)"),
    U32Op::ToNat => yatima!("(∀ (ω x: &#U32) -> #Nat)"),
    U32Op::ToI8 => yatima!("(∀ (ω x: &#U32) -> #I8)"),
    U32Op::ToI16 => yatima!("(∀ (ω x: &#U32) -> #I16)"),
    U32Op::ToI32 => yatima!("(∀ (ω x: &#U32) -> #I32)"),
    U32Op::ToI64 => yatima!("(∀ (ω x: &#U32) -> #I64)"),
    U32Op::ToInt => yatima!("(∀ (ω x: &#U32) -> #Int)"),
    U32Op::ToBits => yatima!("(∀ (ω x: &#U32) -> #Bits)"),
    U32Op::ToBytes => yatima!("(∀ (ω x: &#U32) -> #Bytes)"),
    U32Op::ToChar => yatima!("(∀ (ω x: &#U32) -> #Char)"),
  }
}
pub fn type_of_u64_op(x: &U64Op) -> Term {
  match x {
    U64Op::Max => yatima!("#U64"),
    U64Op::Min => yatima!("#U64"),
    U64Op::Eql => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #Bool)"),
    U64Op::Lte => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #Bool)"),
    U64Op::Lth => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #Bool)"),
    U64Op::Gth => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #Bool)"),
    U64Op::Gte => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #Bool)"),
    U64Op::Not => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #Bool)"),
    U64Op::And => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #Bool)"),
    U64Op::Or => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #Bool)"),
    U64Op::Xor => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #Bool)"),
    U64Op::Add => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #U64)"),
    U64Op::Sub => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #U64)"),
    U64Op::Mul => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #U64)"),
    U64Op::Div => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #U64)"),
    U64Op::Mod => yatima!("(∀ (ω x: &#U64) (ω y: #U64) -> #U64)"),
    U64Op::Pow => yatima!("(∀ (ω x: &#U64) (ω y: #U32) -> #U64)"),
    U64Op::Shl => yatima!("(∀ (ω x: &#U32) (ω y: #U64) -> #U64)"),
    U64Op::Shr => yatima!("(∀ (ω x: &#U32) (ω y: #U64) -> #U64)"),
    U64Op::Rol => yatima!("(∀ (ω x: &#U32) (ω y: #U64) -> #U64)"),
    U64Op::Ror => yatima!("(∀ (ω x: &#U32) (ω y: #U64) -> #U64)"),
    U64Op::CountZeros => yatima!("(∀ (ω x: &#U64) -> #U32)"),
    U64Op::CountOnes => yatima!("(∀ (ω x: &#U64) -> #U32)"),
    U64Op::ToU8 => yatima!("(∀ (ω x: &#U64) -> #U8)"),
    U64Op::ToU16 => yatima!("(∀ (ω x: &#U64) -> #U16)"),
    U64Op::ToU32 => yatima!("(∀ (ω x: &#U64) -> #U32)"),
    U64Op::ToNat => yatima!("(∀ (ω x: &#U64) -> #Nat)"),
    U64Op::ToI8 => yatima!("(∀ (ω x: &#U64) -> #I8)"),
    U64Op::ToI16 => yatima!("(∀ (ω x: &#U64) -> #I16)"),
    U64Op::ToI32 => yatima!("(∀ (ω x: &#U64) -> #I32)"),
    U64Op::ToI64 => yatima!("(∀ (ω x: &#U64) -> #I64)"),
    U64Op::ToInt => yatima!("(∀ (ω x: &#U64) -> #Int)"),
    U64Op::ToBits => yatima!("(∀ (ω x: &#U64) -> #Bits)"),
    U64Op::ToBytes => yatima!("(∀ (ω x: &#U64) -> #Bytes)"),
  }
}

pub fn littype_induction(x: &LitType, val: Term) -> Option<Term> {
  match x {
    LitType::Nat => Some(yatima!(
      "∀ (0 P: ∀ (ω _: #Nat) -> Type)
         (& zero: P (0 :: ω #Nat))
         (& succ: ∀ (ω pred: #Nat) -> P (#Nat.suc (pred :: ω #Nat) :: ω #Nat))
       -> P (#$0 :: ω #Nat)
      ",
      val
    )),
    LitType::Int => Some(yatima!(
      "∀ (0 P: ∀ (ω _: #Int) -> Type)
         (& int: ∀ (ω sign: #Bool) (ω abs: #Nat)
         -> P (#Int.new (sign :: ω #Bool) (abs :: ω #Nat) :: ω #Int))
       -> P (#$0 :: ω #Int)
          ",
      val
    )),
    LitType::Bytes => Some(yatima!(
      "∀ (0 P: ∀ (ω _: #Bytes) -> Type)
         (& nil: P (x\'\' :: ω #Bytes))
         (& cons: ∀ (ω x: &#U8) (ω xs: #Bytes)
         -> P (#Bytes.cons (x :: ω #U8) (xs :: ω #Bytes) :: ω #Bytes))
       -> P (#$0 :: ω #Bytes)
          ",
      val
    )),
    LitType::Bits => Some(yatima!(
      "∀ (0 P: ∀ (ω _: #Bits) -> Type)
         (& nil: P (#b :: ω #Bits))
         (& cons: ∀ (ω x: &#Bool) (ω xs: #Bits)
         -> P (#Bits.cons (x :: ω #Bool) (xs :: ω #Bits) :: ω #Bits))
       -> P (#$0 :: ω #Bits)
          ",
      val
    )),
    LitType::Text => Some(yatima!(
      "∀ (0 P: ∀ (ω _: #Text) -> Type)
         (& nil: P (\"\" :: ω #Text))
         (& cons: ∀ (ω x: &#Char) (ω xs: #Text)
         -> P (#Text.cons (x :: ω #Char) (xs :: ω #Text) :: ω #Text))
       -> P (#$0 :: ω #Text)
          ",
      val
    )),
    LitType::Bool => Some(yatima!(
      "∀ (0 P: ∀ (ω _:#Bool) -> Type)
         (& t: P (#Bool.true :: ω #Bool))
         (& f: P (#Bool.false :: ω #Bool))
       -> P (#$0 :: ω #Bool)
          ",
      val
    )),
    _ => None,
  }
}

pub fn literal_expand(x: &Literal) -> Option<Term> {
  match x {
    Literal::Nat(n) => {
      if *n == BigUint::from(0u64) {
        Some(yatima!(
          "λ (ω P: ∀ (ω _: #Nat) -> Type) (ω z: #Nat) (ω s: #Nat) => z"
        ))
      }
      else {
        Some(yatima!(
          "λ (ω P: ∀ (ω _: #Nat) -> Type) (ω z: #Nat) (ω s: #Nat)
            => s (#$0 :: #Nat)",
          Term::Lit(Pos::None, Literal::Nat(n - BigUint::from(1u64)))
        ))
      }
    }
    Literal::Int(_) => {
      Some(yatima!("λ (ω P: ∀ (ω _: #Int) -> Type) (ω i: #Int) => i"))
    }
    // Self::Bits(mut t) => {
    //  let c = t.pop();
    //  match c {
    //    None => Some(yatima!("λ P n c => n")),
    //    Some(c) => Some(yatima!(
    //      "λ P n c => c #$0 #$1",
    //      Term::Lit(Pos::None, Literal::Bool(c)),
    //      Term::Lit(Pos::None, Literal::Bits(t))
    //    )),
    //  }
    //}
    // Self::Bytes(mut t) => {
    //  let c = t.pop();
    //  match c {
    //    None => Some(yatima!("λ P n c => n")),
    //    Some(c) => Some(yatima!(
    //      "λ P n c => c #$0 #$1",
    //      Term::Lit(Pos::None, Literal::U8(c)),
    //      Term::Lit(Pos::None, Literal::Bytes(t))
    //    )),
    //  }
    //}
    // Self::Text(t) => match text::safe_head(t) {
    //  None => Some(yatima!("λ P n c => n")),
    //  Some((c, t)) => Some(yatima!(
    //    "λ P n c => c #$0 #$1",
    //    Term::Lit(Pos::None, Literal::Char(c)),
    //    Term::Lit(Pos::None, Literal::Text(t))
    //  )),
    //},
    // Self::Bool(true) => Some(yatima!("λ P t f => t")),
    // Self::Bool(false) => Some(yatima!("λ P t f => f")),
    _ => None,
  }
}
