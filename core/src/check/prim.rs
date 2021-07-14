// pub fn type_of_opr(self) -> Term {
//  match self {
//    Self::Nat(op) => op.type_of(),
//    Self::Int(op) => op.type_of(),
//    Self::Bits(op) => op.type_of(),
//    Self::Bytes(op) => op.type_of(),
//    Self::Text(op) => op.type_of(),
//    Self::Char(op) => op.type_of(),
//    Self::Bool(op) => op.type_of(),
//    Self::U8(op) => op.type_of(),
//    Self::U16(op) => op.type_of(),
//    Self::U32(op) => op.type_of(),
//    Self::U64(op) => op.type_of(),
//    // Self::U128(op) => op.type_of(),
//    Self::I8(op) => op.type_of(),
//    Self::I16(op) => op.type_of(),
//    Self::I32(op) => op.type_of(),
//    Self::I64(op) => op.type_of(),
//    // Self::I128(op) => op.type_of(),
//  }
//}
//

//  pub fn type_of_bits_opr(self) -> Term {
//    match self {
//      Self::Cons => yatima!("∀ #Bool #Bits -> #Bits"),
//      Self::Len => yatima!("∀ #Bits -> #Nat"),
//      Self::Head => yatima!("∀ #Bits -> #U8"),
//      Self::Tail => yatima!("∀ #Bits -> #Bits"),
//      Self::Take => yatima!("∀ #Nat #Bits -> #Bits"),
//      Self::Drop => yatima!("∀ #Nat #Bits -> #Bits"),
//      Self::Append => yatima!("∀ #Bits #Bits -> #Bits"),
//      Self::Insert => yatima!("∀ #Bool #Nat #Bits -> #Bits"),
//      Self::Remove => yatima!("∀ #Nat #Bits -> #Bits"),
//      Self::Index => yatima!("∀ #Nat #Bits -> #U8"),
//      Self::ToBytes => yatima!("∀ #Bits -> #Bytes"),
//    }
//  }

//  pub fn type_of_bool_opr(self) -> Term {
//    match self {
//      Self::Eql => yatima!("∀ #Bool #Bool -> #Bool"),
//      Self::Lte => yatima!("∀ #Bool #Bool -> #Bool"),
//      Self::Lth => yatima!("∀ #Bool #Bool -> #Bool"),
//      Self::Gte => yatima!("∀ #Bool #Bool -> #Bool"),
//      Self::Gth => yatima!("∀ #Bool #Bool -> #Bool"),
//      Self::And => yatima!("∀ #Bool #Bool -> #Bool"),
//      Self::Or => yatima!("∀ #Bool #Bool -> #Bool"),
//      Self::Xor => yatima!("∀ #Bool #Bool -> #Bool"),
//      Self::Not => yatima!("∀ #Bool -> #Bool"),
//    }
//  }
//

pub fn type_of_bytes_opr(self) -> Term {
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

pub fn type_of_char_opr(self) -> Term {
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

pub fn type_of_i8_opr(self) -> Term {
  match self {
    Self::Abs => yatima!("∀ #I8 -> #U8"),
    Self::Sgn => yatima!("∀ #I8 -> #Bool"),
    Self::Max => yatima!("#I8"),
    Self::Min => yatima!("#I8"),
    Self::Eql => yatima!("∀ #I8 #I8 -> #Bool"),
    Self::Lte => yatima!("∀ #I8 #I8 -> #Bool"),
    Self::Lth => yatima!("∀ #I8 #I8 -> #Bool"),
    Self::Gth => yatima!("∀ #I8 #I8 -> #Bool"),
    Self::Gte => yatima!("∀ #I8 #I8 -> #Bool"),
    Self::Not => yatima!("∀ #I8 #I8 -> #Bool"),
    Self::And => yatima!("∀ #I8 #I8 -> #Bool"),
    Self::Or => yatima!("∀ #I8 #I8 -> #Bool"),
    Self::Xor => yatima!("∀ #I8 #I8 -> #Bool"),
    Self::Add => yatima!("∀ #I8 #I8 -> #I8"),
    Self::Sub => yatima!("∀ #I8 #I8 -> #I8"),
    Self::Mul => yatima!("∀ #I8 #I8 -> #I8"),
    Self::Div => yatima!("∀ #I8 #I8 -> #I8"),
    Self::Mod => yatima!("∀ #I8 #I8 -> #I8"),
    Self::Pow => yatima!("∀ #I8 #U32 -> #I8"),
    Self::Shl => yatima!("∀ #U32 #I8 -> #I8"),
    Self::Shr => yatima!("∀ #U32 #I8 -> #I8"),
    Self::Rol => yatima!("∀ #U32 #I8 -> #I8"),
    Self::Ror => yatima!("∀ #U32 #I8 -> #I8"),
    Self::CountZeros => yatima!("∀ #I8 -> #U32"),
    Self::CountOnes => yatima!("∀ #I8 -> #U32"),
    Self::ToU8 => yatima!("∀ #I8 -> #U8"),
    Self::ToU16 => yatima!("∀ #I8 -> #U16"),
    Self::ToU32 => yatima!("∀ #I8 -> #U32"),
    Self::ToU64 => yatima!("∀ #I8 -> #U64"),
    Self::ToU128 => yatima!("∀ #I8 -> #U128"),
    Self::ToNat => yatima!("∀ #I8 -> #Nat"),
    Self::ToI16 => yatima!("∀ #I8 -> #I16"),
    Self::ToI32 => yatima!("∀ #I8 -> #I32"),
    Self::ToI64 => yatima!("∀ #I8 -> #I64"),
    Self::ToI128 => yatima!("∀ #I8 -> #I128"),
    Self::ToInt => yatima!("∀ #I8 -> #Int"),
    Self::ToBits => yatima!("∀ #I8 -> #Bits"),
    Self::ToBytes => yatima!("∀ #I8 -> #Bytes"),
  }
}

pub fn type_of_i16_opr(self) -> Term {
  match self {
    Self::Abs => yatima!("∀ #I16 -> #U16"),
    Self::Sgn => yatima!("∀ #I16 -> #Bool"),
    Self::Max => yatima!("#I16"),
    Self::Min => yatima!("#I16"),
    Self::Eql => yatima!("∀ #I16 #I16 -> #Bool"),
    Self::Lte => yatima!("∀ #I16 #I16 -> #Bool"),
    Self::Lth => yatima!("∀ #I16 #I16 -> #Bool"),
    Self::Gth => yatima!("∀ #I16 #I16 -> #Bool"),
    Self::Gte => yatima!("∀ #I16 #I16 -> #Bool"),
    Self::Not => yatima!("∀ #I16 #I16 -> #Bool"),
    Self::And => yatima!("∀ #I16 #I16 -> #Bool"),
    Self::Or => yatima!("∀ #I16 #I16 -> #Bool"),
    Self::Xor => yatima!("∀ #I16 #I16 -> #Bool"),
    Self::Add => yatima!("∀ #I16 #I16 -> #I16"),
    Self::Sub => yatima!("∀ #I16 #I16 -> #I16"),
    Self::Mul => yatima!("∀ #I16 #I16 -> #I16"),
    Self::Div => yatima!("∀ #I16 #I16 -> #I16"),
    Self::Mod => yatima!("∀ #I16 #I16 -> #I16"),
    Self::Pow => yatima!("∀ #I16 #U32 -> #I16"),
    Self::Shl => yatima!("∀ #U32 #I16 -> #I16"),
    Self::Shr => yatima!("∀ #U32 #I16 -> #I16"),
    Self::Rol => yatima!("∀ #U32 #I16 -> #I16"),
    Self::Ror => yatima!("∀ #U32 #I16 -> #I16"),
    Self::CountZeros => yatima!("∀ #I16 -> #U32"),
    Self::CountOnes => yatima!("∀ #I16 -> #U32"),
    Self::ToU8 => yatima!("∀ #I16 -> #U8"),
    Self::ToU16 => yatima!("∀ #I16 -> #U16"),
    Self::ToU32 => yatima!("∀ #I16 -> #U32"),
    Self::ToU64 => yatima!("∀ #I16 -> #U64"),
    Self::ToU128 => yatima!("∀ #I16 -> #U128"),
    Self::ToNat => yatima!("∀ #I16 -> #Nat"),
    Self::ToI8 => yatima!("∀ #I16 -> #I8"),
    Self::ToI32 => yatima!("∀ #I16 -> #I32"),
    Self::ToI64 => yatima!("∀ #I16 -> #I64"),
    Self::ToI128 => yatima!("∀ #I16 -> #I128"),
    Self::ToInt => yatima!("∀ #I16 -> #Int"),
    Self::ToBits => yatima!("∀ #I16 -> #Bits"),
    Self::ToBytes => yatima!("∀ #I16 -> #Bytes"),
  }
}

pub fn type_of_i32_opr(self) -> Term {
  match self {
    Self::Abs => yatima!("∀ #I32 -> #U32"),
    Self::Sgn => yatima!("∀ #I32 -> #Bool"),
    Self::Max => yatima!("#I32"),
    Self::Min => yatima!("#I32"),
    Self::Eql => yatima!("∀ #I32 #I32 -> #Bool"),
    Self::Lte => yatima!("∀ #I32 #I32 -> #Bool"),
    Self::Lth => yatima!("∀ #I32 #I32 -> #Bool"),
    Self::Gth => yatima!("∀ #I32 #I32 -> #Bool"),
    Self::Gte => yatima!("∀ #I32 #I32 -> #Bool"),
    Self::Not => yatima!("∀ #I32 #I32 -> #Bool"),
    Self::And => yatima!("∀ #I32 #I32 -> #Bool"),
    Self::Or => yatima!("∀ #I32 #I32 -> #Bool"),
    Self::Xor => yatima!("∀ #I32 #I32 -> #Bool"),
    Self::Add => yatima!("∀ #I32 #I32 -> #I32"),
    Self::Sub => yatima!("∀ #I32 #I32 -> #I32"),
    Self::Mul => yatima!("∀ #I32 #I32 -> #I32"),
    Self::Div => yatima!("∀ #I32 #I32 -> #I32"),
    Self::Mod => yatima!("∀ #I32 #I32 -> #I32"),
    Self::Pow => yatima!("∀ #I32 #U32 -> #I32"),
    Self::Shl => yatima!("∀ #U32 #I32 -> #I32"),
    Self::Shr => yatima!("∀ #U32 #I32 -> #I32"),
    Self::Rol => yatima!("∀ #U32 #I32 -> #I32"),
    Self::Ror => yatima!("∀ #U32 #I32 -> #I32"),
    Self::CountZeros => yatima!("∀ #I32 -> #U32"),
    Self::CountOnes => yatima!("∀ #I32 -> #U32"),
    Self::ToU8 => yatima!("∀ #I32 -> #U8"),
    Self::ToU16 => yatima!("∀ #I32 -> #U16"),
    Self::ToU32 => yatima!("∀ #I32 -> #U32"),
    Self::ToU64 => yatima!("∀ #I32 -> #U64"),
    Self::ToU128 => yatima!("∀ #I32 -> #U128"),
    Self::ToNat => yatima!("∀ #I32 -> #Nat"),
    Self::ToI8 => yatima!("∀ #I32 -> #I8"),
    Self::ToI16 => yatima!("∀ #I32 -> #I16"),
    Self::ToI64 => yatima!("∀ #I32 -> #I64"),
    Self::ToI128 => yatima!("∀ #I32 -> #I128"),
    Self::ToInt => yatima!("∀ #I32 -> #Int"),
    Self::ToBytes => yatima!("∀ #I32 -> #Bytes"),
    Self::ToBits => yatima!("∀ #I32 -> #Bits"),
  }
}

pub fn type_of_i64_opr(self) -> Term {
  match self {
    Self::Abs => yatima!("∀ #I64 -> #U64"),
    Self::Sgn => yatima!("∀ #I64 -> #Bool"),
    Self::Max => yatima!("#I64"),
    Self::Min => yatima!("#I64"),
    Self::Eql => yatima!("∀ #I64 #I64 -> #Bool"),
    Self::Lte => yatima!("∀ #I64 #I64 -> #Bool"),
    Self::Lth => yatima!("∀ #I64 #I64 -> #Bool"),
    Self::Gth => yatima!("∀ #I64 #I64 -> #Bool"),
    Self::Gte => yatima!("∀ #I64 #I64 -> #Bool"),
    Self::Not => yatima!("∀ #I64 #I64 -> #Bool"),
    Self::And => yatima!("∀ #I64 #I64 -> #Bool"),
    Self::Or => yatima!("∀ #I64 #I64 -> #Bool"),
    Self::Xor => yatima!("∀ #I64 #I64 -> #Bool"),
    Self::Add => yatima!("∀ #I64 #I64 -> #I64"),
    Self::Sub => yatima!("∀ #I64 #I64 -> #I64"),
    Self::Mul => yatima!("∀ #I64 #I64 -> #I64"),
    Self::Div => yatima!("∀ #I64 #I64 -> #I64"),
    Self::Mod => yatima!("∀ #I64 #I64 -> #I64"),
    Self::Pow => yatima!("∀ #I64 #U32 -> #I64"),
    Self::Shl => yatima!("∀ #U32 #I64 -> #I64"),
    Self::Shr => yatima!("∀ #U32 #I64 -> #I64"),
    Self::Rol => yatima!("∀ #U32 #I64 -> #I64"),
    Self::Ror => yatima!("∀ #U32 #I64 -> #I64"),
    Self::CountZeros => yatima!("∀ #I64 -> #U32"),
    Self::CountOnes => yatima!("∀ #I64 -> #U32"),
    Self::ToU8 => yatima!("∀ #I64 -> #U8"),
    Self::ToU16 => yatima!("∀ #I64 -> #U16"),
    Self::ToU32 => yatima!("∀ #I64 -> #U32"),
    Self::ToU64 => yatima!("∀ #I64 -> #U64"),
    Self::ToU128 => yatima!("∀ #I64 -> #U128"),
    Self::ToNat => yatima!("∀ #I64 -> #Nat"),
    Self::ToI8 => yatima!("∀ #I64 -> #I8"),
    Self::ToI16 => yatima!("∀ #I64 -> #I16"),
    Self::ToI32 => yatima!("∀ #I64 -> #I32"),
    Self::ToI128 => yatima!("∀ #I64 -> #I128"),
    Self::ToInt => yatima!("∀ #I64 -> #Int"),
    Self::ToBytes => yatima!("∀ #I64 -> #Bytes"),
    Self::ToBits => yatima!("∀ #I64 -> #Bits"),
  }
}

pub fn type_of_int_opr(self) -> Term {
  match self {
    Self::New => yatima!("∀ #Bool #Nat -> #Int"),
    Self::Sgn => yatima!("∀ #Int -> #Bool"),
    Self::Abs => yatima!("∀ #Int -> #Nat"),
    Self::Eql => yatima!("∀ #Int #Int -> #Bool"),
    Self::Lte => yatima!("∀ #Int #Int -> #Bool"),
    Self::Lth => yatima!("∀ #Int #Int -> #Bool"),
    Self::Gte => yatima!("∀ #Int #Int -> #Bool"),
    Self::Gth => yatima!("∀ #Int #Int -> #Bool"),
    Self::Add => yatima!("∀ #Int #Int -> #Int"),
    Self::Sub => yatima!("∀ #Int #Int -> #Int"),
    Self::Mul => yatima!("∀ #Int #Int -> #Int"),
    Self::Div => yatima!("∀ #Int #Int -> #Int"),
    Self::Mod => yatima!("∀ #Int #Int -> #Int"),
  }
}

pub fn type_of_nat_opr(self) -> Term {
  match self {
    Self::Suc => yatima!("∀ #Nat -> #Nat"),
    Self::Pre => yatima!("∀ #Nat -> #Nat"),
    Self::Eql => yatima!("∀ #Nat #Nat -> #Bool"),
    Self::Lte => yatima!("∀ #Nat #Nat -> #Bool"),
    Self::Lth => yatima!("∀ #Nat #Nat -> #Bool"),
    Self::Gte => yatima!("∀ #Nat #Nat -> #Bool"),
    Self::Gth => yatima!("∀ #Nat #Nat -> #Bool"),
    Self::Add => yatima!("∀ #Nat #Nat -> #Nat"),
    Self::Sub => yatima!("∀ #Nat #Nat -> #Nat"),
    Self::Mul => yatima!("∀ #Nat #Nat -> #Nat"),
    Self::Div => yatima!("∀ #Nat #Nat -> #Nat"),
    Self::Mod => yatima!("∀ #Nat #Nat -> #Nat"),
  }
}

pub fn type_of_u8_opr(self) -> Term {
  match self {
    Self::Max => yatima!("#U8"),
    Self::Min => yatima!("#U8"),
    Self::Eql => yatima!("∀ #U8 #U8 -> #Bool"),
    Self::Lte => yatima!("∀ #U8 #U8 -> #Bool"),
    Self::Lth => yatima!("∀ #U8 #U8 -> #Bool"),
    Self::Gth => yatima!("∀ #U8 #U8 -> #Bool"),
    Self::Gte => yatima!("∀ #U8 #U8 -> #Bool"),
    Self::Not => yatima!("∀ #U8 #U8 -> #Bool"),
    Self::And => yatima!("∀ #U8 #U8 -> #Bool"),
    Self::Or => yatima!("∀ #U8 #U8 -> #Bool"),
    Self::Xor => yatima!("∀ #U8 #U8 -> #Bool"),
    Self::Add => yatima!("∀ #U8 #U8 -> #U8"),
    Self::Sub => yatima!("∀ #U8 #U8 -> #U8"),
    Self::Mul => yatima!("∀ #U8 #U8 -> #U8"),
    Self::Div => yatima!("∀ #U8 #U8 -> #U8"),
    Self::Mod => yatima!("∀ #U8 #U8 -> #U8"),
    Self::Pow => yatima!("∀ #U8 #U32 -> #U8"),
    Self::Shl => yatima!("∀ #U32 #U8 -> #U8"),
    Self::Shr => yatima!("∀ #U32 #U8 -> #U8"),
    Self::Rol => yatima!("∀ #U32 #U8 -> #U8"),
    Self::Ror => yatima!("∀ #U32 #U8 -> #U8"),
    Self::CountZeros => yatima!("∀ #U8 -> #U32"),
    Self::CountOnes => yatima!("∀ #U8 -> #U32"),
    Self::ToU16 => yatima!("∀ #U8 -> #U16"),
    Self::ToU32 => yatima!("∀ #U8 -> #U32"),
    Self::ToU64 => yatima!("∀ #U8 -> #U64"),
    Self::ToU128 => yatima!("∀ #U8 -> #U128"),
    Self::ToNat => yatima!("∀ #U8 -> #Nat"),
    Self::ToI8 => yatima!("∀ #U8 -> #I8"),
    Self::ToI16 => yatima!("∀ #U8 -> #I16"),
    Self::ToI32 => yatima!("∀ #U8 -> #I32"),
    Self::ToI64 => yatima!("∀ #U8 -> #I64"),
    Self::ToI128 => yatima!("∀ #U8 -> #I128"),
    Self::ToInt => yatima!("∀ #U8 -> #Int"),
    Self::ToBits => yatima!("∀ #U8 -> #Bits"),
    Self::ToBytes => yatima!("∀ #U8 -> #Bytes"),
    Self::ToChar => yatima!("∀ #U8 -> #Char"),
  }
}
pub fn type_of_text_opr(self) -> Term {
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

pub fn type_of_u16_opr(self) -> Term {
  match self {
    Self::Max => yatima!("#U16"),
    Self::Min => yatima!("#U16"),
    Self::Eql => yatima!("∀ #U16 #U16 -> #Bool"),
    Self::Lte => yatima!("∀ #U16 #U16 -> #Bool"),
    Self::Lth => yatima!("∀ #U16 #U16 -> #Bool"),
    Self::Gth => yatima!("∀ #U16 #U16 -> #Bool"),
    Self::Gte => yatima!("∀ #U16 #U16 -> #Bool"),
    Self::Not => yatima!("∀ #U16 #U16 -> #Bool"),
    Self::And => yatima!("∀ #U16 #U16 -> #Bool"),
    Self::Or => yatima!("∀ #U16 #U16 -> #Bool"),
    Self::Xor => yatima!("∀ #U16 #U16 -> #Bool"),
    Self::Add => yatima!("∀ #U16 #U16 -> #U16"),
    Self::Sub => yatima!("∀ #U16 #U16 -> #U16"),
    Self::Mul => yatima!("∀ #U16 #U16 -> #U16"),
    Self::Div => yatima!("∀ #U16 #U16 -> #U16"),
    Self::Mod => yatima!("∀ #U16 #U16 -> #U16"),
    Self::Pow => yatima!("∀ #U16 #U32 -> #U16"),
    Self::Shl => yatima!("∀ #U32 #U16 -> #U16"),
    Self::Shr => yatima!("∀ #U32 #U16 -> #U16"),
    Self::Rol => yatima!("∀ #U32 #U16 -> #U16"),
    Self::Ror => yatima!("∀ #U32 #U16 -> #U16"),
    Self::CountZeros => yatima!("∀ #U16 -> #U32"),
    Self::CountOnes => yatima!("∀ #U16 -> #U32"),
    Self::ToU8 => yatima!("∀ #U16 -> #U8"),
    Self::ToU32 => yatima!("∀ #U16 -> #U32"),
    Self::ToU64 => yatima!("∀ #U16 -> #U64"),
    Self::ToU128 => yatima!("∀ #U16 -> #U128"),
    Self::ToNat => yatima!("∀ #U16 -> #Nat"),
    Self::ToI8 => yatima!("∀ #U16 -> #I8"),
    Self::ToI16 => yatima!("∀ #U16 -> #I16"),
    Self::ToI32 => yatima!("∀ #U16 -> #I32"),
    Self::ToI64 => yatima!("∀ #U16 -> #I64"),
    Self::ToI128 => yatima!("∀ #U16 -> #I128"),
    Self::ToInt => yatima!("∀ #U16 -> #Int"),
    Self::ToBits => yatima!("∀ #U8 -> #Bits"),
    Self::ToBytes => yatima!("∀ #U16 -> #Bytes"),
  }
}

pub fn type_of_u32_opr(self) -> Term {
  match self {
    Self::Max => yatima!("#U32"),
    Self::Min => yatima!("#U32"),
    Self::Eql => yatima!("∀ #U32 #U32 -> #Bool"),
    Self::Lte => yatima!("∀ #U32 #U32 -> #Bool"),
    Self::Lth => yatima!("∀ #U32 #U32 -> #Bool"),
    Self::Gth => yatima!("∀ #U32 #U32 -> #Bool"),
    Self::Gte => yatima!("∀ #U32 #U32 -> #Bool"),
    Self::Not => yatima!("∀ #U32 #U32 -> #Bool"),
    Self::And => yatima!("∀ #U32 #U32 -> #Bool"),
    Self::Or => yatima!("∀ #U32 #U32 -> #Bool"),
    Self::Xor => yatima!("∀ #U32 #U32 -> #Bool"),
    Self::Add => yatima!("∀ #U32 #U32 -> #U32"),
    Self::Sub => yatima!("∀ #U32 #U32 -> #U32"),
    Self::Mul => yatima!("∀ #U32 #U32 -> #U32"),
    Self::Div => yatima!("∀ #U32 #U32 -> #U32"),
    Self::Mod => yatima!("∀ #U32 #U32 -> #U32"),
    Self::Pow => yatima!("∀ #U32 #U32 -> #U32"),
    Self::Shl => yatima!("∀ #U32 #U32 -> #U32"),
    Self::Shr => yatima!("∀ #U32 #U32 -> #U32"),
    Self::Rol => yatima!("∀ #U32 #U32 -> #U32"),
    Self::Ror => yatima!("∀ #U32 #U32 -> #U32"),
    Self::CountZeros => yatima!("∀ #U32 -> #U32"),
    Self::CountOnes => yatima!("∀ #U32 -> #U32"),
    Self::ToU8 => yatima!("∀ #U32 -> #U8"),
    Self::ToU16 => yatima!("∀ #U32 -> #U16"),
    Self::ToU64 => yatima!("∀ #U32 -> #U64"),
    Self::ToU128 => yatima!("∀ #U32 -> #U128"),
    Self::ToNat => yatima!("∀ #U32 -> #Nat"),
    Self::ToI8 => yatima!("∀ #U32 -> #I8"),
    Self::ToI16 => yatima!("∀ #U32 -> #I16"),
    Self::ToI32 => yatima!("∀ #U32 -> #I32"),
    Self::ToI64 => yatima!("∀ #U32 -> #I64"),
    Self::ToI128 => yatima!("∀ #U32 -> #I128"),
    Self::ToInt => yatima!("∀ #U32 -> #Int"),
    Self::ToBits => yatima!("∀ #U32 -> #Bits"),
    Self::ToBytes => yatima!("∀ #U32 -> #Bytes"),
    Self::ToChar => yatima!("∀ #U32 -> #Char"),
  }
}

pub fn type_of_u64_opr(self) -> Term {
  match self {
    Self::Max => yatima!("#U64"),
    Self::Min => yatima!("#U64"),
    Self::Eql => yatima!("∀ #U64 #U64 -> #Bool"),
    Self::Lte => yatima!("∀ #U64 #U64 -> #Bool"),
    Self::Lth => yatima!("∀ #U64 #U64 -> #Bool"),
    Self::Gth => yatima!("∀ #U64 #U64 -> #Bool"),
    Self::Gte => yatima!("∀ #U64 #U64 -> #Bool"),
    Self::Not => yatima!("∀ #U64 #U64 -> #Bool"),
    Self::And => yatima!("∀ #U64 #U64 -> #Bool"),
    Self::Or => yatima!("∀ #U64 #U64 -> #Bool"),
    Self::Xor => yatima!("∀ #U64 #U64 -> #Bool"),
    Self::Add => yatima!("∀ #U64 #U64 -> #U64"),
    Self::Sub => yatima!("∀ #U64 #U64 -> #U64"),
    Self::Mul => yatima!("∀ #U64 #U64 -> #U64"),
    Self::Div => yatima!("∀ #U64 #U64 -> #U64"),
    Self::Mod => yatima!("∀ #U64 #U64 -> #U64"),
    Self::Pow => yatima!("∀ #U64 #U32 -> #U64"),
    Self::Shl => yatima!("∀ #U32 #U64 -> #U64"),
    Self::Shr => yatima!("∀ #U32 #U64 -> #U64"),
    Self::Rol => yatima!("∀ #U32 #U64 -> #U64"),
    Self::Ror => yatima!("∀ #U32 #U64 -> #U64"),
    Self::CountZeros => yatima!("∀ #U64 -> #U32"),
    Self::CountOnes => yatima!("∀ #U64 -> #U32"),
    Self::ToU8 => yatima!("∀ #U64 -> #U8"),
    Self::ToU16 => yatima!("∀ #U64 -> #U16"),
    Self::ToU32 => yatima!("∀ #U64 -> #U32"),
    Self::ToU128 => yatima!("∀ #U64 -> #U128"),
    Self::ToNat => yatima!("∀ #U64 -> #Nat"),
    Self::ToI8 => yatima!("∀ #U64 -> #I8"),
    Self::ToI16 => yatima!("∀ #U64 -> #I16"),
    Self::ToI32 => yatima!("∀ #U64 -> #I32"),
    Self::ToI64 => yatima!("∀ #U64 -> #I64"),
    Self::ToI128 => yatima!("∀ #U64 -> #I128"),
    Self::ToInt => yatima!("∀ #U64 -> #Int"),
    Self::ToBits => yatima!("∀ #U64 -> #Bits"),
    Self::ToBytes => yatima!("∀ #U64 -> #Bytes"),
  }
}
pub fn littype_induction(self, val: Term) -> Option<Term> {
  match self {
    Self::Nat => Some(yatima!(
      "∀ (0 P: ∀ #Nat -> Type)
             (& zero: P 0)
             (& succ: ∀ (pred: #Nat) -> P (#Nat.suc pred))
           -> P #$0
          ",
      val
    )),
    Self::Int => Some(yatima!(
      "∀ (0 P: ∀ #Int -> Type)
             (& int: ∀ (sign: #Bool) (abs: #Nat) -> P (#Int.new sign abs))
           -> P #$0
          ",
      val
    )),
    Self::Bytes => Some(yatima!(
      "∀ (0 P: ∀ #Bytes -> Type)
             (& nil: P \"\")
             (& cons: ∀ (x: #U8) (xs: #Bytes) -> P (#Bytes.cons x xs))
           -> P #$0
          ",
      val
    )),
    Self::Bits => Some(yatima!(
      "∀ (0 P: ∀ #Bits -> Type)
             (& nil: P #b)
             (& cons: ∀ (x: #Bool) (xs: #Bits) -> P (#Bits.cons x xs))
           -> P #$0
          ",
      val
    )),
    Self::Text => Some(yatima!(
      "∀ (0 P: ∀ #Text -> Type)
             (& nil: P \"\")
             (& cons: ∀ (x: #Char) (xs: #Text) -> P (#Text.cons x xs))
           -> P #$0
          ",
      val
    )),
    Self::Bool => Some(yatima!(
      "∀ (0 P: ∀ #Bool -> Type)
             (& t: P #Bool.true)
             (& f: P #Bool.false)
           -> P #$0
          ",
      val
    )),
    _ => None,
  }
}

pub fn literal_expand(self) -> Option<Term> {
  match self {
    Self::Nat(n) => {
      if n == BigUint::from(0u64) {
        Some(yatima!("λ P z s => z"))
      }
      else {
        Some(yatima!(
          "λ P z s => s #$0",
          Term::Lit(Pos::None, Literal::Nat(n - BigUint::from(1u64)))
        ))
      }
    }
    Self::Int(_) => Some(yatima!("λ P i => i")),
    Self::Bits(mut t) => {
      let c = t.pop();
      match c {
        None => Some(yatima!("λ P n c => n")),
        Some(c) => Some(yatima!(
          "λ P n c => c #$0 #$1",
          Term::Lit(Pos::None, Literal::Bool(c)),
          Term::Lit(Pos::None, Literal::Bits(t))
        )),
      }
    }
    Self::Bytes(mut t) => {
      let c = t.pop();
      match c {
        None => Some(yatima!("λ P n c => n")),
        Some(c) => Some(yatima!(
          "λ P n c => c #$0 #$1",
          Term::Lit(Pos::None, Literal::U8(c)),
          Term::Lit(Pos::None, Literal::Bytes(t))
        )),
      }
    }
    Self::Text(t) => match text::safe_head(t) {
      None => Some(yatima!("λ P n c => n")),
      Some((c, t)) => Some(yatima!(
        "λ P n c => c #$0 #$1",
        Term::Lit(Pos::None, Literal::Char(c)),
        Term::Lit(Pos::None, Literal::Text(t))
      )),
    },
    Self::Bool(true) => Some(yatima!("λ P t f => t")),
    Self::Bool(false) => Some(yatima!("λ P t f => f")),
    _ => None,
  }
}
