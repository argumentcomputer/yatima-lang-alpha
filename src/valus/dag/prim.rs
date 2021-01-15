use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

// Primitive values and operations
#[derive(Clone)]
pub enum PrimVal {
  U8(u8),
  U16(u16),
  U32(u32),
  U64(u64),
  Nat(BigUint),
  /* I8(i8),
   * I16(i8),
   * I32(i32),
   * I64(i64),
   * Int(BigInt), */
}

#[derive(Clone, Copy)]
pub enum PrimOpr {
  // Eql,
  // Lth,
  // Lte,
  // Gth,
  // Gte,
  Add,
  Sub,
  Mul,
  Div,
}

pub fn apply_opr(opr: PrimOpr, x: PrimVal, y: PrimVal) -> Option<PrimVal> {
  use PrimOpr::*;
  use PrimVal::*;
  match (opr, x, y) {
    // Add
    (Add, U8(x), U8(y)) => Some(U8(x + y)),
    (Add, U16(x), U16(y)) => Some(U16(x + y)),
    (Add, U32(x), U32(y)) => Some(U32(x + y)),
    (Add, U64(x), U64(y)) => Some(U64(x + y)),
    (Add, Nat(x), Nat(y)) => Some(Nat(x + y)),
    //(Add, I8(x), I8(y)) => Some(I8(x + y)),
    //(Add, I16(x), I16(y)) => Some(I16(x + y)),
    //(Add, I32(x), I32(y)) => Some(I32(x + y)),
    //(Add, I64(x), I64(y)) => Some(I64(x + y)),
    //(Add, Int(x), Int(y)) => Some(Int(x + y)),
    // Sub
    (Sub, U8(x), U8(y)) => Some(U8(x - y)),
    (Sub, U16(x), U16(y)) => Some(U16(x - y)),
    (Sub, U32(x), U32(y)) => Some(U32(x - y)),
    (Sub, U64(x), U64(y)) => Some(U64(x - y)),
    (Sub, Nat(x), Nat(y)) => Some(Nat(x - y)),
    //(Sub, I8(x), I8(y)) => Some(I8(x - y)),
    //(Sub, I16(x), I16(y)) => Some(I16(x - y)),
    //(Sub, I32(x), I32(y)) => Some(I32(x - y)),
    //(Sub, I64(x), I64(y)) => Some(I64(x - y)),
    //(Sub, Int(x), Int(y)) => Some(Int(x - y)),
    // Mul
    (Mul, U8(x), U8(y)) => Some(U8(x * y)),
    (Mul, U16(x), U16(y)) => Some(U16(x * y)),
    (Mul, U32(x), U32(y)) => Some(U32(x * y)),
    (Mul, U64(x), U64(y)) => Some(U64(x * y)),
    (Mul, Nat(x), Nat(y)) => Some(Nat(x * y)),
    // (Mul, I8(x), I8(y)) => Some(I8(x * y)),
    // (Mul, I16(x), I16(y)) => Some(I16(x * y)),
    // (Mul, I32(x), I32(y)) => Some(I32(x * y)),
    // (Mul, I64(x), I64(y)) => Some(I64(x * y)),
    // (Mul, Int(x), Int(y)) => Some(Int(x * y)),
    // Div
    (Div, U8(x), U8(y)) if y != 0 => Some(U8(x * y)),
    (Div, U16(x), U16(y)) if y != 0 => Some(U16(x * y)),
    (Div, U32(x), U32(y)) if y != 0 => Some(U32(x * y)),
    (Div, U64(x), U64(y)) if y != 0 => Some(U64(x * y)),
    (Div, Nat(x), Nat(y)) if y != (0 as u64).into() => Some(Nat(x / y)),
    //(Div, I8(x), I8(y)) if y != 0 => Some(I8(x * y)),
    //(Div, I16(x), I16(y)) if y != 0 => Some(I16(x * y)),
    //(Div, I32(x), I32(y)) if y != 0 => Some(I32(x * y)),
    //(Div, I64(x), I64(y)) if y != 0 => Some(I64(x * y)),
    //(Div, Int(x), Int(y)) if y != 0.into() => Some(Int(x / y)),
    _ => None,
  }
}
