use bit_vec::BitVec;
use std::convert::TryInto;

use num_bigint::{BigInt, BigUint};

use crate::core::literal::Literal;

use crate::decode_error::{DecodeError, Expected};
use hashexpr::{atom::Atom::*, Expr, Expr::Atom};
use std::fmt;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum PrimOp {
    /// equality
    Eql,
    /// less-than
    Lth,
    /// less-than-or-equal
    Lte, //
    /// greater-than
    Gth,
    /// greater-than-or-equal
    Gte,
    /// bitwise or
    Bor,
    /// bitwise and
    And,
    /// bitwise xor
    Xor,
    /// bitwise negation
    Not,
    /// successor, increment
    Suc,
    /// predecessor, decrement
    Pre,
    /// addition
    Add,
    /// subtraction
    Sub,
    /// multiplication
    Mul,
    /// division
    Div,
    /// modulus
    Mod,
    /// shift left
    Shl,
    /// shift right
    Shr,
    /// length
    Len,
    /// concatenate
    Cat,
    // Cst
}

impl PrimOp {
    #[must_use]
    pub fn symbol(self) -> String {
        match self {
            Self::Eql => String::from("#eql"),
            Self::Lth => String::from("#lth"),
            Self::Lte => String::from("#lte"),
            Self::Gth => String::from("#gth"),
            Self::Gte => String::from("#gte"),
            Self::Bor => String::from("#bor"),
            Self::And => String::from("#and"),
            Self::Xor => String::from("#xor"),
            Self::Not => String::from("#not"),
            Self::Suc => String::from("#suc"),
            Self::Pre => String::from("#pre"),
            Self::Add => String::from("#add"),
            Self::Sub => String::from("#sub"),
            Self::Mul => String::from("#mul"),
            Self::Div => String::from("#div"),
            Self::Mod => String::from("#mod"),
            Self::Shl => String::from("#shl"),
            Self::Shr => String::from("#shr"),
            Self::Len => String::from("#len"),
            Self::Cat => String::from("#cat"),
            // Self::Cst => String::from("#cst"),
        }
    }

    #[must_use]
    pub fn from_symbol(s: String) -> Option<Self> {
        match s.as_str() {
            "#eql" => Some(Self::Eql),
            "#lth" => Some(Self::Lth),
            "#lte" => Some(Self::Lte),
            "#gth" => Some(Self::Gth),
            "#gte" => Some(Self::Gte),
            "#bor" => Some(Self::Bor),
            "#and" => Some(Self::And),
            "#xor" => Some(Self::Xor),
            "#not" => Some(Self::Not),
            "#suc" => Some(Self::Suc),
            "#pre" => Some(Self::Pre),
            "#add" => Some(Self::Add),
            "#sub" => Some(Self::Sub),
            "#mul" => Some(Self::Mul),
            "#div" => Some(Self::Div),
            "#mod" => Some(Self::Mod),
            "#shl" => Some(Self::Shl),
            "#shr" => Some(Self::Shr),
            "#len" => Some(Self::Len),
            "#cat" => Some(Self::Cat),
            //"#cst" => Some(Self::Cst),
            _ => None,
        }
    }

    #[must_use]
    pub const fn arity(self) -> u64 {
        match self {
            Self::Not | Self::Len | Self::Suc | Self::Pre => 1,
            _ => 2,
        }
    }

    #[must_use]
    pub fn encode(self) -> Expr {
        Atom(None, Text(self.symbol()))
    }

    pub fn decode(x: Expr) -> Result<Self, DecodeError> {
        let err = |pos| DecodeError::new(pos, vec![Expected::PrimOp]);
        match x {
            Atom(pos, Text(n)) => Self::from_symbol(n).ok_or_else(|| err(pos)),
            x => Err(err(x.position())),
        }
    }
}

impl fmt::Display for PrimOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", (*self).symbol())
    }
}

#[must_use]
pub fn apply_una_op(opr: PrimOp, x: Literal) -> Option<Literal> {
    use Literal::*;
    use PrimOp::*;
    let one: BigUint = 1_u64.into();
    let one_i: BigInt = 1_u64.into();
    match (opr, x) {
        (Not, BitString(x)) => {
            let mut bs = BitVec::from_bytes(&x);
            bs.negate();
            Some(BitString(bs.to_bytes()))
        }
        (Suc, Natural(x)) => Some(Natural(x + one)),
        (Suc, Integer(x)) => Some(Integer(x + one_i)),
        (Pre, Natural(x)) if x != 0_u64.into() => Some(Natural(x - one)),
        (Pre, Integer(x)) => Some(Integer(x - one_i)),
        (Len, Natural(x)) => Some(Natural(x.bits().into())),
        (Len, Integer(x)) => Some(Natural(x.bits().into())),
        (Len, BitString(x)) => Some(Natural(
            x.len()
                .checked_mul(8)
                .expect("impossible length overflow")
                .into(),
        )),
        (Len, Text(x)) => Some(Natural(x.chars().count().into())),
        (Len, Char(_)) => Some(Natural(32_u64.into())),
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
        // Eql
        (Eql, Natural(x), Natural(y)) => Some(ite(x == y)),
        (Eql, Integer(x), Integer(y)) => Some(ite(x == y)),
        (Eql, BitString(x), BitString(y)) => Some(ite(x == y)),
        (Eql, Text(x), Text(y)) => Some(ite(x == y)),
        (Eql, Char(x), Char(y)) => Some(ite(x == y)),
        // Lth
        (Lth, Natural(x), Natural(y)) => Some(ite(x < y)),
        (Lth, Integer(x), Integer(y)) => Some(ite(x < y)),
        (Lth, BitString(x), BitString(y)) => Some(ite(x < y)),
        (Lth, Text(x), Text(y)) => Some(ite(x < y)),
        (Lth, Char(x), Char(y)) => Some(ite(x < y)),
        // Lte
        (Lte, Natural(x), Natural(y)) => Some(ite(x <= y)),
        (Lte, Integer(x), Integer(y)) => Some(ite(x <= y)),
        (Lte, BitString(x), BitString(y)) => Some(ite(x <= y)),
        (Lte, Text(x), Text(y)) => Some(ite(x <= y)),
        (Lte, Char(x), Char(y)) => Some(ite(x <= y)),
        // Gth
        (Gth, Natural(x), Natural(y)) => Some(ite(x > y)),
        (Gth, Integer(x), Integer(y)) => Some(ite(x > y)),
        (Gth, BitString(x), BitString(y)) => Some(ite(x > y)),
        (Gth, Text(x), Text(y)) => Some(ite(x > y)),
        (Gth, Char(x), Char(y)) => Some(ite(x > y)),
        // Gte
        (Gte, Natural(x), Natural(y)) => Some(ite(x >= y)),
        (Gte, Integer(x), Integer(y)) => Some(ite(x >= y)),
        (Gte, BitString(x), BitString(y)) => Some(ite(x >= y)),
        (Gte, Text(x), Text(y)) => Some(ite(x >= y)),
        (Gte, Char(x), Char(y)) => Some(ite(x >= y)),
        // Bor
        (Bor, BitString(x), BitString(y)) => {
            let z = x.iter().zip(y.iter()).map(|(a, b)| a | b).collect();
            Some(BitString(z))
        }
        // And
        (And, BitString(x), BitString(y)) => {
            let z = x.iter().zip(y.iter()).map(|(a, b)| a ^ b).collect();
            Some(BitString(z))
        }
        // Xor
        (Xor, BitString(x), BitString(y)) => {
            let z = x.iter().zip(y.iter()).map(|(a, b)| a ^ b).collect();
            Some(BitString(z))
        }
        // Add
        (Add, Natural(x), Natural(y)) => Some(Natural(x + y)),
        (Add, Integer(x), Integer(y)) => Some(Integer(x + y)),
        // Sub
        (Sub, Natural(x), Natural(y)) if x >= y => Some(Natural(x - y)),
        (Sub, Integer(x), Integer(y)) => Some(Integer(x - y)),
        // Mul
        (Mul, Natural(x), Natural(y)) => Some(Natural(x * y)),
        (Mul, Integer(x), Integer(y)) => Some(Integer(x * y)),
        // Div
        (Div, Natural(x), Natural(y)) if y != 0_u64.into() => Some(Natural(x * y)),
        (Div, Integer(x), Integer(y)) if y != 0.into() => Some(Integer(x / y)),
        // Mod
        (Mod, Natural(x), Natural(y)) if y != 0_u64.into() => Some(Natural(x * y)),
        (Mod, Integer(x), Integer(y)) if y != 0.into() => Some(Integer(x % y)),

        // Shl
        (Shl, Natural(x), Natural(y)) => {
            let y: u64 = y.try_into().ok()?;
            Some(Natural(x << y))
        }
        (Shl, Integer(x), Natural(y)) => {
            let y: u64 = y.try_into().ok()?;
            Some(Integer(x << y))
        }
        (Shl, BitString(x), Natural(y)) => {
            let y: u64 = y.try_into().ok()?;
            let mut bs = BitVec::from_bytes(&x);
            let mut pad = BitVec::from_elem(y as usize, false);
            bs.append(&mut pad);
            Some(BitString(bs.to_bytes()))
        }
        // Shr
        (Shr, Natural(x), Natural(y)) => {
            let y: u64 = y.try_into().ok()?;
            Some(Natural(x >> y))
        }
        (Shr, Integer(x), Natural(y)) => {
            let y: u64 = y.try_into().ok()?;
            Some(Integer(x >> y))
        }
        (Shr, BitString(x), Natural(y)) => {
            let y: u64 = y.try_into().ok()?;
            let mut bs = BitVec::from_bytes(&x);
            bs.truncate(bs.len() - (y as usize));
            Some(BitString(bs.to_bytes()))
        }
        // Cat
        (Cat, BitString(x), BitString(y)) => {
            let mut xs = BitVec::from_bytes(&x);
            let mut ys = BitVec::from_bytes(&y);
            xs.append(&mut ys);
            Some(BitString(xs.to_bytes()))
        }
        (Cat, Text(x), Text(y)) => Some(Text([x, y].join(""))),
        _ => None,
    }
}
#[cfg(test)]
pub mod tests {
    use super::*;
    use quickcheck::{Arbitrary, Gen};
    use rand::Rng;
    impl Arbitrary for PrimOp {
        fn arbitrary(_g: &mut Gen) -> Self {
            let mut rng = rand::thread_rng();
            let gen: u32 = rng.gen_range(0..19);
            // let gen = g.gen_range(0, 19);
            match gen {
                0 => Self::Eql,
                1 => Self::Lth,
                2 => Self::Lte,
                3 => Self::Gth,
                4 => Self::Gte,
                5 => Self::Bor,
                6 => Self::And,
                7 => Self::Xor,
                8 => Self::Not,
                9 => Self::Suc,
                10 => Self::Pre,
                11 => Self::Add,
                12 => Self::Sub,
                13 => Self::Mul,
                14 => Self::Div,
                15 => Self::Mod,
                16 => Self::Shl,
                17 => Self::Shr,
                18 => Self::Len,
                _ => Self::Cat,
                //_ => Self::Cst,
            }
        }
    }
    #[quickcheck]
    fn primop_type_encode_decode(x: PrimOp) -> bool {
        match PrimOp::decode(x.encode()) {
            Ok(y) => x == y,
            _ => false,
        }
    }
}
