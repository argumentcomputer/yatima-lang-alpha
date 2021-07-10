use sp_ipld::Ipld;

use sp_std::{
  borrow::ToOwned,
  fmt,
};

use alloc::string::{
  String,
  ToString,
};

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
  yatima,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum CharOp {
  FromU32,
  ToU32,
  IsAlphabetic,
  IsAlphanumeric,
  IsAscii,
  IsAsciiAlphabetic,
  IsAsciiAlphanumeric,
  IsAsciiControl,
  IsAsciiDigit,
  IsAsciiGraphic,
  IsAsciiHexDigit,
  IsAsciiLowerCase,
  IsAsciiPunctuation,
  IsAsciiUpperCase,
  IsAsciiWhitespace,
  IsControl,
  IsDigit,
  IsLowercase,
  IsNumeric,
  IsUppercase,
  IsWhitespace,
  LenUTF8,
  LenUTF16,
  ToAsciiLowercase,
  ToAsciiUppercase,
  ToLowercase,
  ToUppercase,
  Eql,
  Lte,
  Lth,
  Gth,
  Gte,
}

impl CharOp {
  pub fn symbol(self) -> String {
    match self {
      Self::FromU32 => "from_U32".to_owned(),
      Self::ToU32 => "to_U32".to_owned(),
      Self::IsAlphabetic => "is_alphabetic".to_owned(),
      Self::IsAlphanumeric => "is_alphanumeric".to_owned(),
      Self::IsAscii => "is_ascii".to_owned(),
      Self::IsAsciiAlphabetic => "is_ascii_alphabetic".to_owned(),
      Self::IsAsciiAlphanumeric => "is_ascii_alphanumeric".to_owned(),
      Self::IsAsciiControl => "is_ascii_control".to_owned(),
      Self::IsAsciiDigit => "is_ascii_digit".to_owned(),
      Self::IsAsciiGraphic => "is_ascii_graphic".to_owned(),
      Self::IsAsciiHexDigit => "is_ascii_hexdigit".to_owned(),
      Self::IsAsciiLowerCase => "is_ascii_lowercase".to_owned(),
      Self::IsAsciiPunctuation => "is_ascii_punctuation".to_owned(),
      Self::IsAsciiUpperCase => "is_ascii_uppercase".to_owned(),
      Self::IsAsciiWhitespace => "is_ascii_whitespace".to_owned(),
      Self::IsControl => "is_control".to_owned(),
      Self::IsDigit => "is_digit".to_owned(),
      Self::IsLowercase => "is_lowercase".to_owned(),
      Self::IsNumeric => "is_numeric".to_owned(),
      Self::IsUppercase => "is_uppercase".to_owned(),
      Self::IsWhitespace => "is_whitespace".to_owned(),
      Self::LenUTF8 => "len_utf8".to_owned(),
      Self::LenUTF16 => "len_utf16".to_owned(),
      Self::ToAsciiLowercase => "to_ascii_lowercase".to_owned(),
      Self::ToAsciiUppercase => "to_ascii_uppercase".to_owned(),
      Self::ToLowercase => "to_lowercase".to_owned(),
      Self::ToUppercase => "to_uppercase".to_owned(),
      Self::Eql => "eql".to_owned(),
      Self::Lte => "lte".to_owned(),
      Self::Lth => "lth".to_owned(),
      Self::Gth => "gth".to_owned(),
      Self::Gte => "gte".to_owned(),
    }
  }

  pub fn from_symbol(x: &str) -> Option<Self> {
    match x {
      "from_U32" => Some(Self::FromU32),
      "to_U32" => Some(Self::ToU32),
      "is_alphabetic" => Some(Self::IsAlphabetic),
      "is_alphanumeric" => Some(Self::IsAlphanumeric),
      "is_ascii" => Some(Self::IsAscii),
      "is_ascii_alphabetic" => Some(Self::IsAsciiAlphabetic),
      "is_ascii_alphanumeric" => Some(Self::IsAsciiAlphanumeric),
      "is_ascii_control" => Some(Self::IsAsciiControl),
      "is_ascii_digit" => Some(Self::IsAsciiDigit),
      "is_ascii_graphic" => Some(Self::IsAsciiGraphic),
      "is_ascii_hexdigit" => Some(Self::IsAsciiHexDigit),
      "is_ascii_lowercase" => Some(Self::IsAsciiLowerCase),
      "is_ascii_punctuation" => Some(Self::IsAsciiPunctuation),
      "is_ascii_uppercase" => Some(Self::IsAsciiUpperCase),
      "is_ascii_whitespace" => Some(Self::IsAsciiWhitespace),
      "is_control" => Some(Self::IsControl),
      "is_digit" => Some(Self::IsDigit),
      "is_lowercase" => Some(Self::IsLowercase),
      "is_numeric" => Some(Self::IsNumeric),
      "is_uppercase" => Some(Self::IsUppercase),
      "is_whitespace" => Some(Self::IsWhitespace),
      "len_utf8" => Some(Self::LenUTF8),
      "len_utf16" => Some(Self::LenUTF16),
      "to_ascii_lowercase" => Some(Self::ToAsciiLowercase),
      "to_ascii_uppercase" => Some(Self::ToAsciiUppercase),
      "to_lowercase" => Some(Self::ToLowercase),
      "to_uppercase" => Some(Self::ToUppercase),
      "eql" => Some(Self::Eql),
      "lth" => Some(Self::Lth),
      "lte" => Some(Self::Lte),
      "gth" => Some(Self::Gth),
      "gte" => Some(Self::Gte),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::FromU32 => yatima!("∀ #U32 -> #Char"),
      Self::ToU32 => yatima!("∀ #Char -> #U32"),
      Self::IsAlphabetic => yatima!("∀ #Char -> #Bool"),
      Self::IsAlphanumeric => yatima!("∀ #Char -> #Bool"),
      Self::IsAscii => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiAlphabetic => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiAlphanumeric => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiControl => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiDigit => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiGraphic => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiHexDigit => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiLowerCase => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiPunctuation => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiUpperCase => yatima!("∀ #Char -> #Bool"),
      Self::IsAsciiWhitespace => yatima!("∀ #Char -> #Bool"),
      Self::IsControl => yatima!("∀ #Char -> #Bool"),
      Self::IsDigit => yatima!("∀ #Char -> #Bool"),
      Self::IsLowercase => yatima!("∀ #Char -> #Bool"),
      Self::IsNumeric => yatima!("∀ #Char -> #Bool"),
      Self::IsUppercase => yatima!("∀ #Char -> #Bool"),
      Self::IsWhitespace => yatima!("∀ #Char -> #Bool"),
      Self::LenUTF8 => yatima!("∀ #Char -> #Nat"),
      Self::LenUTF16 => yatima!("∀ #Char -> #Nat"),
      Self::ToAsciiLowercase => yatima!("∀ #Char -> #Bool"),
      Self::ToAsciiUppercase => yatima!("∀ #Char -> #Bool"),
      Self::ToLowercase => yatima!("∀ #Char -> #Bool"),
      Self::ToUppercase => yatima!("∀ #Char -> #Bool"),
      Self::Eql => yatima!("∀ #Char #Char -> #Bool"),
      Self::Lte => yatima!("∀ #Char #Char -> #Bool"),
      Self::Lth => yatima!("∀ #Char #Char -> #Bool"),
      Self::Gth => yatima!("∀ #Char #Char -> #Bool"),
      Self::Gte => yatima!("∀ #Char #Char -> #Bool"),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::FromU32 => Ipld::Integer(0),
      Self::ToU32 => Ipld::Integer(1),
      Self::IsAlphabetic => Ipld::Integer(2),
      Self::IsAlphanumeric => Ipld::Integer(3),
      Self::IsAscii => Ipld::Integer(4),
      Self::IsAsciiAlphabetic => Ipld::Integer(5),
      Self::IsAsciiAlphanumeric => Ipld::Integer(6),
      Self::IsAsciiControl => Ipld::Integer(7),
      Self::IsAsciiDigit => Ipld::Integer(8),
      Self::IsAsciiGraphic => Ipld::Integer(9),
      Self::IsAsciiHexDigit => Ipld::Integer(10),
      Self::IsAsciiLowerCase => Ipld::Integer(11),
      Self::IsAsciiPunctuation => Ipld::Integer(12),
      Self::IsAsciiUpperCase => Ipld::Integer(13),
      Self::IsAsciiWhitespace => Ipld::Integer(14),
      Self::IsControl => Ipld::Integer(15),
      Self::IsDigit => Ipld::Integer(16),
      Self::IsLowercase => Ipld::Integer(17),
      Self::IsNumeric => Ipld::Integer(18),
      Self::IsUppercase => Ipld::Integer(19),
      Self::IsWhitespace => Ipld::Integer(20),
      Self::LenUTF8 => Ipld::Integer(21),
      Self::LenUTF16 => Ipld::Integer(22),
      Self::ToAsciiLowercase => Ipld::Integer(23),
      Self::ToAsciiUppercase => Ipld::Integer(24),
      Self::ToLowercase => Ipld::Integer(25),
      Self::ToUppercase => Ipld::Integer(26),
      Self::Eql => Ipld::Integer(27),
      Self::Lte => Ipld::Integer(28),
      Self::Lth => Ipld::Integer(29),
      Self::Gth => Ipld::Integer(30),
      Self::Gte => Ipld::Integer(31),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::FromU32),
      Ipld::Integer(1) => Ok(Self::ToU32),
      Ipld::Integer(2) => Ok(Self::IsAlphabetic),
      Ipld::Integer(3) => Ok(Self::IsAlphanumeric),
      Ipld::Integer(4) => Ok(Self::IsAscii),
      Ipld::Integer(5) => Ok(Self::IsAsciiAlphabetic),
      Ipld::Integer(6) => Ok(Self::IsAsciiAlphanumeric),
      Ipld::Integer(7) => Ok(Self::IsAsciiControl),
      Ipld::Integer(8) => Ok(Self::IsAsciiDigit),
      Ipld::Integer(9) => Ok(Self::IsAsciiGraphic),
      Ipld::Integer(10) => Ok(Self::IsAsciiHexDigit),
      Ipld::Integer(11) => Ok(Self::IsAsciiLowerCase),
      Ipld::Integer(12) => Ok(Self::IsAsciiPunctuation),
      Ipld::Integer(13) => Ok(Self::IsAsciiUpperCase),
      Ipld::Integer(14) => Ok(Self::IsAsciiWhitespace),
      Ipld::Integer(15) => Ok(Self::IsControl),
      Ipld::Integer(16) => Ok(Self::IsDigit),
      Ipld::Integer(17) => Ok(Self::IsLowercase),
      Ipld::Integer(18) => Ok(Self::IsNumeric),
      Ipld::Integer(19) => Ok(Self::IsUppercase),
      Ipld::Integer(20) => Ok(Self::IsWhitespace),
      Ipld::Integer(21) => Ok(Self::LenUTF8),
      Ipld::Integer(22) => Ok(Self::LenUTF16),
      Ipld::Integer(23) => Ok(Self::ToAsciiLowercase),
      Ipld::Integer(24) => Ok(Self::ToAsciiUppercase),
      Ipld::Integer(25) => Ok(Self::ToLowercase),
      Ipld::Integer(26) => Ok(Self::ToUppercase),
      Ipld::Integer(27) => Ok(Self::Eql),
      Ipld::Integer(28) => Ok(Self::Lte),
      Ipld::Integer(29) => Ok(Self::Lth),
      Ipld::Integer(30) => Ok(Self::Gth),
      Ipld::Integer(31) => Ok(Self::Gte),
      xs => Err(IpldError::CharOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::FromU32 => 1,
      Self::ToU32 => 1,
      Self::IsAlphabetic => 1,
      Self::IsAlphanumeric => 1,
      Self::IsAscii => 1,
      Self::IsAsciiAlphabetic => 1,
      Self::IsAsciiAlphanumeric => 1,
      Self::IsAsciiControl => 1,
      Self::IsAsciiDigit => 1,
      Self::IsAsciiGraphic => 1,
      Self::IsAsciiHexDigit => 1,
      Self::IsAsciiLowerCase => 1,
      Self::IsAsciiPunctuation => 1,
      Self::IsAsciiUpperCase => 1,
      Self::IsAsciiWhitespace => 1,
      Self::IsControl => 1,
      Self::IsDigit => 2,
      Self::IsLowercase => 1,
      Self::IsNumeric => 1,
      Self::IsUppercase => 1,
      Self::IsWhitespace => 1,
      Self::LenUTF8 => 1,
      Self::LenUTF16 => 1,
      Self::ToAsciiLowercase => 1,
      Self::ToAsciiUppercase => 1,
      Self::ToLowercase => 1,
      Self::ToUppercase => 1,
      Self::Eql => 2,
      Self::Lte => 2,
      Self::Lth => 2,
      Self::Gth => 2,
      Self::Gte => 2,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::FromU32, U32(x)) => char::from_u32(*x).map(Char),
      (Self::ToU32, Char(x)) => Some(U32((*x).into())),
      (Self::IsAlphabetic, Char(x)) => Some(Bool(x.is_alphabetic())),
      (Self::IsAlphanumeric, Char(x)) => Some(Bool(x.is_alphanumeric())),
      (Self::IsAscii, Char(x)) => Some(Bool(x.is_ascii())),
      (Self::IsAsciiAlphabetic, Char(x)) => Some(Bool(x.is_ascii_alphabetic())),
      (Self::IsAsciiAlphanumeric, Char(x)) => {
        Some(Bool(x.is_ascii_alphanumeric()))
      }
      (Self::IsAsciiControl, Char(x)) => Some(Bool(x.is_ascii_control())),
      (Self::IsAsciiDigit, Char(x)) => Some(Bool(x.is_ascii_digit())),
      (Self::IsAsciiGraphic, Char(x)) => Some(Bool(x.is_ascii_graphic())),
      (Self::IsAsciiHexDigit, Char(x)) => Some(Bool(x.is_ascii_hexdigit())),
      (Self::IsAsciiLowerCase, Char(x)) => Some(Bool(x.is_ascii_lowercase())),
      (Self::IsAsciiPunctuation, Char(x)) => {
        Some(Bool(x.is_ascii_punctuation()))
      }
      (Self::IsAsciiUpperCase, Char(x)) => Some(Bool(x.is_ascii_uppercase())),
      (Self::IsAsciiWhitespace, Char(x)) => Some(Bool(x.is_ascii_whitespace())),
      (Self::IsControl, Char(x)) => Some(Bool(x.is_control())),
      (Self::IsLowercase, Char(x)) => Some(Bool(x.is_lowercase())),
      (Self::IsNumeric, Char(x)) => Some(Bool(x.is_numeric())),
      (Self::IsUppercase, Char(x)) => Some(Bool(x.is_uppercase())),
      (Self::IsWhitespace, Char(x)) => Some(Bool(x.is_whitespace())),
      (Self::LenUTF8, Char(x)) => Some(Nat(x.len_utf8().into())),
      (Self::LenUTF16, Char(x)) => Some(Nat(x.len_utf16().into())),
      (Self::ToAsciiLowercase, Char(x)) => Some(Char(x.to_ascii_lowercase())),
      (Self::ToAsciiUppercase, Char(x)) => Some(Char(x.to_ascii_uppercase())),
      (Self::ToLowercase, Char(x)) => {
        Some(Text(x.to_lowercase().to_string().into()))
      }
      (Self::ToUppercase, Char(x)) => {
        Some(Text(x.to_uppercase().to_string().into()))
      }
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      // TODO: Hardcoding the maximum radix here is probably bad, not sure how
      // to do it differently though.
      (Self::IsDigit, Char(x), U32(y)) => {
        if *y > 36 {
          None
        }
        else {
          Some(Bool(x.is_digit(*y)))
        }
      }
      (Self::Eql, Char(x), Char(y)) => Some(Bool(x == y)),
      (Self::Lte, Char(x), Char(y)) => Some(Bool(x <= y)),
      (Self::Lth, Char(x), Char(y)) => Some(Bool(x < y)),
      (Self::Gth, Char(x), Char(y)) => Some(Bool(x > y)),
      (Self::Gte, Char(x), Char(y)) => Some(Bool(x >= y)),
      _ => None,
    }
  }
}

impl fmt::Display for CharOp {
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
    TestResult,
  };
  use rand::Rng;
  use sp_std::mem;
  use Literal::{
    Bool,
    Char,
    Nat,
    Text,
    U32,
  };
  impl Arbitrary for CharOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..=31);
      match gen {
        0 => Self::FromU32,
        1 => Self::ToU32,
        2 => Self::IsAlphabetic,
        3 => Self::IsAlphanumeric,
        4 => Self::IsAscii,
        5 => Self::IsAsciiAlphabetic,
        6 => Self::IsAsciiAlphanumeric,
        7 => Self::IsAsciiControl,
        8 => Self::IsAsciiDigit,
        9 => Self::IsAsciiGraphic,
        10 => Self::IsAsciiHexDigit,
        11 => Self::IsAsciiLowerCase,
        12 => Self::IsAsciiPunctuation,
        13 => Self::IsAsciiUpperCase,
        14 => Self::IsAsciiWhitespace,
        15 => Self::IsControl,
        16 => Self::IsDigit,
        17 => Self::IsLowercase,
        18 => Self::IsNumeric,
        19 => Self::IsUppercase,
        20 => Self::IsWhitespace,
        21 => Self::LenUTF8,
        22 => Self::LenUTF16,
        23 => Self::ToAsciiLowercase,
        24 => Self::ToAsciiUppercase,
        25 => Self::ToLowercase,
        26 => Self::ToUppercase,
        27 => Self::Eql,
        28 => Self::Lte,
        29 => Self::Lth,
        30 => Self::Gth,
        _ => Self::Gte,
      }
    }
  }

  #[quickcheck]
  fn char_op_ipld(x: CharOp) -> bool {
    match CharOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn test_apply(op: CharOp, a: u32, b: char, c: char) -> TestResult {
    let apply1_u32 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(CharOp::apply1(op, &U32(a)) == expected)
    };

    let apply1_char = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(CharOp::apply1(op, &Char(b)) == expected)
    };

    let apply2_char_u32 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(CharOp::apply2(op, &Char(b), &U32(a)) == expected)
    };
    let apply2_char_char = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(CharOp::apply2(op, &Char(b), &Char(c)) == expected)
    };

    match op {
      CharOp::FromU32 => apply1_u32(char::from_u32(a).map(Char)),
      CharOp::ToU32 => apply1_char(Some(U32(b.into()))),
      CharOp::IsAlphabetic => apply1_char(Some(Bool(b.is_alphabetic()))),
      CharOp::IsAlphanumeric => apply1_char(Some(Bool(b.is_alphanumeric()))),
      CharOp::IsAscii => apply1_char(Some(Bool(b.is_ascii()))),
      CharOp::IsAsciiAlphabetic => {
        apply1_char(Some(Bool(b.is_ascii_alphabetic())))
      }
      CharOp::IsAsciiAlphanumeric => {
        apply1_char(Some(Bool(b.is_ascii_alphanumeric())))
      }
      CharOp::IsAsciiControl => apply1_char(Some(Bool(b.is_ascii_control()))),
      CharOp::IsAsciiDigit => apply1_char(Some(Bool(b.is_ascii_digit()))),
      CharOp::IsAsciiGraphic => apply1_char(Some(Bool(b.is_ascii_graphic()))),
      CharOp::IsAsciiHexDigit => apply1_char(Some(Bool(b.is_ascii_hexdigit()))),
      CharOp::IsAsciiLowerCase => {
        apply1_char(Some(Bool(b.is_ascii_lowercase())))
      }
      CharOp::IsAsciiPunctuation => {
        apply1_char(Some(Bool(b.is_ascii_punctuation())))
      }
      CharOp::IsAsciiUpperCase => {
        apply1_char(Some(Bool(b.is_ascii_uppercase())))
      }
      CharOp::IsAsciiWhitespace => {
        apply1_char(Some(Bool(b.is_ascii_whitespace())))
      }
      CharOp::IsControl => apply1_char(Some(Bool(b.is_control()))),
      CharOp::IsDigit => apply2_char_u32(
        // TODO: Hardcoding the maximum radix here is probably bad, not sure
        // how to do it differently though.
        if a > 36 { None } else { Some(Bool(b.is_digit(a))) },
      ),
      CharOp::IsLowercase => apply1_char(Some(Bool(b.is_lowercase()))),
      CharOp::IsNumeric => apply1_char(Some(Bool(b.is_numeric()))),
      CharOp::IsUppercase => apply1_char(Some(Bool(b.is_uppercase()))),
      CharOp::IsWhitespace => apply1_char(Some(Bool(b.is_whitespace()))),
      CharOp::LenUTF8 => apply1_char(Some(Nat(b.len_utf8().into()))),
      CharOp::LenUTF16 => apply1_char(Some(Nat(b.len_utf16().into()))),
      CharOp::ToAsciiLowercase => {
        apply1_char(Some(Char(b.to_ascii_lowercase())))
      }
      CharOp::ToAsciiUppercase => {
        apply1_char(Some(Char(b.to_ascii_uppercase())))
      }
      CharOp::ToLowercase => {
        apply1_char(Some(Text(b.to_lowercase().to_string().into())))
      }
      CharOp::ToUppercase => {
        apply1_char(Some(Text(b.to_uppercase().to_string().into())))
      }
      CharOp::Eql => apply2_char_char(Some(Bool(b == c))),
      CharOp::Lth => apply2_char_char(Some(Bool(b < c))),
      CharOp::Lte => apply2_char_char(Some(Bool(b <= c))),
      CharOp::Gth => apply2_char_char(Some(Bool(b > c))),
      CharOp::Gte => apply2_char_char(Some(Bool(b >= c))),
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: CharOp,
    a: Literal,
    b: u32,
    c: char,
    test_arg_2: bool,
  ) -> TestResult {
    let test_apply1_none_on_invalid = |valid_arg: Literal| -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      }
      else {
        TestResult::from_bool(CharOp::apply1(op, &a) == None)
      }
    };

    let test_apply2_none_on_invalid =
      |valid_arg: Literal, a_: Literal, b_: Literal| -> TestResult {
        let go = || TestResult::from_bool(CharOp::apply2(op, &a_, &b_) == None);
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

    match op {
      // Arity 1, valid is U32.
      CharOp::FromU32 => test_apply1_none_on_invalid(U32(b)),
      // Arity 1, valid is Char.
      CharOp::ToU32
      | CharOp::IsAlphabetic
      | CharOp::IsAlphanumeric
      | CharOp::IsAscii
      | CharOp::IsAsciiAlphabetic
      | CharOp::IsAsciiAlphanumeric
      | CharOp::IsAsciiControl
      | CharOp::IsAsciiDigit
      | CharOp::IsAsciiGraphic
      | CharOp::IsAsciiHexDigit
      | CharOp::IsAsciiLowerCase
      | CharOp::IsAsciiPunctuation
      | CharOp::IsAsciiUpperCase
      | CharOp::IsAsciiWhitespace
      | CharOp::IsControl
      | CharOp::IsLowercase
      | CharOp::IsNumeric
      | CharOp::IsUppercase
      | CharOp::IsWhitespace
      | CharOp::LenUTF8
      | CharOp::LenUTF16
      | CharOp::ToAsciiLowercase
      | CharOp::ToAsciiUppercase
      | CharOp::ToLowercase
      | CharOp::ToUppercase => test_apply1_none_on_invalid(Char(c)),
      // Arity 2, valid are Char on a and U32 on b
      CharOp::IsDigit => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Char(c), a, U32(b))
        }
        else {
          test_apply2_none_on_invalid(U32(b), Char(c), a)
        }
      }
      // Arity 2, valid are Char on a and Char on b
      CharOp::Eql | CharOp::Lth | CharOp::Lte | CharOp::Gth | CharOp::Gte => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Char(c), a, Char(c))
        }
        else {
          test_apply2_none_on_invalid(Char(c), Char(c), a)
        }
      }
    }
  }

  //#[test]
  // fn test_apply_bin_op() {
  //  assert_eq!(
  //    Some(Literal::Text(ropey::Rope::from_str("foo"))),
  //    apply_bin_op(
  //      PrimOp::TextCons,
  //      Literal::Char('f'),
  //      Literal::Text(ropey::Rope::from_str("oo"))
  //    )
  //  )
  //}
}
