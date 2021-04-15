use crate::{
    atom::Atom,
    base,
    base::Base,
    error::{DeserialError, DeserialErrorKind, ParseError, ParseErrorKind},
    span::Span,
    Expr,
};
use nom::{bytes::complete::tag, combinator::opt, Err, Err::Error, IResult};
use std::fmt;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Link(blake3::Hash);

impl Link {
    #[must_use]
    pub fn make(x: &[u8]) -> Self {
        Self(blake3::hash(x))
    }

    #[must_use]
    pub fn from(x: [u8; 32]) -> Self {
        Self(blake3::Hash::from(x))
    }

    #[must_use]
    pub const fn as_hash(&self) -> &blake3::Hash {
        match self {
            Link(h) => h,
        }
    }

    #[must_use]
    pub fn as_bytes(&self) -> &[u8; 32] {
        self.as_hash().as_bytes()
    }

    #[must_use]
    pub fn serialize(&self) -> Vec<u8> {
        Expr::Atom(None, Atom::Link(*self)).serialize()
    }

    pub fn deserialize(i: &[u8]) -> IResult<&[u8], Self, DeserialError<&[u8]>> {
        match Expr::deserialize(i) {
            Ok((i, Expr::Atom(_, Atom::Link(x)))) => Ok((i, x)),
            Ok((i, _)) => Err(Error(DeserialError::new(
                i,
                DeserialErrorKind::ExpectedLink,
            ))),
            Err(e) => Err(e),
        }
    }

    pub fn parse(i: &str) -> IResult<Span, Self, ParseError<Span>> {
        let i = Span::new(i);
        let (i, _) = opt(tag("#"))(i)?;
        let (i, (_, raw)) = base::parse(i).map_err(nom::Err::convert)?;
        let (_, x) = Self::deserialize(&raw).map_err(|e| match e {
            Err::Incomplete(n) => Err::Incomplete(n),
            Err::Error(e) | Err::Failure(e) => Err::Error(ParseError::new(
                i,
                ParseErrorKind::DeserialErr(e.clone().input_as_bytes()),
            )),
        })?;
        Ok((i, x))
    }
}

impl fmt::Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", base::encode(Base::_58, self.serialize()))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use quickcheck::{Arbitrary, Gen};

    impl Arbitrary for Link {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
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
            Link::from(bytes)
        }
    }
    #[quickcheck]
    fn link_deserial_serial(x: Link) -> bool {
        match Link::deserialize(&x.serialize()) {
            Ok((_, y)) => x == y,
            _ => false,
        }
    }
    #[quickcheck]
    fn link_print_parse(x: Link) -> bool {
        match Link::parse(&format!("{}", x)) {
            Ok((_, y)) => x == y,
            _ => false,
        }
    }
}
