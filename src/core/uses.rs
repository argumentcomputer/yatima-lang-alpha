use crate::decode_error::{DecodeError, Expected};

use hashexpr::{atom, atom::Atom::*, Expr};
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Uses {
    None,
    Affi,
    Once,
    Many,
}

impl std::ops::Mul for Uses {
    type Output = Self;

    fn mul(self, y: Self) -> Self::Output {
        match (self, y) {
            (Self::None, _) | (_, Self::None) => Self::None,
            (Self::Many, _) | (_, Self::Many) => Self::Many,
            (Self::Affi, _) => Self::Affi,
            (Self::Once, x) => x,
        }
    }
}

impl std::ops::Add for Uses {
    type Output = Self;

    fn add(self, y: Self) -> Self::Output {
        match (self, y) {
            (Self::None, y) => y,
            (x, Self::None) => x,
            (..) => Self::Many,
        }
    }
}

impl Uses {
    #[must_use]
    pub const fn lte(x: Self, y: Self) -> bool {
        !matches!(
            (x, y),
            (Self::None, Self::Once)
                | (Self::Affi, Self::None)
                | (Self::Affi, Self::Once)
                | (Self::Once, Self::None)
                | (Self::Many, _)
        )
    }

    #[must_use]
    pub const fn gth(x: Self, y: Self) -> bool {
        !Self::lte(x, y)
    }

    #[must_use]
    pub fn encode(self) -> Expr {
        match self {
            Self::None => text!("0"),
            Self::Affi => text!("&"),
            Self::Once => text!("1"),
            Self::Many => text!("\u{3c9}"),
        }
    }

    pub fn decode(x: Expr) -> Result<Self, DecodeError> {
        match x {
            Expr::Atom(_, Text(n)) if n == "0" => Ok(Self::None),
            Expr::Atom(_, Text(n)) if n == "&" => Ok(Self::Affi),
            Expr::Atom(_, Text(n)) if n == "1" => Ok(Self::Once),
            Expr::Atom(_, Text(n)) if n == "\u{3c9}" => Ok(Self::Many),
            x => Err(DecodeError::new(x.position(), vec![Expected::Uses])),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use quickcheck::{Arbitrary, Gen};

    use crate::term::tests::frequency;

    impl Arbitrary for Uses {
        fn arbitrary(g: &mut Gen) -> Self {
            let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Self>)> = vec![
                (1, Box::new(|_| Self::None)),
                (1, Box::new(|_| Self::Affi)),
                (1, Box::new(|_| Self::Once)),
                (1, Box::new(|_| Self::Many)),
            ];
            frequency(g, input)
        }
    }
}
