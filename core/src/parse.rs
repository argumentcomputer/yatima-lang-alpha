pub mod base;
pub mod error;
pub mod literal;
pub mod op;
pub mod package;
pub mod span;
pub mod string;
pub mod term;
pub mod typedef;

use nom::Finish;
use sp_cid::Cid;

/// Try to parse a str to a Cid
pub fn parse_cid(
  s: &str,
) -> Result<Cid, error::ParseError<nom_locate::LocatedSpan<&str>>> {
  let result = package::parse_link(span::Span::new(s)).finish().map(|(_, x)| x);
  result
}
