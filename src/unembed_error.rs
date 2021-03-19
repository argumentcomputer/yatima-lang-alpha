use crate::{
  anon_term::AnonTerm,
  decode_error::DecodeError,
  meta_term::MetaTerm,
  term::Link,
};

use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub enum UnembedError {
  FreeVariable,
  DecodeError(DecodeError),
  DeserialError,
  UnexpectedCtor(AnonTerm, MetaTerm),
  UnknownLink(Link),
  BadLet,
}

// impl fmt::Display for UnembedError {
//  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {}
//}
