use crate::{
  anon_term::AnonTerm,
  decode_error::DecodeError,
  meta_term::MetaTerm,
  term::Link,
};

#[derive(Clone, Debug)]
pub enum UnembedError {
  FreeVariable,
  DecodeError(DecodeError),
  DeserialError,
  UnexpectedCtor(AnonTerm, MetaTerm),
  UnknownLink(Link),
  BadLet,
}
