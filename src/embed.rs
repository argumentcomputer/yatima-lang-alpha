#[derive(Clone, PartialEq, Debug)]
pub enum EmbedError {
  FreeVariable,
  DecodeError(DecodeError),
  DeserialError,
  UnexpectedCtor(i128, i128),
  UnknownLink(Link),
}
