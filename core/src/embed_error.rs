use crate::{
  anon::Anon,
  meta::Meta,
};

#[derive(PartialEq, Clone, Debug)]
pub enum EmbedError {
  Term(Anon, Meta),
}
