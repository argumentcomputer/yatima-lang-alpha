use crate::{
  anon::Anon,
  meta::Meta,
};

/// Contains the anonymous and metadata terms if they fail to unembed
#[derive(PartialEq, Clone, Debug)]
pub enum EmbedError {
  Term(Anon, Meta),
}
