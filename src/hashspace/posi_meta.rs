use hashexpr::position::Pos;

/// The source file position metadata
#[derive(Debug, Clone)]
pub enum PosiMeta {
  Ctor(Option<Pos>, Vec<PosiMeta>),
  Leaf,
}
