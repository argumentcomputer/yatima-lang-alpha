use hashexpr::position::Pos;

/// The source file position metadata
pub enum PositionMeta {
  Ctor(Option<Pos>, Vec<PositionMeta>),
  Leaf(Option<Pos>),
}
