use hashexpr::position::Pos;

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Expected {
  Uses,
  PrimOp,
  Literal,
  ExceptionLiteral,
  LitType,
  BoundVar,
  DefinedRef,
  TypeOfTypes,
  LetRec,
  Let,
  Lambda,
  Forall,
  Annotation,
  SelfType,
  Case,
  Data,
  Constructor,
  TermDef,
  TermDefContents,
  Definition,
  DefinitionContents,
  DefinitionList,
  UniqueDefinitionName,
  Import,
  Imports,
  Package,
  PackageDecls,
  PackageDeclaration,
  PackageOpenWith,
  PackageDefinition,
  PackageContents,
  AnonTermCons,
  AnonTermAtom,
  AnonTermVariU64,
  MetaTerm,
}

#[derive(PartialEq, Clone, Debug)]
pub struct DecodeError {
  position: Option<Pos>,
  expected: Vec<Expected>,
}

impl DecodeError {
  #[must_use]
  pub fn new(p: Option<Pos>, es: Vec<Expected>) -> Self {
    Self { position: p, expected: es }
  }

  #[must_use]
  pub fn join(self, other: Self) -> Self {
    match (self.position, other.position) {
      (Some(sp), Some(op)) => match sp.from_offset.cmp(&op.from_offset) {
        std::cmp::Ordering::Greater => self,
        std::cmp::Ordering::Less => other,
        std::cmp::Ordering::Equal => {
          let (upto_offset, upto_line, upto_column) =
            if sp.upto_offset >= op.upto_offset {
              (sp.upto_offset, sp.upto_line, sp.upto_column)
            }
            else {
              (op.upto_offset, op.upto_line, op.upto_column)
            };
          let new_pos = Pos {
            from_offset: sp.from_offset,
            from_line: sp.from_line,
            from_column: sp.from_column,
            upto_offset,
            upto_line,
            upto_column,
          };
          let mut exp = Vec::new();
          exp.extend(self.expected.into_iter());
          exp.extend(other.expected.into_iter());
          Self { position: Some(new_pos), expected: exp }
        }
      },
      (Some(_), _) => self,
      (_, Some(_)) => other,
      _ => {
        let mut exp = Vec::new();
        exp.extend(self.expected.into_iter());
        exp.extend(other.expected.into_iter());
        Self { position: None, expected: exp }
      }
    }
  }
}
pub fn or_else_join<A>(
  a: Result<A, DecodeError>,
  b: Result<A, DecodeError>,
) -> Result<A, DecodeError> {
  a.or_else(|ea| b.map_err(|eb| ea.join(eb)))
}
