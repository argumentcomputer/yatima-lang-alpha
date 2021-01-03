use hashexpr::{
  position::Pos,
  Link,
};

pub struct Def {
  name: String,
  docs: String,
  term_anon: Link,
  type_anon: Link,
  term_meta: Meta,
  type_meta: Meta,
}
