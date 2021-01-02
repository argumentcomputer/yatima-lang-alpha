use hashexpr::{
  atom::Atom::*,
  link::Link,
  Expr,
  Expr::*,
};

use crate::decode_error::{
  DecodeError,
  Expected,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Import {
  pub alias: String,
  pub link: Link,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Imports {
  pub imports: Vec<Import>,
}

impl Import {
  pub fn new(alias: String, link: Link) -> Self { Import { alias, link } }

  pub fn encode(self) -> Expr {
    Expr::Cons(None, vec![
      atom!(symb!("import")),
      atom!(symb!(self.alias)),
      atom!(link!(self.link)),
    ])
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(n)), Atom(_, Symbol(alias)), Atom(_, Link(link))]
          if *n == String::from("import") =>
        {
          Ok(Import { alias: alias.to_owned(), link: link.to_owned() })
        }
        _ => Err(DecodeError::new(pos, vec![Expected::Import])),
      },
      _ => Err(DecodeError::new(x.position(), vec![Expected::Import])),
    }
  }
}

impl Imports {
  pub fn new(imports: Vec<Import>) -> Self { Imports { imports } }

  pub fn encode(self) -> Expr {
    let mut imports: Vec<Expr> = Vec::new();
    for i in self.imports {
      imports.push(i.encode());
    }
    Expr::Cons(None, imports)
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(_, xs) => {
        let mut imports: Vec<Import> = Vec::new();
        for x in xs {
          let import = Import::decode(x)?;
          imports.push(import)
        }
        Ok(Imports { imports })
      }
      _ => Err(DecodeError::new(expr.position(), vec![Expected::Imports])),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;

  use crate::term::tests::arbitrary_name;

  use crate::package::tests::test_package;

  // use cr

  impl Arbitrary for Import {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      Import { alias: arbitrary_name(g), link: test_package().encode().link() }
    }
  }
  impl Arbitrary for Imports {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let n = g.gen_range(0, 10);
      let mut imports: Vec<Import> = Vec::new();
      for _ in 0..n {
        imports.push(Arbitrary::arbitrary(g))
      }
      Imports { imports }
    }
  }
  #[quickcheck]
  fn import_encode_decode(x: Import) -> bool {
    match Import::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn imports_encode_decode(x: Imports) -> bool {
    match Imports::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
