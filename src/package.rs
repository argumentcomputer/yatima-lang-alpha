use hashexpr::{
  atom::Atom::*,
  link::Link,
  Expr,
  Expr::*,
};

use std::{
  collections::HashMap,
  fmt,
};

use crate::{
  decode_error::{
    DecodeError,
    Expected,
  },
  defs::Defs,
  imports::{
    Import,
    Imports,
  },
};

#[derive(PartialEq, Clone, Debug)]
pub struct Package {
  name: String,
  docs: String,
  imports: Imports,
  defs: Defs,
}

impl Package {
  pub fn encode(self) -> Expr {
    let mut defs: Vec<Expr> = Vec::new();
    for d in self.defs.defs {
      defs.push(d.encode());
    }

    Expr::Cons(None, vec![
      atom!(symb!("package")),
      atom!(symb!(self.name)),
      atom!(text!(self.docs)),
      Imports::encode(self.imports),
      Expr::Cons(None, defs),
    ])
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("package") => {
          match tail {
            [Atom(_, Symbol(nam)), Atom(_, Text(doc, _)), imports, defs] => {
              let imports = Imports::decode(imports.to_owned())?;
              let refs = HashMap::new();
              let (defs, _) = Defs::decode(refs, defs.to_owned())?;
              Ok(Package {
                name: nam.to_owned(),
                docs: doc.to_owned(),
                imports,
                defs,
              })
            }
            _ => Err(DecodeError::new(pos, vec![Expected::PackageContents])),
          }
        }
        _ => Err(DecodeError::new(pos, vec![Expected::Package])),
      },
      _ => Err(DecodeError::new(expr.position(), vec![Expected::Package])),
    }
  }
}

impl fmt::Display for Package {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.docs.is_empty() {
      write!(f, "package {} {}where\n{}", self.name, self.imports, self.defs)
    }
    else {
      write!(
        f,
        "/*{}*/\npackage {} {}where\n{}",
        self.docs, self.name, self.imports, self.defs
      )
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use crate::term::tests::arbitrary_name;

  pub fn test_package() -> Package {
    let refs: HashMap<String, (Link, Link)> = HashMap::new();
    Package {
      name: String::from("test"),
      docs: String::from("This is a test package"),
      imports: Imports { imports: Vec::new() },
      defs: Defs::decode(
        refs,
        hashexpr::parse(
          "((def id \"The identity function\" (forall ω A Type A) (lambda x \
           x)))",
        )
        .unwrap()
        .1,
      )
      .unwrap()
      .0,
    }
  }

  impl Arbitrary for Package {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let name = arbitrary_name(g);
      Package {
        name,
        docs: String::from(""),
        imports: Imports::new(Vec::new()),
        defs: Arbitrary::arbitrary(g),
      }
    }
  }

  #[quickcheck]
  fn package_encode_decode(x: Package) -> bool {
    match Package::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
