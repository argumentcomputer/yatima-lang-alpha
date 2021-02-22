use hashexpr::{
  AVal,
  AVal::*,
  Expr,
  Expr::*,
  Link,
};
use std::fmt;

use crate::{
  anon_term::AnonTerm,
  decode_error::{
    DecodeError,
    Expected,
  },
  definition::Definition,
  hashspace,
  imports::{
    Import,
    Imports,
  },
  term::Def,
  unembed_error::UnembedError,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Package {
  pub name: String,
  pub docs: String,
  pub source: Link,
  pub imports: Imports,
  pub decls: Vec<Declaration>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Declaration {
  Defn { name: String, defn: Link, term: Link },
  /* Open { name: String, alias: String, from: Link },
   * Data { name: String, typ_: Term, ctors: HashMap<String, Term> }, */
}

impl Declaration {
  pub fn encode(self) -> Expr {
    match self {
      Self::Defn { name, defn, term } => {
        cons!(None, symb!(name), link!(defn), link!(term))
      }
    }
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(n)), Atom(_, Link(d)), Atom(_, Link(a))] => {
          Ok(Self::Defn { name: n.to_owned(), defn: *d, term: *a })
        }
        _ => Err(DecodeError::new(pos, vec![Expected::PackageDefinition])),
      },
      x => {
        Err(DecodeError::new(x.position(), vec![Expected::PackageDeclaration]))
      }
    }
  }

  pub fn to_def(self) -> Result<Def, UnembedError> {
    match self {
      Self::Defn { defn, .. } => {
        let def =
          hashspace::get(defn).ok_or(UnembedError::UnknownLink(defn))?;
        let def =
          Definition::decode(def).map_err(|e| UnembedError::DecodeError(e))?;
        let type_anon = hashspace::get(def.type_anon)
          .ok_or(UnembedError::UnknownLink(defn))?;
        let type_anon = AnonTerm::decode(type_anon)
          .map_err(|e| UnembedError::DecodeError(e))?;
        let term_anon = hashspace::get(def.term_anon)
          .ok_or(UnembedError::UnknownLink(defn))?;
        let term_anon = AnonTerm::decode(term_anon)
          .map_err(|e| UnembedError::DecodeError(e))?;
        Def::unembed(def, type_anon, term_anon)
      }
    }
  }
}

impl fmt::Display for Declaration {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let def = self.clone().to_def().expect("unembed error");
    write!(f, "{}", def)
  }
}

impl Package {
  pub fn encode(self) -> Expr {
    let mut xs = Vec::new();
    for d in self.decls {
      xs.push(d.encode());
    }
    cons!(
      None,
      symb!("package"),
      symb!(self.name),
      text!(self.docs),
      link!(self.source),
      self.imports.encode(),
      Expr::Cons(None, xs)
    )
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(c)), tail @ ..] if *c == String::from("package") => {
          match tail {
            [Atom(_, Symbol(n)), Atom(_, Text(d)), Atom(_, Link(s)), x, y] => {
              let imports = Imports::decode(x.to_owned())?;
              let mut decls = Vec::new();
              match y {
                Cons(_, xs) => {
                  for x in xs {
                    let decl = Declaration::decode(x.to_owned())?;
                    decls.push(decl);
                  }
                  Ok(Package {
                    name: n.to_owned(),
                    docs: d.to_owned(),
                    source: s.to_owned(),
                    imports,
                    decls,
                  })
                }
                expr => Err(DecodeError::new(expr.position(), vec![
                  Expected::PackageDecls,
                ])),
              }
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
      write!(f, "package {} where\n", self.name)?;
    }
    else {
      write!(f, "{{#{}#}}\npackage {}where\n", self.docs, self.name)?;
    }
    for x in self.decls.clone() {
      write!(f, "{}\n", x)?;
    }
    Ok(())
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use crate::term::{
    tests::arbitrary_name,
    Def,
  };

  use crate::parse::package::{
    parse_package,
    PackageEnv,
  };

  use hashexpr::span::Span;
  use std::path::PathBuf;

  pub fn test_package() -> Package {
    let source = "package Test where\n def id (A: Type) (x: A): A := x";
    let source_link = text!(String::from(source)).link();
    let (_, (_, p)) = parse_package(
      PackageEnv::new(PathBuf::from("Test.ya")),
      source_link,
    )(Span::new(source))
    .unwrap();
    p
  }

  // impl Arbitrary for Package {
  //  fn arbitrary<G: Gen>(g: &mut G) -> Self {
  //    let name = arbitrary_name(g);
  //    Package {
  //      name,
  //      docs: String::from(""),
  //      imports: Imports::new(Vec::new()),
  //      defs: Arbitrary::arbitrary(g),
  //    }
  //  }
  //}

  //#[quickcheck]
  // fn package_encode_decode(x: Package) -> bool {
  //  match Package::decode(x.clone().encode()) {
  //    Ok(y) => x == y,
  //    _ => false,
  //  }
  //}
}
