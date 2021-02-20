use hashexpr::{
  AVal,
  AVal::*,
  Expr,
  Expr::*,
  Link,
};

use crate::{
  decode_error::{
    DecodeError,
    Expected,
  },
  hashspace,
  term::{
    Def,
    Defs,
  },
  unembed_error::UnembedError,
};
use im::OrdMap;
use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub struct Package {
  pub name: String,
  pub docs: String,
  pub source: Link,
  pub decls: Vec<Declaration>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Declaration {
  Defn { name: String, defn: Link, term: Link },
  Open { name: String, alias: String, from: Link },
  // Data { name: String, typ_: Term, ctors: HashMap<String, Term> },
}

impl Declaration {
  pub fn encode(self) -> Expr {
    match self {
      Self::Defn { name, defn, term } => {
        cons!(None, symb!("defn"), symb!(name), link!(defn), link!(term))
      }
      Self::Open { name, alias, from } => {
        cons!(None, symb!("open"), symb!(name), symb!(alias), link!(from))
      }
    }
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(c)), Atom(_, Symbol(n)), Atom(_, Link(d)), Atom(_, Link(a))]
          if *c == String::from("defn") =>
        {
          Ok(Self::Defn { name: n.to_owned(), defn: *d, term: *a })
        }
        [Atom(_, Symbol(c)), Atom(_, Symbol(n)), Atom(_, Symbol(a)), Atom(_, Link(f))]
          if *c == String::from("open") =>
        {
          Ok(Self::Open { name: n.to_owned(), alias: a.to_owned(), from: *f })
        }
        _ => Err(DecodeError::new(pos, vec![Expected::PackageDefinition])),
      },
      x => {
        Err(DecodeError::new(x.position(), vec![Expected::PackageDeclaration]))
      }
    }
  }
}

impl fmt::Display for Declaration {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Defn { defn, .. } => {
        let def = Def::unembed_link(*defn).expect("unembed error");
        write!(f, "{}", def)
      }
      Self::Open { name, alias, from } => {
        if *alias == String::from("") {
          write!(f, "open {} from {}", name, from)
        }
        else {
          write!(f, "open {} as {} from {}", name, alias, from)
        }
      }
    }
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
      Expr::Cons(None, xs)
    )
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(c)), tail @ ..] if *c == String::from("package") => {
          match tail {
            [Atom(_, Symbol(n)), Atom(_, Text(d)), Atom(_, Link(s)), ds] => {
              let mut decls = Vec::new();
              match ds {
                Cons(_, xs) => {
                  for x in xs {
                    let decl = Declaration::decode(x.to_owned())?;
                    decls.push(decl);
                  }
                  Ok(Package {
                    name: n.to_owned(),
                    docs: d.to_owned(),
                    source: s.to_owned(),
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

  pub fn defs(self) -> Result<Defs, UnembedError> {
    let mut defs: Defs = OrdMap::new();
    for d in self.decls {
      match d {
        Declaration::Defn { name, defn, term } => {
          defs.insert(name, (defn, term));
        }
        Declaration::Open { alias, from, .. } => {
          let pack =
            hashspace::get(from).ok_or(UnembedError::UnknownLink(from))?;
          let pack =
            Package::decode(pack).map_err(|e| UnembedError::DecodeError(e))?;
          let import_defs = pack.defs()?;
          defs = merge_defs(defs, import_defs, alias)
        }
      }
    }
    Ok(defs)
  }
}

pub fn merge_defs(left: Defs, right: Defs, alias: String) -> Defs {
  let defs = if alias != String::from("") {
    right.iter().map(|(k, v)| (format!("{}.{}", alias, k), *v)).collect()
  }
  else {
    right
  };
  left.union_with(defs, |_, right| right)
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
    let (_, (_, _, p)) = parse_package(
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
