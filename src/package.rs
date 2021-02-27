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
    Refs,
  },
  unembed_error::UnembedError,
};
use im::{
  HashMap,
  HashSet,
};
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
  Open { name: String, alias: String, with: Option<Vec<String>>, from: Link },
  // Data { name: String, typ_: Term, ctors: HashMap<String, Term> },
}

impl Declaration {
  pub fn encode(self) -> Expr {
    match self {
      Self::Defn { name, defn, term } => {
        cons!(None, symb!("defn"), symb!(name), link!(defn), link!(term))
      }
      Self::Open { name, alias, with, from } => match with {
        Some(ns) => {
          let mut xs = Vec::new();
          for n in ns {
            xs.push(symb!(n))
          }
          cons!(
            None,
            symb!("open"),
            symb!(name),
            symb!(alias),
            Expr::Cons(None, xs),
            link!(from)
          )
        }
        None => {
          cons!(None, symb!("open"), symb!(name), symb!(alias), link!(from))
        }
      },
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
        [Atom(_, Symbol(c)), Atom(_, Symbol(n)), Atom(_, Symbol(a)), Cons(_, xs), Atom(_, Link(f))]
          if *c == String::from("open") =>
        {
          let mut ns = Vec::new();
          for x in xs {
            match x {
              Atom(_, Symbol(n)) => {
                ns.push(n.to_owned());
              }
              _ => {
                return Err(DecodeError::new(pos, vec![
                  Expected::PackageOpenWith,
                ]));
              }
            }
          }
          let with = Some(ns.to_owned());
          Ok(Self::Open {
            name: n.to_owned(),
            alias: a.to_owned(),
            with,
            from: *f,
          })
        }
        [Atom(_, Symbol(c)), Atom(_, Symbol(n)), Atom(_, Symbol(a)), Atom(_, Link(f))]
          if *c == String::from("open") =>
        {
          Ok(Self::Open {
            name: n.to_owned(),
            alias: a.to_owned(),
            with: None,
            from: *f,
          })
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
        let def = Def::get_link(*defn).expect("unembed error");
        write!(f, "{}", def)
      }
      Self::Open { name, alias, with, from } => {
        let with = match with {
          None => String::from(" "),
          Some(ns) => {
            let mut s = String::from("(");
            for n in ns {
              s.push_str(n);
              s.push_str(", ");
            }
            s.push_str(") ");
            s
          }
        };
        if *alias == String::from("") {
          write!(f, "open {} {}from {}", name, with, from)
        }
        else {
          write!(f, "open {} as {} {}from {}", name, alias, with, from)
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

  pub fn get_link(pack: Link) -> Result<Self, UnembedError> {
    let pack = hashspace::get(pack).ok_or(UnembedError::UnknownLink(pack))?;
    Package::decode(pack).map_err(|e| UnembedError::DecodeError(e))
  }

  pub fn refs_defs(self) -> Result<(Refs, Defs), UnembedError> {
    let mut refs: Refs = HashMap::new();
    let mut defs: Defs = HashMap::new();
    for d in self.decls {
      match d {
        Declaration::Defn { name, defn, term } => {
          refs.insert(name, (defn, term));
          let def = Def::get_link(defn)?;
          defs.insert(defn, def);
        }
        Declaration::Open { alias, from, with, .. } => {
          let pack =
            hashspace::get(from).ok_or(UnembedError::UnknownLink(from))?;
          let pack =
            Package::decode(pack).map_err(|e| UnembedError::DecodeError(e))?;
          let (import_refs, import_defs) = pack.refs_defs()?;
          refs = merge_refs(refs, import_refs, alias, with);
          defs = merge_defs(defs, import_defs);
        }
      }
    }
    Ok((refs, defs))
  }
}

pub fn merge_refs(
  left: Refs,
  right: Refs,
  alias: String,
  with: Option<Vec<String>>,
) -> Refs {
  let mut refs = right;
  match with {
    Some(ns) => {
      let set: HashSet<String> = ns.iter().collect();
      refs.retain(|k, _| set.contains(k));
      if alias != String::from("") {
        refs =
          refs.iter().map(|(k, v)| (format!("{}.{}", alias, k), *v)).collect();
      }
      left.union_with(refs, |_, right| right)
    }
    None => {
      if alias != String::from("") {
        refs =
          refs.iter().map(|(k, v)| (format!("{}.{}", alias, k), *v)).collect();
      }
      left.union_with(refs, |_, right| right)
    }
  }
}

pub fn merge_defs(left: Defs, right: Defs) -> Defs { left.union(right) }

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
    let (_, (_, p, ..)) = parse_package(
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
