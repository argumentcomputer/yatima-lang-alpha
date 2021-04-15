use hashexpr::{atom, atom::Atom::*, Expr, Expr::*, Link};

use crate::{
    decode_error::{DecodeError, Expected},
    hashspace::{Hashspace, HashspaceDependent, HashspaceImplWrapper},
    term::{Def, Defs, Refs},
    unembed_error::UnembedError,
};
use im::{HashMap, HashSet};
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
    Defn {
        name: String,
        defn: Link,
        term: Link,
    },
    Open {
        name: String,
        alias: String,
        with: Option<Vec<String>>,
        from: Link,
    },
    // Data { name: String, typ_: Term, ctors: HashMap<String, Term> },
}

impl Declaration {
    #[must_use]
    pub fn encode(self) -> Expr {
        match self {
            Self::Defn { name, defn, term } => {
                cons!(None, text!("defn"), text!(name), link!(defn), link!(term))
            }
            Self::Open {
                name,
                alias,
                with,
                from,
            } => match with {
                Some(ns) => {
                    let mut xs = Vec::new();
                    for n in ns {
                        xs.push(text!(n))
                    }
                    cons!(
                        None,
                        text!("open"),
                        text!(name),
                        text!(alias),
                        Expr::Cons(None, xs),
                        link!(from)
                    )
                }
                None => {
                    cons!(None, text!("open"), text!(name), text!(alias), link!(from))
                }
            },
        }
    }

    pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
        match expr {
            Cons(pos, xs) => match xs.as_slice() {
                [Atom(_, Text(c)), Atom(_, Text(n)), Atom(_, Link(d)), Atom(_, Link(a))]
                    if *c == "defn" =>
                {
                    Ok(Self::Defn {
                        name: n.clone(),
                        defn: *d,
                        term: *a,
                    })
                }
                [Atom(_, Text(c)), Atom(_, Text(n)), Atom(_, Text(a)), Cons(_, xs), Atom(_, Link(f))]
                    if *c == "open" =>
                {
                    let mut ns = Vec::new();
                    for x in xs {
                        match x {
                            Atom(_, Text(n)) => {
                                ns.push(n.clone());
                            }
                            _ => {
                                return Err(DecodeError::new(pos, vec![Expected::PackageOpenWith]));
                            }
                        }
                    }
                    Ok(Self::Open {
                        name: n.clone(),
                        alias: a.clone(),
                        with: Some(ns),
                        from: *f,
                    })
                }
                [Atom(_, Text(c)), Atom(_, Text(n)), Atom(_, Text(a)), Atom(_, Link(f))]
                    if *c == "open" =>
                {
                    Ok(Self::Open {
                        name: n.clone(),
                        alias: a.clone(),
                        with: None,
                        from: *f,
                    })
                }
                _ => Err(DecodeError::new(pos, vec![Expected::PackageDefinition])),
            },
            x @ Atom(..) => Err(DecodeError::new(
                x.position(),
                vec![Expected::PackageDeclaration],
            )),
        }
    }
}

impl HashspaceDependent for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, hashspace: &Hashspace) -> fmt::Result {
        match self {
            Self::Defn { defn, .. } => {
                let def = Def::get_link(*defn, hashspace).expect("unembed error");
                write!(f, "{}", def)
            }
            Self::Open {
                name,
                alias,
                with,
                from,
            } => {
                let with = match with {
                    None => String::from(" "),
                    Some(ns) => {
                        let mut s = String::from("(");
                        let mut ns = ns.iter().peekable();
                        while let Some(n) = ns.next() {
                            s.push_str(n);
                            if ns.peek().is_some() {
                                s.push_str(", ")
                            } else {
                                s.push_str(") ");
                            }
                        }
                        s
                    }
                };
                if alias.is_empty() {
                    write!(f, "open {} {}from {}", name, with, from)
                } else {
                    write!(f, "open {} as {} {}from {}", name, alias, with, from)
                }
            }
        }
    }
}

impl Package {
    #[must_use]
    pub fn encode(self) -> Expr {
        let mut xs = Vec::new();
        for d in self.decls {
            xs.push(d.encode());
        }
        cons!(
            None,
            text!("package"),
            text!(self.name),
            text!(self.docs),
            link!(self.source),
            Expr::Cons(None, xs)
        )
    }

    pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
        match expr {
            Cons(pos, xs) => match xs.as_slice() {
                [Atom(_, Text(c)), tail @ ..] if *c == "package" => match tail {
                    [Atom(_, Text(n)), Atom(_, Text(d)), Atom(_, Link(s)), ds] => {
                        let mut decls = Vec::new();
                        match ds {
                            Cons(_, xs) => {
                                for x in xs {
                                    let decl = Declaration::decode(x.clone())?;
                                    decls.push(decl);
                                }
                                Ok(Self {
                                    name: n.clone(),
                                    docs: d.clone(),
                                    source: *s,
                                    decls,
                                })
                            }
                            expr @ Atom(..) => Err(DecodeError::new(
                                expr.position(),
                                vec![Expected::PackageDecls],
                            )),
                        }
                    }
                    _ => Err(DecodeError::new(pos, vec![Expected::PackageContents])),
                },
                _ => Err(DecodeError::new(pos, vec![Expected::Package])),
            },
            Atom(..) => Err(DecodeError::new(expr.position(), vec![Expected::Package])),
        }
    }

    pub fn get_link(pack: Link, hashspace: &Hashspace) -> Result<Self, UnembedError> {
        let pack = hashspace.get(pack).ok_or(UnembedError::UnknownLink(pack))?;
        Self::decode(pack).map_err(UnembedError::DecodeError)
    }

    pub fn refs_defs(self, hashspace: &Hashspace) -> Result<(Refs, Defs), UnembedError> {
        let mut refs: Refs = HashMap::new();
        let mut defs: Defs = HashMap::new();
        for d in self.decls {
            match d {
                Declaration::Defn { name, defn, term } => {
                    refs.insert(name, (defn, term));
                    let def = Def::get_link(defn, hashspace)?;
                    defs.insert(defn, def);
                }
                Declaration::Open {
                    alias, from, with, ..
                } => {
                    let pack = hashspace.get(from).ok_or(UnembedError::UnknownLink(from))?;
                    let pack = Self::decode(pack).map_err(UnembedError::DecodeError)?;
                    let (import_refs, import_defs) = pack.refs_defs(hashspace)?;
                    refs = merge_refs(refs, import_refs, &alias, with);
                    defs = merge_defs(defs, import_defs);
                }
            }
        }
        Ok((refs, defs))
    }
}

#[must_use]
pub fn merge_refs(left: Refs, right: Refs, alias: &str, with: Option<Vec<String>>) -> Refs {
    let mut refs = right;
    if let Some(ns) = with {
        let set: HashSet<String> = ns.iter().collect();
        refs.retain(|k, _| set.contains(k));
        if !alias.is_empty() {
            refs = refs
                .iter()
                .map(|(k, v)| (format!("{}.{}", alias, k), *v))
                .collect();
        }
        left.union_with(refs, |_, right| right)
    } else {
        if !alias.is_empty() {
            refs = refs
                .iter()
                .map(|(k, v)| (format!("{}.{}", alias, k), *v))
                .collect();
        }
        left.union_with(refs, |_, right| right)
    }
}

#[must_use]
pub fn merge_defs(left: Defs, right: Defs) -> Defs {
    left.union(right)
}

impl HashspaceDependent for Package {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, hashspace: &Hashspace) -> fmt::Result {
        if self.docs.is_empty() {
            writeln!(f, "package {} where", self.name)?;
        } else {
            writeln!(f, "{{#{}#}}\npackage {}where", self.docs, self.name)?;
        }
        for x in self.decls.clone() {
            writeln!(f, "{}", HashspaceImplWrapper::wrap(hashspace, &x))?;
        }
        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    // use quickcheck::{
    //   Arbitrary,
    //   Gen,
    // };

    // use crate::term::{
    //   tests::arbitrary_name,
    //   Def,
    // };

    use crate::parse::package::{parse_package, PackageEnv};

    use crate::hashspace;
    use hashexpr::span::Span;
    use std::path::PathBuf;

    #[must_use]
    pub fn test_package() -> Package {
        let hashspace = hashspace::Hashspace::local();
        let source = "package Test where\n def id (A: Type) (x: A): A := x";
        let source_link = text!(String::from(source)).link();
        let (_, (_, p, ..)) = parse_package(
            Some(PackageEnv::new(PathBuf::from("Test.ya"))),
            source_link,
            &hashspace,
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
