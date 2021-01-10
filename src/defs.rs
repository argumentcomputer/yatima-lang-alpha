use hashexpr::{
  atom::Atom::*,
  link::Link,
  position::Pos,
  Expr,
  Expr::*,
};

use im::Vector;
use std::{
  collections::HashMap,
  fmt,
};

use crate::{
  decode_error::{
    DecodeError,
    Expected,
  },
  hashspace::embed::desaturate_term,
  term::Term,
};

#[derive(Clone, Debug)]
pub struct Def {
  pub pos: Option<Pos>,
  pub name: String,
  pub doc: String,
  pub typ_: Term,
  pub term: Term,
}

impl PartialEq for Def {
  fn eq(&self, other: &Def) -> bool {
    self.name == other.name
      && self.doc == other.doc
      && self.typ_ == other.typ_
      && self.term == other.term
  }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Defs {
  pub defs: Vec<Def>,
}

/// A map of names to pairs of links. The first link is to the dag::Def
/// (expression definition with metadata), the
/// second is to the dag::Anon (anonymous computational term).
pub type Refs = HashMap<String, (Link, Link)>;

impl Def {
  pub fn new(
    pos: Option<Pos>,
    name: String,
    doc: String,
    typ_: Term,
    term: Term,
  ) -> Self {
    Def { pos, name, doc, typ_, term }
  }

  pub fn encode(self) -> Expr {
    Expr::Cons(self.pos, vec![
      atom!(symb!("def")),
      atom!(symb!(self.name)),
      atom!(text!(self.doc)),
      Term::encode(self.typ_),
      Term::encode(self.term),
    ])
  }

  pub fn decode(
    refs: HashMap<String, (Link, Link)>,
    expr: Expr,
  ) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("def") => {
          match tail {
            [Atom(_, Symbol(name)), Atom(_, Text(doc, None)), typ_, term] => {
              let mut ctx = Vector::new();
              let typ_ =
                Term::decode(refs.to_owned(), ctx.to_owned(), typ_.to_owned())?;
              ctx.push_front(name.clone());
              let term = Term::decode(refs, ctx, term.to_owned())?;
              Ok(Def::new(
                pos.to_owned(),
                name.to_owned(),
                doc.to_owned(),
                typ_,
                term,
              ))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::DefinitionContents])),
          }
        }
        _ => Err(DecodeError::new(pos, vec![Expected::Definition])),
      },
      _ => Err(DecodeError::new(expr.position(), vec![Expected::Definition])),
    }
  }
}

impl Defs {
  pub fn encode(self) -> Expr {
    let mut defs = Vec::new();
    for d in self.defs {
      defs.push(d.encode())
    }
    Expr::Cons(None, defs)
  }

  pub fn decode(
    mut refs: HashMap<String, (Link, Link)>,
    expr: Expr,
  ) -> Result<(Defs, HashMap<String, (Link, Link)>), DecodeError> {
    match expr {
      Cons(pos, xs) => {
        let mut defs = Vec::new();
        for x in xs {
          let def = Def::decode(refs.clone(), x)?;
          if refs.contains_key(&def.name) {
            return Err(DecodeError::new(pos, vec![
              Expected::UniqueDefinitionName,
            ]));
          }
          let def_link = def.clone().encode().link();
          let ast_link = desaturate_term(def.term.clone()).0.encode().link();
          refs.insert(def.name.clone(), (def_link, ast_link));
          defs.push(def);
        }
        Ok((Defs { defs }, refs))
      }
      _ => {
        Err(DecodeError::new(expr.position(), vec![Expected::DefinitionList]))
      }
    }
  }
}

impl fmt::Display for Def {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.doc.is_empty() {
      write!(f, "def {} : {} = {}", self.name, self.typ_, self.term)
    }
    else {
      write!(
        f,
        "//{}\n def {} : {} = {}",
        self.doc, self.name, self.typ_, self.term
      )
    }
  }
}

impl fmt::Display for Defs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      self.defs.iter().map(|x| format!("{}\n", x)).collect::<String>()
    )
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

  use crate::term::tests::{
    arbitrary_name,
    arbitrary_term,
    test_refs,
  };

  fn arbitrary_def<G: Gen>(g: &mut G, refs: Refs, name: String) -> Def {
    let mut ctx = Vector::new();
    ctx.push_front(name.clone());
    Def {
      pos: None,
      name,
      doc: String::from(""),
      typ_: arbitrary_term(g, refs.clone(), Vector::new()),
      term: arbitrary_term(g, refs, ctx),
    }
  }

  impl Arbitrary for Def {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let name = arbitrary_name(g);
      arbitrary_def(g, HashMap::new(), name)
    }
  }
  impl Arbitrary for Defs {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let n = g.gen_range(0, 3);
      let mut defs: Vec<Def> = Vec::new();
      let mut refs: Refs = HashMap::new();
      for _ in 0..n {
        let mut nam: String = arbitrary_name(g);
        while refs.clone().contains_key(&nam) {
          nam = arbitrary_name(g);
        }
        let def = arbitrary_def(g, refs.clone(), nam.clone());
        let def_link = def.clone().encode().link();
        let ast_link =
          crate::hashspace::embed::desaturate_term(def.term.clone())
            .0
            .encode()
            .link();
        refs.insert(nam.clone(), (def_link, ast_link));
        defs.push(def)
      }
      Defs { defs }
    }
  }

  #[quickcheck]
  fn def_encode_decode(x: Def) -> bool {
    match Def::decode(HashMap::new(), x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn defs_encode_decode(x: Defs) -> bool {
    match Defs::decode(HashMap::new(), x.clone().encode()) {
      Ok((y, _)) => x == y,
      _ => false,
    }
  }

  #[test]
  fn test_cases() {
    use crate::term::{
      uses::Uses,
      Term,
      Term::*,
    };
    let typ_ = All(
      None,
      Uses::Many,
      String::from("A"),
      Box::new(Typ(None)),
      Box::new(Var(None, String::from("A"), 0)),
    );
    let term =
      Lam(None, String::from("x"), Box::new(Var(None, String::from("x"), 0)));
    let def = Def {
      pos: None,
      name: String::from("id"),
      doc: String::from(""),
      typ_: typ_.clone(),
      term: term.clone(),
    };
    let inp = "(def id \"\" (forall ω A Type A) (lambda x x))";
    assert_eq!(inp, format!("{}", def.clone().encode()));

    assert_eq!(
      def,
      Def::decode(HashMap::new(), hashexpr::parse(inp).unwrap().1).unwrap()
    );

    let def2 = Def {
      pos: None,
      name: String::from("id2"),
      doc: String::from(""),
      typ_: typ_.clone(),
      term: term.clone(),
    };

    let defs = Defs { defs: vec![def, def2] };
    let inp = "((def id \"\" (forall ω A Type A) (lambda x x)) (def id2 \"\" \
               (forall ω A Type A) (lambda x x)))";

    assert_eq!(inp, format!("{}", defs.clone().encode()));

    assert_eq!(
      defs,
      Defs::decode(HashMap::new(), hashexpr::parse(inp).unwrap().1).unwrap().0
    );
  }
}
