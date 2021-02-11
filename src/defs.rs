use hashexpr::{
  atom::Atom::*,
  link::Link,
  position::Pos,
  Expr,
  Expr::*,
};

use im::{
  HashMap,
  OrdMap,
  Vector,
};
use std::fmt;

use crate::{
  decode_error::{
    DecodeError,
    Expected,
  },
  hashspace::embed::embed_term,
  term::Term,
};

//#[derive(PartialEq, Clone, Debug)]
// pub struct Defs {
//  pub defs: OrdMap<LInk, Def>,
//}

// impl Defs {
//  pub fn encode(self) -> Expr {
//    let mut defs = Vec::new();
//    for (n, d) in self.defs {
//      defs.push(cons!(None, atom!(symb!(n)), d.encode()))
//    }
//    Expr::Cons(None, defs)
//  }
//
//  pub fn decode(
//    mut refs: HashMap<String, Link>,
//    expr: Expr,
//  ) -> Result<(Defs, HashMap<String, Link>), DecodeError> {
//    match expr {
//      Cons(pos, xs) => {
//        let mut defs = Vec::new();
//        for x in xs {
//          let def = Def::decode(refs.clone(), x)?;
//          if refs.contains_key(&def.name) {
//            return Err(DecodeError::new(pos, vec![
//              Expected::UniqueDefinitionName,
//            ]));
//          }
//          let link = embed_term(def.term.clone()).0.encode().link();
//          refs.insert(def.name.clone(), link);
//          defs.push(def);
//        }
//        Ok((Defs { defs }, refs))
//      }
//      _ => {
//        Err(DecodeError::new(expr.position(), vec![Expected::DefinitionList]))
//      }
//    }
//  }
//}

impl fmt::Display for Def {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.docs.is_empty() {
      write!(f, "def {} : {} = {}", self.name, self.typ_, self.term)
    }
    else {
      write!(
        f,
        "//{}\n def {} : {} = {}",
        self.docs, self.name, self.typ_, self.term
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
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;

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
        let link = embed_term(def.term.clone()).0.encode().link();
        refs.insert(nam.clone(), link);
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
      Term,
      Term::*,
      Uses,
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
      docs: String::from(""),
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
      docs: String::from(""),
      typ_: typ_.clone(),
      term: term.clone(),
    };

    // let defs = Defs { defs: vec![def, def2] };
    // let inp = "((def id \"\" (forall ω A Type A) (lambda x x)) (def id2 \"\"
    // \           (forall ω A Type A) (lambda x x)))";

    // assert_eq!(inp, format!("{}", defs.clone().encode()));

    // assert_eq!(
    //  defs,
    //  Defs::decode(HashMap::new(), hashexpr::parse(inp).unwrap().1).unwrap().0
    //);
  }
}
