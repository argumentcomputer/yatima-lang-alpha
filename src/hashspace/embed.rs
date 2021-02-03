use crate::{
  decode_error::{
    DecodeError,
    Expected,
  },
  defs,
  hashspace,
  hashspace::{
    anon_term::AnonTerm,
    cache::Cache,
    meta_term::MetaTerm,
  },
  imports::Imports,
  term::{
    LitType,
    Literal,
    PrimOp,
    Term,
    Term::*,
    Uses,
  },
};

use hashexpr::{
  position::Pos,
  Expr,
  Expr::{
    Atom,
    Cons,
  },
  Link,
  Symbol,
  Text,
};

use im::{
  HashMap,
  Vector,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Package {
  name: String,
  docs: String,
  source: Link,
  imports: Imports,
  defs: Defs,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Defs {
  defs: Vec<Def>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Def {
  name: String,
  pos: Option<Pos>,
  docs: String,
  type_anon: Link,
  term_anon: Link,
  type_meta: MetaTerm,
  term_meta: MetaTerm,
}

impl Def {
  pub fn encode(self) -> Expr {
    cons!(
      self.pos,
      atom!(symb!("def")),
      atom!(symb!(self.name)),
      atom!(text!(self.docs)),
      atom!(link!(self.type_anon)),
      atom!(link!(self.term_anon)),
      self.type_meta.encode(),
      self.term_meta.encode()
    )
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(c)), tail @ ..] if *c == String::from("def") => {
          match tail {
            [Atom(_, Symbol(n)), Atom(_, Text(d, _)), Atom(_, Link(t)), Atom(_, Link(x)), tm, xm] =>
            {
              let type_meta = MetaTerm::decode(tm.to_owned())?;
              let term_meta = MetaTerm::decode(xm.to_owned())?;
              Ok(Def {
                name: n.to_owned(),
                pos,
                docs: d.to_owned(),
                type_anon: *t,
                term_anon: *x,
                type_meta,
                term_meta,
              })
            }
            _ => Err(DecodeError::new(pos, vec![Expected::EmbedDefContents])),
          }
        }
        _ => Err(DecodeError::new(pos, vec![Expected::EmbedDef])),
      },
      _ => Err(DecodeError::new(expr.position(), vec![Expected::EmbedDef])),
    }
  }
}

impl Defs {
  pub fn encode(self) -> Expr {
    let mut xs = Vec::new();
    for d in self.defs {
      xs.push(d.encode());
    }
    Expr::Cons(None, xs)
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    let mut defs = Vec::new();
    match expr {
      Cons(_, xs) => {
        for x in xs {
          let d = Def::decode(x)?;
          defs.push(d);
        }
        Ok(Defs { defs })
      }
      _ => Err(DecodeError::new(expr.position(), vec![Expected::EmbedDefs])),
    }
  }
}

impl Package {
  pub fn encode(self) -> Expr {
    cons!(
      None,
      atom!(symb!("package")),
      atom!(symb!(self.name)),
      atom!(text!(self.docs)),
      atom!(link!(self.source)),
      self.imports.encode(),
      self.defs.encode()
    )
  }

  pub fn decode(expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(c)), tail @ ..] if *c == String::from("package") => {
          match tail {
            [Atom(_, Symbol(n)), Atom(_, Text(d, _)), Atom(_, Link(s)), x, y] =>
            {
              let imports = Imports::decode(x.to_owned())?;
              let defs = Defs::decode(y.to_owned())?;
              Ok(Package {
                name: n.to_owned(),
                docs: d.to_owned(),
                source: s.to_owned(),
                imports,
                defs,
              })
            }
            _ => {
              Err(DecodeError::new(pos, vec![Expected::EmbedPackageContents]))
            }
          }
        }
        _ => Err(DecodeError::new(pos, vec![Expected::EmbedPackage])),
      },
      _ => Err(DecodeError::new(expr.position(), vec![Expected::EmbedPackage])),
    }
  }
}

pub fn embed_term(term: Term) -> (AnonTerm, MetaTerm) {
  match term {
    Var(pos, _, idx) => (
      AnonTerm::Ctor(String::from("var"), vec![AnonTerm::Vari(idx)]),
      MetaTerm::Ctor(pos, vec![MetaTerm::Leaf]),
    ),
    Ref(pos, name, link) => (
      AnonTerm::Ctor(String::from("ref"), vec![AnonTerm::Link(link)]),
      MetaTerm::Ctor(pos, vec![MetaTerm::Link(name)]),
    ),
    Lit(pos, lit) => (
      AnonTerm::Ctor(String::from("lit"), vec![AnonTerm::Data(
        lit.encode().serialize(),
      )]),
      MetaTerm::Ctor(pos, vec![MetaTerm::Leaf]),
    ),
    LTy(pos, lty) => (
      AnonTerm::Ctor(String::from("lty"), vec![AnonTerm::Data(
        lty.encode().serialize(),
      )]),
      MetaTerm::Ctor(pos, vec![MetaTerm::Leaf]),
    ),
    Opr(pos, opr) => (
      AnonTerm::Ctor(String::from("opr"), vec![AnonTerm::Data(
        opr.encode().serialize(),
      )]),
      MetaTerm::Ctor(pos, vec![MetaTerm::Leaf]),
    ),
    Typ(pos) => {
      (AnonTerm::Ctor(String::from("typ"), vec![]), MetaTerm::Ctor(pos, vec![]))
    }
    Lam(pos, name, body) => {
      let (anon, meta) = embed_term(*body);
      (
        AnonTerm::Ctor(String::from("lam"), vec![AnonTerm::Bind(Box::new(
          anon,
        ))]),
        MetaTerm::Ctor(pos, vec![MetaTerm::Bind(
          name.to_owned(),
          Box::new(meta),
        )]),
      )
    }
    Slf(pos, name, body) => {
      let (anon, meta) = embed_term(*body);
      (
        AnonTerm::Ctor(String::from("slf"), vec![AnonTerm::Bind(Box::new(
          anon,
        ))]),
        MetaTerm::Ctor(pos, vec![MetaTerm::Bind(
          name.to_owned(),
          Box::new(meta),
        )]),
      )
    }
    App(pos, fun, arg) => {
      let (fun_anon, fun_meta) = embed_term(*fun);
      let (arg_anon, arg_meta) = embed_term(*arg);
      (
        AnonTerm::Ctor(String::from("app"), vec![fun_anon, arg_anon]),
        MetaTerm::Ctor(pos, vec![fun_meta, arg_meta]),
      )
    }
    Ann(pos, val, typ) => {
      let (val_anon, val_meta) = embed_term(*val);
      let (typ_anon, typ_meta) = embed_term(*typ);
      (
        AnonTerm::Ctor(String::from("ann"), vec![val_anon, typ_anon]),
        MetaTerm::Ctor(pos, vec![val_meta, typ_meta]),
      )
    }
    Dat(pos, body) => {
      let (anon, meta) = embed_term(*body);
      (
        AnonTerm::Ctor(String::from("dat"), vec![anon]),
        MetaTerm::Ctor(pos, vec![meta]),
      )
    }
    Cse(pos, body) => {
      let (anon, meta) = embed_term(*body);
      (
        AnonTerm::Ctor(String::from("cse"), vec![anon]),
        MetaTerm::Ctor(pos, vec![meta]),
      )
    }
    All(pos, uses, name, typ_, body) => {
      let (typ_anon, typ_meta) = embed_term(*typ_);
      let (bod_anon, bod_meta) = embed_term(*body);
      (
        AnonTerm::Ctor(String::from("all"), vec![
          AnonTerm::Data(uses.encode().serialize()),
          typ_anon,
          bod_anon,
        ]),
        MetaTerm::Ctor(pos, vec![
          MetaTerm::Leaf,
          typ_meta,
          MetaTerm::Bind(name, Box::new(bod_meta)),
        ]),
      )
    }
    Let(pos, true, uses, name, typ_, expr, body) => {
      let (typ_anon, typ_meta) = embed_term(*typ_);
      let (exp_anon, exp_meta) = embed_term(*expr);
      let (bod_anon, bod_meta) = embed_term(*body);
      (
        AnonTerm::Ctor(String::from("rec"), vec![
          AnonTerm::Data(uses.encode().serialize()),
          typ_anon,
          AnonTerm::Bind(Box::new(exp_anon)),
          AnonTerm::Bind(Box::new(bod_anon)),
        ]),
        MetaTerm::Ctor(pos, vec![
          MetaTerm::Leaf,
          typ_meta,
          MetaTerm::Bind(name.clone(), Box::new(exp_meta)),
          MetaTerm::Bind(name, Box::new(bod_meta)),
        ]),
      )
    }
    Let(pos, false, uses, name, typ_, expr, body) => {
      let (typ_anon, typ_meta) = embed_term(*typ_);
      let (exp_anon, exp_meta) = embed_term(*expr);
      let (bod_anon, bod_meta) = embed_term(*body);
      (
        AnonTerm::Ctor(String::from("let"), vec![
          AnonTerm::Data(uses.encode().serialize()),
          typ_anon,
          exp_anon,
          AnonTerm::Bind(Box::new(bod_anon)),
        ]),
        MetaTerm::Ctor(pos, vec![
          MetaTerm::Leaf,
          typ_meta,
          exp_meta,
          MetaTerm::Bind(name, Box::new(bod_meta)),
        ]),
      )
    }
  }
}

#[derive(Clone, Debug)]
pub enum EmbeddingError {
  FreeVariable,
  DecodeError(DecodeError),
  DeserialError,
  UnexpectedCtor(AnonTerm, MetaTerm),
  UnknownLink(Link),
  BadLet,
}

pub fn unembed_term(
  ctx: Vector<String>,
  anon_term: &AnonTerm,
  name_meta: &MetaTerm,
) -> Result<Term, EmbeddingError> {
  match (anon_term, name_meta) {
    (AnonTerm::Ctor(n, xs), MetaTerm::Ctor(pos, ys)) => {
      match (&n[..], xs.as_slice(), ys.as_slice()) {
        ("var", [AnonTerm::Vari(idx)], [MetaTerm::Leaf]) => {
          match ctx.iter().enumerate().find(|(i, _)| (*i as u64) == *idx) {
            Some((_, n)) => Ok(Term::Var(*pos, n.to_owned(), *idx)),
            None => Err(EmbeddingError::FreeVariable),
          }
        }
        ("ref", [AnonTerm::Link(link)], [MetaTerm::Link(name)]) => {
          Ok(Term::Ref(*pos, name.clone(), *link))
        }
        ("lit", [AnonTerm::Data(data)], [MetaTerm::Leaf]) => {
          let (_, lit) = hashexpr::Expr::deserialize(&data)
            .map_err(|_| EmbeddingError::DeserialError)?;
          let lit =
            Literal::decode(lit).map_err(|e| EmbeddingError::DecodeError(e))?;
          Ok(Term::Lit(*pos, lit))
        }
        ("lty", [AnonTerm::Data(data)], [MetaTerm::Leaf]) => {
          let (_, lty) = hashexpr::Expr::deserialize(&data)
            .map_err(|_| EmbeddingError::DeserialError)?;
          let lty =
            LitType::decode(lty).map_err(|e| EmbeddingError::DecodeError(e))?;
          Ok(Term::LTy(*pos, lty))
        }
        ("opr", [AnonTerm::Data(data)], [MetaTerm::Leaf]) => {
          let (_, opr) = hashexpr::Expr::deserialize(&data)
            .map_err(|_| EmbeddingError::DeserialError)?;
          let opr =
            PrimOp::decode(opr).map_err(|e| EmbeddingError::DecodeError(e))?;
          Ok(Term::Opr(*pos, opr))
        }
        ("typ", [], []) => Ok(Term::Typ(*pos)),
        ("dat", [anon], [meta]) => {
          let body = unembed_term(ctx, anon, meta)?;
          Ok(Term::Dat(*pos, Box::new(body)))
        }
        ("cse", [anon], [meta]) => {
          let body = unembed_term(ctx, anon, meta)?;
          Ok(Term::Cse(*pos, Box::new(body)))
        }
        ("lam", [AnonTerm::Bind(anon)], [MetaTerm::Bind(n, meta)]) => {
          let mut new_ctx = ctx.clone();
          new_ctx.push_front(n.clone());
          let body = unembed_term(new_ctx, &anon, meta)?;
          Ok(Term::Lam(*pos, n.clone(), Box::new(body)))
        }
        ("slf", [AnonTerm::Bind(anon)], [MetaTerm::Bind(n, meta)]) => {
          let mut new_ctx = ctx.clone();
          new_ctx.push_front(n.clone());
          let body = unembed_term(new_ctx, &anon, meta)?;
          Ok(Term::Slf(*pos, n.clone(), Box::new(body)))
        }
        ("app", [fanon, aanon], [fmeta, ameta]) => {
          let fun = unembed_term(ctx.clone(), fanon, fmeta)?;
          let arg = unembed_term(ctx.clone(), aanon, ameta)?;
          Ok(Term::App(*pos, Box::new(fun), Box::new(arg)))
        }
        ("ann", [xanon, tanon], [xmeta, tmeta]) => {
          let xpr = unembed_term(ctx.clone(), xanon, xmeta)?;
          let typ = unembed_term(ctx.clone(), tanon, tmeta)?;
          Ok(Term::Ann(*pos, Box::new(xpr), Box::new(typ)))
        }
        (
          "all",
          [AnonTerm::Data(uses), tanon, banon],
          [MetaTerm::Leaf, tmeta, MetaTerm::Bind(n, bmeta)],
        ) => {
          let (_, uses) = hashexpr::Expr::deserialize(&uses)
            .map_err(|e| EmbeddingError::DeserialError)?;
          let uses =
            Uses::decode(uses).map_err(|e| EmbeddingError::DecodeError(e))?;
          let typ_ = unembed_term(ctx.clone(), tanon, tmeta)?;
          let mut new_ctx = ctx.clone();
          new_ctx.push_front(n.clone());
          let body = unembed_term(new_ctx, banon, bmeta)?;
          Ok(Term::All(*pos, uses, n.clone(), Box::new(typ_), Box::new(body)))
        }
        (
          "rec",
          [AnonTerm::Data(uses), tanon, AnonTerm::Bind(xanon), AnonTerm::Bind(banon)],
          [MetaTerm::Leaf, tmeta, MetaTerm::Bind(n1, xmeta), MetaTerm::Bind(n2, bmeta)],
        ) => {
          let name =
            if n1 == n2 { Ok(n1) } else { Err(EmbeddingError::BadLet) }?;
          let (_, uses) = hashexpr::Expr::deserialize(&uses)
            .map_err(|e| EmbeddingError::DeserialError)?;
          let uses =
            Uses::decode(uses).map_err(|e| EmbeddingError::DecodeError(e))?;
          let typ_ = unembed_term(ctx.clone(), tanon, tmeta)?;
          let mut new_ctx = ctx.clone();
          new_ctx.push_front(name.clone());
          let exp = unembed_term(new_ctx.clone(), &xanon, xmeta)?;
          let body = unembed_term(new_ctx, &banon, bmeta)?;
          Ok(Term::Let(
            *pos,
            true,
            uses,
            name.clone(),
            Box::new(typ_),
            Box::new(exp),
            Box::new(body),
          ))
        }
        (
          "let",
          [AnonTerm::Data(uses), tanon, xanon, AnonTerm::Bind(banon)],
          [MetaTerm::Leaf, tmeta, xmeta, MetaTerm::Bind(name, bmeta)],
        ) => {
          let (_, uses) = hashexpr::Expr::deserialize(&uses)
            .map_err(|e| EmbeddingError::DeserialError)?;
          let uses =
            Uses::decode(uses).map_err(|e| EmbeddingError::DecodeError(e))?;
          let typ_ = unembed_term(ctx.clone(), tanon, tmeta)?;
          let exp = unembed_term(ctx.clone(), xanon, xmeta)?;
          let mut new_ctx = ctx;
          new_ctx.push_front(name.clone());
          let body = unembed_term(new_ctx, &banon, bmeta)?;
          Ok(Term::Let(
            *pos,
            false,
            uses,
            name.clone(),
            Box::new(typ_),
            Box::new(exp),
            Box::new(body),
          ))
        }
        _ => Err(EmbeddingError::UnexpectedCtor(
          anon_term.clone(),
          name_meta.clone(),
        )),
      }
    }
    _ => {
      Err(EmbeddingError::UnexpectedCtor(anon_term.clone(), name_meta.clone()))
    }
  }
}

pub fn embed_def(
  def: defs::Def,
) -> (hashspace::embed::Def, AnonTerm, AnonTerm) {
  let (term_anon, term_meta) = embed_term(def.term);
  let (type_anon, type_meta) = embed_term(def.typ_);
  let d = Def {
    pos: def.pos,
    name: def.name,
    docs: def.docs,
    term_anon: term_anon.clone().encode().link(),
    type_anon: type_anon.clone().encode().link(),
    term_meta,
    type_meta,
  };
  (d, type_anon, term_anon)
}

pub fn unembed_def(
  def: hashspace::embed::Def,
  type_anon: AnonTerm,
  term_anon: AnonTerm,
) -> Result<defs::Def, EmbeddingError> {
  let typ_ = unembed_term(Vector::new(), &type_anon, &def.type_meta)?;
  let term = unembed_term(
    Vector::from(vec![def.name.clone()]),
    &term_anon,
    &def.term_meta,
  )?;
  Ok(defs::Def { pos: def.pos, name: def.name, docs: def.docs, typ_, term })
}

pub fn embed_defs(defs: defs::Defs) -> (hashspace::embed::Defs, Cache) {
  let mut ds = Vec::new();
  let mut cache = HashMap::new();
  for d in defs.defs {
    let (d, ta, xa) = embed_def(d);
    ds.push(d);
    let ta = ta.encode();
    cache.insert(ta.link(), ta);
    let xa = xa.encode();
    cache.insert(xa.link(), xa);
  }
  (hashspace::embed::Defs { defs: ds }, cache)
}

pub fn unembed_defs(
  defs: hashspace::embed::Defs,
  cache: Cache,
) -> Result<defs::Defs, EmbeddingError> {
  let mut ds = Vec::new();
  for d in defs.defs {
    let type_anon = cache
      .get(&d.type_anon)
      .ok_or(EmbeddingError::UnknownLink(d.type_anon))
      .and_then(|x| {
        AnonTerm::decode(x.to_owned())
          .map_err(|e| EmbeddingError::DecodeError(e))
      })?;
    let term_anon = cache
      .get(&d.term_anon)
      .ok_or(EmbeddingError::UnknownLink(d.type_anon))
      .and_then(|x| {
        AnonTerm::decode(x.to_owned())
          .map_err(|e| EmbeddingError::DecodeError(e))
      })?;
    let def = unembed_def(d, type_anon, term_anon)?;
    ds.push(def);
  }
  Ok(defs::Defs { defs: ds })
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use crate::{
    defs::tests::arbitrary_def,
    term::tests::{
      arbitrary_name,
      test_refs,
    },
  };

  #[quickcheck]
  fn term_embed_unembed(x: Term) -> bool {
    let (a, m) = embed_term(x.clone());
    match unembed_term(Vector::new(), &a, &m) {
      Ok(y) => {
        if x == y {
          true
        }
        else {
          //          println!("x: {:?}", x);
          //          println!("y: {:?}", y);
          false
        }
      }
      e => {
        //        println!("x: {:?}", x);
        //        println!("a: {:?}", a);
        //        println!("m: {:?}", m);
        //        println!("e: {:?}", e);
        false
      }
    }
  }

  impl Arbitrary for Def {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let name = arbitrary_name(g);
      embed_def(arbitrary_def(g, test_refs(), name)).0
    }
  }

  impl Arbitrary for Defs {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      embed_defs(Arbitrary::arbitrary(g)).0
    }
  }

  #[quickcheck]
  fn def_encode_decode(x: Def) -> bool {
    match Def::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn defs_encode_decode(x: Defs) -> bool {
    match Defs::decode(x.clone().encode()) {
      Ok(y) => x == y,
      e => {
        // println!("x: {:?}", x);
        // println!("xe: {:?}", x.encode());
        // println!("e: {:?}", e);
        false
      }
    }
  }

  #[quickcheck]
  fn def_embed_unembed(x: defs::Def) -> bool {
    let (d, ta, xa) = embed_def(x.clone());
    match unembed_def(d, ta, xa) {
      Ok(y) => {
        if x == y {
          true
        }
        else {
          //          println!("x: {:?}", x);
          //          println!("y: {:?}", y);
          false
        }
      }
      e => {
        //        println!("x: {:?}", x);
        //        println!("a: {:?}", a);
        //        println!("m: {:?}", m);
        //        println!("e: {:?}", e);
        false
      }
    }
  }

  #[quickcheck]
  fn defs_embed_unembed(x: defs::Defs) -> bool {
    let (ds, c) = embed_defs(x.clone());
    match unembed_defs(ds, c) {
      Ok(y) => {
        if x == y {
          true
        }
        else {
          //          println!("x: {:?}", x);
          //          println!("y: {:?}", y);
          false
        }
      }
      e => {
        //        println!("x: {:?}", x);
        //        println!("a: {:?}", a);
        //        println!("m: {:?}", m);
        //        println!("e: {:?}", e);
        false
      }
    }
  }
}
