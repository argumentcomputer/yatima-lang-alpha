use im::Vector;

use crate::term::{
  LitType,
  Literal,
  PrimOp,
  Term,
  Term::*,
  Uses,
};

use crate::hashspace::{
  anon_term::AnonTerm,
  name_meta::NameMeta,
  posi_meta::PosiMeta,
};

pub fn desaturate_term(term: Term) -> (AnonTerm, NameMeta, PosiMeta) {
  match term {
    Var(pos, _, idx) => (
      AnonTerm::Ctor(String::from("var"), vec![AnonTerm::Vari(idx)]),
      NameMeta::Ctor(vec![NameMeta::Leaf]),
      PosiMeta::Ctor(pos, vec![PosiMeta::Leaf]),
    ),
    Ref(pos, name, def_link, anon_link) => (
      AnonTerm::Ctor(String::from("ref"), vec![AnonTerm::Link(anon_link)]),
      NameMeta::Ctor(vec![NameMeta::Link(name, def_link)]),
      PosiMeta::Ctor(pos, vec![PosiMeta::Leaf]),
    ),
    Lit(pos, lit) => (
      AnonTerm::Ctor(String::from("lit"), vec![AnonTerm::Data(
        lit.encode().serialize(),
      )]),
      NameMeta::Ctor(vec![NameMeta::Leaf]),
      PosiMeta::Ctor(pos, vec![PosiMeta::Leaf]),
    ),
    LTy(pos, lty) => (
      AnonTerm::Ctor(String::from("lty"), vec![AnonTerm::Data(
        lty.encode().serialize(),
      )]),
      NameMeta::Ctor(vec![NameMeta::Leaf]),
      PosiMeta::Ctor(pos, vec![PosiMeta::Leaf]),
    ),
    Opr(pos, opr) => (
      AnonTerm::Ctor(String::from("opr"), vec![AnonTerm::Data(
        opr.encode().serialize(),
      )]),
      NameMeta::Ctor(vec![NameMeta::Leaf]),
      PosiMeta::Ctor(pos, vec![PosiMeta::Leaf]),
    ),
    Typ(pos) => (
      AnonTerm::Ctor(String::from("typ"), vec![]),
      NameMeta::Ctor(vec![]),
      PosiMeta::Ctor(pos, vec![]),
    ),
    Lam(pos, name, body) => {
      let (anon, meta, posi) = desaturate_term(*body);
      (
        AnonTerm::Ctor(String::from("lam"), vec![AnonTerm::Bind(Box::new(
          anon,
        ))]),
        NameMeta::Ctor(vec![NameMeta::Bind(name.to_owned(), Box::new(meta))]),
        PosiMeta::Ctor(pos, vec![posi]),
      )
    }
    Slf(pos, name, body) => {
      let (anon, meta, posi) = desaturate_term(*body);
      (
        AnonTerm::Ctor(String::from("slf"), vec![AnonTerm::Bind(Box::new(
          anon,
        ))]),
        NameMeta::Ctor(vec![NameMeta::Bind(name.to_owned(), Box::new(meta))]),
        PosiMeta::Ctor(pos, vec![posi]),
      )
    }
    App(pos, fun, arg) => {
      let (fun_anon, fun_meta, fun_posi) = desaturate_term(*fun);
      let (arg_anon, arg_meta, arg_posi) = desaturate_term(*arg);
      (
        AnonTerm::Ctor(String::from("app"), vec![fun_anon, arg_anon]),
        NameMeta::Ctor(vec![fun_meta, arg_meta]),
        PosiMeta::Ctor(pos, vec![fun_posi, arg_posi]),
      )
    }
    Ann(pos, val, typ) => {
      let (val_anon, val_meta, val_posi) = desaturate_term(*val);
      let (typ_anon, typ_meta, typ_posi) = desaturate_term(*typ);
      (
        AnonTerm::Ctor(String::from("ann"), vec![val_anon, typ_anon]),
        NameMeta::Ctor(vec![val_meta, typ_meta]),
        PosiMeta::Ctor(pos, vec![val_posi, typ_posi]),
      )
    }
    Dat(pos, body) => {
      let (anon, meta, posi) = desaturate_term(*body);
      (
        AnonTerm::Ctor(String::from("dat"), vec![anon]),
        NameMeta::Ctor(vec![meta]),
        PosiMeta::Ctor(pos, vec![posi]),
      )
    }
    Cse(pos, body) => {
      let (anon, meta, posi) = desaturate_term(*body);
      (
        AnonTerm::Ctor(String::from("cse"), vec![anon]),
        NameMeta::Ctor(vec![meta]),
        PosiMeta::Ctor(pos, vec![posi]),
      )
    }
    All(pos, uses, name, typ_, body) => {
      let (typ_anon, typ_meta, typ_posi) = desaturate_term(*typ_);
      let (bod_anon, bod_meta, bod_posi) = desaturate_term(*body);
      (
        AnonTerm::Ctor(String::from("all"), vec![
          AnonTerm::Data(uses.encode().serialize()),
          typ_anon,
          bod_anon,
        ]),
        NameMeta::Ctor(vec![
          NameMeta::Leaf,
          typ_meta,
          NameMeta::Bind(name, Box::new(bod_meta)),
        ]),
        PosiMeta::Ctor(pos, vec![PosiMeta::Leaf, typ_posi, bod_posi]),
      )
    }
    Let(pos, true, uses, name, typ_, expr, body) => {
      let (typ_anon, typ_meta, typ_posi) = desaturate_term(*typ_);
      let (exp_anon, exp_meta, exp_posi) = desaturate_term(*expr);
      let (bod_anon, bod_meta, bod_posi) = desaturate_term(*body);
      (
        AnonTerm::Ctor(String::from("rec"), vec![
          AnonTerm::Data(uses.encode().serialize()),
          typ_anon,
          AnonTerm::Bind(Box::new(exp_anon)),
          AnonTerm::Bind(Box::new(bod_anon)),
        ]),
        NameMeta::Ctor(vec![
          NameMeta::Leaf,
          typ_meta,
          NameMeta::Bind(name.clone(), Box::new(exp_meta)),
          NameMeta::Bind(name, Box::new(bod_meta)),
        ]),
        PosiMeta::Ctor(pos, vec![PosiMeta::Leaf, typ_posi, exp_posi, bod_posi]),
      )
    }
    Let(pos, false, uses, name, typ_, expr, body) => {
      let (typ_anon, typ_meta, typ_posi) = desaturate_term(*typ_);
      let (exp_anon, exp_meta, exp_posi) = desaturate_term(*expr);
      let (bod_anon, bod_meta, bod_posi) = desaturate_term(*body);
      (
        AnonTerm::Ctor(String::from("let"), vec![
          AnonTerm::Data(uses.encode().serialize()),
          typ_anon,
          exp_anon,
          AnonTerm::Bind(Box::new(bod_anon)),
        ]),
        NameMeta::Ctor(vec![
          NameMeta::Leaf,
          typ_meta,
          exp_meta,
          NameMeta::Bind(name, Box::new(bod_meta)),
        ]),
        PosiMeta::Ctor(pos, vec![PosiMeta::Leaf, typ_posi, exp_posi, bod_posi]),
      )
    }
  }
}

#[derive(Clone, Debug)]
pub enum ResaturationError {
  FreeVariable,
  DeserialError,
  UnexpectedCtor(AnonTerm, NameMeta, PosiMeta),
  BadLet,
}

pub fn resaturate_term(
  ctx: Vector<String>,
  anon_term: &AnonTerm,
  name_meta: &NameMeta,
  position_meta: &PosiMeta,
) -> Result<Term, ResaturationError> {
  match (anon_term, name_meta, position_meta) {
    (AnonTerm::Ctor(n, xs), NameMeta::Ctor(ys), PosiMeta::Ctor(pos, zs)) => {
      match (&n[..], xs.as_slice(), ys.as_slice(), zs.as_slice()) {
        ("var", [AnonTerm::Vari(idx)], [NameMeta::Leaf], [PosiMeta::Leaf]) => {
          match ctx.iter().enumerate().find(|(i, _)| (*i as u64) == *idx) {
            Some((_, n)) => Ok(Term::Var(*pos, n.to_owned(), *idx)),
            None => Err(ResaturationError::FreeVariable),
          }
        }
        (
          "ref",
          [AnonTerm::Link(anon)],
          [NameMeta::Link(name, def)],
          [PosiMeta::Leaf],
        ) => Ok(Term::Ref(*pos, name.clone(), *def, *anon)),
        ("lit", [AnonTerm::Data(data)], [NameMeta::Leaf], [PosiMeta::Leaf]) => {
          let (_, lit) = hashexpr::Expr::deserialize(data)
            .map_err(|_| ResaturationError::DeserialError)?;
          let lit = Literal::decode(lit)
            .map_err(|_| ResaturationError::DeserialError)?;
          Ok(Term::Lit(*pos, lit))
        }
        ("lty", [AnonTerm::Data(data)], [NameMeta::Leaf], [PosiMeta::Leaf]) => {
          let (_, lty) = hashexpr::Expr::deserialize(data)
            .map_err(|_| ResaturationError::DeserialError)?;
          let lty = LitType::decode(lty)
            .map_err(|_| ResaturationError::DeserialError)?;
          Ok(Term::LTy(*pos, lty))
        }
        ("opr", [AnonTerm::Data(data)], [NameMeta::Leaf], [PosiMeta::Leaf]) => {
          let (_, opr) = hashexpr::Expr::deserialize(data)
            .map_err(|_| ResaturationError::DeserialError)?;
          let opr = PrimOp::decode(opr)
            .map_err(|_| ResaturationError::DeserialError)?;
          Ok(Term::Opr(*pos, opr))
        }
        ("typ", [], [], []) => Ok(Term::Typ(*pos)),
        ("dat", [anon], [meta], [posi]) => {
          let body = resaturate_term(ctx, anon, meta, posi)?;
          Ok(Term::Dat(*pos, Box::new(body)))
        }
        ("cse", [anon], [meta], [posi]) => {
          let body = resaturate_term(ctx, anon, meta, posi)?;
          Ok(Term::Cse(*pos, Box::new(body)))
        }
        ("lam", [AnonTerm::Bind(anon)], [NameMeta::Bind(n, meta)], [posi]) => {
          let mut new_ctx = ctx.clone();
          new_ctx.push_front(n.clone());
          let body = resaturate_term(new_ctx, anon, meta, posi)?;
          Ok(Term::Lam(*pos, n.clone(), Box::new(body)))
        }
        ("slf", [AnonTerm::Bind(anon)], [NameMeta::Bind(n, meta)], [posi]) => {
          let mut new_ctx = ctx.clone();
          new_ctx.push_front(n.clone());
          let body = resaturate_term(new_ctx, anon, meta, posi)?;
          Ok(Term::Slf(*pos, n.clone(), Box::new(body)))
        }
        ("app", [fanon, aanon], [fmeta, ameta], [fposi, aposi]) => {
          let fun = resaturate_term(ctx.clone(), fanon, fmeta, fposi)?;
          let arg = resaturate_term(ctx.clone(), aanon, ameta, aposi)?;
          Ok(Term::App(*pos, Box::new(fun), Box::new(arg)))
        }
        ("ann", [xanon, tanon], [xmeta, tmeta], [xposi, tposi]) => {
          let xpr = resaturate_term(ctx.clone(), xanon, xmeta, xposi)?;
          let typ = resaturate_term(ctx.clone(), tanon, tmeta, tposi)?;
          Ok(Term::Ann(*pos, Box::new(xpr), Box::new(typ)))
        }
        (
          "all",
          [AnonTerm::Data(uses), tanon, banon],
          [NameMeta::Leaf, tmeta, NameMeta::Bind(n, bmeta)],
          [PosiMeta::Leaf, tposi, bposi],
        ) => {
          let (_, uses) = hashexpr::Expr::deserialize(uses)
            .map_err(|_| ResaturationError::DeserialError)?;
          let uses =
            Uses::decode(uses).map_err(|_| ResaturationError::DeserialError)?;
          let typ_ = resaturate_term(ctx.clone(), tanon, tmeta, tposi)?;
          let mut new_ctx = ctx.clone();
          new_ctx.push_front(n.clone());
          let body = resaturate_term(new_ctx, banon, bmeta, bposi)?;
          Ok(Term::All(*pos, uses, n.clone(), Box::new(typ_), Box::new(body)))
        }
        (
          "rec",
          [AnonTerm::Data(uses), tanon, AnonTerm::Bind(xanon), AnonTerm::Bind(banon)],
          [NameMeta::Leaf, tmeta, NameMeta::Bind(n1, xmeta), NameMeta::Bind(n2, bmeta)],
          [PosiMeta::Leaf, tposi, xposi, bposi],
        ) => {
          let name =
            if n1 == n2 { Ok(n1) } else { Err(ResaturationError::BadLet) }?;
          let (_, uses) = hashexpr::Expr::deserialize(uses)
            .map_err(|_| ResaturationError::DeserialError)?;
          let uses =
            Uses::decode(uses).map_err(|_| ResaturationError::DeserialError)?;
          let typ_ = resaturate_term(ctx.clone(), tanon, tmeta, tposi)?;
          let mut new_ctx = ctx.clone();
          new_ctx.push_front(name.clone());
          let exp = resaturate_term(new_ctx.clone(), xanon, xmeta, xposi)?;
          let body = resaturate_term(new_ctx, banon, bmeta, bposi)?;
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
          [NameMeta::Leaf, tmeta, xmeta, NameMeta::Bind(name, bmeta)],
          [PosiMeta::Leaf, tposi, xposi, bposi],
        ) => {
          let (_, uses) = hashexpr::Expr::deserialize(uses)
            .map_err(|_| ResaturationError::DeserialError)?;
          let uses =
            Uses::decode(uses).map_err(|_| ResaturationError::DeserialError)?;
          let typ_ = resaturate_term(ctx.clone(), tanon, tmeta, tposi)?;
          let exp = resaturate_term(ctx.clone(), xanon, xmeta, xposi)?;
          let mut new_ctx = ctx;
          new_ctx.push_front(name.clone());
          let body = resaturate_term(new_ctx, banon, bmeta, bposi)?;
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
        _ => Err(ResaturationError::UnexpectedCtor(
          anon_term.clone(),
          name_meta.clone(),
          position_meta.clone(),
        )),
      }
    }
    _ => Err(ResaturationError::UnexpectedCtor(
      anon_term.clone(),
      name_meta.clone(),
      position_meta.clone(),
    )),
  }
}
#[cfg(test)]
pub mod tests {
  use super::*;

  #[quickcheck]
  fn term_separate_merge(x: Term) -> bool {
    let (a, m, p) = desaturate_term(x.clone());
    match resaturate_term(Vector::new(), &a, &m, &p) {
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
