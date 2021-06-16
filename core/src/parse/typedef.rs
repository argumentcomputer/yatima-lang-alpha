use crate::{
  defs::{
    Def,
    Defs,
  },
  name::Name,
  package::{
    Entry,
    Import,
    Index,
  },
  parse::{
    error::{
      ParseError,
      ParseErrorKind,
    },
    span::Span,
    term::*,
  },
  term::*,
  yatima,
};
use nom::{
  branch::alt,
  bytes::complete::tag,
  combinator::{
    eof,
    opt,
    peek,
  },
  multi::separated_list0,
  sequence::{
    delimited,
    preceded,
    terminated,
  },
  Err,
  IResult,
};
use sp_std::{
  cell::RefCell,
  collections::vec_deque::VecDeque,
  convert::TryFrom,
  iter::once,
  rc::Rc,
};

use cid::Cid;
use sp_im::ConsList;

// type Vector (A: Type): forall Nat -> Type {
//    Nil: Vector A 0,
//    Cons (k: Nat) (a: A) (as: Vector A k): Vector A (Nat.suc k),
// }
//
// def Vector (A: Type) (k: Nat): Type =
//  @Vector.self ∀
//  (0 P : ∀ (k: Nat) (Vector A k) -> Type)
//  (& Nil : P 0 (data λ Vector.Motive Vector.Nil Vector.Cons => Vector.Nil))
//  (& Cons: ∀ (0 k: Nat) (x: A) (xs: Vector A k)
//    -> P (succ k) (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
//  -> P k Vector.self
//
// def Vector.Nil (0 A: Type): Vector A zero
// = data λ P Vector.Nil Vector.Cons => Vector.Nil
//
// def Vector.Cons (0 A: Type) (0 k: Nat) (x: A) (xs: Vector A k)
// : Vector A (succ k)
// = (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
//
#[derive(PartialEq, Clone, Debug)]
pub struct TypeDef {
  pub pos: Pos,
  pub name: Name,
  pub ty_params: Vec<(Uses, Name, Term)>,
  pub ty_indices: Vec<(Uses, Name, Term)>,
  pub variants: Vec<Variant>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Variant {
  pub name: Name,
  pub bind: Vec<(Uses, Name, Term)>,
  pub ty_fun: Term,
  pub ty_params: Vec<Term>,
  pub ty_indices: Vec<Term>,
}

impl TypeDef {
  pub fn type_of(&self) -> Term {
    self.ty_params.iter().chain(self.ty_indices.iter()).rev().fold(
      Term::Typ(Pos::None),
      |acc, (u, n, t)| {
        Term::All(Pos::None, *u, n.clone(), Box::new((t.clone(), acc)))
      },
    )
  }

  pub fn motive(&self) -> Term {
    let args = self
      .ty_params
      .iter()
      .chain(self.ty_indices.iter())
      .rev()
      .enumerate()
      .map(|(i, (_, n, _))| Term::Var(Pos::None, n.clone(), i as u64));
    let slf_ty: Term =
      args.into_iter().fold(Term::Rec(Pos::None), |acc, arg| {
        Term::App(Pos::None, Box::new((acc, arg)))
      });
    self
      .ty_indices
      .iter()
      .chain(once(&(Uses::Many, Name::from("#_"), slf_ty)))
      .rev()
      .fold(Term::Typ(Pos::None), |acc, (u, n, t)| {
        Term::All(Pos::None, *u, n.clone(), Box::new((t.clone(), acc)))
      })
  }

  pub fn variant_data(&self) -> Vec<Term> {
    let mut lam_bind: Vec<Name> = vec![Name::from("P")];
    for v in self.variants.iter() {
      lam_bind.push(Name::from(format!("{}.{}", self.name, v.name)));
    }
    let mut data = vec![];
    let n = self.variants.len();
    for (i, v) in self.variants.iter().enumerate() {
      let bod: Term = v
        .bind
        .iter()
        .rev()
        .enumerate()
        .map(|(j, (_, n, _))| {
          Term::Var(Pos::None, n.clone(), (j + lam_bind.len()) as u64)
        })
        .fold(
          Term::Var(Pos::None, v.name.clone(), (n - i) as u64),
          |acc, arg| Term::App(Pos::None, Box::new((acc, arg))),
        );
      let trm = v.bind.iter().rev().fold(bod, |acc, (_, n, _)| {
        Term::Lam(Pos::None, n.clone(), Box::new(acc))
      });
      data.push(Term::Dat(Pos::None, Box::new(trm)));
    }
    data
  }

  pub fn term_of(&self) -> Term {
    let mut alls: Vec<(Uses, Name, Term)> = vec![];
    alls.push((Uses::None, Name::from("P"), self.motive()));
    let data_terms = self.variant_data();
    for (i, v) in self.variants.iter().enumerate() {
      let img = v.ty_indices.iter().chain(once(&data_terms[i])).fold(
        Term::Var(Pos::None, Name::from("P"), (i + v.bind.len()) as u64),
        |acc, arg| Term::App(Pos::None, Box::new((acc, arg.clone()))),
      );
      let all = v.bind.iter().rev().fold(img, |acc, (u, n, t)| {
        Term::All(Pos::None, *u, n.clone(), Box::new((t.clone(), acc)))
      });
      alls.push((
        Uses::Affi,
        Name::from(format!("{}.{}", self.name, v.name)),
        all,
      ));
    }
    let len = self.variants.len() as u64;
    let mot = Term::Var(Pos::None, Name::from("P"), len);
    let slf_name = Name::from(format!("{}.self", self.name));
    let slf = Term::Var(Pos::None, slf_name.clone(), len + 1);
    let indices = self
      .ty_indices
      .iter()
      .rev()
      .enumerate()
      .map(|(i, (_, n, _))| Term::Var(Pos::None, n.clone(), i as u64 + len));
    let img = indices
      .chain(once(slf))
      .fold(mot, |acc, arg| Term::App(Pos::None, Box::new((acc, arg.clone()))));
    let forall = alls.iter().rev().fold(img, |acc, (u, n, t)| {
      Term::All(Pos::None, *u, n.clone(), Box::new((t.clone(), acc)))
    });
    let bod = Term::Slf(Pos::None, slf_name, Box::new(forall));
    self
      .ty_params
      .iter()
      .chain(self.ty_indices.iter())
      .rev()
      .fold(bod, |acc, (_, n, _)| {
        Term::Lam(Pos::None, n.clone(), Box::new(acc))
      })
  }
}

pub fn parse_motive_binders(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  ctx: Ctx,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = alt((tag("∀"), tag("forall")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, bs) = parse_binders1(
      input,
      defs.clone(),
      None,
      ctx.clone(),
      Rc::new(VecDeque::new()),
      true,
      "->",
    )(i)?;
    let (i, _) = parse_space(i)?;
    Ok((i, bs))
  }
}
pub fn parse_variant_type_apps(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
) -> impl Fn(Span) -> IResult<Span, (Term, Vec<Term>), ParseError<Span>> {
  move |from: Span| {
    let (i2, _) = parse_space(from)?;
    let (i2, fun) = parse_term(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      Rc::new(VecDeque::new()),
    )(i2)?;
    let mut i = i2;
    let mut args = Vec::new();
    loop {
      let (i2, _) = parse_space(i)?;
      match parse_app_end(i2) {
        Ok((..)) => {
          // let _pos = Pos::from_upto(input, from, i2);
          return Ok((i2, (fun, args)));
        }
        _ => {
          let (i2, arg) = parse_term(
            input,
            defs.clone(),
            rec.clone(),
            ctx.clone(),
            Rc::new(VecDeque::new()),
          )(i2)?;
          args.push(arg);
          i = i2
        }
      }
    }
  }
}

pub fn parse_typedef(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
) -> impl Fn(Span) -> IResult<Span, TypeDef, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("type")(from)?;
    let (i, _) = parse_space(i)?;
    let (i, type_name) = parse_name(i)?;
    if defs.borrow().names.get(&type_name).is_some() {
      Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::TopLevelRedefinition(type_name),
      )))
    }
    else {
      let (i, _) = parse_space(i)?;
      let (i, ty_params) = parse_binders(
        input,
        defs.clone(),
        None,
        ConsList::new(),
        Rc::new(VecDeque::new()),
        false,
        ":",
      )(i)?;
      let (i, _) = parse_space(i)?;
      let mut ctx = ConsList::new();
      for (_, n, _) in ty_params.iter() {
        ctx = ctx.cons(n.clone());
      }
      let (i, ty_indices) =
        opt(parse_motive_binders(input, defs.clone(), ctx.clone()))(i)?;
      let ty_indices = ty_indices.unwrap_or_else(|| vec![]);
      let (i, _) = parse_type(input)(i)?;
      let (i, _) = parse_space(i)?;
      for (_, n, _) in ty_indices.iter() {
        ctx = ctx.cons(n.clone());
      }
      ctx = ctx.cons(Name::from(format!("{}.Motive", type_name)));
      let (i, _) = tag("{")(i)?;
      let mut vars = Vec::new();
      let mut ctx = ctx;
      let mut i = i;
      loop {
        match preceded(parse_space, tag("}"))(i) {
          Ok((i2, _)) => {
            let pos = Pos::from_upto(input, from, i2);
            return Ok((i2, TypeDef {
              pos,
              name: type_name,
              ty_params,
              ty_indices,
              variants: vars,
            }));
          }
          _ => {}
        }
        match delimited(
          parse_space,
          parse_variant(
            input,
            defs.clone(),
            ctx.clone(),
            type_name.clone(),
            ty_params.len(),
          ),
          alt((peek(tag("}")), tag(","))),
        )(i)
        {
          Err(e) => return Err(e),
          Ok((i2, vari)) => {
            i = i2;
            ctx = ctx.cons(vari.name.clone());
            vars.push(vari);
          }
        }
      }
    }
  }
}

pub fn parse_variant(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  ctx: Ctx,
  type_name: Name,
  len_params: usize,
) -> impl Fn(Span) -> IResult<Span, Variant, ParseError<Span>> {
  move |from: Span| {
    let (i, vari_name) = parse_name(from)?;
    let full_name = Name::from(format!("{}.{}", type_name, vari_name));
    if defs.borrow().names.get(&full_name).is_some() {
      Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::TopLevelRedefinition(full_name),
      )))
    }
    else {
      let (i, _) = parse_space(i)?;
      let (i, bs) = parse_binders(
        input,
        defs.clone(),
        Some(type_name.clone()),
        ctx.clone(),
        Rc::new(VecDeque::new()),
        false,
        ":",
      )(i)?;
      let (i, _) = parse_space(i)?;
      let mut vari_type_ctx = ctx.clone();
      for (_, n, _) in bs.iter() {
        vari_type_ctx = vari_type_ctx.cons(n.clone());
      }
      let (i, (ty_fun, ty_args)) = parse_variant_type_apps(
        input,
        defs.clone(),
        Some(type_name.clone()),
        vari_type_ctx,
      )(i)?;
      let (ty_params, ty_indices) = {
        let mut args = ty_args;
        if len_params > args.len() {
          (args, vec![])
        }
        else {
          let ty_indices = args.split_off(len_params);
          (args, ty_indices)
        }
      };
      Ok((i, Variant {
        name: vari_name,
        bind: bs,
        ty_fun,
        ty_params,
        ty_indices,
      }))
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn variant_parse() {
    fn test(n: usize, i: &str) -> IResult<Span, Variant, ParseError<Span>> {
      parse_variant(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        ConsList::new(),
        Name::from("Test"),
        n,
      )(Span::new(i))
    }
    let res = test(1, "New : Test");
    match res {
      Ok(res) => {
        assert!(
          res.1
            == Variant {
              name: Name::from("New"),
              bind: vec![],
              ty_fun: Term::Rec(Pos::None),
              ty_params: vec![],
              ty_indices: vec![],
            }
        );
      }
      e => {
        println!("{:?}", e);
        assert!(false);
      }
    }
    let res = test(1, "New : Test #Nat 1");
    match res {
      Ok(res) => {
        println!("{:?}", res.1);
        assert!(
          res.1
            == Variant {
              name: Name::from("New"),
              bind: vec![],
              ty_fun: Term::Rec(Pos::None),
              ty_params: vec![yatima!("#Nat")],
              ty_indices: vec![yatima!("1")],
            }
        );
      }
      e => {
        println!("{:?}", e);
        assert!(false);
      }
    }
    let res = test(2, "New (x: #Nat): Test #Nat x 1");
    match res {
      Ok(res) => {
        println!("{:?}", res.1);
        assert!(
          res.1
            == Variant {
              name: Name::from("New"),
              bind: vec![(Uses::Many, Name::from("x"), yatima!("#Nat"))],
              ty_fun: Term::Rec(Pos::None),
              ty_params: vec![
                yatima!("#Nat"),
                Term::Var(Pos::None, Name::from("x"), 0 as u64)
              ],
              ty_indices: vec![yatima!("1")],
            }
        );
      }
      e => {
        println!("{:?}", e);
        assert!(false);
      }
    }
  }

  #[test]
  fn typedef_parse() {
    fn test(i: &str) -> IResult<Span, TypeDef, ParseError<Span>> {
      parse_typedef(input_cid(i), Rc::new(RefCell::new(Defs::new())))(
        Span::new(i),
      )
    }
    let res = test("type Test: Type { }");
    assert!(res.is_ok());
    let res = test("type Test: Type { New : Test, }");
    assert!(res.is_ok());
    let res = test("type Test: Type { New : Test }");
    assert!(res.is_ok());
    #[rustfmt::skip]
    let res = test(
      "type List (A: Type): Type { \
         Nil : List A, \
         Cons (x: A) (xs: List A): List A, \
       }",
    );
    assert!(res.is_ok());
    #[rustfmt::skip]
    let res = test(
      "type Vector (A: Type): ∀ (k: #Nat) -> Type { \
         Nil : Vector A 0, \
         Cons (k: #Nat) (x: A) (xs: Vector A k): Vector A (#Nat.suc k), \
       }",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    println!("res: {}", res.clone().type_of());
    println!("res: {:?}", res.clone().term_of());
    println!("res: {}", res.clone().term_of());
    assert!(false);
  }
}
