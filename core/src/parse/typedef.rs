use crate::{
  defs::{
    Def,
    Defs,
  },
  name::Name,
  package::Entry,
  parse::{
    error::{
      ParseError,
      ParseErrorKind,
    },
    span::Span,
    term::*,
  },
  term::*,
};
use nom::{
  branch::alt,
  bytes::complete::tag,
  combinator::{
    opt,
    peek,
    value,
  },
  sequence::preceded,
  Err,
  IResult,
};
use sp_std::{
  cell::RefCell,
  collections::vec_deque::VecDeque,
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
#[derive(Clone, Debug)]
pub struct TypeDef {
  pub pos: Pos,
  pub name: Name,
  pub ty_params: Vec<(Uses, Name, Term)>,
  pub ty_indices: Vec<(Uses, Name, Term)>,
  pub variants: Vec<Variant>,
}

impl PartialEq for TypeDef {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
      && self.ty_params == other.ty_params
      && self.ty_indices == other.ty_indices
      && self.variants == other.variants
  }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Variant {
  pub name: Name,
  pub bind: Vec<(Uses, Name, Term)>,
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
    let i_len = self.ty_indices.len();
    let params: Vec<Term> = self
      .ty_params
      .iter()
      .rev()
      .enumerate()
      .map(|(i, (_, n, _))| {
        Term::Var(Pos::None, n.clone(), (i + 1 + 2 * i_len) as u64)
      })
      .collect();
    let indices: Vec<Term> = self
      .ty_indices
      .iter()
      .rev()
      .enumerate()
      .map(|(i, (_, n, _))| Term::Var(Pos::None, n.clone(), i as u64))
      .collect();
    let slf_ty: Term = params
      .into_iter()
      .rev()
      .chain(indices.into_iter().rev())
      .fold(Term::Rec(Pos::None), |acc, arg| {
        Term::App(Pos::None, Box::new((acc, arg)))
      });
    self
      .ty_indices
      .clone()
      .into_iter()
      // TODO: Check shift
      .map(|(u, n, t)| (u, n, t.shift((1 + i_len) as i64, Some(0))))
      .chain(once((Uses::Many, Name::from("_"), slf_ty)))
      .rev()
      .fold(Term::Typ(Pos::None), |acc, (u, n, t)| {
        Term::All(Pos::None, u, n, Box::new((t, acc)))
      })
  }

  pub fn data_terms(&self) -> Vec<Term> {
    let mut lam_bind: Vec<Name> = vec![Name::from("P")];
    for v in self.variants.iter() {
      let name = Name::from(format!("{}.{}", self.name, v.name));
      lam_bind.push(name);
    }
    let mut data = vec![];
    let n = self.variants.len();
    for (i, v) in self.variants.iter().enumerate() {
      let name = Name::from(format!("{}.{}", self.name, v.name));
      let bod: Term = v
        .bind
        .iter()
        .rev()
        .enumerate()
        .map(|(j, (_, n, _))| {
          Term::Var(Pos::None, n.clone(), (j + lam_bind.len()) as u64)
        })
        .rev()
        .fold(Term::Var(Pos::None, name, (n - i - 1) as u64), |acc, arg| {
          Term::App(Pos::None, Box::new((acc, arg)))
        });
      let trm = lam_bind
        .iter()
        .rev()
        .fold(bod, |acc, n| Term::Lam(Pos::None, n.clone(), Box::new(acc)));
      data.push(Term::Dat(Pos::None, Box::new(trm)));
    }
    data
  }

  pub fn term_of(&self) -> Term {
    let mut alls: Vec<(Uses, Name, Term)> = vec![];
    alls.push((Uses::None, Name::from("P"), self.motive()));
    let data_terms = self.data_terms();
    for (i, v) in self.variants.iter().enumerate() {
      let img = v.ty_indices.iter().chain(once(&data_terms[i])).fold(
        Term::Var(Pos::None, Name::from("P"), (i + v.bind.len()) as u64),
        |acc, arg| Term::App(Pos::None, Box::new((acc, arg.clone()))),
      );
      let all = v.bind.iter().rev().fold(img, |acc, (u, n, t)| {
        Term::All(Pos::None, *u, n.clone(), Box::new((t.clone(), acc)))
      });
      alls.push((Uses::Affi, Name::from(format!("{}", v.name)), all));
    }
    let len = self.variants.len() as u64;
    let mot = Term::Var(Pos::None, Name::from("P"), len);
    let slf_name = Name::from(format!("{}.self", self.name));
    let slf = Term::Var(Pos::None, slf_name.clone(), len + 1);
    let indices =
      self.ty_indices.iter().rev().enumerate().map(|(i, (_, n, _))| {
        Term::Var(Pos::None, n.clone(), i as u64 + len + 2)
      });
    let img = indices
      .rev()
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

  pub fn type_def(&self) -> (Name, Def, Entry) {
    let (d, e) = Def::make(Pos::None, self.type_of(), self.term_of());
    (self.name.clone(), d, e)
  }

  pub fn type_ref(&self) -> Term {
    let (_, _, ty_entry) = self.type_def();
    Term::Ref(Pos::None, self.name.clone(), ty_entry.cid(), ty_entry.term_anon)
  }

  pub fn constructors(&self) -> Vec<(Name, Def, Entry)> {
    let mut res = Vec::new();
    let data = self.data_terms();
    for (i, v) in self.variants.iter().enumerate() {
      let trm = self
        .ty_params
        .iter()
        .chain(v.bind.iter())
        .rev()
        .fold(data[i].clone(), |acc, (_, n, _)| {
          Term::Lam(Pos::None, n.clone(), Box::new(acc))
        });
      let offset = -((self.ty_indices.len() + 2 + i) as i64);
      // println!("v.ty_params: {:?}", v.ty_params);
      let v_params: Vec<Term> = v
        .ty_params
        .clone()
        .into_iter()
        .map(|t| t.shift(offset, None))
        .collect();
      let v_indices: Vec<Term> = v
        .ty_indices
        .clone()
        .into_iter()
        .map(|t| t.shift(offset, Some(v.bind.len() as u64)))
        .collect();
      // println!("params: {:?}", v_params);
      let typ = v_params
        .iter()
        .chain(v_indices.iter())
        .fold(self.type_ref(), |acc, arg| {
          Term::App(Pos::None, Box::new((acc, arg.clone())))
        });
      let v_bind: Vec<(Uses, Name, Term)> = v
        .bind
        .clone()
        .into_iter()
        .enumerate()
        .map(|(i, (u, n, t))| (u, n, t.shift(offset, Some(i as u64))))
        .collect();
      let typ = self.ty_params.iter().chain(v_bind.iter()).rev().fold(
        typ,
        |acc, (u, n, t)| {
          Term::All(Pos::None, *u, n.clone(), Box::new((t.clone(), acc)))
        },
      );
      let typ = typ.un_rec(Rc::new(self.type_ref()));
      let (d, e) = Def::make(Pos::None, typ, trm);
      res.push((Name::from(format!("{}.{}", self.name, v.name)), d, e));
    }
    res
  }
}

pub fn parse_motive_binders(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  ctx: Ctx,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag(":")(from)?;
    let (i, _) = parse_space(i)?;
    let inner = |i| {
      let (i, _) = alt((tag("∀"), tag("forall")))(i)?;
      let (i, _) = parse_space(i)?;
      let (i, bs) = parse_binders1(
        input,
        defs.clone(),
        None,
        ctx.clone(),
        Rc::new(VecDeque::new()),
        true,
        vec!['-'],
        Uses::None,
      )(i)?;
      let (i, _) = tag("->")(i)?;
      let (i, _) = parse_space(i)?;
      Ok((i, bs))
    };
    let (i, bs) = opt(inner)(i)?;
    let bs = bs.unwrap_or_else(|| vec![]);
    let (i, _) = parse_type(input)(i)?;
    let (i, _) = parse_space(i)?;
    Ok((i, bs))
  }
}
pub fn parse_variant_end()
-> impl Fn(Span) -> IResult<Span, Span, ParseError<Span>> {
  move |from: Span| peek(alt((tag(","), tag("}"))))(from)
}
pub fn parse_variant_params(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  len_params: usize,
) -> impl Fn(Span) -> IResult<Span, (Vec<Term>, Vec<Term>), ParseError<Span>> {
  move |from: Span| {
    let (i, _) = parse_space(from)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i2, fun) =
      parse_var(input, defs.clone(), rec.clone(), ctx.clone())(i)?;
    if let Term::Rec(..) = fun {
      let mut i = i2;
      let mut args = Vec::new();
      loop {
        let (i2, _) = parse_space(i)?;
        match parse_variant_end()(i2) {
          Ok((..)) => {
            let (ty_params, ty_indices) = {
              if len_params > args.len() {
                (args, vec![])
              }
              else {
                let ty_indices = args.split_off(len_params);
                (args, ty_indices)
              }
            };
            // let _pos = Pos::from_upto(input, from, i2);
            return Ok((i2, (ty_params, ty_indices)));
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
    else {
      Err(Err::Error(ParseError::new(
        i2,
        ParseErrorKind::TypeDefConstructorMustReturnItsType,
      )))
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
        vec![':', '{'],
        Uses::None,
      )(i)?;

      let mut ctx = ConsList::new();
      for (_, n, _) in ty_params.iter() {
        ctx = ctx.cons(n.clone());
      }
      let (i, ty_indices) =
        opt(parse_motive_binders(input, defs.clone(), ctx.clone()))(i)?;
      let ty_indices = ty_indices.unwrap_or_else(|| vec![]);
      for (_, n, _) in ty_indices.iter() {
        ctx = ctx.cons(n.clone());
      }
      ctx = ctx.cons(Name::from(format!("{}.self", type_name)));
      ctx = ctx.cons(Name::from(format!("{}.P", type_name)));
      let (i, _) = tag("{")(i)?;
      let mut vars = Vec::new();
      let mut ctx = ctx;
      let mut i = i;
      let rc_params = Rc::new(ty_params);
      loop {
        match preceded(parse_space, tag("}"))(i) {
          Ok((i2, _)) => {
            let pos = Pos::from_upto(input, from, i2);
            return Ok((i2, TypeDef {
              pos,
              name: type_name,
              ty_params: rc_params.as_ref().clone(),
              ty_indices,
              variants: vars,
            }));
          }
          _ => {}
        }
        match preceded(
          parse_space,
          parse_variant(
            input,
            defs.clone(),
            ctx.clone(),
            type_name.clone(),
            rc_params.clone(),
            ty_indices.is_empty(),
          ),
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
  typedef_params: Rc<Vec<(Uses, Name, Term)>>,
  empty_ty_indices: bool,
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
        true,
        vec![':', ',', '}'],
        Uses::Many,
      )(i)?;
      let mut vari_type_ctx = ctx.clone();
      for (_, n, _) in bs.iter() {
        vari_type_ctx = vari_type_ctx.cons(n.clone());
      }
      if empty_ty_indices {
        // println!("vari_ctx: {:?}", vari_type_ctx);
        // println!("typedef_params: {:?}", typedef_params);
        let default_params = typedef_params
          .as_ref()
          .iter()
          .rev()
          .enumerate()
          .map(|(i, (_, n, _))| {
            Term::Var(
              Pos::None,
              n.clone(),
              (i + vari_type_ctx.len() - typedef_params.len()) as u64,
            )
          })
          .rev()
          .collect();
        // println!("default_params: {:?}", default_params);
        let (i, (ty_params, ty_indices)) = alt((
          parse_variant_params(
            input,
            defs.clone(),
            Some(type_name.clone()),
            vari_type_ctx,
            typedef_params.len(),
          ),
          value((default_params, vec![]), parse_space),
        ))(i)?;
        let (i, _) = alt((tag(","), peek(tag("}"))))(i)?;
        Ok((i, Variant { name: vari_name, bind: bs, ty_params, ty_indices }))
      }
      else {
        let (i, (ty_params, ty_indices)) = parse_variant_params(
          input,
          defs.clone(),
          Some(type_name.clone()),
          vari_type_ctx,
          typedef_params.len(),
        )(i)?;
        let (i, _) = alt((tag(","), peek(tag("}"))))(i)?;
        Ok((i, Variant { name: vari_name, bind: bs, ty_params, ty_indices }))
      }
    }
  }
}
pub fn parse_typedef_elaborated(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
) -> impl Fn(Span) -> IResult<Span, Vec<(Name, Def, Entry)>, ParseError<Span>> {
  move |from: Span| {
    let (i, typedef) = parse_typedef(input, defs.clone())(from)?;
    let mut res: Vec<(Name, Def, Entry)> = vec![];
    res.push(typedef.type_def());
    res.append(&mut typedef.constructors());
    Ok((i, res))
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::yatima;

  #[test]
  fn variant_parse() {
    fn test(
      n: Vec<(Uses, Name, Term)>,
      e: bool,
      i: &str,
    ) -> IResult<Span, Variant, ParseError<Span>> {
      parse_variant(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        n.iter().map(|(_, n, _)| n).collect(),
        Name::from("Test"),
        Rc::new(n),
        e,
      )(Span::new(i))
    }
    let res = test(vec![], true, "New : Test,");
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, Variant {
      name: Name::from("New"),
      bind: vec![],
      ty_params: vec![],
      ty_indices: vec![],
    });
    let res = test(vec![], false, "New,");
    assert!(res.is_err());
    let res = test(vec![], true, "New,");
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, Variant {
      name: Name::from("New"),
      bind: vec![],
      ty_params: vec![],
      ty_indices: vec![],
    });
    let res = test(vec![], true, "New}");
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, Variant {
      name: Name::from("New"),
      bind: vec![],
      ty_params: vec![],
      ty_indices: vec![],
    });
    let res = test(
      vec![(Uses::Many, Name::from("A"), yatima!("Type"))],
      true,
      "New: Test A,",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, Variant {
      name: Name::from("New"),
      bind: vec![],
      ty_params: vec![Term::Var(Pos::None, Name::from("A"), 0)],
      ty_indices: vec![],
    });
    let res = test(
      vec![(Uses::Many, Name::from("A"), yatima!("Type"))],
      true,
      "New: Test A}",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, Variant {
      name: Name::from("New"),
      bind: vec![],
      ty_params: vec![Term::Var(Pos::None, Name::from("A"), 0)],
      ty_indices: vec![],
    });
    let res = test(
      vec![(Uses::Many, Name::from("A"), yatima!("Type"))],
      false,
      "New: Test #Nat 1,",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, Variant {
      name: Name::from("New"),
      bind: vec![],
      ty_params: vec![yatima!("#Nat")],
      ty_indices: vec![yatima!("1")],
    });
    let res = test(
      vec![
        (Uses::Many, Name::from("A"), yatima!("Type")),
        (Uses::Many, Name::from("x"), yatima!("#Nat")),
      ],
      false,
      "New: Test #Nat 1,",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, Variant {
      name: Name::from("New"),
      bind: vec![],
      ty_params: vec![yatima!("#Nat"), yatima!("1"),],
      ty_indices: vec![],
    });
    let res = test(
      vec![
        (Uses::Many, Name::from("A"), yatima!("Type")),
        (Uses::Many, Name::from("x"), yatima!("#Nat")),
      ],
      false,
      "New (x: #Nat): Test #Nat x 1,",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, Variant {
      name: Name::from("New"),
      bind: vec![(Uses::Many, Name::from("x"), yatima!("#Nat"))],
      ty_params: vec![
        yatima!("#Nat"),
        Term::Var(Pos::None, Name::from("x"), 0 as u64)
      ],
      ty_indices: vec![yatima!("1")],
    });
    let res = test(
      vec![
        (Uses::Many, Name::from("A"), yatima!("Type")),
        (Uses::Many, Name::from("x"), yatima!("#Nat")),
      ],
      false,
      "New (x y: #Nat): Test #Nat x y 1,",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, Variant {
      name: Name::from("New"),
      bind: vec![
        (Uses::Many, Name::from("x"), yatima!("#Nat")),
        (Uses::Many, Name::from("y"), yatima!("#Nat"))
      ],
      ty_params: vec![
        yatima!("#Nat"),
        Term::Var(Pos::None, Name::from("x"), 1 as u64)
      ],
      ty_indices: vec![
        Term::Var(Pos::None, Name::from("y"), 0 as u64),
        yatima!("1")
      ],
    });
  }

  fn test_parse(i: &str) -> IResult<Span, TypeDef, ParseError<Span>> {
    parse_typedef(input_cid(i), Rc::new(RefCell::new(Defs::new())))(Span::new(
      i,
    ))
  }
  #[test]
  fn typedef_empty() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Empty: Type { }",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    let res_no_sigs = test_parse("type Empty { }");
    println!("{:?}", res_no_sigs);
    assert!(res_no_sigs.is_ok());
    let res_no_sigs = res_no_sigs.unwrap().1;
    assert_eq!(res, res_no_sigs);
    assert_eq!(res.type_of(), yatima!("Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("@Empty.self ∀\
      (0 P: ∀ #$0 -> Type)
      -> P Empty.self
    ", Term::Rec(Pos::None));
    assert_eq!(res1, res2);
  }
  #[test]
  fn typedef_bool() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Bool: Type { True: Bool, False: Bool }",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    let res_no_ty_sig = test_parse("type Bool { True: Bool, False: Bool }");
    assert!(res_no_ty_sig.is_ok());
    let res_no_ty_sig = res_no_ty_sig.unwrap().1;
    assert_eq!(res, res_no_ty_sig);
    let res_no_sigs = test_parse("type Bool { True, False }");
    println!("{:?}", res_no_sigs);
    assert!(res_no_sigs.is_ok());
    let res_no_sigs = res_no_sigs.unwrap().1;
    assert_eq!(res, res_no_sigs);
    assert!(res.type_of() == yatima!("Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("@Bool.self ∀\
      (0 P: ∀ #$0 -> Type)
      (& True:  P (data λ P Bool.True Bool.False => Bool.True))
      (& False: P (data λ P Bool.True Bool.False => Bool.False))
      -> P Bool.self
    ", Term::Rec(Pos::None));
    assert_eq!(res1, res2);
  }
  #[test]
  fn typedef_nat() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Nat: Type { Z: Nat, S (x: Nat): Nat }",
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    let res_no_ty_sig = test_parse("type Nat { Z, S (x: Nat) }");
    assert!(res_no_ty_sig.is_ok());
    let res_no_ty_sig = res_no_ty_sig.unwrap().1;
    assert_eq!(res, res_no_ty_sig);
    assert!(res.type_of() == yatima!("Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("@Nat.self ∀\
      (0 P: ∀ #$0 -> Type)
      (& Z: P (data λ P Nat.Z Nat.S => Nat.Z))
      (& S: ∀ (x: #$0) -> (P (data λ P Nat.Z Nat.S => Nat.S x)))
      -> P Nat.self
    ", Term::Rec(Pos::None));
    assert_eq!(res1, res2);
    let res_no_name = test_parse("type Nat { Z, S Nat }");
    let res3 = yatima!(
      "@Nat.self ∀(0 P: ∀ #$0 -> Type)
      (& Z: P (data λ P Nat.Z Nat.S => Nat.Z))
      (& S: ∀ #$0 -> (P (data λ P Nat.Z Nat.S => Nat.S _)))
      -> P Nat.self
    ",
      Term::Rec(Pos::None)
    );
    assert!(res_no_name.is_ok());
    let res_no_name = res_no_name.unwrap().1;
    assert_eq!(res_no_name.term_of(), res3);
  }
  #[test]
  fn typedef_list() {
    #[rustfmt::skip]
    let res = test_parse(
      "type List (A: Type): Type { \
         Nil : List A, \
         Cons (x: A) (xs: List A): List A, \
       }",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res.type_of(), yatima!("∀ (0 A: Type) -> Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("λ A => @List.self ∀\
      (0 P: ∀ (#$0 A) -> Type) \
      (& Nil: P (data λ P List.Nil List.Cons => List.Nil)) \
      (& Cons : ∀ (x: A) (xs: (#$0 A))\
        -> P (data λ P List.Nil List.Cons => List.Cons x xs)) \
      -> P List.self
    ", Term::Rec(Pos::None));
    println!("res1: {}", res1.pretty(Some(&"List".to_string()), true));
    println!("res2: {}", res2.pretty(Some(&"List".to_string()), true));
    assert!(res1 == res2);
    let constrs = res.constructors();
    println!("{:?}", constrs);
    let (nil_name, nil_def, _) = constrs[0].clone();
    assert_eq!(nil_name.to_string(), "List.Nil".to_string());
    let typ1 = nil_def.typ_;
    let typ2 = yatima!("∀ (0 A: Type) -> (#$0 A)", res.type_ref());
    println!("typ1: {}", typ1.pretty(Some(&"List.Nil".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"List.Nil".to_string()), true));
    assert_eq!(typ1, typ2);
    let (cons_name, cons_def, _) = constrs[1].clone();
    assert_eq!(cons_name.to_string(), "List.Cons".to_string());
    let typ1 = cons_def.typ_;
    let typ2 =
      yatima!("∀ (0 A: Type) (x: A) (xs: #$0 A) -> (#$0 A)", res.type_ref());
    println!("typ1: {}", typ1.pretty(Some(&"Vector.Cons".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"Vector.Cons".to_string()), true));
    assert_eq!(typ1, typ2);
    let (nil_name, nil_def, _) = constrs[0].clone();
    assert_eq!(nil_name.to_string(), "List.Nil".to_string());
    let typ1 = nil_def.typ_;
    #[rustfmt::skip]
     let typ2 = yatima!(
      "∀ (0 A: Type) -> (#$0 A)",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"List.Nil".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"List.Nil".to_string()), true));
    assert_eq!(typ1, typ2);
    let (cons_name, cons_def, _) = constrs[1].clone();
    assert_eq!(cons_name.to_string(), "List.Cons".to_string());
    let typ1 = cons_def.typ_;
    #[rustfmt::skip]
     let typ2 = yatima!(
      "∀ (0 A: Type) (x: A) (xs: (#$0 A)) -> (#$0 A)",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"List.Cons".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"List.Cons".to_string()), true));
    assert_eq!(typ1, typ2);
    // let (consb_name, consb_def, _) = constrs[2].clone();
    // assert_eq!(consb_name.to_string(), "Crazy.ConsB".to_string());
    // let typ1 = consb_def.typ_;
    //#[rustfmt::skip]
    // let typ2 = yatima!(
    //  "∀ (0 A B C D E: Type) (0 a b c d e: #Nat) (x: B) (xs: #$0 A B C D E a b
    // c d e) \    -> (#$0 A B C D E a (#Nat.suc b) c d e)",
    //  res.type_ref()
    //);
    // println!("typ1: {}", typ1.pretty(Some(&"Crazy.ConsB".to_string()),
    // true)); println!("typ2. {}",
    // typ2.pretty(Some(&"Crazy.ConsB".to_string()), true));
    // assert_eq!(typ1, typ2);
  }
  #[test]
  fn typedef_vector() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Vector (A: Type): ∀ (k: #Nat) -> Type { \
         Nil : Vector A 0, \
         Cons (0 k: #Nat) (x: A) (xs: Vector A k): Vector A (#Nat.suc k), \
       }",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res.type_of(), yatima!("∀ (0 A: Type) (0 k: #Nat) -> Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("λ A k => @Vector.self ∀\
      (0 P: ∀ (0 k: #Nat) (#$0 A k) -> Type)
      (& Nil: P 0 (data λ P Vector.Nil Vector.Cons => Vector.Nil))
      (& Cons : ∀ (0 k: #Nat) (x: A) (xs: (#$0 A k))
        -> P (#Nat.suc k) (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
      -> P k Vector.self
    ", Term::Rec(Pos::None));
    println!("res1: {}", res1.pretty(Some(&"Vector".to_string()), true));
    println!("res2: {}", res2.pretty(Some(&"Vector".to_string()), true));
    assert!(res1 == res2);
    let constrs = res.constructors();
    println!("{:?}", constrs);
    let (nil_name, nil_def, _) = constrs[0].clone();
    assert_eq!(nil_name.to_string(), "Vector.Nil".to_string());
    let typ1 = nil_def.typ_;
    let typ2 = yatima!("∀ (A: Type) -> (#$0 A 0)", res.type_ref());
    println!("typ1: {}", typ1.pretty(Some(&"Vector.Nil".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"Vector.Nil".to_string()), true));
    let (cons_name, cons_def, _) = constrs[1].clone();
    assert_eq!(cons_name.to_string(), "Vector.Cons".to_string());
    let typ1 = cons_def.typ_;
    let typ2 = yatima!(
      "∀ (0 A: Type) (0 k: #Nat) (x: A) (xs: #$0 A k) -> (#$0 A (#Nat.suc k))",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"Vector.Cons".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"Vector.Cons".to_string()), true));
    assert_eq!(typ1, typ2);
  }
  #[test]
  fn typedef_chain() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Chain (A B: Type): ∀ (x y: #Nat) -> Type { \
         Nil: Chain A B 0 0, \
         Lft (0 x y: #Nat) (a: A) (as: Chain A B x y): Chain A B (#Nat.suc x) y, \
         Rgt (0 x y: #Nat) (a: A) (as: Chain A B x y): Chain A B x (#Nat.suc y), \
       }",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res.type_of(), yatima!("∀ (0 A B: Type) (0 x y: #Nat) -> Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("λ A B x y => @Chain.self ∀ \
      (0 P: ∀ (0 x y: #Nat) (#$0 A B x y) -> Type) \
      (& Nil: P 0 0 (data λ P Chain.Nil Chain.Lft Chain.Rgt => Chain.Nil))\
      (& Lft : ∀ (0 x y: #Nat) (a: A) (as: (#$0 A B x y)) \
        -> P (#Nat.suc x) y \
          (data λ P Chain.Nil Chain.Lft Chain.Rgt => Chain.Lft x y a as)) \
      (& Rgt : ∀ (0 x y: #Nat) (a: A) (as: (#$0 A B x y)) \
        -> P x (#Nat.suc y) \
          (data λ P Chain.Nil Chain.Lft Chain.Rgt => Chain.Rgt x y a as)) \
      -> P x y Chain.self
    ", Term::Rec(Pos::None));
    println!("res1: {}", res1.pretty(Some(&"Chain".to_string()), true));
    println!("res2: {}", res2.pretty(Some(&"Chain".to_string()), true));
    assert!(res1 == res2);
    let constrs = res.constructors();
    let (nil_name, nil_def, _) = constrs[0].clone();
    assert_eq!(nil_name.to_string(), "Chain.Nil".to_string());
    let typ1 = nil_def.typ_;
    let typ2 = yatima!("∀ (0 A B: Type) -> (#$0 A B 0 0)", res.type_ref());
    println!("typ1: {}", typ1.pretty(Some(&"Chain.Nil".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"Chain.Nil".to_string()), true));
    assert_eq!(typ1, typ2);
    let (lft_name, lft_def, _) = constrs[1].clone();
    assert_eq!(lft_name.to_string(), "Chain.Lft".to_string());
    let typ1 = lft_def.typ_;
    #[rustfmt::skip]
    let typ2 = yatima!(
      "∀ (0 A B: Type) (0 x y: #Nat) (a: A) (as: #$0 A B x y) \
        -> (#$0 A B (#Nat.suc x) y)",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"Chain.Lft".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"Chain.Lft".to_string()), true));
    assert_eq!(typ1, typ2);
  }
  #[test]
  fn typedef_pair() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Pair (A B: Type) { \
        New (a: A) (b: B): Pair A B \
      }"
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res.type_of(), yatima!("∀ (0 A B: Type) -> Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("λ A B => @Pair.self ∀ \
      (0 P: ∀ (#$0 A B) -> Type) \
      (& New: ∀ (a: A) (b: B) \
        -> P (data λ P Pair.New => Pair.New a b)) \
      -> P Pair.self
    ", Term::Rec(Pos::None));
    // println!("res1: {}", res1.pretty(Some(&"Pair".to_string()), true));
    // println!("res2. {}", res2.pretty(Some(&"Pair".to_string()), true));
    assert_eq!(res1, res2);
    let constrs = res.constructors();
    let (new_name, new_def, _) = constrs[0].clone();
    assert_eq!(new_name.to_string(), "Pair.New".to_string());
    let typ1 = new_def.typ_;
    let typ2 =
      yatima!("∀ (0 A B: Type) (a: A) (b: B) -> (#$0 A B)", res.type_ref());
    println!("typ1: {}", typ1.pretty(Some(&"Pair.New".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"Pair.New".to_string()), true));
    assert_eq!(typ1, typ2);
  }
  #[test]
  fn typedef_equal() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Equal (A: Type) (a: A): ∀ (b: A) -> Type {\
         Refl: Equal A a a\
       }"
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res.type_of(), yatima!("∀ (0 A: Type) (0 a b: A) -> Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("λ A a b => @Equal.self ∀ \
      (0 P: ∀ (0 b: A) (#$0 A a b) -> Type) \
      (& Refl: P a (data λ P Equal.Refl => Equal.Refl)) \
      -> P b Equal.self
    ", Term::Rec(Pos::None));
    println!("res1: {}", res1.pretty(Some(&"Pair".to_string()), true));
    println!("res2. {}", res2.pretty(Some(&"Pair".to_string()), true));
    assert_eq!(res1, res2);
    let constrs = res.constructors();
    let (refl_name, refl_def, _) = constrs[0].clone();
    assert_eq!(refl_name.to_string(), "Equal.Refl".to_string());
    let typ1 = refl_def.typ_;
    let typ2 = yatima!("∀ (0 A: Type) (0 a: A) -> (#$0 A a a)", res.type_ref());
    println!("typ1: {}", typ1.pretty(Some(&"Equal.Refl".to_string()), true));
    println!("typ2: {}", typ2.pretty(Some(&"Equal.Refl".to_string()), true));
    assert_eq!(typ1, typ2);
  }
  #[test]
  fn typedef_triple() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Triple (A B C: Type): Type {\
         New (a: A) (b: B) (c: C): Triple A B C\
       }"
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res.type_of(), yatima!("∀ (0 A B C: Type) -> Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("λ A B C => @Triple.self ∀ \
      (0 P: ∀ (#$0 A B C) -> Type) \
      (& New: ∀ (a: A) (b: B) (c: C) -> P (data λ P Triple.New => Triple.New a b c)) \
      -> P Triple.self
    ", Term::Rec(Pos::None));
    println!("res1: {}", res1.pretty(Some(&"Triple".to_string()), true));
    println!("res2. {}", res2.pretty(Some(&"Triple".to_string()), true));
    assert_eq!(res1, res2);
    let constrs = res.constructors();
    let (new_name, new_def, _) = constrs[0].clone();
    assert_eq!(new_name.to_string(), "Triple.New".to_string());
    let typ1 = new_def.typ_;
    let typ2 = yatima!(
      "∀ (0 A B C: Type) (a: A) (b: B) (c: C) -> (#$0 A B C)",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"Triple.New".to_string()), true));
    println!("typ2: {}", typ2.pretty(Some(&"Triple.New".to_string()), true));
    assert_eq!(typ1, typ2);
  }
  #[test]
  fn typedef_tuple4() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Tuple4 (A B C D: Type): Type {\
         New (a: A) (b: B) (c: C) (d: D): Tuple4 A B C D\
       }"
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res.type_of(), yatima!("∀ (0 A B C D: Type) -> Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("λ A B C D => @Tuple4.self ∀ \
      (0 P: ∀ (#$0 A B C D) -> Type) \
      (& New: ∀ (a: A) (b: B) (c: C) (d: D) \
        -> P (data λ P Tuple4.New => Tuple4.New a b c d)) \
      -> P Tuple4.self
    ", Term::Rec(Pos::None));
    println!("res1: {}", res1.pretty(Some(&"Tuple4".to_string()), true));
    println!("res2. {}", res2.pretty(Some(&"Tuple4".to_string()), true));
    assert_eq!(res1, res2);
    let constrs = res.constructors();
    let (new_name, new_def, _) = constrs[0].clone();
    assert_eq!(new_name.to_string(), "Tuple4.New".to_string());
    let typ1 = new_def.typ_;
    let typ2 = yatima!(
      "∀ (0 A B C D: Type) (a: A) (b: B) (c: C) (d: D) -> (#$0 A B C D)",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"Tuple4.New".to_string()), true));
    println!("typ2: {}", typ2.pretty(Some(&"Tuple4.New".to_string()), true));
    assert_eq!(typ1, typ2);
  }
  #[test]
  fn typedef_tuple5() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Tuple5 (A B C D E: Type): Type {\
         New (a: A) (b: B) (c: C) (d: D) (e: E): Tuple5 A B C D E\
       }"
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res.type_of(), yatima!("∀ (0 A B C D E: Type) -> Type"));
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("λ A B C D E => @Tuple5.self ∀ \
      (0 P: ∀ (#$0 A B C D E) -> Type) \
      (& New: ∀ (a: A) (b: B) (c: C) (d: D) (e: E)\
        -> P (data λ P Tuple5.New => Tuple5.New a b c d e)) \
      -> P Tuple5.self
    ", Term::Rec(Pos::None));
    println!("res1: {}", res1.pretty(Some(&"Tuple5".to_string()), true));
    println!("res2. {}", res2.pretty(Some(&"Tuple5".to_string()), true));
    assert_eq!(res1, res2);
    let constrs = res.constructors();
    let (new_name, new_def, _) = constrs[0].clone();
    assert_eq!(new_name.to_string(), "Tuple5.New".to_string());
    let typ1 = new_def.typ_;
    #[rustfmt::skip]
    let typ2 = yatima!(
      "∀ (0 A B C D E: Type) (a: A) (b: B) (c: C) (d: D) (e: E) 
        -> (#$0 A B C D E)",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"Tuple5.New".to_string()), true));
    println!("typ2: {}", typ2.pretty(Some(&"Tuple5.New".to_string()), true));
    assert_eq!(typ1, typ2);
  }
  #[test]
  fn typedef_crazy() {
    #[rustfmt::skip]
    let res = test_parse(
      "type Crazy (A B C D E: Type): ∀ (a b c d e: #Nat) -> Type { \
         Nil: Crazy A B C D E 0 0 0 0 0, \
         ConsA (0 a b c d e: #Nat) (x: A) (xs: Crazy A B C D E a b c d e)\
          : Crazy A B C D E (#Nat.suc a) b c d e, \
         ConsB (0 a b c d e: #Nat) (x: B) (xs: Crazy A B C D E a b c d e)\
          : Crazy A B C D E a (#Nat.suc b) c d e, \
         ConsC (0 a b c d e: #Nat) (x: C) (xs: Crazy A B C D E a b c d e)\
          : Crazy A B C D E a b (#Nat.suc c) d e,\
         ConsD (0 a b c d e: #Nat) (x: D) (xs: Crazy A B C D E a b c d e)\
          : Crazy A B C D E a b c (#Nat.suc d) e, \
         ConsE (0 a b c d e: #Nat) (x: E) (xs: Crazy A B C D E a b c d e)\
          : Crazy A B C D E a b c d (#Nat.suc e), \
       }",
    );
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(
      res.type_of(),
      yatima!("∀ (0 A B C D E: Type) (0 a b c d e: #Nat) -> Type")
    );
    let res1 = res.term_of();
    #[rustfmt::skip]
    let res2 = yatima!("λ A B C D E a b c d e => @Crazy.self ∀ \
      (0 P: ∀ (0 a b c d e: #Nat) (#$0 A B C D E a b c d e) -> Type) \
      (& Nil: P 0 0 0 0 0 \
        (data λ P Crazy.Nil Crazy.ConsA Crazy.ConsB Crazy.ConsC Crazy.ConsD Crazy.ConsE => Crazy.Nil))\
      (& ConsA : ∀ (0 a b c d e: #Nat) (x: A) (xs: (#$0 A B C D E a b c d e)) \
        -> P (#Nat.suc a) b c d e \
        (data λ P Crazy.Nil Crazy.ConsA Crazy.ConsB Crazy.ConsC Crazy.ConsD Crazy.ConsE 
          => Crazy.ConsA a b c d e x xs))\
      (& ConsB : ∀ (0 a b c d e: #Nat) (x: B) (xs: (#$0 A B C D E a b c d e)) \
        -> P a (#Nat.suc b) c d e \
        (data λ P Crazy.Nil Crazy.ConsA Crazy.ConsB Crazy.ConsC Crazy.ConsD Crazy.ConsE
          => Crazy.ConsB a b c d e x xs))\
      (& ConsC : ∀ (0 a b c d e: #Nat) (x: C) (xs: (#$0 A B C D E a b c d e)) \
        -> P a b (#Nat.suc c) d e \
        (data λ P Crazy.Nil Crazy.ConsA Crazy.ConsB Crazy.ConsC Crazy.ConsD Crazy.ConsE
          => Crazy.ConsC a b c d e x xs))\
      (& ConsD : ∀ (0 a b c d e: #Nat) (x: D) (xs: (#$0 A B C D E a b c d e)) \
        -> P a b c (#Nat.suc d) e \
        (data λ P Crazy.Nil Crazy.ConsA Crazy.ConsB Crazy.ConsC Crazy.ConsD Crazy.ConsE
          => Crazy.ConsD a b c d e x xs))\
      (& ConsE : ∀ (0 a b c d e: #Nat) (x: E) (xs: (#$0 A B C D E a b c d e)) \
        -> P a b c d (#Nat.suc e) \
        (data λ P Crazy.Nil Crazy.ConsA Crazy.ConsB Crazy.ConsC Crazy.ConsD Crazy.ConsE
          => Crazy.ConsE a b c d e x xs))\
      -> P a b c d e Crazy.self
    ", Term::Rec(Pos::None));
    // println!("res1: {}", res1.pretty(Some(&"Crazy".to_string()), true));
    // println!("res2: {}", res2.pretty(Some(&"Crazy".to_string()), true));
    assert!(res1 == res2);
    let constrs = res.constructors();
    let (nil_name, nil_def, _) = constrs[0].clone();
    assert_eq!(nil_name.to_string(), "Crazy.Nil".to_string());
    let typ1 = nil_def.typ_;
    let typ2 = yatima!(
      "∀ (0 A B C D E: Type) -> (#$0 A B C D E 0 0 0 0 0)",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"Crazy.Nil".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"Crazy.Nil".to_string()), true));
    assert_eq!(typ1, typ2);
    let (consa_name, consa_def, _) = constrs[1].clone();
    assert_eq!(consa_name.to_string(), "Crazy.ConsA".to_string());
    let typ1 = consa_def.typ_;
    #[rustfmt::skip]
     let typ2 = yatima!(
      "∀ (0 A B C D E: Type) (0 a b c d e: #Nat) (x: A) (xs: #$0 A B C D E a b c d e) \
        -> (#$0 A B C D E (#Nat.suc a) b c d e)",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"Crazy.ConsA".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"Crazy.ConsA".to_string()), true));
    assert_eq!(typ1, typ2);
    let (consb_name, consb_def, _) = constrs[2].clone();
    assert_eq!(consb_name.to_string(), "Crazy.ConsB".to_string());
    let typ1 = consb_def.typ_;
    #[rustfmt::skip]
     let typ2 = yatima!(
      "∀ (0 A B C D E: Type) (0 a b c d e: #Nat) (x: B) (xs: #$0 A B C D E a b c d e) \
        -> (#$0 A B C D E a (#Nat.suc b) c d e)",
      res.type_ref()
    );
    println!("typ1: {}", typ1.pretty(Some(&"Crazy.ConsB".to_string()), true));
    println!("typ2. {}", typ2.pretty(Some(&"Crazy.ConsB".to_string()), true));
    assert_eq!(typ1, typ2);
  }

  #[test]
  fn test_no_sigs_list() {
    let res = test_parse(
      "type List (A: Type): Type { Nil: List A, Cons (x: A) (xs: List A): \
       List A}",
    );
    println!("res1: {:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    let res_no_sigs =
      test_parse("type List (A: Type) {Nil, Cons (x: A) (xs: List A)}");
    println!("res2: {:?}", res_no_sigs);
    assert!(res_no_sigs.is_ok());
    let res_no_sigs = res_no_sigs.unwrap().1;
    assert_eq!(res, res_no_sigs);
  }

  #[test]
  fn test_no_sigs_tuple() {
    let res = test_parse(
      "type Tuple2 (A B: Type): Type {New (a: A) (b: B): Tuple2 A B }",
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    let res_no_sigs = test_parse("type Tuple2 (A B: Type) {New (a: A) (b: B)}");
    println!("{:?}", res_no_sigs);
    assert!(res_no_sigs.is_ok());
    let res_no_sigs = res_no_sigs.unwrap().1;
    assert_eq!(res, res_no_sigs);
    let res = test_parse(
      "type Tuple3 (A B C: Type): Type {New (a: A) (b: B) (c: C): Tuple3 A B \
       C}",
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    let res_no_sigs =
      test_parse("type Tuple3 (A B C: Type) {New (a: A) (b: B) (c: C)}");
    println!("{:?}", res_no_sigs);
    assert!(res_no_sigs.is_ok());
    let res_no_sigs = res_no_sigs.unwrap().1;
    assert_eq!(res, res_no_sigs);
    let res = test_parse(
      "type Tuple4 (A B C D: Type): Type {New (a: A) (b: B) (c: C) (d: D): \
       Tuple4 A B C D}",
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    let res_no_sigs = test_parse(
      "type Tuple4 (A B C D : Type) {New (a: A) (b: B) (c: C) (d: D)}",
    );
    println!("{:?}", res_no_sigs);
    assert!(res_no_sigs.is_ok());
    let res_no_sigs = res_no_sigs.unwrap().1;
    assert_eq!(res, res_no_sigs);
    let res = test_parse(
      "type Tuple5 (A B C D E: Type): Type {New (a: A) (b: B) (c: C) (d: D) \
       (e: E): Tuple5 A B C D E}",
    );
    println!("{:?}", res);
    assert!(res.is_ok());
    let res = res.unwrap().1;
    let res_no_sigs = test_parse(
      "type Tuple5 (A B C D E: Type) {New (a: A) (b: B) (c: C) (d: D) (e: E)}",
    );
    println!("{:?}", res_no_sigs);
    assert!(res_no_sigs.is_ok());
    let res_no_sigs = res_no_sigs.unwrap().1;
    assert_eq!(res, res_no_sigs);
  }
}