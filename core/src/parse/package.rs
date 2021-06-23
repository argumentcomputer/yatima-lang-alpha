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
    base::parse_multibase,
    error::{
      ParseError,
      ParseErrorKind,
    },
    span::Span,
    term::*,
    typedef::parse_typedef_elaborated,
  },
  term::*,
};

use sp_std::{
  cell::RefCell,
  collections::vec_deque::VecDeque,
  convert::TryFrom,
  rc::Rc,
  vec::Vec,
};

use nom::{
  branch::alt,
  bytes::complete::tag,
  combinator::{
    eof,
    opt,
  },
  multi::separated_list0,
  sequence::terminated,
  Err,
  IResult,
};

use sp_cid::Cid;
use sp_im::conslist::ConsList;

pub fn parse_link(from: Span) -> IResult<Span, Cid, ParseError<Span>> {
  let (upto, (_, bytes)) = parse_multibase()(from)?;
  match Cid::try_from(bytes) {
    Ok(cid) => Ok((upto, cid)),
    Err(_) => Err(Err::Error(ParseError::new(upto, ParseErrorKind::CidError))),
  }
}

pub fn parse_alias(i: Span) -> IResult<Span, Name, ParseError<Span>> {
  let (i, _) = tag("as")(i)?;
  let (i, _) = parse_space(i)?;
  let (i, a) = parse_name(i)?;
  Ok((i, a))
}

pub fn parse_with(i: Span) -> IResult<Span, Vec<Name>, ParseError<Span>> {
  let (i, _) = tag("(")(i)?;
  let (i, ns) = separated_list0(
    terminated(tag(","), parse_space),
    terminated(parse_name, parse_space),
  )(i)?;
  let (i, _) = tag(")")(i)?;
  Ok((i, ns))
}

pub fn parse_import(i: Span) -> IResult<Span, Import, ParseError<Span>> {
  let (i, _) = tag("import")(i)?;
  let (i, _) = parse_space(i)?;
  let (i, name) = parse_name(i)?;
  let (i, _) = parse_space(i)?;
  let (i, alias) = opt(terminated(parse_alias, parse_space))(i)?;
  let alias = alias.unwrap_or_else(|| Name::from(""));
  let (i, with) = terminated(parse_with, parse_space)(i)?;
  let (i, from) = terminated(parse_link, parse_space)(i)?;
  Ok((i, Import { cid: from, name, alias, with }))
}

pub fn parse_entry(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
) -> impl Fn(Span) -> IResult<Span, Vec<(Name, Def, Entry)>, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("def")(from)?;
    let (i, _) = parse_space(i)?;
    let (i, nam) = parse_name(i)?;
    if defs.borrow().names.get(&nam.clone()).is_some() {
      Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::TopLevelRedefinition(nam),
      )))
    }
    else {
      let (i, _) = parse_space(i)?;
      let (upto, (typ_, term)) = parse_bound_expression(
        input,
        defs.clone(),
        None,
        Some(nam.clone()),
        ConsList::new(),
        Rc::new(VecDeque::new()),
        nam.clone(),
        false,
      )(i)?;
      let pos = Pos::from_upto(input, from, upto);
      let (def, entry) = Def::make(pos, typ_, term);
      Ok((upto, vec![(nam, def, entry)]))
    }
  }
}

pub fn parse_defs(
  input: Cid,
  import_defs: Defs,
) -> impl Fn(Span) -> IResult<Span, (Defs, Index), ParseError<Span>> {
  move |i: Span| {
    let defs = Rc::new(RefCell::new(import_defs.clone()));
    let mut ind: Vec<(Name, Cid)> = Vec::new();
    let mut i = i;
    loop {
      let (i2, _) = parse_space(i)?;
      i = i2;
      let end: IResult<Span, Span, ParseError<Span>> = eof(i);
      if end.is_ok() {
        return Ok((i2, (defs.as_ref().clone().into_inner(), Index(ind))));
      }
      else {
        let (i2, entries) = alt((
          parse_entry(input, defs.clone()),
          parse_typedef_elaborated(input, defs.clone()),
        ))(i)?;
        for (name, def, _) in entries {
          ind.push((name.clone(), def.def_cid));
          defs.borrow_mut().insert(name, def);
        }
        i = i2;
      }
    }
  }
}

// #[cfg(test)]
// pub mod tests {
//  use super::*;
//
//  #[test]
//  fn test_cases() {
//    let res = parse_with(Span::new("()"));
//    println!("res: {:?}", res);
//    assert!(res.is_ok());
//    let res = parse_with(Span::new("(a)"));
//    println!("res: {:?}", res);
//    assert!(res.is_ok());
//    let res = parse_with(Span::new("(a,b)"));
//    println!("res: {:?}", res);
//    assert!(res.is_ok());
//    let res = parse_with(Span::new("(a,b,c)"));
//    println!("res: {:?}", res);
//    assert!(res.is_ok());
//  }
// }
