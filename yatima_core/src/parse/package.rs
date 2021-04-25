use crate::{
  defs::{
    Def,
    Defs,
  },
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
  },
  term::*,
};

use std::{
  convert::TryFrom,
  rc::Rc,
};

use im::Vector;

use nom::{
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

use libipld::Cid;

pub fn parse_link(from: Span) -> IResult<Span, Cid, ParseError<Span>> {
  let (upto, (_, bytes)) = parse_multibase()(from)?;
  match Cid::try_from(bytes) {
    Ok(cid) => Ok((upto, cid)),
    Err(_) => Err(Err::Error(ParseError::new(upto, ParseErrorKind::CidError))),
  }
}

pub fn parse_alias(i: Span) -> IResult<Span, String, ParseError<Span>> {
  let (i, _) = tag("as")(i)?;
  let (i, _) = parse_space(i)?;
  let (i, a) = parse_name(i)?;
  Ok((i, a))
}

pub fn parse_with(i: Span) -> IResult<Span, Vec<String>, ParseError<Span>> {
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
  let alias = alias.unwrap_or(String::from(""));
  let (i, with) = terminated(parse_with, parse_space)(i)?;
  let (i, from) = terminated(parse_link, parse_space)(i)?;
  Ok((i, Import { cid: from, name, alias, with }))
}

pub fn parse_entry(
  input: Cid,
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, (String, Def, Entry), ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("def")(from)?;
    let (i, _) = parse_space(i)?;
    let (i, nam) = parse_name(i)?;
    if defs.0.get(&nam).is_some() {
      Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::TopLevelRedefinition(nam),
      )))
    }
    else {
      let (i, _) = parse_space(i)?;
      let (upto, (typ_, term)) = parse_bound_expression(
        input,
        defs.to_owned(),
        Some(Rc::new(nam.clone())),
        Vector::new(),
        nam.clone(),
        false,
      )(i)?;
      let pos = Pos::from_upto(input, from, upto);
      let (def, entry) = Def::make(pos, typ_, term);
      Ok((upto, (nam.clone(), def, entry)))
    }
  }
}

pub fn parse_defs(
  input: Cid,
  import_defs: Defs,
) -> impl Fn(Span) -> IResult<Span, (Defs, Index), ParseError<Span>> {
  move |i: Span| {
    let mut defs: Defs = import_defs.clone();
    let mut ind: Vec<(String, Cid)> = Vec::new();
    let mut i = i;
    loop {
      let (i2, _) = parse_space(i)?;
      i = i2;
      let end: IResult<Span, Span, ParseError<Span>> = eof(i);
      if end.is_ok() {
        return Ok((i2, (defs, Index(ind))));
      }
      else {
        let (i2, (name, def, _)) = parse_entry(input, defs.clone())(i)?;
        ind.push((name.clone(), def.def_cid));
        defs.0.insert(name, def);
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
