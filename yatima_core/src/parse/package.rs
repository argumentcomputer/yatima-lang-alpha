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
  },
  term::*,
};

use std::convert::TryFrom;

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
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, (Name, Def, Entry), ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("def")(from)?;
    let (i, _) = parse_space(i)?;
    let (i, nam) = parse_name(i)?;
    if defs.names.get(&nam.clone()).is_some() {
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
        Some(nam.clone()),
        Vector::new(),
        Vector::new(),
        nam.clone(),
        false,
      )(i)?;
      let pos = Pos::from_upto(input, from, upto);
      let (def, entry) = Def::make(pos, typ_, term);
      Ok((upto, (nam, def, entry)))
    }
  }
}

pub fn parse_defs(
  input: Cid,
  import_defs: Defs,
) -> impl Fn(Span) -> IResult<Span, (Defs, Index), ParseError<Span>> {
  move |i: Span| {
    let mut defs: Defs = import_defs.clone();
    let mut ind: Vec<(Name, Cid)> = Vec::new();
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
        defs.insert(name, def);
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
