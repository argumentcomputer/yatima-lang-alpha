// pub fn parse_base_code(i: Span) ->
// pub fn parse_bits() -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>>
// {    alt((
//      value(Self::_2, tag("b")),
//      value(Self::_8, tag("o")),
//      value(Self::_10, tag("d")),
//      value(Self::_16, tag("x")),
//      value(Self::_32, tag("v")),
//      value(Self::_58, tag("I")),
//      value(Self::_64, tag("~")),
//    ))(i)
//
//
// pub fn parse_bits() -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>>
// {  move |from: Span| {
//    let (i, _) = tag("#\"")(from)?;
//    let (i, (_, val)) = base::parse(i).map_err(|e| nom::Err::convert(e))?;
//    let (i, _) = tag("\"")(i)?;
//    let (upto, _) = opt(tag(":bits"))(i)?;
//    Ok((i, bits!(Some(Pos::from_upto(from, upto)), val)))
//  }
//}

// pub fn parse_raw() -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>>
// {} pub fn parse_nat() -> impl Fn(Span) -> IResult<Span, Term,
// ParseError<Span>> {} pub fn parse_int() -> impl Fn(Span) -> IResult<Span,
// Term, ParseError<Span>> {} pub fn parse_text() -> impl Fn(Span) ->
// IResult<Span, Term, ParseError<Span>> {} pub fn parse_char() -> impl Fn(Span)
// -> IResult<Span, Term, ParseError<Span>> {}
