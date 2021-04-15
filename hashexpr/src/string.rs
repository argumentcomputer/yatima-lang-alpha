////! Adapted from the examples in the nom repository
////! https://github.com/Geal/nom/blob/master/examples/string.rs
////! which licensed under the following MIT license:
////! https://github.com/Geal/nom/blob/master/LICENSE
////!
use crate::{
    error::{ParseError, ParseErrorKind},
    span::Span,
};
use nom::{
    branch::alt,
    bytes::complete::{take_till1, take_while_m_n},
    character::complete::{char, multispace1},
    combinator::{map, value, verify},
    multi::fold_many0,
    sequence::{delimited, preceded},
    Err, IResult,
};

pub fn parse_codepoint(i: Span) -> IResult<Span, char, ParseError<Span>> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
    let (i, s) = preceded(char('u'), delimited(char('{'), parse_hex, char('}')))(i)?;
    let s = s.fragment();
    match u32::from_str_radix(s, 16) {
        Ok(x) => match std::char::from_u32(x) {
            Some(c) => Ok((i, c)),
            _ => Err(Err::Error(ParseError::new(
                i,
                ParseErrorKind::InvalidBase16EscapeSequence(String::from(s.to_owned())),
            ))),
        },
        Err(e) => Err(Err::Error(ParseError::new(
            i,
            ParseErrorKind::ParseIntErr(e),
        ))),
    }
}
pub fn parse_escape(i: Span) -> IResult<Span, char, ParseError<Span>> {
    let (i, _) = char('\\')(i)?;
    alt((
        parse_codepoint,
        value('\n', char('n')),
        value('\r', char('r')),
        value('\t', char('t')),
        value('\u{08}', char('b')),
        value('\u{0C}', char('f')),
        value('\\', char('\\')),
        value('"', char('"')),
        value('\'', char('\'')),
    ))(i)
}

fn parse_escaped_whitespace(input: Span) -> IResult<Span, Span, ParseError<Span>> {
    preceded(char('\\'), multispace1)(input)
}

fn parse_literal<'a>(
    halt: &str,
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, ParseError<Span<'a>>> {
    let p = |c| halt.chars().any(|x| (x == c) | (c == '\\'));
    verify(take_till1(p), |s: &Span| !s.fragment().is_empty())(input)
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(Span<'a>),
    EscapedChar(char),
    EscapedWS,
}

fn parse_fragment<'a>(
    halt: &str,
    input: Span<'a>,
) -> IResult<Span<'a>, StringFragment<'a>, ParseError<Span<'a>>> {
    let lit = |i| parse_literal(halt, i);
    alt((
        map(lit, StringFragment::Literal),
        map(parse_escape, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))(input)
}

pub fn parse_string<'a>(
    halt: &str,
    input: Span<'a>,
) -> IResult<Span<'a>, String, ParseError<Span<'a>>> {
    let p = |i| parse_fragment(halt, i);
    fold_many0(p, String::new(), |mut string, fragment| {
        match fragment {
            StringFragment::Literal(s) => string.push_str(s.fragment()),
            StringFragment::EscapedChar(c) => string.push(c),
            StringFragment::EscapedWS => {}
        }
        string
    })(input)
}
