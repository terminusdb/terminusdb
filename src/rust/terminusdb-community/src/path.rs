use lazy_static::*;
use nom::bytes::complete::take_while;
use nom::complete::tag;
use nom::Err;
use nom::{
    branch::alt,
    character::{is_alphanumeric, is_digit},
    combinator::{map, map_res},
    error::ErrorKind,
    sequence::{delimited, pair, separated_pair, terminated, tuple},
    IResult,
};
use std::rc::Rc;

/*

P := . | p(String)
Q,R := P> | <P | Q,R | Q;R | plus(Q) | star(Q) | times(Q,N,M)

 */

#[derive(Debug)]
enum Pred {
    Any,
    Named(String),
}

#[derive(Debug)]
enum Path {
    Seq(Rc<Path>, Rc<Path>),
    Choice(Rc<Path>, Rc<Path>),
    Positive(Pred),
    Negative(Pred),
    Plus(Rc<Path>),
    Star(Rc<Path>),
    Times(Rc<Path>, u32, u32),
}

/*
fn take_while<F>(f: F) -> impl Fn(&str) -> IResult<&str, &str>
where
    F: Fn(&char) -> bool,
{
    move |s: &str| {
        let mut characters = s.chars().enumerate();
        while let Some((idx, c)) = characters.next() {
            if !f(&c) {
                return Ok((&s[0..idx], &s[idx..s.len()]));
            }
        }
        Ok((s, &s[s.len()..s.len()]))
    }
}

fn tag<'a>(s: &'a str) -> impl Fn(&str) -> IResult<&str, &str> + 'a {
    let length = s.len();
    move |t: &str| {
        if t[0..length] == *s {
            Ok((&t[0..length], &t[length..t.len()]))
        } else {
            Err(Err::Error(nom::error::Error {
                input: "",
                code: ErrorKind::Tag,
            }))
        }
    }
}*/

fn is_property_char(c: char) -> bool {
    c.is_alphanumeric() || c == ':' || c == '/' || c == '_' || c == '-'
}

fn named(input: &str) -> IResult<&str, &str> {
    take_while(is_property_char)(input)
}

fn num(input: &str) -> IResult<&str, u32> {
    map_res(
        take_while(|c: char| c.is_digit(10)),
        |digits: &str| -> Result<u32, _> { Ok::<u32, ErrorKind>(digits.parse::<u32>().unwrap()) },
    )(input)
}

fn pred(input: &str) -> IResult<&str, Pred> {
    alt((
        map_res(tag("."), |s: _| Pred::Any),
        map_res(named, |string| Pred::Named(string.to_string())),
    ))(input)
}

fn positive(input: &str) -> IResult<&str, Pred> {
    terminated(pred, tag(">"))(input)
}

fn negative(input: &str) -> IResult<&str, Pred> {
    terminated(pred, tag("<"))(input)
}

fn patterns(input: &str) -> IResult<&str, Path> {
    alt((
        map(positive, |elt| Path::Positive(elt)),
        map(negative, |elt| Path::Negative(elt)),
        delimited(tag("(", 8), ands, tag(")", 8)),
    ))(input)
}

fn plus(input: &str) -> IResult<&str, Path> {
    terminated(patterns, tag("+", 8))(input)
}

fn star(input: &str) -> IResult<&str, Path> {
    terminated(patterns, tag("*", 8))(input)
}

fn size_bracket(input: &str) -> IResult<&str, (u32, u32)> {
    delimited(tag("{"), separated_pair(num, tag(","), num), tag("}"))(input)
}

fn times(input: &str) -> IResult<&str, Path> {
    map(pair(patterns, size_bracket), |(p, (n, m))| {
        Path::Times(Rc::new(p), n, m)
    })(input)
}

fn repeat_patterns(input: &str) -> IResult<&str, Path> {
    alt((
        map(plus, |elt| Path::Plus(Rc::new(elt))),
        map(star, |elt| Path::Star(Rc::new(elt))),
        times,
        patterns,
    ))(input)
}

fn ors(input: &str) -> IResult<&str, Path> {
    alt((
        map(
            separated_pair(repeat_patterns, tag("|"), repeat_patterns),
            |(left, right)| Path::Choice(Rc::new(left), Rc::new(right)),
        ),
        repeat_patterns,
    ))(input)
}

fn ands(input: &str) -> IResult<&str, Path> {
    alt((
        map(separated_pair(ors, tag(","), ors), |(left, right)| {
            Path::Seq(Rc::new(left), Rc::new(right))
        }),
        ors,
    ))(input)
}

fn path(input: &str) -> Path {
    ands(input).expect("Should have been ok").1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test() {
        let source = "(<effect,cause)+";
        let results = path(source);
        eprintln!("{results:?}");
        panic!("Test")
    }
}
