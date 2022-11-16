use lazy_static::*;
use nom::{
    branch::alt,
    character::{is_alphanumeric, is_digit},
    combinator::{map, map_res},
    sequence::{delimited, pair, separated_pair, terminated, tuple},
    IResult,
};
use regex::Regex;
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

lazy_static! {
    static ref PROPERTY: Regex =
        Regex::new(r"[A-Za-z]+[A-Za-z0-9:/_]").expect("This is built lazy static");
}

fn take_while(f: Fn(&char) -> bool) -> impl Fn(&str) -> IResult<&str, &str> {
    move |s: &str| {
        let characters = s.chars().enumerate();
        while let Some((idx, c)) = characters.next() {
            if !f(&c) {
                return OK(s[0..idx]);
            }
        }
        OK(s)
    }
}

fn is_property_char(c: &char) -> bool {
    c.is_alphanumeric() || c == ':' || c == '/' || c == '_' || c == '-'
}

fn named(input: &str) -> IResult<&str, &str> {
    take_while(is_property_char)(input)
}

fn num(input: &str) -> IResult<&str, u32> {
    map_res(take_while(is_digit), |digits| {
        digits.parse::<i32>().unwrap()
    })(input)
}

fn pred(input: &str) -> IResult<&str, Path> {
    alt((
        map(tag("."), |_| Path::Any),
        map(named, |string| Path::Named(string)),
    ))(input)
}

fn positive(input: &str) -> IResult<&str, Pred> {
    teriminated(pred, tag(">"))(input)
}

fn negative(input: &str) -> IResult<&str, Pred> {
    teriminated(pred, tag("<"))(input)
}

fn patterns(input: &str) -> IResult<&str, Path> {
    alt((
        map(positive, |elt| Path::Positive(elt)),
        map(negative, |elt| Path::Negative(elt)),
        delimited(tag("("), ands, tag(")")),
    ))(input)
}

fn plus(input: &str) -> IResult<&str, Path> {
    terminated(patterns, '+')(input)
}

fn star(input: &str) -> IResult<&str, Path> {
    terminated(patterns, '*')(input)
}

fn size_bracket(input: &str) -> IResult<&str, (u32, u32)> {
    delimited(tag("{"), separated_pair(num, tag(","), num), tag("}"))(input)
}

fn times(input: &str) -> IResult<&str, Path> {
    map(pair(patterns(input), size_bracket), |(p, (n, m))| {
        Path::Times(Rc::new(p), n, m)
    })(input)
}

fn repeat_patterns(input: &str) -> IResult<&str, Path> {
    alt((
        map(plus, |elt| Path::Plus(Rc::new(elt))),
        map(star, |elt| Path::Star(Rc::new(elt))),
        map(times, |elt| {
            let (p, n, m) = elt;
            Path::Times(Rc::new(p), n, m)
        }),
        patterns,
    ))(input)
}

fn ors(input: &str) -> IResult<&str, Path> {
    alt((
        map(
            separated_pair(repeat_patterns, tag("|"), repeat_patterns),
            (|left, right| Path::Choice(left, right)),
        ),
        repeat_patterns,
    ))(input)
}

fn ands(input: &str) -> IResult<&str, Path> {
    alt((
        map(
            separated_pair(ors, tag(","), ors),
            (|left, right| Path::Seq(left, right)),
        ),
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
