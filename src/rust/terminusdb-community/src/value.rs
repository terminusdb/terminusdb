use lazy_static::*;
use std::borrow::Cow;
use std::collections::HashSet;
use std::str::FromStr;

use serde_json::*;

const TYPE_PREFIX_LEN: usize = "http://www.w3.org/2001/XMLSchema#".len();
// funnily, this type prefix works for both the xsd types, and our own custom terminusdb xdd types, as the prefix is the same length!
// for terminusdb xdd this is   http://terminusdb.com/schema/xdd#

enum LangOrType<'a> {
    Lang(&'a str, &'a str),
    Type(&'a str, &'a str),
}

// FIXME: this does not currently deal with langstrings
fn value_string_to_slices(s: &str) -> LangOrType {
    // The format of these value strings is something like
    if s.as_bytes()[s.len() - 1] == '\'' as u8 {
        let pos = s[..s.len() - 1].rfind('\'').unwrap();
        if s.as_bytes()[pos - 1] == '^' as u8 {
            assert!(s.as_bytes()[pos - 2] == '^' as u8);
            LangOrType::Type(&s[0..pos - 2], &s[pos + 1 + TYPE_PREFIX_LEN..s.len() - 1])
        } else {
            assert!(s.as_bytes()[pos - 1] == '@' as u8);
            LangOrType::Lang(&s[..pos - 1], &s[pos..])
        }
    } else {
        let pos = s.rfind('@').unwrap();
        LangOrType::Lang(&s[..pos], &s[pos + 1..])
    }
}

lazy_static! {
    static ref NUMERIC_TYPES: HashSet<&'static str> = [
        "decimal",
        "double",
        "float",
        "byte",
        "short",
        "int",
        "long",
        "unsignedByte",
        "unsignedShort",
        "unsignedInt",
        "unsignedLong",
        "integer",
        "positiveInteger",
        "nonNegativeInteger",
        "negativeInteger",
        "nonPositiveInteger"
    ]
    .into_iter()
    .collect();
}

fn type_is_numeric(s: &str) -> bool {
    NUMERIC_TYPES.contains(s)
}

pub fn value_string_to_json(s: &str) -> Value {
    match value_string_to_slices(s) {
        LangOrType::Type(val, typ) => {
            if typ == "boolean" {
                Value::Bool(val == "\"true\"")
            } else if typ == "token" && val == "\"null\"" {
                Value::Null
            } else if type_is_numeric(typ) {
                // it will have been saved unquoted
                //Value::Number(Number::from_string_unchecked(val.to_string())) // undocumented api - we know this is a number, so might as well save parse effort
                Value::Number(Number::from_str(val).unwrap())
            } else {
                // it will be something quoted, which we're gonna return as a string
                Value::String(prolog_string_to_string(val).into_owned())
            }
        }
        LangOrType::Lang(val, lang) => {
            let s = prolog_string_to_string(val);
            let l = lang[1..lang.len() - 1].to_string();
            json!({ "@lang" : l, "@value" : s })
        }
    }
}

#[allow(dead_code)]
pub fn enum_node_to_value(enum_type: &str, enum_uri: &str) -> String {
    let enum_list: Vec<&str> = enum_uri.split(enum_type).collect();
    // 1.. is from slash on
    let enum_value = &(enum_list[1])[1..];
    enum_value.to_string()
}

#[allow(dead_code)]
pub enum ScalarInputValue {
    Boolean(bool),
    Int(i32),
    Float(f64),
    String(String),
}

pub fn value_string_to_usize(s: &str) -> usize {
    if let LangOrType::Type(val, typ) = value_string_to_slices(s) {
        if !type_is_numeric(typ) {
            panic!("not a numeric type: {}", typ);
        }

        usize::from_str(val).unwrap()
    } else {
        panic!("unexpected lang string");
    }
}

const SWIPL_CONTROL_CHAR_A: char = 7 as char;
const SWIPL_CONTROL_CHAR_B: char = 8 as char;
const SWIPL_CONTROL_CHAR_E: char = 27 as char;
const SWIPL_CONTROL_CHAR_F: char = 12 as char;
const SWIPL_CONTROL_CHAR_V: char = 11 as char;


fn prolog_string_to_string(s: &str) -> Cow<str> {
    let mut result: Option<String> = None;
    let mut escaping = false;
    let mut characters = s.chars().enumerate().skip(1).take(s.len()-2);
    while let Some((ix, c)) = characters.next() {
        if escaping {
            let result = result.as_mut().unwrap();
            match c {
                '\\' => result.push('\\'),
                '\"' => result.push('\"'),
                'x' => result.push(unescape_legacy_prolog_escape_sequence(&mut characters)),
                'a' => result.push(SWIPL_CONTROL_CHAR_A),
                'b' => result.push(SWIPL_CONTROL_CHAR_B),
                't' => result.push('\t'),
                'n' => result.push('\n'),
                'v' => result.push(SWIPL_CONTROL_CHAR_V),
                'f' => result.push(SWIPL_CONTROL_CHAR_F),
                'r' => result.push('\r'),
                _ => panic!("unknown prolog escape code in string")
            }

            escaping = false;
        }
        else {
            if c == '\\' {
                escaping = true;
                if result.is_none() {
                    let mut r = String::with_capacity(s.len());
                    r.push_str(&s[1..ix]);
                    result = Some(r);
                }
            }
            else if let Some(result) = result.as_mut(){
                result.push(c);
            }
        }
    }

    match result {
        Some(result) => Cow::Owned(result),
        None => Cow::Borrowed(&s[1..s.len()-1])
    }
}

fn unescape_legacy_prolog_escape_sequence(characters: &mut impl Iterator<Item=(usize, char)>) -> char {
    let mut digits: String = String::new();
    loop {
        let (_,digit) = characters.next().unwrap();
        if digit == '\\' {
            let hex = u32::from_str_radix(&digits, 16).unwrap();
            return char::from_u32(hex).unwrap();
        }
        else {
            digits.push(digit);
        }
    }
}

#[allow(dead_code)]
fn string_to_prolog_string(s: &str) -> String {
    // incomplete - lots of control characters gets printed weirdly by prolog.
    let mut result = String::with_capacity(s.len() + 10);
    result.push('"');

    for c in s.chars() {
        match c {
            '\\' => result.push_str(r#"\\"#),
            '\n' => result.push_str(r#"\n"#),
            '\t' => result.push_str(r#"\t"#),
            '\r' => result.push_str(r#"\r"#),
            SWIPL_CONTROL_CHAR_A => result.push_str(r#"\\a"#),
            SWIPL_CONTROL_CHAR_B => result.push_str(r#"\\b"#),
            SWIPL_CONTROL_CHAR_E => result.push_str(r#"\u001B"#),
            SWIPL_CONTROL_CHAR_F => result.push_str(r#"\\f"#),
            SWIPL_CONTROL_CHAR_V => result.push_str(r#"\\v"#),
            '\"' => result.push_str(r#"\""#),
            _ => result.push(c)
        }
    }

    result.push('"');
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn escape_strings_to_prolog_string() {
        assert_eq!(r#""hello there""#, string_to_prolog_string("hello there"));
        assert_eq!(r#""hello\nthere""#, string_to_prolog_string("hello\nthere"));
    }
    #[test]
    fn unescape_prolog_strings() {
        assert_eq!("hello there", prolog_string_to_string(r#""hello there""#));
        assert_eq!("hello\"there", prolog_string_to_string(r#""hello\"there""#));
        assert_eq!("hello\nthere", prolog_string_to_string(r#""hello\nthere""#));
        assert_eq!("hello\\there", prolog_string_to_string(r#""hello\\there""#));
        assert_eq!("\"", prolog_string_to_string(r#""\"""#));
        assert_eq!("foo\u{5555}bar", prolog_string_to_string(r#""foo\x5555\bar""#));
        assert_eq!("foo\u{5555}bar\u{6666}baz", prolog_string_to_string(r#""foo\x5555\bar\x6666\baz""#));
    }
}
