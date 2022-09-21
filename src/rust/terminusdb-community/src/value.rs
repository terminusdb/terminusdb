use lazy_static::*;
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
                Value::String(val[1..val.len() - 1].to_string())
            }
        }
        LangOrType::Lang(val, lang) => {
            let s = val[1..val.len() - 1].to_string();
            let l = lang[1..lang.len() - 1].to_string();
            json!({ "@lang" : l, "@value" : s })
        }
    }
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
