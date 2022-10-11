use juniper::{DefaultScalarValue, FromInputValue};
use lazy_static::*;
use std::collections::HashSet;
use std::str::FromStr;

use serde_json::*;

use crate::consts::XSD_PREFIX;

const TYPE_PREFIX_LEN: usize = XSD_PREFIX.len();
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
    static ref FLOAT_TYPES: HashSet<&'static str> =
        ["decimal", "double", "float",].into_iter().collect();
    static ref INTEGER_TYPES: HashSet<&'static str> = [
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

pub fn type_is_bool(s: &str) -> bool {
    s == "boolean"
}

pub fn type_is_integer(s: &str) -> bool {
    INTEGER_TYPES.contains(s)
}

pub fn type_is_float(s: &str) -> bool {
    FLOAT_TYPES.contains(s)
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

pub fn value_string_to_graphql(s: &str) -> juniper::Value<DefaultScalarValue> {
    match value_string_to_slices(s) {
        LangOrType::Type(val, typ) => {
            if typ == "boolean" {
                juniper::Value::Scalar(DefaultScalarValue::Boolean(val == "\"true\""))
            } else if typ == "token" && val == "\"null\"" {
                juniper::Value::Null
            } else if type_is_integer(typ) {
                juniper::Value::Scalar(DefaultScalarValue::Int(i32::from_str(val).unwrap()))
            } else if type_is_float(typ) {
                juniper::Value::Scalar(DefaultScalarValue::Float(f64::from_str(val).unwrap()))
            } else {
                // it will be something quoted, which we're gonna return as a string
                juniper::Value::Scalar(DefaultScalarValue::String(
                    val[1..val.len() - 1].to_string(),
                ))
            }
        }
        LangOrType::Lang(val, lang) => {
            // TODO: we need to include language tag here somehow
            let s = val[1..val.len() - 1].to_string();
            let _l = lang[1..lang.len() - 1].to_string();
            juniper::Value::Scalar(DefaultScalarValue::String(s))
        }
    }
}

pub fn enum_node_to_value(enum_type: &str, enum_uri: &str) -> juniper::Value<DefaultScalarValue> {
    let enum_list: Vec<&str> = enum_uri.split(enum_type).collect();
    // 1.. is from slash on
    let enum_value = &(enum_list[1])[1..];
    juniper::Value::Scalar(DefaultScalarValue::String(enum_value.to_string()))
}

pub enum ScalarInputValue {
    Boolean(bool),
    Int(i32),
    Float(f64),
    String(String),
}

impl FromInputValue for ScalarInputValue {
    fn from_input_value(v: &juniper::InputValue<DefaultScalarValue>) -> Option<Self> {
        match v {
            juniper::InputValue::Scalar(s) => Some(match s {
                DefaultScalarValue::Int(i) => Self::Int(*i),
                DefaultScalarValue::Float(f) => Self::Float(*f),
                DefaultScalarValue::String(s) => Self::String(s.to_owned()),
                DefaultScalarValue::Boolean(b) => Self::Boolean(*b),
            }),
            _ => None,
        }
    }
}

pub fn graphql_scalar_to_value_string(v: ScalarInputValue, base_type: &str) -> String {
    let base_type = &base_type[4..];
    match v {
        ScalarInputValue::Boolean(b) => {
            assert!(type_is_bool(base_type));
            format!("{}^^'{}{}'", b, XSD_PREFIX, base_type)
        }
        ScalarInputValue::Int(i) => {
            assert!(type_is_integer(base_type));
            format!("{}^^'{}{}'", i, XSD_PREFIX, base_type)
        }
        ScalarInputValue::Float(f) => {
            assert!(type_is_float(base_type));
            format!("{}^^'{}{}'", f, XSD_PREFIX, base_type)
        }
        ScalarInputValue::String(s) => {
            format!("\"{}\"^^'{}{}'", s, XSD_PREFIX, base_type)
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
