use chrono::NaiveDateTime;
use juniper::{DefaultScalarValue, FromInputValue};
use lazy_static::*;
use rug::Integer;
use serde_json::*;
use std::borrow::Cow;
use std::collections::HashSet;
use terminusdb_store_prolog::terminus_store::structure::*;
use terminusdb_store_prolog::value::split_lang_string;

pub fn value_to_string(tde: &TypedDictEntry) -> Cow<str> {
    Cow::Owned(tde.as_val::<String, String>())
}

pub fn value_to_bigint(tde: &TypedDictEntry) -> Integer {
    tde.as_val::<Integer, Integer>()
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
    static ref SMALL_INTEGER_TYPES: HashSet<&'static str> = [
        "byte",
        "short",
        "int",
        "long",
        "unsignedByte",
        "unsignedShort",
        "unsignedInt",
    ]
    .into_iter()
    .collect();
    static ref BIG_INTEGER_TYPES: HashSet<&'static str> = [
        "unsignedLong",
        "integer",
        "positiveInteger",
        "nonNegativeInteger",
        "negativeInteger",
        "nonPositiveInteger"
    ]
    .into_iter()
    .collect();
    static ref DATETIME_TYPES: HashSet<&'static str> =
        ["dateTime", "dateTimeStamp",].into_iter().collect();
}

#[derive(PartialEq, Eq, Debug)]
pub enum BaseTypeKind {
    String,
    SmallInteger,
    BigIntger,
    Boolean,
    DateTime,
    Float,
}

pub fn base_type_kind(s: &str) -> BaseTypeKind {
    if type_is_small_integer(s) {
        BaseTypeKind::SmallInteger
    } else if type_is_big_integer(s) {
        BaseTypeKind::BigIntger
    } else if type_is_bool(s) {
        BaseTypeKind::Boolean
    } else if type_is_float(s) {
        BaseTypeKind::Float
    } else if type_is_datetime(s) {
        BaseTypeKind::DateTime
    } else {
        BaseTypeKind::String
    }
}

pub fn type_is_bool(s: &str) -> bool {
    s == "boolean"
}

pub fn type_is_small_integer(s: &str) -> bool {
    SMALL_INTEGER_TYPES.contains(s)
}

pub fn type_is_big_integer(s: &str) -> bool {
    BIG_INTEGER_TYPES.contains(s)
}

pub fn type_is_float(s: &str) -> bool {
    FLOAT_TYPES.contains(s)
}

pub fn type_is_datetime(s: &str) -> bool {
    DATETIME_TYPES.contains(s)
}

pub fn value_to_json(tde: &TypedDictEntry) -> Value {
    match tde.datatype() {
        Datatype::Boolean => Value::Bool(tde.as_val::<bool, bool>()),
        Datatype::Token => {
            let x = tde.as_val::<String, String>();
            if x == "null" {
                Value::Null
            } else {
                Value::String(x)
            }
        }
        Datatype::UInt8 => Value::Number(tde.as_val::<u8, u8>().into()),
        Datatype::Int8 => Value::Number(tde.as_val::<i8, i8>().into()),
        Datatype::UInt16 => Value::Number(tde.as_val::<u16, u16>().into()),
        Datatype::Int16 => Value::Number(tde.as_val::<i16, i16>().into()),
        Datatype::UInt32 => Value::Number(tde.as_val::<u32, u32>().into()),
        Datatype::Int32 => Value::Number(tde.as_val::<i32, i32>().into()),
        Datatype::UInt64 => Value::Number(tde.as_val::<u64, u64>().into()),
        Datatype::Int64 => Value::Number(tde.as_val::<i64, i64>().into()),
        Datatype::Float32 => {
            Value::Number(Number::from_f64(tde.as_val::<f32, f32>() as f64).unwrap())
        }
        Datatype::Float64 => Value::Number(Number::from_f64(tde.as_val::<f64, f64>()).unwrap()),
        Datatype::String => Value::String(tde.as_val::<String, String>()),
        Datatype::Decimal => Value::String(tde.as_val::<Decimal, String>()),
        Datatype::BigInt => Value::String(tde.as_val::<Integer, String>()),
        Datatype::GYear => Value::String(tde.as_val::<GYear, String>()),
        Datatype::GMonth => Value::String(tde.as_val::<GMonth, String>()),
        Datatype::GDay => Value::String(tde.as_val::<GDay, String>()),
        Datatype::GYearMonth => Value::String(tde.as_val::<GYearMonth, String>()),
        Datatype::GMonthDay => Value::String(tde.as_val::<GMonthDay, String>()),
        // Tokens are currently just used for null but may be used for more things in the future
        //Datatype::Token => Value::String(tde.as_val::<Token, String>()),
        Datatype::LangString => {
            let x = tde.as_val::<LangString, String>();
            let (lang, s) = split_lang_string(&x);
            json!({ "@lang" : lang, "@value" : s })
        }
        Datatype::DateTime => {
            let ndt = tde.as_val::<NaiveDateTime, NaiveDateTime>();
            Value::String(ndt.format("%Y-%m-%dT%H:%M:%S%.fZ").to_string())
        }
        _ => todo!(),
    }
}

pub fn value_to_graphql(tde: &TypedDictEntry) -> juniper::Value<DefaultScalarValue> {
    match tde.datatype() {
        Datatype::Boolean => {
            juniper::Value::Scalar(DefaultScalarValue::Boolean(tde.as_val::<bool, bool>()))
        }
        Datatype::String => {
            juniper::Value::Scalar(DefaultScalarValue::String(tde.as_val::<String, String>()))
        }
        Datatype::UInt32 => {
            juniper::Value::Scalar(DefaultScalarValue::Float(tde.as_val::<u32, u32>() as f64))
        }
        Datatype::Int32 => {
            juniper::Value::Scalar(DefaultScalarValue::Int(tde.as_val::<i32, i32>()))
        }
        Datatype::UInt64 => juniper::Value::Scalar(DefaultScalarValue::String(
            tde.as_val::<u64, u64>().to_string(),
        )),
        Datatype::Int64 => juniper::Value::Scalar(DefaultScalarValue::String(
            tde.as_val::<u64, u64>().to_string(),
        )),
        Datatype::Float32 => {
            juniper::Value::Scalar(DefaultScalarValue::Float(tde.as_val::<f32, f32>() as f64))
        }
        Datatype::Float64 => {
            juniper::Value::Scalar(DefaultScalarValue::Float(tde.as_val::<f64, f64>()))
        }
        Datatype::Decimal => {
            juniper::Value::Scalar(DefaultScalarValue::String(tde.as_val::<Decimal, String>()))
        }
        Datatype::BigInt => {
            juniper::Value::Scalar(DefaultScalarValue::String(tde.as_val::<Integer, String>()))
        }
        Datatype::Token => {
            juniper::Value::Scalar(DefaultScalarValue::String(tde.as_val::<Token, String>()))
        }
        Datatype::LangString => juniper::Value::Scalar(DefaultScalarValue::String(
            tde.as_val::<LangString, String>(),
        )),
        Datatype::GYear => {
            juniper::Value::Scalar(DefaultScalarValue::String(tde.as_val::<GYear, String>()))
        }
        Datatype::GMonth => {
            juniper::Value::Scalar(DefaultScalarValue::String(tde.as_val::<GMonth, String>()))
        }
        Datatype::GDay => {
            juniper::Value::Scalar(DefaultScalarValue::String(tde.as_val::<GDay, String>()))
        }
        Datatype::GYearMonth => juniper::Value::Scalar(DefaultScalarValue::String(
            tde.as_val::<GYearMonth, String>(),
        )),
        Datatype::GMonthDay => juniper::Value::Scalar(DefaultScalarValue::String(
            tde.as_val::<GMonthDay, String>(),
        )),
        Datatype::DateTime => {
            let ndt = tde.as_val::<NaiveDateTime, NaiveDateTime>();
            juniper::Value::Scalar(DefaultScalarValue::String(
                ndt.format("%Y-%m-%dT%H:%M:%S%.fZ").to_string(),
            ))
        }
        _ => todo!(),
    }
}

pub fn enum_node_to_value(enum_type: &str, enum_uri: &str) -> String {
    let enum_list: Vec<&str> = enum_uri.split(enum_type).collect();
    // 1.. is from slash on
    let enum_value = &(enum_list[1])[1..];
    enum_value.to_string()
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

pub fn value_to_usize(tde: &TypedDictEntry) -> usize {
    tde.as_val::<u32, u32>() as usize
}
