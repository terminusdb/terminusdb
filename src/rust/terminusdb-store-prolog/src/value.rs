use chrono::{Datelike, NaiveDate, NaiveDateTime, Timelike};
use rug::Integer;
use swipl::prelude::*;
use swipl::term::Term;
use terminus_store::structure::*;

pub fn make_entry_from_term<C: QueryableContextType>(
    _context: &Context<C>,
    inner_term: &Term,
    ty_term: &Term,
) -> PrologResult<TypedDictEntry> {
    let ty: Atom = ty_term.get_ex()?;
    if atom!("http://www.w3.org/2001/XMLSchema#string") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(String::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#decimal") == ty {
        let inner_number: f64 = inner_term.get_ex()?;
        Ok(f64::make_entry(&inner_number))
    } else if atom!("http://www.w3.org/2001/XMLSchema#integer") == ty {
        let inner_number: PrologText = inner_term.get_ex()?;
        let integer: Integer = Integer::parse(&*inner_number).unwrap().into();
        Ok(Integer::make_entry(&integer))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gYear") == ty {
        let inner_number: i64 = inner_term.get_ex()?;
        Ok(GYear::make_entry(&GYear(inner_number)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#positiveInteger") == ty {
        let inner_number: PrologText = inner_term.get_ex()?;
        let integer: Integer = Integer::parse(&*inner_number).unwrap().into();
        Ok(PositiveInteger::make_entry(&PositiveInteger(integer)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#anyURI") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(AnyURI::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#dateTime") == ty {
        let year: i64 = inner_term.get_arg(1)?;
        let month: i64 = inner_term.get_arg(2)?;
        let day: i64 = inner_term.get_arg(3)?;
        let hour: i64 = inner_term.get_arg(4)?;
        let minute: i64 = inner_term.get_arg(5)?;
        let second: i64 = inner_term.get_arg(6)?;
        let nano: i64 = inner_term.get_arg(7)?;
        let dt = NaiveDate::from_ymd_opt(year as i32, month as u32, day as u32)
            .unwrap()
            .and_hms_nano_opt(hour as u32, minute as u32, second as u32, nano as u32)
            .unwrap();
        Ok(NaiveDateTime::make_entry(&dt))
    } else {
        Err(PrologError::Exception)
    }
}

pub fn make_entry_from_lang_term<C: QueryableContextType>(
    _context: &Context<C>,
    inner_term: &Term,
    lang_term: &Term,
) -> PrologResult<TypedDictEntry> {
    let lang: PrologText = lang_term.get_ex()?;
    let s: PrologText = inner_term.get_ex()?;

    assert!(
        lang.find('@').is_none(),
        "lang term contained an '@', making it invalid"
    );

    let glued = format!("{}@{}", lang.into_inner(), s.into_inner());
    Ok(LangString::make_entry(&glued))
}

pub fn split_lang_string(lang_string: &str) -> (&str, &str) {
    let pos = lang_string
        .find('@')
        .expect("lang strings should have an @ in them");
    let lang = &lang_string[..pos];
    let s = &lang_string[pos + 1..];

    (lang, s)
}

pub fn unify_entry<C: QueryableContextType>(
    context: &Context<C>,
    entry: &TypedDictEntry,
    object_term: &Term,
) -> PrologResult<()> {
    if entry.datatype() == Datatype::LangString {
        object_term.unify(functor!("lang/2"))?;
        let lang_string = entry.as_val::<LangString, String>();
        let (lang, s) = split_lang_string(&lang_string);

        object_term.unify_arg(1, s)?;
        object_term.unify_arg(2, Atom::new(lang))?;

        return Ok(());
    }

    object_term.unify(functor!("value/2"))?;
    match entry.datatype() {
        Datatype::Boolean => {
            let val = entry.as_val::<bool, bool>();
            if val {
                object_term.unify_arg(1, atom!("true"))?;
            } else {
                object_term.unify_arg(1, atom!("false"))?;
            }
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#boolean"))
        }
        Datatype::String => {
            let val = entry.as_val::<String, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#string"))
        }
        Datatype::AnyURI => {
            let val = entry.as_val::<AnyURI, AnyURI>();
            object_term.unify_arg(1, val.as_ref())?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#anyURI"))
        }
        Datatype::UInt32 => {
            let val = entry.as_val::<u32, u32>() as u64;
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#int"))
        }
        Datatype::Int32 => {
            let val = entry.as_val::<i32, i32>() as i64;
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#unsignedInt"))
        }
        Datatype::Float32 => {
            let val = entry.as_val::<f32, f32>() as f64;
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#float"))
        }
        Datatype::UInt64 => {
            let val = entry.as_val::<u64, u64>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#unsignedLong"))
        }
        Datatype::Int64 => {
            let val = entry.as_val::<i64, i64>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#long"))
        }
        Datatype::Float64 => {
            let val = entry.as_val::<f64, f64>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#double"))
        }
        Datatype::Decimal => {
            let val = entry
                .as_val::<Decimal, String>()
                .parse::<f64>()
                .expect("Precision to high for cast to f64");
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#decimal"))
        }
        Datatype::BigInt => {
            let val: Integer = entry.as_val::<Integer, Integer>();
            object_term.unify_arg(1, val.to_string())?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#integer"))
        }
        Datatype::Token => {
            let val = entry.as_val::<String, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#token"))
        }
        Datatype::GYear => {
            let val = entry.as_val::<GYear, GYear>();
            object_term.unify_arg(1, val.0)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#gYear"))
        }
        Datatype::LangString => panic!("Unreachable"),
        Datatype::Date => todo!(),
        Datatype::DateTime => {
            let datetime_term = context.new_term_ref();
            datetime_term.unify(functor!("date_time/7"))?;
            let dt = entry.as_val::<NaiveDateTime, NaiveDateTime>();
            let year = dt.year() as i64;
            let month = dt.month() as i64;
            let day = dt.day() as i64;
            let hour = dt.hour() as i64;
            let minute = dt.minute() as i64;
            let second = dt.second() as i64;
            let nanosecond = dt.nanosecond() as i64;
            datetime_term.unify_arg(1, year)?;
            datetime_term.unify_arg(2, month)?;
            datetime_term.unify_arg(3, day)?;
            datetime_term.unify_arg(4, hour)?;
            datetime_term.unify_arg(5, minute)?;
            datetime_term.unify_arg(6, second)?;
            datetime_term.unify_arg(7, nanosecond)?;
            object_term.unify_arg(1, datetime_term)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#dateTime"))
        }
        _ => todo!(),
    }
}
