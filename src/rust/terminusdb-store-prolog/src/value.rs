use base64;
use chrono::{Datelike, NaiveDate, NaiveDateTime, NaiveTime, Timelike};
use dec::Decimal128;
use rug::Integer;
use std::io::Cursor;
use std::io::Read;
use std::str::FromStr;
use swipl::prelude::*;
use swipl::term::Term;
use terminus_store::structure::*;

pub fn make_entry_from_term<C: QueryableContextType>(
    context: &Context<C>,
    inner_term: &Term,
    ty_term: &Term,
) -> PrologResult<TypedDictEntry> {
    let ty: Atom = ty_term.get_ex()?;
    if atom!("http://www.w3.org/2001/XMLSchema#boolean") == ty {
        let inner: Atom = inner_term.get_ex()?;
        let b = inner == atom!("true");
        Ok(bool::make_entry(&b))
    } else if atom!("http://www.w3.org/2001/XMLSchema#string") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(String::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#language") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(Language::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#normalizedString") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(NormalizedString::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#token") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(Token::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#NMTOKEN") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(NMToken::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#Name") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(Name::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#NCName") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(NCName::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#Notation") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(Notation::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#QName") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(QName::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#ID") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(ID::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#IDRef") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(IDRef::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#Entity") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(Entity::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#anyURI") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(AnyURI::make_entry(&inner_string.into_inner()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#anySimpleType") == ty {
        let inner_string: String = context.string_from_term(inner_term)?;
        Ok(AnySimpleType::make_entry(&inner_string))
    } else if atom!("http://www.w3.org/2001/XMLSchema#unsignedByte") == ty {
        let inner_number: u64 = inner_term.get_ex()?;
        Ok(u8::make_entry(&(inner_number as u8)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#byte") == ty {
        let inner_number: i64 = inner_term.get_ex()?;
        Ok(i8::make_entry(&(inner_number as i8)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#unsignedShort") == ty {
        let inner_number: u64 = inner_term.get_ex()?;
        Ok(u16::make_entry(&(inner_number as u16)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#short") == ty {
        let inner_number: i64 = inner_term.get_ex()?;
        Ok(i16::make_entry(&(inner_number as i16)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#unsignedInt") == ty {
        let inner_number: u64 = inner_term.get_ex()?;
        Ok(u32::make_entry(&(inner_number as u32)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#int") == ty {
        let inner_number: i64 = inner_term.get_ex()?;
        Ok(i32::make_entry(&(inner_number as i32)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#unsignedLong") == ty {
        let inner_number: u64 = inner_term.get_ex()?;
        Ok(u64::make_entry(&inner_number))
    } else if atom!("http://www.w3.org/2001/XMLSchema#long") == ty {
        let inner_number: i64 = inner_term.get_ex()?;
        Ok(i64::make_entry(&inner_number))
    } else if atom!("http://www.w3.org/2001/XMLSchema#float") == ty {
        let inner_number: f64 = inner_term.get_ex()?;
        Ok(f32::make_entry(&(inner_number as f32)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#double") == ty {
        let inner_number: f64 = inner_term.get_ex()?;
        Ok(f64::make_entry(&inner_number))
    } else if atom!("http://www.w3.org/2001/XMLSchema#decimal") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        Ok(Decimal::make_entry(&Decimal::new(inner_number).unwrap()))
    } else if atom!("http://www.w3.org/2001/XMLSchema#integer") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number).unwrap().into();
        Ok(Integer::make_entry(&integer))
    } else if atom!("http://www.w3.org/2001/XMLSchema#positiveInteger") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number).unwrap().into();
        Ok(PositiveInteger::make_entry(&PositiveInteger(integer)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#negativeInteger") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number).unwrap().into();
        Ok(NegativeInteger::make_entry(&NegativeInteger(integer)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#nonPositiveInteger") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number).unwrap().into();
        Ok(NonPositiveInteger::make_entry(&NonPositiveInteger(integer)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#nonNegativeInteger") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number).unwrap().into();
        Ok(NonNegativeInteger::make_entry(&NonNegativeInteger(integer)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gYear") == ty {
        // TODO check that the functor is what we expect
        let year: i64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let offset: u64 = inner_term.get_arg_ex(2)?;
        Ok(GYear::make_entry(&GYear {
            year,
            offset: offset as i16,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gMonth") == ty {
        // TODO check that the functor is what we expect
        let month: u64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let offset: u64 = inner_term.get_arg_ex(2)?;
        Ok(GMonth::make_entry(&GMonth {
            month: month as u8,
            offset: offset as i16,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gDay") == ty {
        // TODO check that the functor is what we expect
        let day: u64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let offset: u64 = inner_term.get_arg_ex(2)?;
        Ok(GDay::make_entry(&GDay {
            day: day as u8,
            offset: offset as i16,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gYearMonth") == ty {
        // TODO check that the functor is what we expect
        let year: i64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let month: u64 = inner_term.get_arg_ex(2)?;
        let offset: u64 = inner_term.get_arg_ex(3)?;
        Ok(GYearMonth::make_entry(&GYearMonth {
            year,
            month: month as u8,
            offset: offset as i16,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gMonthDay") == ty {
        // TODO check that the functor is what we expect
        let month: u64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let day: u64 = inner_term.get_arg_ex(2)?;
        let offset: u64 = inner_term.get_arg_ex(3)?;
        Ok(GMonthDay::make_entry(&GMonthDay {
            month: month as u8,
            day: day as u8,
            offset: offset as i16,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#duration") == ty {
        let sign: i64 = inner_term.get_arg_ex(1)?;
        let year: i64 = inner_term.get_arg_ex(2)?;
        let month: i64 = inner_term.get_arg_ex(3)?;
        let day: i64 = inner_term.get_arg_ex(4)?;
        let hour: i64 = inner_term.get_arg_ex(5)?;
        let minute: i64 = inner_term.get_arg_ex(6)?;
        let second: f64 = inner_term.get_arg_ex(7)?;
        Ok(Duration::make_entry(&Duration {
            sign: sign as i8,
            year,
            month: month as u8,
            day: day as u8,
            hour: hour as u8,
            minute: minute as u8,
            second,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#yearMonthDuration") == ty {
        let sign: i64 = inner_term.get_arg_ex(1)?;
        let year: i64 = inner_term.get_arg_ex(2)?;
        let month: i64 = inner_term.get_arg_ex(3)?;
        let day: i64 = inner_term.get_arg_ex(4)?;
        let hour: i64 = inner_term.get_arg_ex(5)?;
        let minute: i64 = inner_term.get_arg_ex(6)?;
        let second: f64 = inner_term.get_arg_ex(7)?;
        Ok(YearMonthDuration::make_entry(&YearMonthDuration(
            Duration {
                sign: sign as i8,
                year,
                month: month as u8,
                day: day as u8,
                hour: hour as u8,
                minute: minute as u8,
                second,
            },
        )))
    } else if atom!("http://www.w3.org/2001/XMLSchema#dayTimeDuration") == ty {
        let sign: i64 = inner_term.get_arg_ex(1)?;
        let year: i64 = inner_term.get_arg_ex(2)?;
        let month: i64 = inner_term.get_arg_ex(3)?;
        let day: i64 = inner_term.get_arg_ex(4)?;
        let hour: i64 = inner_term.get_arg_ex(5)?;
        let minute: i64 = inner_term.get_arg_ex(6)?;
        let second: f64 = inner_term.get_arg_ex(7)?;
        Ok(DayTimeDuration::make_entry(&DayTimeDuration(Duration {
            sign: sign as i8,
            year,
            month: month as u8,
            day: day as u8,
            hour: hour as u8,
            minute: minute as u8,
            second,
        })))
    } else if atom!("http://www.w3.org/2001/XMLSchema#dateTime") == ty {
        let year: i64 = inner_term.get_arg_ex(1)?;
        let month: i64 = inner_term.get_arg_ex(2)?;
        let day: i64 = inner_term.get_arg_ex(3)?;
        let hour: i64 = inner_term.get_arg_ex(4)?;
        let minute: i64 = inner_term.get_arg_ex(5)?;
        let second: i64 = inner_term.get_arg_ex(6)?;
        let nano: i64 = inner_term.get_arg_ex(7)?;
        let dt = NaiveDate::from_ymd_opt(year as i32, month as u32, day as u32)
            .unwrap()
            .and_hms_nano_opt(hour as u32, minute as u32, second as u32, nano as u32)
            .unwrap();
        Ok(NaiveDateTime::make_entry(&dt))
    } else if atom!("http://www.w3.org/2001/XMLSchema#dateTimeStamp") == ty {
        let year: i64 = inner_term.get_arg_ex(1)?;
        let month: i64 = inner_term.get_arg_ex(2)?;
        let day: i64 = inner_term.get_arg_ex(3)?;
        let hour: i64 = inner_term.get_arg_ex(4)?;
        let minute: i64 = inner_term.get_arg_ex(5)?;
        let second: i64 = inner_term.get_arg_ex(6)?;
        let nano: i64 = inner_term.get_arg_ex(7)?;
        let dt = NaiveDate::from_ymd_opt(year as i32, month as u32, day as u32)
            .unwrap()
            .and_hms_nano_opt(hour as u32, minute as u32, second as u32, nano as u32)
            .unwrap();
        Ok(DateTimeStamp::make_entry(&DateTimeStamp(dt)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#date") == ty {
        let year: i64 = inner_term.get_arg_ex(1)?;
        let month: i64 = inner_term.get_arg_ex(2)?;
        let day: i64 = inner_term.get_arg_ex(3)?;
        let offset: i64 = inner_term.get_arg_ex(4)?;
        let dt = Date {
            year,
            month: month as u8,
            day: day as u8,
            offset: offset as i16,
        };
        Ok(Date::make_entry(&dt))
    } else if atom!("http://www.w3.org/2001/XMLSchema#time") == ty {
        let hour: u64 = inner_term.get_arg_ex(1)?;
        let minute: u64 = inner_term.get_arg_ex(2)?;
        let second: u64 = inner_term.get_arg_ex(3)?;
        let t = NaiveTime::from_hms_opt(hour as u32, minute as u32, second as u32).unwrap();
        Ok(NaiveTime::make_entry(&t))
    } else if atom!("http://www.w3.org/2001/XMLSchema#base64Binary") == ty {
        let base64: String = inner_term.get_ex()?;
        let mut wrapped_reader = Cursor::new(base64);
        let mut decoder = base64::read::DecoderReader::new(&mut wrapped_reader, base64::STANDARD);
        // handle errors as you normally would
        let mut result = Vec::new();
        decoder.read_to_end(&mut result).unwrap();
        Ok(Base64Binary::make_entry(&Base64Binary(result)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#hexBinary") == ty {
        let hexstring: String = inner_term.get_ex()?;
        Ok(HexBinary::make_entry(&HexBinary(
            hex::decode(hexstring).unwrap(),
        )))
    } else if atom!("http://bsonspec.org#decimal128") == ty {
        let val: String = inner_term.get_ex()?;
        Ok(Decimal128::make_entry(
            &Decimal128::from_str(&val).expect("not a decimal128"),
        ))
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
        Datatype::Language => {
            let val = entry.as_val::<Language, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#language"))
        }
        Datatype::NormalizedString => {
            let val = entry.as_val::<NormalizedString, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(
                2,
                atom!("http://www.w3.org/2001/XMLSchema#normalizedString"),
            )
        }
        Datatype::NMToken => {
            let val = entry.as_val::<NMToken, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#NMTOKEN"))
        }
        Datatype::Name => {
            let val = entry.as_val::<Name, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#Name"))
        }
        Datatype::NCName => {
            let val = entry.as_val::<NCName, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#NCName"))
        }
        Datatype::Notation => {
            let val = entry.as_val::<Notation, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#Notation"))
        }
        Datatype::QName => {
            let val = entry.as_val::<QName, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#QName"))
        }
        Datatype::ID => {
            let val = entry.as_val::<ID, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#ID"))
        }
        Datatype::IDRef => {
            let val = entry.as_val::<IDRef, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#IDRef"))
        }
        Datatype::Entity => {
            let val = entry.as_val::<Entity, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#Entity"))
        }
        Datatype::AnyURI => {
            let val = entry.as_val::<AnyURI, AnyURI>();
            object_term.unify_arg(1, val.as_ref())?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#anyURI"))
        }
        Datatype::AnySimpleType => {
            let anysimpletype = entry.as_val::<AnySimpleType, String>();
            {
                let f = context.open_frame();
                let term = f.term_from_string(&anysimpletype)?;
                object_term.unify_arg(1, term)?;
                f.close();
            }
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#anySimpleType"))
        }
        Datatype::UInt8 => {
            let val = entry.as_val::<u8, u8>() as u64;
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#unsignedByte"))
        }
        Datatype::Int8 => {
            let val = entry.as_val::<i8, i8>() as i64;
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#byte"))
        }
        Datatype::UInt16 => {
            let val = entry.as_val::<u16, u16>() as u64;
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#unsignedShort"))
        }
        Datatype::Int16 => {
            let val = entry.as_val::<i16, i16>() as i64;
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#short"))
        }
        Datatype::UInt32 => {
            let val = entry.as_val::<u32, u32>() as u64;
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#unsignedInt"))
        }
        Datatype::Int32 => {
            let val = entry.as_val::<i32, i32>() as i64;
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#int"))
        }
        Datatype::Float32 => {
            let val = entry.as_val::<f32, f64>();
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
            let decimal = entry.as_val::<Decimal, String>();
            {
                let f = context.open_frame();
                let term = f.term_from_string(&decimal)?;
                object_term.unify_arg(1, term)?;
                f.close();
            }
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#decimal"))
        }
        Datatype::BigInt => {
            let val: Integer = entry.as_val::<Integer, Integer>();
            {
                let f = context.open_frame();
                let term = f.term_from_string(&val.to_string())?;
                object_term.unify_arg(1, term)?;
                f.close();
            }
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#integer"))
        }
        Datatype::NonPositiveInteger => {
            let val: Integer = entry.as_val::<NonPositiveInteger, Integer>();
            {
                let f = context.open_frame();
                let term = f.term_from_string(&val.to_string())?;
                object_term.unify_arg(1, term)?;
                f.close();
            }
            object_term.unify_arg(
                2,
                atom!("http://www.w3.org/2001/XMLSchema#nonPositiveInteger"),
            )
        }
        Datatype::NonNegativeInteger => {
            let val: Integer = entry.as_val::<NonNegativeInteger, Integer>();
            {
                let f = context.open_frame();
                let term = f.term_from_string(&val.to_string())?;
                object_term.unify_arg(1, term)?;
                f.close();
            }
            object_term.unify_arg(
                2,
                atom!("http://www.w3.org/2001/XMLSchema#nonNegativeInteger"),
            )
        }
        Datatype::PositiveInteger => {
            let val: Integer = entry.as_val::<PositiveInteger, Integer>();
            {
                let f = context.open_frame();
                let term = f.term_from_string(&val.to_string())?;
                object_term.unify_arg(1, term)?;
                f.close();
            }
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#positiveInteger"))
        }
        Datatype::NegativeInteger => {
            let val: Integer = entry.as_val::<NegativeInteger, Integer>();
            {
                let f = context.open_frame();
                let term = f.term_from_string(&val.to_string())?;
                object_term.unify_arg(1, term)?;
                f.close();
            }
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#negativeInteger"))
        }
        Datatype::Token => {
            let val = entry.as_val::<Token, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#token"))
        }
        Datatype::GYear => {
            let val = entry.as_val::<GYear, GYear>();
            {
                let f = context.open_frame();
                let f_term = f.new_term_ref();
                f_term.unify(functor!("gyear/2"))?;
                f_term.unify_arg(1, val.year)?;
                f_term.unify_arg(2, val.offset as i64)?;

                object_term.unify_arg(1, &f_term)?;
                f.close();
            }

            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#gYear"))
        }
        Datatype::GMonth => {
            let val = entry.as_val::<GMonth, GMonth>();
            {
                let f = context.open_frame();
                let f_term = f.new_term_ref();
                f_term.unify(functor!("gmonth/2"))?;
                f_term.unify_arg(1, val.month as i64)?;
                f_term.unify_arg(2, val.offset as i64)?;

                object_term.unify_arg(1, &f_term)?;
                f.close();
            }

            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#gMonth"))
        }
        Datatype::GDay => {
            let val = entry.as_val::<GDay, GDay>();
            {
                let f = context.open_frame();
                let f_term = f.new_term_ref();
                f_term.unify(functor!("gday/2"))?;
                f_term.unify_arg(1, val.day as i64)?;
                f_term.unify_arg(2, val.offset as i64)?;

                object_term.unify_arg(1, &f_term)?;
                f.close();
            }

            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#gDay"))
        }
        Datatype::GYearMonth => {
            let val = entry.as_val::<GYearMonth, GYearMonth>();
            {
                let f = context.open_frame();
                let f_term = f.new_term_ref();
                f_term.unify(functor!("gyear_month/3"))?;
                f_term.unify_arg(1, val.year)?;
                f_term.unify_arg(2, val.month as u64)?;
                f_term.unify_arg(3, val.offset as i64)?;

                object_term.unify_arg(1, &f_term)?;
                f.close();
            }

            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#gYearMonth"))
        }
        Datatype::GMonthDay => {
            let val = entry.as_val::<GMonthDay, GMonthDay>();
            {
                let f = context.open_frame();
                let f_term = f.new_term_ref();
                f_term.unify(functor!("gmonth_day/3"))?;
                f_term.unify_arg(1, val.month as u64)?;
                f_term.unify_arg(2, val.day as u64)?;
                f_term.unify_arg(3, val.offset as i64)?;

                object_term.unify_arg(1, &f_term)?;
                f.close();
            }

            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#gMonthDay"))
        }
        Datatype::Duration => {
            let duration_term = context.new_term_ref();
            duration_term.unify(functor!("duration/7"))?;
            let duration = entry.as_val::<Duration, Duration>();
            let sign = duration.sign as i64;
            let year = duration.year;
            let month = duration.month as i64;
            let day = duration.day as i64;
            let hour = duration.hour as i64;
            let minute = duration.minute as i64;
            let second = duration.second;
            duration_term.unify_arg(1, sign)?;
            duration_term.unify_arg(2, year)?;
            duration_term.unify_arg(3, month)?;
            duration_term.unify_arg(4, day)?;
            duration_term.unify_arg(5, hour)?;
            duration_term.unify_arg(6, minute)?;
            duration_term.unify_arg(7, second)?;
            object_term.unify_arg(1, duration_term)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#duration"))
        }
        Datatype::YearMonthDuration => {
            let duration_term = context.new_term_ref();
            duration_term.unify(functor!("duration/7"))?;
            let duration = entry.as_val::<YearMonthDuration, YearMonthDuration>().0;
            let sign = duration.sign as i64;
            let year = duration.year;
            let month = duration.month as i64;
            let day = duration.day as i64;
            let hour = duration.hour as i64;
            let minute = duration.minute as i64;
            let second = duration.second;
            duration_term.unify_arg(1, sign)?;
            duration_term.unify_arg(2, year)?;
            duration_term.unify_arg(3, month)?;
            duration_term.unify_arg(4, day)?;
            duration_term.unify_arg(5, hour)?;
            duration_term.unify_arg(6, minute)?;
            duration_term.unify_arg(7, second)?;
            object_term.unify_arg(1, duration_term)?;
            object_term.unify_arg(
                2,
                atom!("http://www.w3.org/2001/XMLSchema#yearMonthDuration"),
            )
        }
        Datatype::DayTimeDuration => {
            let duration_term = context.new_term_ref();
            duration_term.unify(functor!("duration/7"))?;
            let duration = entry.as_val::<DayTimeDuration, DayTimeDuration>().0;
            let sign = duration.sign as i64;
            let year = duration.year;
            let month = duration.month as i64;
            let day = duration.day as i64;
            let hour = duration.hour as i64;
            let minute = duration.minute as i64;
            let second = duration.second;
            duration_term.unify_arg(1, sign)?;
            duration_term.unify_arg(2, year)?;
            duration_term.unify_arg(3, month)?;
            duration_term.unify_arg(4, day)?;
            duration_term.unify_arg(5, hour)?;
            duration_term.unify_arg(6, minute)?;
            duration_term.unify_arg(7, second)?;
            object_term.unify_arg(1, duration_term)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#dayTimeDuration"))
        }
        Datatype::Date => {
            let date_term = context.new_term_ref();
            date_term.unify(functor!("date/4"))?;
            let dt = entry.as_val::<Date, Date>();
            let year = dt.year;
            let month = dt.month as i64;
            let day = dt.day as i64;
            let offset = dt.offset as i64;
            date_term.unify_arg(1, year)?;
            date_term.unify_arg(2, month)?;
            date_term.unify_arg(3, day)?;
            date_term.unify_arg(4, offset)?;
            object_term.unify_arg(1, date_term)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#date"))
        }
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
        Datatype::Time => {
            let time_term = context.new_term_ref();
            time_term.unify(functor!("time/3"))?;
            let t = entry.as_val::<NaiveTime, NaiveTime>();
            let hour = t.hour() as i64;
            let minute = t.minute() as i64;
            let second = t.second() as i64;
            time_term.unify_arg(1, hour)?;
            time_term.unify_arg(2, minute)?;
            time_term.unify_arg(3, second)?;
            object_term.unify_arg(1, time_term)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#time"))
        }
        Datatype::DateTimeStamp => {
            let datetime_term = context.new_term_ref();
            datetime_term.unify(functor!("date_time/7"))?;
            let dt = entry.as_val::<DateTimeStamp, DateTimeStamp>().0;
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
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#dateTimeStamp"))
        }
        Datatype::Base64Binary => {
            let val = entry.as_val::<Base64Binary, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#base64Binary"))
        }
        Datatype::HexBinary => {
            let val = entry.as_val::<HexBinary, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#hexBinary"))
        }
        Datatype::LangString => panic!("Unreachable"),
        Datatype::Decimal128 => {
            let val = entry.as_val::<Decimal128, Decimal128>();
            object_term.unify_arg(1, val.to_string())?;
            object_term.unify_arg(2, atom!("http://bsonspec.org#decimal128"))?;
            Ok(())
        }
        Datatype::BSONObjectId => todo!(),
        Datatype::TimeStamp64 => todo!(),
        Datatype::BSONTimeStamp => todo!(),
        Datatype::Regex => todo!(),
        Datatype::Javascript => todo!(),
        Datatype::BSONBinary => todo!(),
    }
}
