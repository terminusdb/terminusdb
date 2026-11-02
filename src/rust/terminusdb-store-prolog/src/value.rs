use base64;
use chrono::{Datelike, NaiveDate, NaiveDateTime, NaiveTime, Timelike};
use std::convert::TryFrom;
use rug::Integer;
use std::io::Cursor;
use std::io::Read;
use swipl::prelude::*;
use swipl::term::Term;
use tdb_succinct::tfc::lexical_interval::{
    decode_lexical_entry, legacy_interval_to_lexical, lexical_interval_entry,
    lexical_interval_to_legacy,
};
use tdb_succinct::*;

/// Range-checked numeric conversion: a Prolog value that does not fit the
/// target width is a marshalling error, not a silent wrap.
fn exact<T, U>(v: T) -> PrologResult<U>
where
    U: TryFrom<T>,
{
    U::try_from(v).map_err(|_| PrologError::Exception)
}

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
    } else if atom!("https://xbrl.org/2021SQName") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(SQName::make_entry(&inner_string.into_inner()))
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
        Ok(u8::make_entry(&exact::<_, u8>(inner_number)?))
    } else if atom!("http://www.w3.org/2001/XMLSchema#byte") == ty {
        let inner_number: i64 = inner_term.get_ex()?;
        Ok(i8::make_entry(&exact::<_, i8>(inner_number)?))
    } else if atom!("http://www.w3.org/2001/XMLSchema#unsignedShort") == ty {
        let inner_number: u64 = inner_term.get_ex()?;
        Ok(u16::make_entry(&exact::<_, u16>(inner_number)?))
    } else if atom!("http://www.w3.org/2001/XMLSchema#short") == ty {
        let inner_number: i64 = inner_term.get_ex()?;
        Ok(i16::make_entry(&exact::<_, i16>(inner_number)?))
    } else if atom!("http://www.w3.org/2001/XMLSchema#unsignedInt") == ty {
        let inner_number: u64 = inner_term.get_ex()?;
        Ok(u32::make_entry(&exact::<_, u32>(inner_number)?))
    } else if atom!("http://www.w3.org/2001/XMLSchema#int") == ty {
        let inner_number: i64 = inner_term.get_ex()?;
        Ok(i32::make_entry(&exact::<_, i32>(inner_number)?))
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
        // Try to get as PrologText first (for string input with full 20-digit precision)
        // This allows passing arbitrary-precision decimal strings from Prolog
        let decimal_str: String = if let Ok(inner_string) = inner_term.get::<PrologText>() {
            // New way: string from Prolog with full precision (doesn't throw exception on type mismatch)
            inner_string.into_inner()
        } else {
            // Fall back to number for backwards compatibility (converts float/int to string)
            context.string_from_term(inner_term)?
        };
        Ok(Decimal::make_entry(
            &Decimal::new(decimal_str).map_err(|_| PrologError::Exception)?,
        ))
    } else if atom!("http://www.w3.org/2001/XMLSchema#integer") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number)
            .map_err(|_| PrologError::Exception)?
            .into();
        Ok(Integer::make_entry(&integer))
    } else if atom!("http://www.w3.org/2001/XMLSchema#positiveInteger") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number)
            .map_err(|_| PrologError::Exception)?
            .into();
        Ok(PositiveInteger::make_entry(&PositiveInteger(integer)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#negativeInteger") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number)
            .map_err(|_| PrologError::Exception)?
            .into();
        Ok(NegativeInteger::make_entry(&NegativeInteger(integer)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#nonPositiveInteger") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number)
            .map_err(|_| PrologError::Exception)?
            .into();
        Ok(NonPositiveInteger::make_entry(&NonPositiveInteger(integer)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#nonNegativeInteger") == ty {
        let inner_number: String = context.string_from_term(inner_term)?;
        let integer: Integer = Integer::parse(inner_number)
            .map_err(|_| PrologError::Exception)?
            .into();
        Ok(NonNegativeInteger::make_entry(&NonNegativeInteger(integer)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gYear") == ty {
        // TODO check that the functor is what we expect
        let year: i64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let offset: u64 = inner_term.get_arg_ex(2)?;
        Ok(GYear::make_entry(&GYear {
            year,
            offset: exact(offset)?,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gMonth") == ty {
        // TODO check that the functor is what we expect
        let month: u64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let offset: u64 = inner_term.get_arg_ex(2)?;
        Ok(GMonth::make_entry(&GMonth {
            month: exact(month)?,
            offset: exact(offset)?,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gDay") == ty {
        // TODO check that the functor is what we expect
        let day: u64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let offset: u64 = inner_term.get_arg_ex(2)?;
        Ok(GDay::make_entry(&GDay {
            day: exact(day)?,
            offset: exact(offset)?,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gYearMonth") == ty {
        // TODO check that the functor is what we expect
        let year: i64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let month: u64 = inner_term.get_arg_ex(2)?;
        let offset: u64 = inner_term.get_arg_ex(3)?;
        Ok(GYearMonth::make_entry(&GYearMonth {
            year,
            month: exact(month)?,
            offset: exact(offset)?,
        }))
    } else if atom!("http://www.w3.org/2001/XMLSchema#gMonthDay") == ty {
        // TODO check that the functor is what we expect
        let month: u64 = inner_term.get_arg_ex(1)?; // TODO should this throw if the arg is not there?
        let day: u64 = inner_term.get_arg_ex(2)?;
        let offset: u64 = inner_term.get_arg_ex(3)?;
        Ok(GMonthDay::make_entry(&GMonthDay {
            month: exact(month)?,
            day: exact(day)?,
            offset: exact(offset)?,
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
            sign: exact(sign)?,
            year,
            month: exact(month)?,
            day: exact(day)?,
            hour: exact(hour)?,
            minute: exact(minute)?,
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
                sign: exact(sign)?,
                year,
                month: exact(month)?,
                day: exact(day)?,
                hour: exact(hour)?,
                minute: exact(minute)?,
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
            sign: exact(sign)?,
            year,
            month: exact(month)?,
            day: exact(day)?,
            hour: exact(hour)?,
            minute: exact(minute)?,
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
        let dt = NaiveDate::from_ymd_opt(exact(year)?, exact(month)?, exact(day)?)
            .ok_or(PrologError::Exception)?
            .and_hms_nano_opt(exact(hour)?, exact(minute)?, exact(second)?, exact(nano)?)
            .ok_or(PrologError::Exception)?;
        Ok(NaiveDateTime::make_entry(&dt))
    } else if atom!("http://www.w3.org/2001/XMLSchema#dateTimeStamp") == ty {
        let year: i64 = inner_term.get_arg_ex(1)?;
        let month: i64 = inner_term.get_arg_ex(2)?;
        let day: i64 = inner_term.get_arg_ex(3)?;
        let hour: i64 = inner_term.get_arg_ex(4)?;
        let minute: i64 = inner_term.get_arg_ex(5)?;
        let second: i64 = inner_term.get_arg_ex(6)?;
        let nano: i64 = inner_term.get_arg_ex(7)?;
        let dt = NaiveDate::from_ymd_opt(exact(year)?, exact(month)?, exact(day)?)
            .ok_or(PrologError::Exception)?
            .and_hms_nano_opt(exact(hour)?, exact(minute)?, exact(second)?, exact(nano)?)
            .ok_or(PrologError::Exception)?;
        Ok(DateTimeStamp::make_entry(&DateTimeStamp(dt)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#date") == ty {
        let year: i64 = inner_term.get_arg_ex(1)?;
        let month: i64 = inner_term.get_arg_ex(2)?;
        let day: i64 = inner_term.get_arg_ex(3)?;
        let offset: i64 = inner_term.get_arg_ex(4)?;
        let dt = Date {
            year,
            month: exact(month)?,
            day: exact(day)?,
            offset: exact(offset)?,
        };
        Ok(Date::make_entry(&dt))
    } else if atom!("http://www.w3.org/2001/XMLSchema#time") == ty {
        let hour: u64 = inner_term.get_arg_ex(1)?;
        let minute: u64 = inner_term.get_arg_ex(2)?;
        let second: u64 = inner_term.get_arg_ex(3)?;
        let t = NaiveTime::from_hms_opt(exact(hour)?, exact(minute)?, exact(second)?)
            .ok_or(PrologError::Exception)?;
        Ok(NaiveTime::make_entry(&t))
    } else if atom!("http://www.w3.org/2001/XMLSchema#base64Binary") == ty {
        let base64: String = inner_term.get_ex()?;
        let mut wrapped_reader = Cursor::new(base64);
        let mut decoder = base64::read::DecoderReader::new(&mut wrapped_reader, base64::STANDARD);
        let mut result = Vec::new();
        decoder
            .read_to_end(&mut result)
            .map_err(|_| PrologError::Exception)?;
        Ok(Base64Binary::make_entry(&Base64Binary(result)))
    } else if atom!("http://www.w3.org/2001/XMLSchema#hexBinary") == ty {
        let hexstring: String = inner_term.get_ex()?;
        Ok(HexBinary::make_entry(&HexBinary(
            hex::decode(hexstring).map_err(|_| PrologError::Exception)?,
        )))
    } else if atom!("http://terminusdb.com/schema/xdd#dateTimeInterval") == ty {
        // Serialize compound term to string and parse in Rust,
        // because swipl-rs get_arg cannot extract compound sub-terms.
        let term_string: String = context.string_from_term(inner_term)?;
        parse_interval_term(&term_string).map_err(|_| PrologError::Exception)
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
        Datatype::SQName => {
            let val = entry.as_val::<SQName, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("https://xbrl.org/2021SQName"))
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
            // CRITICAL FIX: Unify as string (PrologText) instead of parsing as term
            object_term.unify_arg(1, &decimal)?;
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
        Datatype::DateTimeInterval => {
            let bytes = entry.to_bytes();
            let iv = match bytes.len() {
                INTERVAL_LEGACY_ENCODED_WIDTH => {
                    DateTimeInterval::from_legacy_lexical(bytes)
                }
                INTERVAL_ENCODED_WIDTH => DateTimeInterval::from_lexical(bytes),
                _ => decode_lexical_entry(&bytes)
                    .ok()
                    .and_then(|iv| lexical_interval_to_legacy(&iv))
                    .unwrap_or_else(|| {
                        panic!("undecodable DateTimeInterval entry: {} bytes", bytes.len())
                    }),
            };
            let term_string = format_interval_term(&iv);
            {
                let f = context.open_frame();
                let term = f.term_from_string(&term_string)?;
                object_term.unify_arg(1, term)?;
                f.close();
            }
            object_term.unify_arg(
                2,
                atom!("http://terminusdb.com/schema/xdd#dateTimeInterval"),
            )
        }
        Datatype::LangString => panic!("Unreachable"),
    }
}

// --- DateTimeInterval string parsing/formatting helpers ---

fn split_top_level_args(s: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => {
                parts.push(s[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    parts.push(s[start..].trim());
    parts
}

fn parse_component_timestamp(s: &str, is_start: bool) -> Result<(i64, u32, u8), String> {
    match s {
        "neg_infinity" if is_start => return Ok((INTERVAL_NEG_INF, 0, INTERVAL_COMPONENT_OPEN)),
        "infinity" if !is_start => return Ok((INTERVAL_POS_INF, 0, INTERVAL_COMPONENT_OPEN)),
        "unknown" if is_start => {
            return Ok((INTERVAL_UNKNOWN_START, 0, INTERVAL_COMPONENT_UNKNOWN))
        }
        "unknown" => return Ok((INTERVAL_UNKNOWN_END, 0, INTERVAL_COMPONENT_UNKNOWN)),
        _ => {}
    }
    if let Some(inner) = s
        .strip_prefix("date_time(")
        .and_then(|s| s.strip_suffix(")"))
    {
        let parts: Vec<&str> = inner.split(',').map(|p| p.trim()).collect();
        if parts.len() != 7 {
            return Err(format!("invalid date_time arity: {}", parts.len()));
        }
        let y: i32 = parts[0].parse().map_err(|e| format!("{e}"))?;
        let mo: u32 = parts[1].parse().map_err(|e| format!("{e}"))?;
        let d: u32 = parts[2].parse().map_err(|e| format!("{e}"))?;
        let h: u32 = parts[3].parse().map_err(|e| format!("{e}"))?;
        let mi: u32 = parts[4].parse().map_err(|e| format!("{e}"))?;
        let s_val: u32 = parts[5].parse().map_err(|e| format!("{e}"))?;
        let ns: u32 = parts[6].parse().map_err(|e| format!("{e}"))?;
        let dt = NaiveDate::from_ymd_opt(y, mo, d)
            .ok_or_else(|| format!("invalid date: {y}-{mo}-{d}"))?
            .and_hms_nano_opt(h, mi, s_val, ns)
            .ok_or_else(|| format!("invalid time: {h}:{mi}:{s_val}.{ns}"))?;
        Ok((
            dt.and_utc().timestamp(),
            dt.and_utc().timestamp_subsec_nanos(),
            INTERVAL_COMPONENT_DATETIME,
        ))
    } else if let Some(inner) = s.strip_prefix("date(").and_then(|s| s.strip_suffix(")")) {
        let parts: Vec<&str> = inner.split(',').map(|p| p.trim()).collect();
        if parts.len() != 4 {
            return Err(format!("invalid date arity: {}", parts.len()));
        }
        let y: i32 = parts[0].parse().map_err(|e| format!("{e}"))?;
        let m: u32 = parts[1].parse().map_err(|e| format!("{e}"))?;
        let d: u32 = parts[2].parse().map_err(|e| format!("{e}"))?;
        // Arg 4 is the component's lexical zone in SWI seconds-west
        // convention (component_tz/2 copies it into the tz fields, which are
        // authoritative). The stored instant is the nominal UTC day, so the
        // value contributes nothing — but it must still be a sane offset.
        let off: i64 = parts[3].parse().map_err(|e| format!("{e}"))?;
        if off.unsigned_abs() > 86400 {
            return Err(format!("invalid date zone offset: {off}"));
        }
        let dt = NaiveDate::from_ymd_opt(y, m, d)
            .ok_or_else(|| format!("invalid date: {y}-{m}-{d}"))?
            .and_hms_opt(0, 0, 0)
            .ok_or("invalid hms")?;
        Ok((dt.and_utc().timestamp(), 0, INTERVAL_COMPONENT_DATE))
    } else {
        Err(format!("unknown component: {s}"))
    }
}

fn parse_duration_fields(s: &str) -> Result<Duration, String> {
    let inner = s
        .strip_prefix("duration(")
        .and_then(|s| s.strip_suffix(")"))
        .ok_or_else(|| format!("invalid duration: {s}"))?;
    let parts: Vec<&str> = inner.split(',').map(|p| p.trim()).collect();
    if parts.len() != 7 {
        return Err(format!("invalid duration arity: {}", parts.len()));
    }
    let sign: i8 = parts[0].parse().map_err(|e| format!("{e}"))?;
    if !matches!(sign, 1 | -1) {
        return Err(format!("invalid duration sign: {sign}"));
    }
    let second: f64 = parts[6].parse().map_err(|e| format!("{e}"))?;
    if !second.is_finite() || second < 0.0 {
        return Err(format!("invalid duration seconds: {second}"));
    }
    Ok(Duration {
        sign,
        year: parts[1].parse().map_err(|e| format!("{e}"))?,
        month: parts[2].parse().map_err(|e| format!("{e}"))?,
        day: parts[3].parse().map_err(|e| format!("{e}"))?,
        hour: parts[4].parse().map_err(|e| format!("{e}"))?,
        minute: parts[5].parse().map_err(|e| format!("{e}"))?,
        second,
    })
}

/// Parse a `tz(Seconds)` or `none` term into the stored i16 minutes offset.
/// The Prolog side follows the SWI convention of seconds *west* of
/// Greenwich, so a +02:00 zone arrives as tz(-7200) → 120 minutes.
fn parse_interval_tz(s: &str) -> Result<i16, String> {
    if s == "none" {
        return Ok(INTERVAL_NO_ZONE);
    }
    let secs: i64 = s
        .strip_prefix("tz(")
        .and_then(|s| s.strip_suffix(")"))
        .ok_or_else(|| format!("invalid tz term: {s}"))?
        .parse()
        .map_err(|e| format!("invalid tz offset: {e}"))?;
    // Seconds must convert exactly to whole minutes: truncation here would
    // silently change the stored zone.
    if secs % 60 != 0 {
        return Err(format!("tz offset not whole minutes: {secs}"));
    }
    i16::try_from(-secs / 60).map_err(|_| format!("tz offset out of range: {secs}"))
}

fn format_interval_tz(tz: i16) -> String {
    if tz == INTERVAL_NO_ZONE {
        "none".to_string()
    } else {
        format!("tz({})", -(tz as i64) * 60)
    }
}

fn parse_interval_term(s: &str) -> Result<TypedDictEntry, String> {
    let inner = s
        .strip_prefix("date_time_interval(")
        .and_then(|s| s.strip_suffix(")"))
        .ok_or_else(|| format!("invalid interval term: {s}"))?;
    let args = split_top_level_args(inner);
    if args.len() != 6 {
        return Err(format!("expected 6 args, got {}", args.len()));
    }
    let (start_seconds, start_nanos, start_type) = parse_component_timestamp(args[0], true)?;
    let (end_seconds, end_nanos, end_type) = parse_component_timestamp(args[1], false)?;
    let duration = parse_duration_fields(args[2])?;
    let flag = match args[3] {
        "explicit" => INTERVAL_FLAG_EXPLICIT,
        "start_duration" => INTERVAL_FLAG_START_DURATION,
        "duration_end" => INTERVAL_FLAG_DURATION_END,
        other => return Err(format!("unknown flag: {other}")),
    };
    let start_tz = parse_interval_tz(args[4])?;
    let end_tz = parse_interval_tz(args[5])?;
    // The typed dictionary canonically stores interval entries in lexical
    // encoding (`upgrade_lexical_intervals` rewrites fixed-width entries at
    // layer load). Marshaling to `DateTimeInterval::to_lexical` here would
    // store fine but produce lookup misses in `object_value_id` — and silent
    // noop removals, since removal resolves the object id through the same
    // lookup. Encode the canonical form up front instead.
    Ok(lexical_interval_entry(&legacy_interval_to_lexical(
        &DateTimeInterval {
            start_seconds,
            start_nanos,
            end_seconds,
            end_nanos,
            start_type,
            end_type,
            flag,
            duration,
            start_tz,
            end_tz,
        },
    )))
}

fn format_component_term(seconds: i64, nanos: u32, component_type: u8, is_start: bool) -> String {
    use chrono::DateTime;
    match component_type {
        INTERVAL_COMPONENT_OPEN => {
            return if is_start { "neg_infinity" } else { "infinity" }.to_string()
        }
        INTERVAL_COMPONENT_UNKNOWN => return "unknown".to_string(),
        _ => {}
    }
    let dt = DateTime::from_timestamp(seconds, nanos)
        .map(|dt| dt.naive_utc())
        .expect("stored interval instant outside representable range");
    if component_type == INTERVAL_COMPONENT_DATE {
        format!("date({},{},{},0)", dt.year(), dt.month(), dt.day())
    } else {
        format!(
            "date_time({},{},{},{},{},{},{})",
            dt.year(),
            dt.month(),
            dt.day(),
            dt.hour(),
            dt.minute(),
            dt.second(),
            dt.nanosecond()
        )
    }
}

fn format_interval_term(iv: &DateTimeInterval) -> String {
    let start = format_component_term(iv.start_seconds, iv.start_nanos, iv.start_type, true);
    let end = format_component_term(iv.end_seconds, iv.end_nanos, iv.end_type, false);
    let dur = format!(
        "duration({},{},{},{},{},{},{})",
        iv.duration.sign,
        iv.duration.year,
        iv.duration.month,
        iv.duration.day,
        iv.duration.hour,
        iv.duration.minute,
        iv.duration.second
    );
    let flag = match iv.flag {
        INTERVAL_FLAG_START_DURATION => "start_duration",
        INTERVAL_FLAG_DURATION_END => "duration_end",
        _ => "explicit",
    };
    format!(
        "date_time_interval({},{},{},{},{},{})",
        start,
        end,
        dur,
        flag,
        format_interval_tz(iv.start_tz),
        format_interval_tz(iv.end_tz)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use tdb_succinct::tfc::lexical_interval::encode_lexical_interval;

    /// The dictionary canonically holds `encode_lexical_interval` bytes for
    /// `DateTimeInterval` values (layer load upgrades fixed-width entries).
    /// `parse_interval_term` must marshal to that same canonical form or
    /// `object_value_id` — and therefore any delete or id lookup — misses.
    #[test]
    fn interval_term_marshals_to_canonical_lexical_encoding() {
        let entry = parse_interval_term("date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0),explicit,tz(0),tz(0))")
            .unwrap();
        assert_eq!(entry.datatype(), Datatype::DateTimeInterval);
        assert_ne!(
            entry.to_bytes().len(),
            INTERVAL_ENCODED_WIDTH,
            "fixed-width encoding is upgraded away at layer load and can never match"
        );
        decode_lexical_entry(&entry.to_bytes()).expect("entry must be a decodable lexical interval");
    }

    /// Delete-by-lookup decodes a stored entry to a term, then re-marshals it.
    /// The marshal must be a fixed point of decode-encode or the removal
    /// resolves a nonexistent id and silently does nothing.
    #[test]
    fn interval_term_roundtrip_is_stable() {
        let term = "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0),explicit,tz(0),tz(0))";
        let entry = parse_interval_term(term).unwrap();
        let decoded = decode_lexical_entry(&entry.to_bytes()).unwrap();
        let legacy = lexical_interval_to_legacy(&decoded).expect("must be legacy-representable");
        let reparsed = parse_interval_term(&format_interval_term(&legacy)).unwrap();
        assert_eq!(entry.to_bytes(), reparsed.to_bytes());
    }

    /// A stored `to_lexical` (fixed-width) entry and the canonical lexical
    /// form of the same value must be identical after the dict upgrade —
    /// i.e. `legacy_interval_to_lexical` and `encode_lexical_interval`
    /// compose to the canonicalization applied by the store.
    #[test]
    fn lexical_encoding_matches_store_upgrade() {
        let secs = NaiveDate::from_ymd_opt(2020, 1, 15)
            .unwrap()
            .and_hms_opt(12, 30, 45)
            .unwrap()
            .and_utc()
            .timestamp();
        let iv = DateTimeInterval {
            start_seconds: secs,
            start_nanos: 0,
            end_seconds: secs,
            end_nanos: 0,
            start_type: INTERVAL_COMPONENT_DATETIME,
            end_type: INTERVAL_COMPONENT_DATETIME,
            flag: INTERVAL_FLAG_EXPLICIT,
            duration: Duration {
                sign: 1,
                year: 0,
                month: 0,
                day: 0,
                hour: 0,
                minute: 0,
                second: 0.0,
            },
            start_tz: 0,
            end_tz: 0,
        };
        let upgraded = encode_lexical_interval(&legacy_interval_to_lexical(&iv));
        let entry = parse_interval_term("date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0),explicit,tz(0),tz(0))").unwrap();
        assert_eq!(entry.to_bytes().as_ref(), upgraded.as_slice());
    }

    /// Boundary strictness: malformed interval terms must fail rather than
    /// marshal a guessed or truncated value.
    #[test]
    fn interval_term_rejects_bad_input() {
        for bad in [
            "date_time_interval(date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0),explicit,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(0,0,0,0,0,0,0),explicit,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,-1),explicit,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0),bogus,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0),explicit,tz(30),tz(0))",
            "date_time_interval(date_time(2020,2,30,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0),explicit,tz(0),tz(0))",
        ] {
            assert!(parse_interval_term(bad).is_err(), "accepted: {}", bad);
        }
    }
}
