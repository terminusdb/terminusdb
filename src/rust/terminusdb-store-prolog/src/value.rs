use base64;
use chrono::{Datelike, NaiveDate, NaiveDateTime, NaiveTime, Timelike};
use rug::ops::Pow;
use rug::Integer;
use std::convert::TryFrom;
use std::io::Cursor;
use std::io::Read;
use swipl::prelude::*;
use swipl::term::Term;
use tdb_succinct::tfc::lexical_interval::{
    decode_lexical_entry, duration_parts_to_period, extend_component, legacy_interval_to_lexical,
    lexical_interval_entry, lexical_interval_from_parts, parse_lexical_interval,
    render_lexical_interval, FLAG_DURATION_END, FLAG_EXPLICIT, FLAG_POINT, FLAG_START_DURATION,
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

/// Marshal/decode failures are domain errors in the caller's term, never
/// silent failures: a value that cannot be represented must raise, so
/// document writes and queries stop instead of dropping data.
fn raise_domain<C: QueryableContextType>(
    context: &Context<C>,
    culprit: &Term,
) -> PrologResult<TypedDictEntry> {
    context.raise_exception(&term! {context: error(domain_error(value(), #culprit), _)}?)
}

/// A `duration/8` term as the bounded legacy `Duration`. Seconds arrive as
/// integer, float, or `NrD` rational and are normalised to decimal digits
/// before the `f64` conversion; anything outside the stored field widths —
/// or a non-terminating/underflowing fraction — is `Err`, and the caller
/// raises `domain_error` rather than dropping the value.
fn duration_from_term<C: QueryableContextType>(
    context: &Context<C>,
    inner_term: &Term,
) -> Result<Duration, String> {
    // Stringify then parse: `get_arg` cannot extract compound sub-terms and
    // the shared parser keeps validation identical to the interval path.
    let term_string: String = context
        .string_from_term(inner_term)
        .map_err(|_| "duration term not serialisable".to_string())?;
    let (sign, week, year, month, day, hour, minute, digits) = parse_duration_parts(&term_string)?;
    if week != 0 {
        return Err("week durations need the lexical duration format".to_string());
    }
    let bounded = |v: i64| -> Result<u8, String> {
        u8::try_from(v).map_err(|_| format!("duration component out of range: {v}"))
    };
    let second: f64 = digits
        .parse()
        .map_err(|_| format!("invalid duration seconds: {digits}"))?;
    // Reject non-finite overflow and underflow to zero of a non-zero value:
    // both silently change the stored duration.
    if !second.is_finite() || (second == 0.0 && digits.bytes().any(|b| b != b'0' && b != b'.')) {
        return Err(format!("unrepresentable duration seconds: {term_string}"));
    }
    Ok(Duration {
        sign,
        year,
        month: bounded(month)?,
        day: bounded(day)?,
        hour: bounded(hour)?,
        minute: bounded(minute)?,
        second,
    })
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
        match duration_from_term(context, inner_term) {
            Ok(d) => Ok(Duration::make_entry(&d)),
            Err(_) => raise_domain(context, inner_term),
        }
    } else if atom!("http://www.w3.org/2001/XMLSchema#yearMonthDuration") == ty {
        match duration_from_term(context, inner_term) {
            Ok(d) => Ok(YearMonthDuration::make_entry(&YearMonthDuration(d))),
            Err(_) => raise_domain(context, inner_term),
        }
    } else if atom!("http://www.w3.org/2001/XMLSchema#dayTimeDuration") == ty {
        match duration_from_term(context, inner_term) {
            Ok(d) => Ok(DayTimeDuration::make_entry(&DayTimeDuration(d))),
            Err(_) => raise_domain(context, inner_term),
        }
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
        // The inner value is the canonical lexical string — the Rust
        // grammar is the sole authority, so text marshals straight to an
        // entry. A legacy `date_time_interval/6` compound still marshals
        // through the term parser for producers that build intervals in
        // Prolog.
        match inner_term.get::<PrologText>() {
            Ok(text) => match parse_lexical_interval(&text.into_inner()) {
                Ok(iv) => Ok(lexical_interval_entry(&iv)),
                Err(_) => raise_domain(context, inner_term),
            },
            Err(_) => {
                let term_string: String = context.string_from_term(inner_term)?;
                match parse_interval_term(&term_string) {
                    Ok(entry) => Ok(entry),
                    Err(_) => raise_domain(context, inner_term),
                }
            }
        }
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
            duration_term.unify(functor!("duration/8"))?;
            let duration = entry.as_val::<Duration, Duration>();
            let sign = duration.sign as i64;
            let year = duration.year;
            let month = duration.month as i64;
            let day = duration.day as i64;
            let hour = duration.hour as i64;
            let minute = duration.minute as i64;
            let second = duration.second;
            duration_term.unify_arg(1, sign)?;
            duration_term.unify_arg(2, 0i64)?; // legacy Duration has no week field
            duration_term.unify_arg(3, year)?;
            duration_term.unify_arg(4, month)?;
            duration_term.unify_arg(5, day)?;
            duration_term.unify_arg(6, hour)?;
            duration_term.unify_arg(7, minute)?;
            duration_term.unify_arg(8, second)?;
            object_term.unify_arg(1, duration_term)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#duration"))
        }
        Datatype::YearMonthDuration => {
            let duration_term = context.new_term_ref();
            duration_term.unify(functor!("duration/8"))?;
            let duration = entry.as_val::<YearMonthDuration, YearMonthDuration>().0;
            let sign = duration.sign as i64;
            let year = duration.year;
            let month = duration.month as i64;
            let day = duration.day as i64;
            let hour = duration.hour as i64;
            let minute = duration.minute as i64;
            let second = duration.second;
            duration_term.unify_arg(1, sign)?;
            duration_term.unify_arg(2, 0i64)?; // legacy YearMonthDuration has no week field
            duration_term.unify_arg(3, year)?;
            duration_term.unify_arg(4, month)?;
            duration_term.unify_arg(5, day)?;
            duration_term.unify_arg(6, hour)?;
            duration_term.unify_arg(7, minute)?;
            duration_term.unify_arg(8, second)?;
            object_term.unify_arg(1, duration_term)?;
            object_term.unify_arg(
                2,
                atom!("http://www.w3.org/2001/XMLSchema#yearMonthDuration"),
            )
        }
        Datatype::DayTimeDuration => {
            let duration_term = context.new_term_ref();
            duration_term.unify(functor!("duration/8"))?;
            let duration = entry.as_val::<DayTimeDuration, DayTimeDuration>().0;
            let sign = duration.sign as i64;
            let year = duration.year;
            let month = duration.month as i64;
            let day = duration.day as i64;
            let hour = duration.hour as i64;
            let minute = duration.minute as i64;
            let second = duration.second;
            duration_term.unify_arg(1, sign)?;
            duration_term.unify_arg(2, 0i64)?; // legacy DayTimeDuration has no week field
            duration_term.unify_arg(3, year)?;
            duration_term.unify_arg(4, month)?;
            duration_term.unify_arg(5, day)?;
            duration_term.unify_arg(6, hour)?;
            duration_term.unify_arg(7, minute)?;
            duration_term.unify_arg(8, second)?;
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
                    legacy_interval_to_lexical(&DateTimeInterval::from_legacy_lexical(bytes))
                }
                INTERVAL_ENCODED_WIDTH => {
                    legacy_interval_to_lexical(&DateTimeInterval::from_lexical(bytes))
                }
                _ => match decode_lexical_entry(&bytes) {
                    Ok(iv) => iv,
                    Err(_) => return context.raise_exception(
                        &term! {context: error(domain_error(xdd_interval_value, #object_term), _)}?,
                    ),
                },
            };
            // The read value is the canonical lexical string — the same
            // spelling the write path accepts, for the full entry domain
            // (deep years and exact fractions cannot pass through the
            // legacy term representation).
            object_term.unify_arg(1, render_lexical_interval(&iv).as_str())?;
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

/// A `duration(Sign,W,Y,Mo,D,H,Mi,S)` arg string as parts: `i64` magnitudes
/// plus a canonical seconds digit string. The legacy `Duration` struct caps
/// four fields at `u8`; the parts shape carries what the term actually
/// says so wide values reach the lexical encoders instead of erroring.
/// The ISO week form is exclusive — `W>0` with any other nonzero field is
/// a malformed term.
fn parse_duration_parts(s: &str) -> Result<(i8, i64, i64, i64, i64, i64, i64, String), String> {
    let inner = s
        .strip_prefix("duration(")
        .and_then(|s| s.strip_suffix(")"))
        .ok_or_else(|| format!("invalid duration: {s}"))?;
    let parts: Vec<&str> = inner.split(',').map(|p| p.trim()).collect();
    if parts.len() != 8 {
        return Err(format!("invalid duration arity: {}", parts.len()));
    }
    let sign: i64 = parts[0].parse().map_err(|e| format!("{e}"))?;
    if !matches!(sign, 1 | -1) {
        return Err(format!("invalid duration sign: {sign}"));
    }
    let mag = |i: usize| -> Result<i64, String> {
        let v: i64 = parts[i].parse().map_err(|e| format!("{e}"))?;
        if v < 0 {
            return Err(format!("negative duration component: {v}"));
        }
        Ok(v)
    };
    let second = normalise_seconds_arg(parts[7])?;
    let (week, year, month, day, hour, minute) =
        (mag(1)?, mag(2)?, mag(3)?, mag(4)?, mag(5)?, mag(6)?);
    if week != 0
        && (year != 0
            || month != 0
            || day != 0
            || hour != 0
            || minute != 0
            || !second.bytes().all(|b| b == b'0' || b == b'.'))
    {
        return Err(format!("week form cannot combine with other fields: {s}"));
    }
    Ok((sign as i8, week, year, month, day, hour, minute, second))
}

/// A `duration/8` seconds argument — integer, float, or `NrD` rational
/// spelling — as a canonical decimal digit string. Rationals only expand
/// when the denominator is `2^a·5^b` (finite decimal); anything else is a
/// marshal error, not a silent rounding.
fn normalise_seconds_arg(s: &str) -> Result<String, String> {
    if let Some((num, den)) = s.split_once('r') {
        let n: Integer = num
            .parse()
            .map_err(|_| format!("invalid rational seconds: {s}"))?;
        let d: Integer = den
            .parse()
            .map_err(|_| format!("invalid rational seconds: {s}"))?;
        rational_to_decimal(&n, &d).ok_or_else(|| format!("non-terminating rational seconds: {s}"))
    } else {
        expand_decimal_spelling(s)
    }
}

/// `N/D` with `D = 2^a·5^b` as a canonical decimal digit string.
/// `None` when the decimal expansion does not terminate.
fn rational_to_decimal(n: &Integer, d: &Integer) -> Option<String> {
    if *d <= 0 || *n < 0 {
        return None;
    }
    let (rest, twos) = d.clone().remove_factor(&Integer::from(2));
    let (rest, fives) = rest.remove_factor(&Integer::from(5));
    if rest != 1 {
        return None;
    }
    let scale = twos.max(fives) as usize;
    let scaled = if twos >= fives {
        n.clone() * Integer::from(5).pow(twos - fives)
    } else {
        n.clone() * Integer::from(2).pow(fives - twos)
    };
    let digits = scaled.to_string();
    let padded = if digits.len() <= scale {
        format!("{}{}", "0".repeat(scale - digits.len() + 1), digits)
    } else {
        digits
    };
    let (int, frac) = padded.split_at(padded.len() - scale);
    let frac = frac.trim_end_matches('0');
    Some(if frac.is_empty() {
        int.to_string()
    } else {
        format!("{int}.{frac}")
    })
}

/// An integer or float spelling (digits, optional `.`, optional exponent)
/// as a canonical decimal digit string: no exponent, no trailing zeros,
/// no leading zeros. Signs and non-decimal input are rejected.
fn expand_decimal_spelling(s: &str) -> Result<String, String> {
    let (mantissa, exp) = match s.split_once(|c| c == 'e' || c == 'E') {
        Some((m, e)) => (
            m,
            e.parse::<i64>()
                .map_err(|_| format!("invalid exponent: {s}"))?,
        ),
        None => (s, 0),
    };
    if mantissa.is_empty() || mantissa.starts_with(['-', '+']) {
        return Err(format!("invalid seconds: {s}"));
    }
    let (int, frac) = mantissa.split_once('.').unwrap_or((mantissa, ""));
    if int.is_empty()
        || !int.bytes().all(|b| b.is_ascii_digit())
        || !frac.bytes().all(|b| b.is_ascii_digit())
    {
        return Err(format!("invalid seconds: {s}"));
    }
    let all = format!("{int}{frac}");
    let point = int.len() as i64 + exp;
    let out = if point <= 0 {
        format!("0.{}{}", "0".repeat((-point) as usize), all)
    } else if point as usize >= all.len() {
        format!("{}{}", all, "0".repeat(point as usize - all.len()))
    } else {
        format!("{}.{}", &all[..point as usize], &all[point as usize..])
    };
    let (i, f) = out
        .split_once('.')
        .map_or((out.as_str(), ""), |(i, f)| (i, f));
    let i = i.trim_start_matches('0');
    let i = if i.is_empty() { "0" } else { i };
    let f = f.trim_end_matches('0');
    Ok(if f.is_empty() {
        i.to_string()
    } else {
        format!("{i}.{f}")
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
    let (sign, week, year, month, day, hour, minute, second) = parse_duration_parts(args[2])?;
    let flag = match args[3] {
        "explicit" => FLAG_EXPLICIT,
        "start_duration" => FLAG_START_DURATION,
        "duration_end" => FLAG_DURATION_END,
        "point" => FLAG_POINT,
        other => return Err(format!("unknown flag: {other}")),
    };
    let start_tz = parse_interval_tz(args[4])?;
    let end_tz = parse_interval_tz(args[5])?;
    // The typed dictionary canonically stores interval entries in lexical
    // encoding (`upgrade_lexical_intervals` rewrites fixed-width entries at
    // layer load). Marshaling through the legacy `DateTimeInterval` — whose
    // `Duration.day` is a `u8` — drops wide periods before they ever reach
    // the encoder: lookups miss, removals silently noop. Build the lexical
    // interval directly so the period carries the term's actual digits.
    let period = duration_parts_to_period(sign, week, year, month, day, hour, minute, &second);
    let iv = lexical_interval_from_parts(
        (start_seconds, start_nanos, start_type, start_tz),
        (end_seconds, end_nanos, end_type, end_tz),
        flag,
        &period,
    );
    // Duration forms: a marshalled term's non-anchor side is a placeholder
    // (`infinity`, `unknown`) or an already-materialised bound — either way
    // the duration defines it. The canonical from_parts builds is the spelled
    // `T/P`/`P/T` literal, so reparsing materialises the bound through the
    // same code path as any other interval write. A date-kind end canonical
    // is already the inclusive spelling, so the reparsed bound stays
    // exclusive — no double bump.
    let iv = match flag {
        FLAG_START_DURATION | FLAG_DURATION_END => {
            let (a, b) = iv.canonical.split_once('/').unwrap_or(("", ""));
            let lit = if flag == FLAG_START_DURATION {
                format!("{}/{}", extend_component(a), b)
            } else {
                format!("{}/{}", a, extend_component(b))
            };
            parse_lexical_interval(&lit)
                .map_err(|e| format!("invalid interval term anchor: {e}"))?
        }
        _ => iv,
    };
    Ok(lexical_interval_entry(&iv))
}

#[cfg(test)]
mod tests {
    use super::*;
    use tdb_succinct::tfc::lexical_interval::{
        cmp_interval_bounds, encode_lexical_interval, parse_lexical_interval,
        render_lexical_interval,
    };

    /// The dictionary canonically holds `encode_lexical_interval` bytes for
    /// `DateTimeInterval` values (layer load upgrades fixed-width entries).
    /// `parse_interval_term` must marshal to that same canonical form or
    /// `object_value_id` — and therefore any delete or id lookup — misses.
    #[test]
    fn interval_term_marshals_to_canonical_lexical_encoding() {
        let entry = parse_interval_term("date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0,0),explicit,tz(0),tz(0))")
            .unwrap();
        assert_eq!(entry.datatype(), Datatype::DateTimeInterval);
        assert_ne!(
            entry.to_bytes().len(),
            INTERVAL_ENCODED_WIDTH,
            "fixed-width encoding is upgraded away at layer load and can never match"
        );
        decode_lexical_entry(&entry.to_bytes())
            .expect("entry must be a decodable lexical interval");
    }

    /// Delete-by-lookup decodes a stored entry to a value, then re-marshals
    /// it. The marshal must be a fixed point of decode-encode or the removal
    /// resolves a nonexistent id and silently does nothing.
    #[test]
    fn interval_term_roundtrip_is_stable() {
        let term = "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0,0),explicit,tz(0),tz(0))";
        let entry = parse_interval_term(term).unwrap();
        let decoded = decode_lexical_entry(&entry.to_bytes()).unwrap();
        let reparsed = lexical_interval_entry(
            &parse_lexical_interval(&render_lexical_interval(&decoded)).unwrap(),
        );
        assert_eq!(entry.to_bytes(), reparsed.to_bytes());
    }

    /// A bare instant literal marshals under the point flag: the stored
    /// entry keeps the single-component canonical, distinct from a
    /// supplied `T/T` pair at the same instant — which renders the pair.
    #[test]
    fn interval_term_point_marshals_single_spelling() {
        let point = parse_interval_term("date_time_interval(date_time(2025,3,14,22,0,0,0),date_time(2025,3,14,22,0,0,0),duration(1,0,0,0,0,0,0,0),point,tz(-7200),tz(-7200))")
            .unwrap();
        let dec = decode_lexical_entry(&point.to_bytes()).unwrap();
        assert_eq!(dec.flag, FLAG_POINT);
        assert_eq!(render_lexical_interval(&dec), "2025-03-15T00:00:00+02:00");
        let pair = parse_interval_term("date_time_interval(date_time(2025,3,14,22,0,0,0),date_time(2025,3,14,22,0,0,0),duration(1,0,0,0,0,0,0,0),explicit,tz(-7200),tz(-7200))")
            .unwrap();
        let dec = decode_lexical_entry(&pair.to_bytes()).unwrap();
        assert_eq!(dec.flag, FLAG_EXPLICIT);
        assert_eq!(
            render_lexical_interval(&dec),
            "2025-03-15T00:00:00+02:00/2025-03-15T00:00:00+02:00"
        );
    }

    /// A whole-day date point round-trips: `date` components, derived
    /// `P1D` and the point flag survive decode → term → re-marshal, and
    /// the canonical renders the single supplied date.
    #[test]
    fn interval_term_point_date_roundtrip() {
        let term = "date_time_interval(date(2026,10,1,0),date(2026,10,2,0),duration(1,0,0,0,1,0,0,0),point,none,none)";
        let entry = parse_interval_term(term).unwrap();
        let decoded = decode_lexical_entry(&entry.to_bytes()).unwrap();
        assert_eq!(render_lexical_interval(&decoded), "2026-10-01");
        let reparsed = lexical_interval_entry(&parse_lexical_interval("2026-10-01").unwrap());
        assert_eq!(entry.to_bytes(), reparsed.to_bytes());
    }

    /// A `date`-kind pair marshals DATE endpoints: the start is the named
    /// day and the (already exclusive) end decodes back as the inclusive
    /// last day. The entry must be byte-identical to the lexical parse of
    /// the same `D1/D2` literal, and read back as a `date` pair.
    #[test]
    fn interval_term_date_pair_marshals_date_kind() {
        let term = "date_time_interval(date(2025,1,1,0),date(2025,4,1,0),duration(1,0,0,0,90,0,0,0.0),explicit,none,none)";
        let entry = parse_interval_term(term).unwrap();
        let expected =
            lexical_interval_entry(&parse_lexical_interval("2025-01-01/2025-03-31").unwrap());
        assert_eq!(entry.to_bytes(), expected.to_bytes());
        let decoded = decode_lexical_entry(&entry.to_bytes()).unwrap();
        assert_eq!(render_lexical_interval(&decoded), "2025-01-01/2025-03-31");
    }

    /// Wide day counts used to die in the legacy `Duration` `u8` field
    /// before reaching the encoder — silent lookup misses and dropped
    /// document fields. Marshal must carry the digits through. Rendered
    /// spellings re-parse to the same bounds: `tz(0)` on a date carries no
    /// renderable designator, so the spelling metadata may differ while the
    /// interval is identical.
    #[test]
    fn interval_term_accepts_wide_day_counts() {
        for (term, want_render) in [
            ("date_time_interval(date(2025,1,20,0),date(2025,10,13,0),duration(1,0,0,0,273,0,0,0),explicit,tz(0),tz(0))", "2025-01-20/2025-10-12"),
            ("date_time_interval(date(2025,1,20,0),infinity,duration(1,0,0,0,300,0,0,0),start_duration,tz(0),tz(0))", "2025-01-20/P300D"),
        ] {
            let entry = parse_interval_term(term).unwrap();
            let decoded = decode_lexical_entry(&entry.to_bytes()).unwrap();
            assert_eq!(render_lexical_interval(&decoded), want_render);
            let reparsed = decode_lexical_entry(
                &lexical_interval_entry(&parse_lexical_interval(want_render).unwrap())
                    .to_bytes(),
            )
            .unwrap();
            assert_eq!(
                cmp_interval_bounds(&decoded, &reparsed),
                std::cmp::Ordering::Equal
            );
        }
    }

    /// Seconds arrive as integers, floats or `NrD` rationals; all must
    /// marshal through the exact decimal spelling — `1r10` lands as the
    /// exact `PT0.1S` declared period, not a float approximation.
    #[test]
    fn interval_term_accepts_rational_seconds() {
        let entry = parse_interval_term("date_time_interval(date(2025,1,20,0),infinity,duration(1,0,0,0,0,0,0,1r10),start_duration,tz(0),tz(0))")
            .unwrap();
        let decoded = decode_lexical_entry(&entry.to_bytes()).unwrap();
        assert_eq!(render_lexical_interval(&decoded), "2025-01-20/PT0.1S");
        let reparsed = lexical_interval_entry(
            &parse_lexical_interval(&render_lexical_interval(&decoded)).unwrap(),
        );
        assert_eq!(entry.to_bytes(), reparsed.to_bytes());
    }

    #[test]
    fn seconds_arg_normalises_to_canonical_decimal() {
        for (input, expected) in [
            ("0", "0"),
            ("1.5", "1.5"),
            ("1.50", "1.5"),
            ("1r10", "0.1"),
            ("65r10", "6.5"),
            ("1r1000", "0.001"),
            ("1.5e-3", "0.0015"),
            ("1e2", "100"),
            ("273.0", "273"),
        ] {
            assert_eq!(normalise_seconds_arg(input).unwrap(), expected, "{}", input);
        }
        for bad in ["1r3", "-1.5", "nan", "1.5.2", "abc", "2r0", "+1"] {
            assert!(normalise_seconds_arg(bad).is_err(), "accepted: {}", bad);
        }
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
        let entry = parse_interval_term("date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0,0),explicit,tz(0),tz(0))").unwrap();
        assert_eq!(entry.to_bytes().as_ref(), upgraded.as_slice());
    }

    /// Boundary strictness: malformed interval terms must fail rather than
    /// marshal a guessed or truncated value.
    #[test]
    fn interval_term_rejects_bad_input() {
        for bad in [
            "date_time_interval(date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0,0),explicit,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(0,0,0,0,0,0,0,0),explicit,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0,-1),explicit,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0,0),bogus,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0,0),explicit,tz(30),tz(0))",
            "date_time_interval(date_time(2020,2,30,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0,0),explicit,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,0,0,0,0,1r3),explicit,tz(0),tz(0))",
            "date_time_interval(date_time(2020,1,15,12,30,45,0),date_time(2020,1,15,12,30,45,0),duration(1,0,0,-1,0,0,0,0),explicit,tz(0),tz(0))",
        ] {
            assert!(parse_interval_term(bad).is_err(), "accepted: {}", bad);
        }
    }
}
