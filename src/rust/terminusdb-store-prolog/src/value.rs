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
    } else {
        todo!()
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
    _context: &Context<C>,
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
            todo!()
            /*
            let val = entry.as_val::<Integer, u64>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#decimal"))
            */
        }
        Datatype::Token => {
            let val = entry.as_val::<String, String>();
            object_term.unify_arg(1, val)?;
            object_term.unify_arg(2, atom!("http://www.w3.org/2001/XMLSchema#token"))
        }
        Datatype::LangString => panic!("Unreachable"),
    }
}
