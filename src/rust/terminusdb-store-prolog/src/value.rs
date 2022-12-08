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
    let ty: Atom = lang_term.get_ex()?;
    if atom!("http://www.w3.org/2001/XMLSchema#string") == ty {
        let inner_string: PrologText = inner_term.get_ex()?;
        Ok(String::make_entry(&inner_string.into_inner()))
    } else {
        todo!()
    }
}

pub fn unify_entry<C: QueryableContextType>(
    _context: &Context<C>,
    entry: &TypedDictEntry,
    object_term: &Term,
) -> PrologResult<()> {
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
        Datatype::UInt32 => todo!(),
        Datatype::Int32 => todo!(),
        Datatype::Float32 => todo!(),
        Datatype::UInt64 => todo!(),
        Datatype::Int64 => todo!(),
        Datatype::Float64 => todo!(),
        Datatype::Decimal => todo!(),
        Datatype::BigInt => todo!(),
        Datatype::Token => todo!(),
    }
}
