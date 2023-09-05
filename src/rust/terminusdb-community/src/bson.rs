use bson;
use swipl::prelude::*;

fn bson_array_to_term<'a, C: QueryableContextType>(
    context: &'a Context<C>,
    array: &Vec<bson::Bson>,
) -> PrologResult<Term<'a>> {
    let term = context.new_term_ref();
    let frame = context.open_frame();
    let terms = frame.new_term_refs_vec(array.len());
    for (ix, val) in array.iter().enumerate() {
        let val_term = bson_value_to_term(context, val)?;
        terms[ix].unify(val_term)?;
    }

    frame.close();

    Ok(term)
}

fn bson_value_unsupported<'a, C: QueryableContextType>(
    context: &'a Context<C>,
    value: &bson::Bson,
) -> PrologResult<()> {
    let element_type = Atom::new(&format!("{:?}", value.element_type()));
    context.raise_exception(&term! {context: error(bson_value_unsupported(#element_type))}?)
}

fn bson_value_to_term<'a, C: QueryableContextType>(
    context: &'a Context<C>,
    value: &bson::Bson,
) -> PrologResult<Term<'a>> {
    let term = context.new_term_ref();
    let frame = context.open_frame();
    match value {
        bson::Bson::Double(d) => term.unify(d)?,
        bson::Bson::String(s) => term.unify(s)?,
        bson::Bson::Array(v) => term.unify(bson_array_to_term(&frame, v)?)?,
        bson::Bson::Document(d) => term.unify(bson_to_dict(&frame, d)?)?,
        bson::Bson::Boolean(b) => term.unify(b)?,
        bson::Bson::Null => term.unify(Nil)?,
        bson::Bson::RegularExpression(_) => todo!(),
        bson::Bson::JavaScriptCode(_) => todo!(),
        bson::Bson::JavaScriptCodeWithScope(_) => todo!(),
        bson::Bson::Int32(i) => term.unify(*i as i64)?,
        bson::Bson::Int64(i) => term.unify(i)?,
        bson::Bson::Timestamp(_) => todo!(),
        bson::Bson::Binary(_) => todo!(),
        bson::Bson::ObjectId(o) => {
            term.unify(functor!("bson_object_id", 1))?;
            term.unify_arg(1, o.to_hex())?;
        }
        bson::Bson::DateTime(_) => todo!(),
        bson::Bson::Symbol(_) => todo!(),
        bson::Bson::Decimal128(d) => term.unify(&d.to_string())?,
        _ => bson_value_unsupported(context, value)?,
    }

    frame.close();
    Ok(term)
}

fn bson_to_dict<'a, C: QueryableContextType>(
    context: &'a Context<C>,
    document: &bson::Document,
) -> PrologResult<Term<'a>> {
    let term = context.new_term_ref();
    let frame = context.open_frame();
    let mut builder = DictBuilder::new();
    builder.set_tag(atom!("bson"));

    for (key, val) in document.iter() {
        let val_term = bson_value_to_term(&frame, val)?;
        builder.add_entry(Atom::new(key), val_term);
    }

    term.unify(builder)?;
    frame.close();

    Ok(term)
}

predicates! {
    semidet fn read_bson_value(context, stream_term, val_term) {
        let stream: ReadablePrologStream = stream_term.get_ex()?;
        let value = context.try_or_die_generic(bson::from_reader::<_,bson::Bson>(stream))?;
        let t = bson_value_to_term(context, &value)?;
        val_term.unify(t)

    }
    semidet fn read_bson_document(context, stream_term, dict_term) {
        let stream: ReadablePrologStream = stream_term.get_ex()?;
        let document = context.try_or_die_generic(bson::Document::from_reader(stream))?;
        let t = bson_to_dict(context, &document)?;
        dict_term.unify(t)
    }
}

pub fn register(module: Option<&str>) {
    register_read_bson_value_in_module(module);
    register_read_bson_document_in_module(module);
}

#[cfg(test)]
mod tests {
    use bson::{doc, document};

    #[test]
    fn blah() {
        let bson = doc! {"x":42};
        let mut v = Vec::new();
        bson.to_writer(&mut v).unwrap();
        for x in v {
            print!("{x:02x}");
        }

        panic!("die");
    }
}
