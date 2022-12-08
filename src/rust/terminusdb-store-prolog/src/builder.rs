use super::value::*;
use crate::layer::*;
use std::io::{self, Write};
use swipl::prelude::*;
use terminus_store::layer::*;
use terminus_store::storage::name_to_string;
use terminus_store::store::sync::*;

predicates! {
    pub semidet fn nb_add_id_triple(context, builder_term, subject_id_term, predicate_id_term, object_id_term) {
        let builder: WrappedBuilder = builder_term.get_ex()?;
        let subject_id = subject_id_term.get_ex()?;
        let predicate_id = predicate_id_term.get_ex()?;
        let object_id = object_id_term.get_ex()?;

        context.try_or_die(builder.add_id_triple(IdTriple::new(subject_id, predicate_id, object_id)))
    }

    pub semidet fn nb_add_object_triple(context, builder_term, subject_term, predicate_term, object_term) {
        let builder: WrappedBuilder = builder_term.get_ex()?;
        let subject: PrologText = subject_term.get_ex()?;
        let predicate: PrologText = predicate_term.get_ex()?;

        let inner = context.new_term_ref();
        let ty = context.new_term_ref();
        if attempt(object_term.unify(term!{context: node(#&inner)}?))? {
            let object: PrologText = inner.get_ex()?;
            context.try_or_die(builder.add_value_triple(ValueTriple::new_node(&subject, &predicate, &object)))
        }
        else if attempt(object_term.unify(term!{context: value(#&inner, #&ty)}?))? {
            let entry = make_entry_from_term(context,&inner,&ty)?;
            context.try_or_die(builder.add_value_triple(ValueTriple::new_value(&subject, &predicate, entry)))
        }
        else if attempt(object_term.unify(term!{context: lang(#&inner, #&ty)}?))? {
            let entry = make_entry_from_lang_term(context,&inner,&ty)?;
            context.try_or_die(builder.add_value_triple(ValueTriple::new_value(&subject, &predicate, entry)))
        }
        else {
            context.raise_exception(&term!{context: error(domain_error(oneof([node(), value()]), #object_term), _)}?)
        }
    }

    pub semidet fn nb_remove_id_triple(context, builder_term, subject_id_term, predicate_id_term, object_id_term) {
        let builder: WrappedBuilder = builder_term.get_ex()?;
        let subject_id = subject_id_term.get_ex()?;
        let predicate_id = predicate_id_term.get_ex()?;
        let object_id = object_id_term.get_ex()?;

        context.try_or_die(builder.remove_id_triple(IdTriple::new(subject_id, predicate_id, object_id)))
    }

    pub semidet fn nb_remove_object_triple(context, builder_term, subject_term, predicate_term, object_term) {
        let builder: WrappedBuilder = builder_term.get_ex()?;
        let subject: PrologText = subject_term.get_ex()?;
        let predicate: PrologText = predicate_term.get_ex()?;

        let inner = context.new_term_ref();
        let ty = context.new_term_ref();
        if attempt(object_term.unify(term!{context: node(#&inner)}?))? {
            let object: PrologText = inner.get_ex()?;
            context.try_or_die(builder.remove_value_triple(ValueTriple::new_node(&subject, &predicate, &object)))
        }
        else if attempt(object_term.unify(term!{context: value(#&inner,#&ty)}?))? {
            let entry = make_entry_from_term(context,&inner,&ty)?;
            context.try_or_die(builder.remove_value_triple(ValueTriple::new_value(&subject, &predicate, entry)))
        }
        else if attempt(object_term.unify(term!{context: lang(#&inner,#&ty)}?))? {
            let entry = make_entry_from_lang_term(context,&inner,&ty)?;
            context.try_or_die(builder.remove_value_triple(ValueTriple::new_value(&subject, &predicate, entry)))
        }
        else {
            context.raise_exception(&term!{context: error(domain_error(oneof([node(), value()]), #object_term), _)}?)
        }
    }

    pub semidet fn builder_committed(_context, builder_term) {
        let builder: WrappedBuilder = builder_term.get_ex()?;

        into_prolog_result(builder.committed())
    }

    pub semidet fn nb_commit(context, builder_term, layer_term) {
        let builder: WrappedBuilder = builder_term.get_ex()?;
        let layer = context.try_or_die(builder.commit())?;
        layer_term.unify(WrappedLayer(layer))
    }

    pub semidet fn nb_apply_delta(context, builder_term, layer_term) {
        let builder: WrappedBuilder = builder_term.get_ex()?;
        let layer: WrappedLayer = layer_term.get_ex()?;

        context.try_or_die(builder.apply_delta(&layer))?;

        Ok(())
    }

    pub semidet fn nb_apply_diff(context, builder_term, layer_term) {
        let builder: WrappedBuilder = builder_term.get_ex()?;
        let layer: WrappedLayer = layer_term.get_ex()?;

        context.try_or_die(builder.apply_diff(&layer))?;

        Ok(())
    }
}

wrapped_clone_blob!("builder", pub WrappedBuilder, SyncStoreLayerBuilder);

impl CloneBlobImpl for WrappedBuilder {
    fn write(&self, stream: &mut PrologStream) -> io::Result<()> {
        write!(stream, "<builder {}>", name_to_string(self.name()))
    }
}
