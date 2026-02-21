use super::value::*;
use crate::store::*;
use std::io::{self, Write};
use std::iter::Peekable;
use swipl::prelude::*;
use tdb_succinct::TypedDictEntry;
use terminus_store::layer::{IdTriple, ObjectType};
use terminus_store::storage::{name_to_string, string_to_name};
use terminus_store::store::sync::*;
use terminus_store::Layer;

predicates! {
    pub semidet fn node_and_value_count(_context, layer_term, count_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let count = layer.node_and_value_count() as u64;

        count_term.unify(count)
    }

    pub semidet fn predicate_count(_context, layer_term, count_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let count = layer.predicate_count() as u64;

        count_term.unify(count)
    }

    pub semidet fn subject_to_id(_context, layer_term, subject_term, id_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let subject: PrologText = subject_term.get_ex()?;

        match layer.subject_id(&subject) {
            Some(id) => id_term.unify(id),
            None => Err(PrologError::Failure)
        }
    }

    pub semidet fn id_to_subject(_context, layer_term, id_term, subject_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let id: u64 = id_term.get_ex()?;

        match layer.id_subject(id) {
            Some(subject) => subject_term.unify(subject),
            None => Err(PrologError::Failure)
        }
    }

    pub semidet fn predicate_to_id(_context, layer_term, predicate_term, id_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let predicate: PrologText = predicate_term.get_ex()?;

        match layer.predicate_id(&predicate) {
            Some(id) => id_term.unify(id),
            None => Err(PrologError::Failure)
        }
    }

    pub semidet fn id_to_predicate(_context, layer_term, id_term, predicate_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let id: u64 = id_term.get_ex()?;

        match layer.id_predicate(id) {
            Some(predicate) => predicate_term.unify(predicate),
            None => Err(PrologError::Failure)
        }
    }

    pub semidet fn object_to_id(context, layer_term, object_term, id_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;

        let inner = context.new_term_ref();
        let ty = context.new_term_ref();
        let id: Option<u64>;
        if attempt(object_term.unify(term!{context: node(#&inner)}?))? {
            let object: PrologText = inner.get_ex()?;
            id = layer.object_node_id(&object);
        }
        else if attempt(object_term.unify(term!{context: value(#&inner,#&ty)}?))? {
            let entry = make_entry_from_term(context,&inner,&ty)?;
            id = layer.object_value_id(&entry);
        }
        else if attempt(object_term.unify(term!{context: lang(#&inner,#&ty)}?))? {
            let entry = make_entry_from_lang_term(context,&inner,&ty)?;
            id = layer.object_value_id(&entry);
        }
        else {
            return context.raise_exception(&term!{context: error(domain_error(oneof([node(), value()]), #object_term), _)}?);
        }


        match id {
            Some(id) => id_term.unify(id),
            None => Err(PrologError::Failure)
        }
    }

    pub semidet fn id_to_object(context, layer_term, id_term, object_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let id: u64 = id_term.get_ex()?;

        match layer.id_object(id) {
            Some(ObjectType::Node(object)) => {
                object_term.unify(functor!("node/1"))?;
                object_term.unify_arg(1, object)
            }
            Some(ObjectType::Value(object)) => {
                unify_entry(context, &object, object_term)
            }
            None => Err(PrologError::Failure)
        }
    }

    pub semidet fn parent(context, layer_term, parent_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        match context.try_or_die(layer.parent())? {
            Some(p) => parent_term.unify(WrappedLayer(p)),
            None => Err(PrologError::Failure)
        }
    }

    pub semidet fn squash(context, layer_term, squashed_layer_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let squashed = context.try_or_die(layer.squash())?;
        squashed_layer_term.unify(&WrappedLayer(squashed))
    }

    pub semidet fn squash_upto(context, layer_term, upto_term, squashed_layer_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let upto: WrappedLayer = upto_term.get_ex()?;
        let squashed = context.try_or_die(layer.squash_upto(&upto))?;
        squashed_layer_term.unify(&WrappedLayer(squashed))
    }

    pub semidet fn rollup(context, layer_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        context.try_or_die(layer.rollup())
    }

    pub semidet fn rollup_upto(context, layer_term, upto_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let upto: WrappedLayer = upto_term.get_ex()?;
        context.try_or_die(layer.rollup_upto(&upto))
    }

    pub semidet fn imprecise_rollup_upto(context, layer_term, upto_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let upto: WrappedLayer = upto_term.get_ex()?;
        context.try_or_die(layer.imprecise_rollup_upto(&upto))
    }

    pub semidet fn layer_addition_count(context, layer_term, count_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let count = context.try_or_die(layer.triple_layer_addition_count())? as u64;

        count_term.unify(count)
    }

    pub semidet fn layer_removal_count(context, layer_term, count_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let count = context.try_or_die(layer.triple_layer_removal_count())? as u64;

        count_term.unify(count)
    }

    pub semidet fn layer_total_addition_count(_context, layer_term, count_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let count = layer.triple_addition_count() as u64;

        count_term.unify(count)
    }

    pub semidet fn layer_total_removal_count(_context, layer_term, count_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let count = layer.triple_removal_count() as u64;

        count_term.unify(count)
    }

    pub semidet fn layer_total_triple_count(_context, layer_term, count_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let count = layer.triple_count() as u64;

        count_term.unify(count)
    }

    pub semidet fn layer_to_id(_context, layer_term, id_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let name = name_to_string(layer.name());

        id_term.unify(name)
    }

    pub semidet fn store_id_layer(context, store_term, id_term, layer_term) {
        if layer_term.is_var() {
            let store: WrappedStore = store_term.get_ex()?;
            let id: PrologText = id_term.get_ex()?;
            let name = context.try_or_die(string_to_name(&id))?;

            match context.try_or_die(store.get_layer_from_id(name))? {
                Some(layer) => layer_term.unify(&WrappedLayer(layer)),
                None => Err(PrologError::Failure)
            }
        }
        else {
            let layer: WrappedLayer = layer_term.get_ex()?;
            let name = name_to_string(layer.name());

            id_term.unify(name)
        }
    }

    pub nondet fn id_triple<Peekable<Box<dyn Iterator<Item=IdTriple>+Send>>>(context, layer_term, subject_id_term, predicate_id_term, object_id_term) {
        setup => {
            let layer: WrappedLayer = layer_term.get_ex()?;

            let iter: Box<dyn Iterator<Item=IdTriple>+Send>;
            if let Some(subject_id) = attempt_opt(subject_id_term.get::<u64>())? {
                if let Some(predicate_id) = attempt_opt(predicate_id_term.get::<u64>())? {
                    if let Some(object_id) = attempt_opt(object_id_term.get::<u64>())? {
                        // everything is known
                        if layer.triple_exists(subject_id, predicate_id, object_id) {
                            return Ok(None);
                        }
                        else {
                            return Err(PrologError::Failure)
                        }
                    }
                    else {
                        // subject and predicate are known, object is not
                        iter = layer.triples_sp(subject_id, predicate_id);
                    }
                }
                else {
                    // subject is known, predicate is not. object may or may not be bound already.
                    if let Some(object_id) = attempt_opt(object_id_term.get::<u64>())? {
                        // object is known so predicate is the only unknown
                        iter = Box::new(layer.triples_s(subject_id)
                                        .filter(move |t| t.object == object_id));
                    }
                    else {
                        // both predicate and object are unknown
                        iter = layer.triples_s(subject_id);
                    }
                }
            }
            else if let Some(object_id) = attempt_opt(object_id_term.get::<u64>())? {
                // subject is unknown
                if let Some(predicate_id) = attempt_opt(predicate_id_term.get::<u64>())? {
                    // predicate is known
                    iter = Box::new(layer.triples_o(object_id)
                                    .filter(move |t| t.predicate == predicate_id));
                }
                else {
                    // predicate is unknown, only object is known
                    iter = layer.triples_o(object_id)
                }
            }
            else if let Some(predicate_id) = attempt_opt(predicate_id_term.get::<u64>())? {
                // only predicate is known
                iter = layer.triples_p(predicate_id);
            }
            else {
                // nothing is known so return everything
                iter = layer.triples();
            }

            // lets make it peekable
            let iter = iter.peekable();

            Ok(Some(iter))
        },
        call(iter) => {
            if let Some(triple) = iter.next() {
                subject_id_term.unify(triple.subject)?;
                predicate_id_term.unify(triple.predicate)?;
                object_id_term.unify(triple.object)?;

                Ok(iter.peek().is_some())
            }
            else {
                Err(PrologError::Failure)
            }
        }
    }

    pub semidet fn sp_card(_context, layer_term, subject_id_term, predicate_id_term, count_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let subject_id: u64 = subject_id_term.get_ex()?;
        let predicate_id: u64 = predicate_id_term.get_ex()?;
        let iter = layer.triples_sp(subject_id, predicate_id);
        let count = iter.count() as u64;
        count_term.unify(count)
    }

    pub semidet fn op_card(_context, layer_term, object_id_term, predicate_id_term, count_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let object_id: u64 = object_id_term.get_ex()?;
        let predicate_id: u64 = predicate_id_term.get_ex()?;
        let count = layer.triples_o(object_id)
            .filter(|t| t.predicate == predicate_id)
            .count() as u64;
        count_term.unify(count)
    }

    pub nondet fn id_triple_addition<Peekable<Box<dyn Iterator<Item=IdTriple>+Send>>>(context, layer_term, subject_id_term, predicate_id_term, object_id_term) {
        setup => {
            let layer: WrappedLayer = layer_term.get_ex()?;

            let iter: Box<dyn Iterator<Item=IdTriple>+Send>;
            if let Some(subject_id) = attempt_opt(subject_id_term.get::<u64>())? {
                if let Some(predicate_id) = attempt_opt(predicate_id_term.get::<u64>())? {
                    if let Some(object_id) = attempt_opt(object_id_term.get::<u64>())? {
                        // everything is known
                        if context.try_or_die(layer.triple_addition_exists(subject_id, predicate_id, object_id))? {
                            return Ok(None);
                        }
                        else {
                            return Err(PrologError::Failure)
                        }
                    }
                    else {
                        // subject and predicate are known, object is not
                        iter = context.try_or_die(layer.triple_additions_sp(subject_id, predicate_id))?;
                    }
                }
                else {
                    // subject is known, predicate is not. object may or may not be bound already.
                    if let Some(object_id) = attempt_opt(object_id_term.get::<u64>())? {
                        // object is known so predicate is the only unknown
                        iter = Box::new(context.try_or_die(layer.triple_additions_s(subject_id))?
                                        .filter(move |t| t.object == object_id));
                    }
                    else {
                        // both predicate and object are unknown
                        iter = context.try_or_die(layer.triple_additions_s(subject_id))?;
                    }
                }
            }
            else if let Some(object_id) = attempt_opt(object_id_term.get::<u64>())? {
                // subject is unknown
                if let Some(predicate_id) = attempt_opt(predicate_id_term.get::<u64>())? {
                    // predicate is known
                    iter = Box::new(context.try_or_die(layer.triple_additions_o(object_id))?
                                    .filter(move |t| t.predicate == predicate_id));
                }
                else {
                    // predicate is unknown, only object is known
                    iter = context.try_or_die(layer.triple_additions_o(object_id))?
                }
            }
            else if let Some(predicate_id) = attempt_opt(predicate_id_term.get::<u64>())? {
                // only predicate is known
                iter = context.try_or_die(layer.triple_additions_p(predicate_id))?;
            }
            else {
                // nothing is known so return everything
                iter = context.try_or_die(layer.triple_additions())?;
            }

            // lets make it peekable
            let iter = iter.peekable();

            Ok(Some(iter))
        },
        call(iter) => {
            if let Some(triple) = iter.next() {
                subject_id_term.unify(triple.subject)?;
                predicate_id_term.unify(triple.predicate)?;
                object_id_term.unify(triple.object)?;

                Ok(iter.peek().is_some())
            }
            else {
                Err(PrologError::Failure)
            }
        }
    }

    pub nondet fn id_triple_removal<Peekable<Box<dyn Iterator<Item=IdTriple>+Send>>>(context, layer_term, subject_id_term, predicate_id_term, object_id_term) {
        setup => {
            let layer: WrappedLayer = layer_term.get_ex()?;

            let iter: Box<dyn Iterator<Item=IdTriple>+Send>;
            if let Some(subject_id) = attempt_opt(subject_id_term.get::<u64>())? {
                if let Some(predicate_id) = attempt_opt(predicate_id_term.get::<u64>())? {
                    if let Some(object_id) = attempt_opt(object_id_term.get::<u64>())? {
                        // everything is known
                        if context.try_or_die(layer.triple_removal_exists(subject_id, predicate_id, object_id))? {
                            return Ok(None);
                        }
                        else {
                            return Err(PrologError::Failure)
                        }
                    }
                    else {
                        // subject and predicate are known, object is not
                        iter = context.try_or_die(layer.triple_removals_sp(subject_id, predicate_id))?;
                    }
                }
                else {
                    // subject is known, predicate is not. object may or may not be bound already.
                    if let Some(object_id) = attempt_opt(object_id_term.get::<u64>())? {
                        // object is known so predicate is the only unknown
                        iter = Box::new(context.try_or_die(layer.triple_removals_s(subject_id))?
                                        .filter(move |t| t.object == object_id));
                    }
                    else {
                        // both predicate and object are unknown
                        iter = context.try_or_die(layer.triple_removals_s(subject_id))?;
                    }
                }
            }
            else if let Some(object_id) = attempt_opt(object_id_term.get::<u64>())? {
                // subject is unknown
                if let Some(predicate_id) = attempt_opt(predicate_id_term.get::<u64>())? {
                    // predicate is known
                    iter = Box::new(context.try_or_die(layer.triple_removals_o(object_id))?
                                    .filter(move |t| t.predicate == predicate_id));
                }
                else {
                    // predicate is unknown, only object is known
                    iter = context.try_or_die(layer.triple_removals_o(object_id))?
                }
            }
            else if let Some(predicate_id) = attempt_opt(predicate_id_term.get::<u64>())? {
                // only predicate is known
                iter = context.try_or_die(layer.triple_removals_p(predicate_id))?;
            }
            else {
                // nothing is known so return everything
                iter = context.try_or_die(layer.triple_removals())?;
            }

            // lets make it peekable
            let iter = iter.peekable();

            Ok(Some(iter))
        },
        call(iter) => {
            if let Some(triple) = iter.next() {
                subject_id_term.unify(triple.subject)?;
                predicate_id_term.unify(triple.predicate)?;
                object_id_term.unify(triple.object)?;

                Ok(iter.peek().is_some())
            }
            else {
                Err(PrologError::Failure)
            }
        }
    }

    pub nondet fn id_triple_value_range<Peekable<Box<dyn Iterator<Item=IdTriple>+Send>>>(context, layer_term, low_term, high_term, subject_id_term, predicate_id_term, object_id_term) {
        setup => {
            let layer: WrappedLayer = layer_term.get_ex()?;

            let low_inner = context.new_term_ref();
            let low_ty = context.new_term_ref();
            let low_entry;
            if attempt(low_term.unify(term!{context: value(#&low_inner, #&low_ty)}?))? {
                low_entry = make_entry_from_term(context, &low_inner, &low_ty)?;
            } else if attempt(low_term.unify(term!{context: lang(#&low_inner, #&low_ty)}?))? {
                low_entry = make_entry_from_lang_term(context, &low_inner, &low_ty)?;
            } else {
                return context.raise_exception(&term!{context: error(domain_error(oneof([value(), lang()]), #low_term), _)}?);
            }

            let high_inner = context.new_term_ref();
            let high_ty = context.new_term_ref();
            let high_entry;
            if attempt(high_term.unify(term!{context: value(#&high_inner, #&high_ty)}?))? {
                high_entry = make_entry_from_term(context, &high_inner, &high_ty)?;
            } else if attempt(high_term.unify(term!{context: lang(#&high_inner, #&high_ty)}?))? {
                high_entry = make_entry_from_lang_term(context, &high_inner, &high_ty)?;
            } else {
                return context.raise_exception(&term!{context: error(domain_error(oneof([value(), lang()]), #high_term), _)}?);
            }

            let iter = layer.triples_value_range(&low_entry, &high_entry).peekable();

            Ok(Some(iter))
        },
        call(iter) => {
            if let Some(triple) = iter.next() {
                subject_id_term.unify(triple.subject)?;
                predicate_id_term.unify(triple.predicate)?;
                object_id_term.unify(triple.object)?;

                Ok(iter.peek().is_some())
            }
            else {
                Err(PrologError::Failure)
            }
        }
    }

    pub semidet fn id_triple_sp_value_next(context, layer_term, subject_id_term, predicate_id_term, reference_term, result_object_id_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let subject: u64 = subject_id_term.get_ex()?;
        let predicate: u64 = predicate_id_term.get_ex()?;

        let ref_inner = context.new_term_ref();
        let ref_ty = context.new_term_ref();
        let reference;
        if attempt(reference_term.unify(term!{context: value(#&ref_inner, #&ref_ty)}?))? {
            reference = make_entry_from_term(context, &ref_inner, &ref_ty)?;
        } else if attempt(reference_term.unify(term!{context: lang(#&ref_inner, #&ref_ty)}?))? {
            reference = make_entry_from_lang_term(context, &ref_inner, &ref_ty)?;
        } else {
            return context.raise_exception(&term!{context: error(domain_error(oneof([value(), lang()]), #reference_term), _)}?);
        }

        let ref_dt = reference.datatype();
        let mut best: Option<(u64, TypedDictEntry)> = None;

        for triple in layer.triples_sp(subject, predicate) {
            if let Some(ObjectType::Value(entry)) = layer.id_object(triple.object) {
                if entry.datatype() == ref_dt && entry > reference {
                    if let Some((_, ref best_entry)) = best {
                        if entry < *best_entry {
                            best = Some((triple.object, entry));
                        }
                    } else {
                        best = Some((triple.object, entry));
                    }
                }
            }
        }

        match best {
            Some((oid, _)) => result_object_id_term.unify(oid),
            None => Err(PrologError::Failure),
        }
    }

    pub semidet fn id_triple_sp_value_previous(context, layer_term, subject_id_term, predicate_id_term, reference_term, result_object_id_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let subject: u64 = subject_id_term.get_ex()?;
        let predicate: u64 = predicate_id_term.get_ex()?;

        let ref_inner = context.new_term_ref();
        let ref_ty = context.new_term_ref();
        let reference;
        if attempt(reference_term.unify(term!{context: value(#&ref_inner, #&ref_ty)}?))? {
            reference = make_entry_from_term(context, &ref_inner, &ref_ty)?;
        } else if attempt(reference_term.unify(term!{context: lang(#&ref_inner, #&ref_ty)}?))? {
            reference = make_entry_from_lang_term(context, &ref_inner, &ref_ty)?;
        } else {
            return context.raise_exception(&term!{context: error(domain_error(oneof([value(), lang()]), #reference_term), _)}?);
        }

        let ref_dt = reference.datatype();
        let mut best: Option<(u64, TypedDictEntry)> = None;

        for triple in layer.triples_sp(subject, predicate) {
            if let Some(ObjectType::Value(entry)) = layer.id_object(triple.object) {
                if entry.datatype() == ref_dt && entry < reference {
                    if let Some((_, ref best_entry)) = best {
                        if entry > *best_entry {
                            best = Some((triple.object, entry));
                        }
                    } else {
                        best = Some((triple.object, entry));
                    }
                }
            }
        }

        match best {
            Some((oid, _)) => result_object_id_term.unify(oid),
            None => Err(PrologError::Failure),
        }
    }

    pub semidet fn retrieve_layer_stack_names(context, layer_term, layer_stack_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;

        let names = context.try_or_die(layer.retrieve_layer_stack_names())?;
        let name_strings: Vec<String> = names.into_iter()
            .map(name_to_string)
            .collect();

        layer_stack_term.unify(name_strings.as_slice())
    }

    pub semidet fn layer_stored_size(_context, layer_term, size_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let size = layer.stored_size();
        size_term.unify(size as u64)
    }

    pub semidet fn layer_equals(_context, layer1_term, layer2_term) {
        let layer1: WrappedLayer = layer1_term.get_ex()?;
        let layer2: WrappedLayer = layer2_term.get_ex()?;

        into_prolog_result(*layer1 == *layer2)
    }
}

wrapped_clone_blob!("layer", pub WrappedLayer, SyncStoreLayer);

impl CloneBlobImpl for WrappedLayer {
    fn write(&self, stream: &mut PrologStream) -> io::Result<()> {
        write!(stream, "<layer {}>", name_to_string(self.name()))
    }
}
