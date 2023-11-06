use std::{
    collections::{HashMap, HashSet},
    io,
};

use swipl::{atom, predicates, prelude::Atom, result::PrologError};
use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;

use crate::{
    consts::{RdfIds, SysIds, RDF_TYPE, SYS_JSON_DOCUMENT},
    schema::SchemaQueryContext,
    terminus_store::Layer,
    types::{transaction_instance_layer, transaction_schema_layer},
};

#[derive(Debug)]
pub enum ChangeType {
    Added(u64),
    Deleted(u64),
    Changed,
}

impl ChangeType {
    fn as_atom(&self) -> Atom {
        match self {
            Self::Added(_) => atom!("added"),
            Self::Deleted(_) => atom!("deleted"),
            Self::Changed => atom!("changed"),
        }
    }

    fn is_addition(&self) -> bool {
        match self {
            Self::Added(_) => true,
            _ => false,
        }
    }
}

pub fn changed_document_ids(
    schema: &SyncStoreLayer,
    instance: &SyncStoreLayer,
) -> io::Result<Vec<(u64, ChangeType)>> {
    let schema_rdf = RdfIds::new(Some(schema.clone()));
    let schema_sys = SysIds::new(Some(schema.clone()));
    let schema_context = SchemaQueryContext::new(schema, &schema_rdf, &schema_sys);
    let schema_document_type_ids = schema_context.get_document_type_ids_from_schema();
    let document_type_ids: HashSet<u64> = schema_context
        .schema_to_instance_types(instance, schema_document_type_ids)
        .collect();
    let rdf_type_id = instance.predicate_id(RDF_TYPE);
    let json_document_id = instance.subject_id(SYS_JSON_DOCUMENT);
    if rdf_type_id.is_none() {
        return Ok(Vec::with_capacity(0));
    }
    let rdf_type_id = rdf_type_id.unwrap();

    let mut changes: HashMap<u64, ChangeType> = HashMap::new();
    for t in instance.triple_additions()? {
        if t.predicate == rdf_type_id {
            changes.insert(t.subject, ChangeType::Added(t.object));
        } else if !changes.contains_key(&t.subject) {
            changes.insert(t.subject, ChangeType::Changed);
        }
    }
    for t in instance.triple_removals()? {
        if t.predicate == rdf_type_id {
            if changes.contains_key(&t.subject) && changes[&t.subject].is_addition() {
                changes.insert(t.subject, ChangeType::Changed);
            } else {
                changes.insert(t.subject, ChangeType::Deleted(t.object));
            }
        } else if !changes.contains_key(&t.subject) {
            changes.insert(t.subject, ChangeType::Changed);
        }
    }

    let mut visited: HashSet<u64> = HashSet::new();
    let mut result: Vec<(u64, ChangeType)> = Vec::new();

    for (id, change_type) in changes {
        if visited.contains(&id) {
            continue;
        }
        visited.insert(id);
        let type_id = match change_type {
            ChangeType::Added(t) => t,
            ChangeType::Deleted(t) => t,
            ChangeType::Changed => {
                if let Some(t) = instance.single_triple_sp(id, rdf_type_id) {
                    t.object
                } else {
                    // no type? Not a document!
                    continue;
                }
            }
        };
        if json_document_id != Some(type_id) && !document_type_ids.contains(&type_id) {
            // if this is a deletion or an addition, there should be another triple addition/removal at a higher level. no need to query backwards.
            match change_type {
                ChangeType::Deleted(_) => continue,
                ChangeType::Added(_) => continue,
                ChangeType::Changed => {}
            }
        } else {
            result.push((id, change_type));
            continue;
        }

        let mut current = id;
        let mut found = false;
        loop {
            if let Some(parent_triple) = instance.triples_o(current).next() {
                if current != id && visited.contains(&parent_triple.subject) {
                    break;
                }
                visited.insert(parent_triple.subject);
                current = parent_triple.subject;

                if let Some(parent_type_triple) =
                    instance.single_triple_sp(parent_triple.subject, rdf_type_id)
                {
                    let parent_type = parent_type_triple.object;
                    if json_document_id == Some(parent_type)
                        || document_type_ids.contains(&parent_type)
                    {
                        found = true;
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if !found {
            continue;
        }

        result.push((current, ChangeType::Changed));
    }

    result.shrink_to_fit();
    Ok(result)
}

struct State {
    changes: Vec<(u64, ChangeType)>,
    layer: SyncStoreLayer,
}

predicates! {
    #[module("$changes")]
    nondet fn changed_document_id<State>(context, transaction_term, id_term, change_type_term) {
        setup => {
            let schema_layer = transaction_schema_layer(context, transaction_term)?;
            let instance_layer = transaction_instance_layer(context, transaction_term)?;
            match (schema_layer, instance_layer) {
                (Some(schema_layer), Some(instance_layer)) => {
                    let changes = context.try_or_die(changed_document_ids(&schema_layer, &instance_layer))?;
                    Ok(Some(State { changes, layer: instance_layer }))
                },
                _ => Err(PrologError::Failure)
            }
        },
        call(state) => {
            let State{changes, layer} = state;
            if let Some((id, change_type)) = changes.pop() {
                let iri = layer.id_subject(id).expect("id was not in dictionary");
                id_term.unify(iri)?;
                change_type_term.unify(change_type.as_atom())?;

                Ok(!changes.is_empty())
            } else {
                Err(PrologError::Failure)
            }
        }
    }
}

pub fn register() {
    register_changed_document_id();
}
