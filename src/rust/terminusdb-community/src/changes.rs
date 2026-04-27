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

/// Check whether a type_id corresponds to a document type using numeric
/// ID comparison.  The `document_type_ids` set contains instance-layer IDs
/// translated from the schema via `schema_to_instance_types`.  This works
/// because terminus-store uses a shared node dictionary for subjects and
/// object nodes, so `translate_subject_id` (which calls `subject_id`) and
/// triple `t.object` IDs are in the same numbering space.
fn is_document_type(
    type_id: u64,
    json_document_id: Option<u64>,
    document_type_ids: &HashSet<u64>,
) -> bool {
    json_document_id == Some(type_id) || document_type_ids.contains(&type_id)
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

    // Phase 1 — classify every changed subject by its rdf:type delta
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

    // Phase 2 — partition into document-type entries and non-document entries.
    // Document-type entries keep their original change classification;
    // only Changed non-document entries are collected for the containment
    // walk-up in Phase 3.  Added/Deleted non-document entries (subdocuments,
    // Cons cells) are skipped: their parent document always has its own
    // change entry (Added parent for new docs, Changed parent for updates,
    // link-removal for deletions), so the walk-up would be redundant.
    // Walking them up would also cause non-deterministic results because
    // HashMap iteration order is randomised per-process, and shared Cons
    // cells can pollute the visited set via different walk-up paths.
    let mut result: HashMap<u64, ChangeType> = HashMap::new();
    let mut visited: HashSet<u64> = HashSet::new();
    let mut nondoc_ids: Vec<u64> = Vec::new();

    for (id, change_type) in changes {
        let type_id = match &change_type {
            ChangeType::Added(t) | ChangeType::Deleted(t) => *t,
            ChangeType::Changed => {
                if let Some(t) = instance.single_triple_sp(id, rdf_type_id) {
                    t.object
                } else {
                    continue;
                }
            }
        };

        if is_document_type(type_id, json_document_id, &document_type_ids) {
            visited.insert(id);
            result.insert(id, change_type);
        } else {
            // Only Changed non-doc entries need containment walk-up.
            // Added/Deleted entries always have a parent with its own entry.
            if matches!(change_type, ChangeType::Changed) {
                nondoc_ids.push(id);
            }
        }
    }

    // Phase 3 — walk up the containment hierarchy for non-document entries
    // to find the enclosing document and report it as Changed.
    // Deletions are skipped: the parent's link-removal triple guarantees
    // the parent already appears in `changes` with its own entry.
    for id in nondoc_ids {
        if visited.contains(&id) {
            continue;
        }
        visited.insert(id);

        let mut current = id;
        loop {
            if let Some(parent_triple) = instance.triples_o(current).next() {
                let parent_id = parent_triple.subject;
                if current != id && visited.contains(&parent_id) {
                    break;
                }
                visited.insert(parent_id);
                current = parent_id;

                if let Some(parent_type_triple) =
                    instance.single_triple_sp(parent_id, rdf_type_id)
                {
                    let parent_type = parent_type_triple.object;
                    if is_document_type(parent_type, json_document_id, &document_type_ids) {
                        result.entry(current).or_insert(ChangeType::Changed);
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    let mut result: Vec<(u64, ChangeType)> = result.into_iter().collect();
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

    /// Semidet predicate that computes all document changes in a single call
    /// and returns three lists: Added, Changed, Deleted.
    /// This avoids the 3x overhead and cross-process non-determinism of
    /// calling the nondet changed_document_id predicate three times via findall.
    #[module("$changes")]
    semidet fn collect_changed_documents(context, transaction_term, added_term, changed_term, deleted_term) {
        let schema_layer = transaction_schema_layer(context, transaction_term)?;
        let instance_layer = transaction_instance_layer(context, transaction_term)?;
        match (schema_layer, instance_layer) {
            (Some(schema_layer), Some(instance_layer)) => {
                let changes = context.try_or_die(changed_document_ids(&schema_layer, &instance_layer))?;

                let mut added: Vec<String> = Vec::new();
                let mut changed: Vec<String> = Vec::new();
                let mut deleted: Vec<String> = Vec::new();

                for (id, change_type) in changes {
                    let iri = instance_layer.id_subject(id).expect("id was not in dictionary");
                    match change_type {
                        ChangeType::Added(_) => added.push(iri),
                        ChangeType::Changed => changed.push(iri),
                        ChangeType::Deleted(_) => deleted.push(iri),
                    }
                }

                added_term.unify(added.as_slice())?;
                changed_term.unify(changed.as_slice())?;
                deleted_term.unify(deleted.as_slice())?;

                Ok(())
            },
            _ => Err(PrologError::Failure)
        }
    }
}

pub fn register() {
    register_changed_document_id();
    register_collect_changed_documents();
}
