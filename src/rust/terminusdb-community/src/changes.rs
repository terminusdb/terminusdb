use std::{
    collections::{HashMap, HashSet},
    io,
};

use swipl::{atom, predicates, prelude::Atom, result::PrologError};
use swipl::prelude::PrologText;
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

    // Phase 4 — @shared reference propagation.
    //
    // When a @shared document is Added or Changed, any document that
    // references it should also be reported as Changed, because the
    // content visible through that reference has changed. This is the
    // reference-graph analogue of the containment walk-up in Phase 3,
    // but for @shared documents (which have their own IRIs and are
    // referenced, not contained).
    //
    // The propagation is transitive: if A references @shared B, and B
    // references @shared C, then a change to C should mark both B and
    // A as Changed. Cycle safety is handled via a dedicated visited set.
    //
    // Deleted @shared documents are NOT propagated here: the cascade-
    // delete pre-commit hook already removes them from the instance
    // graph, and the referrer's link-removal triple ensures the
    // referrer already appears in `changes` with its own entry.
    let shared_schema_ids = schema_context.get_shared_ids_from_schema();
    if !shared_schema_ids.is_empty() {
        let shared_instance_ids: HashSet<u64> = schema_context
            .schema_to_instance_types(instance, shared_schema_ids.iter().copied())
            .collect();

        if !shared_instance_ids.is_empty() {
            // Collect all Added/Changed documents whose type is @shared.
            // These are the seeds for the reference walk-up.
            let mut shared_work: Vec<u64> = Vec::new();
            for (&id, change_type) in &result {
                if matches!(change_type, ChangeType::Added(_) | ChangeType::Changed) {
                    if let Some(type_triple) = instance.single_triple_sp(id, rdf_type_id) {
                        if shared_instance_ids.contains(&type_triple.object) {
                            shared_work.push(id);
                        }
                    }
                }
            }

            // Walk up the reference graph from each changed @shared document.
            // A separate visited set prevents infinite loops on cycles.
            let mut ref_visited: HashSet<u64> = HashSet::new();
            while let Some(shared_id) = shared_work.pop() {
                if !ref_visited.insert(shared_id) {
                    continue;
                }

                // Find all documents that reference this @shared document
                // (i.e., triples where shared_id is the object).
                for ref_triple in instance.triples_o(shared_id) {
                    let referrer = ref_triple.subject;

                    // Skip self-references
                    if referrer == shared_id {
                        continue;
                    }

                    // Only mark document-type referrers (skip Cons cells,
                    // subdocuments, etc. — those are handled by Phase 3).
                    if let Some(ref_type_triple) =
                        instance.single_triple_sp(referrer, rdf_type_id)
                    {
                        if is_document_type(
                            ref_type_triple.object,
                            json_document_id,
                            &document_type_ids,
                        ) {
                            // Mark referrer as Changed if it doesn't already
                            // have an entry. or_insert preserves existing
                            // Added/Deleted/Changed entries — we never
                            // downgrade a stronger change type.
                            result.entry(referrer).or_insert(ChangeType::Changed);

                            // If the referrer is itself @shared, continue
                            // propagating transitively.
                            if shared_instance_ids.contains(&ref_type_triple.object)
                                && !ref_visited.contains(&referrer)
                            {
                                shared_work.push(referrer);
                            }
                        }
                    }
                }
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

    /// Nondet predicate that enumerates changed document IDs filtered by
    /// a specific type IRI. This avoids the overhead of enumerating all
    /// changes and filtering post-hoc in Prolog.
    ///
    /// Signature: changed_document_id_filtered(+Transaction, -Id, -ChangeType, +TypeIRI)
    /// TypeIRI is the full IRI string of the type to filter by (e.g.
    /// "terminusdb:///schema#MyClass"). If the type IRI is not in the
    /// instance layer's dictionary, the predicate fails (no documents of
    /// that type exist).
    #[module("$changes")]
    nondet fn changed_document_id_filtered<State>(context, transaction_term, id_term, change_type_term, type_iri_term) {
        setup => {
            let schema_layer = transaction_schema_layer(context, transaction_term)?;
            let instance_layer = transaction_instance_layer(context, transaction_term)?;
            match (schema_layer, instance_layer) {
                (Some(schema_layer), Some(instance_layer)) => {
                    let type_iri: String = type_iri_term.get_ex()?;
                    let type_id = match instance_layer.subject_id(&type_iri) {
                        Some(id) => id,
                        None => return Err(PrologError::Failure),
                    };
                    let all_changes = context.try_or_die(changed_document_ids(&schema_layer, &instance_layer))?;
                    let rdf_type_id = match instance_layer.predicate_id(RDF_TYPE) {
                        Some(id) => id,
                        None => return Err(PrologError::Failure),
                    };
                    // Filter: keep only entries whose rdf:type matches type_id.
                    let filtered: Vec<(u64, ChangeType)> = all_changes
                        .into_iter()
                        .filter(|(id, change_type)| {
                            let entry_type_id = match change_type {
                                ChangeType::Added(t) | ChangeType::Deleted(t) => *t,
                                ChangeType::Changed => {
                                    match instance_layer.single_triple_sp(*id, rdf_type_id) {
                                        Some(triple) => triple.object,
                                        None => return false,
                                    }
                                }
                            };
                            entry_type_id == type_id
                        })
                        .collect();
                    Ok(Some(State { changes: filtered, layer: instance_layer }))
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

    /// Nondet predicate that enumerates changed document IDs filtered by
    /// a list of type IRIs, streaming solutions one at a time.
    ///
    /// Signature: collect_changed_documents_filtered(+Transaction, +TypeIRIs, -Id, -ChangeType)
    /// TypeIRIs is a Prolog list of type IRI strings to filter by (e.g.
    /// ["terminusdb:///schema#Article", "terminusdb:///schema#Tutorial"]).
    /// Only documents whose rdf:type matches one of the provided type IRIs
    /// are yielded. Type IRIs not in the instance layer's dictionary are
    /// silently skipped (no documents of that type exist in this layer).
    ///
    /// Each solution is a (Id, ChangeType) pair where ChangeType is the
    /// atom `added`, `changed`, or `deleted`.
    #[module("$changes")]
    nondet fn collect_changed_documents_filtered<State>(context, transaction_term, type_iris_term, id_term, change_type_term) {
        setup => {
            let schema_layer = transaction_schema_layer(context, transaction_term)?;
            let instance_layer = transaction_instance_layer(context, transaction_term)?;
            match (schema_layer, instance_layer) {
                (Some(schema_layer), Some(instance_layer)) => {
                    let changes = context.try_or_die(changed_document_ids(&schema_layer, &instance_layer))?;

                    // Collect type IRI strings from the Prolog list term.
                    // PrologText handles both atoms and strings (IRIs are
                    // typically atoms in TerminusDB).
                    let type_iris: Vec<String> = context
                        .term_list_iter(type_iris_term)
                        .map(|t| t.get_ex::<PrologText>().map(|t| t.into_inner()))
                        .collect::<Result<_, _>>()?;

                    // Convert type IRIs to numeric IDs. Skip any that are not
                    // in the instance layer dictionary.
                    let mut filter_type_ids: HashSet<u64> = HashSet::new();
                    for iri in &type_iris {
                        if let Some(id) = instance_layer.subject_id(iri) {
                            filter_type_ids.insert(id);
                        }
                    }

                    // If no filter type IDs resolved, return empty (fail immediately).
                    if filter_type_ids.is_empty() {
                        return Err(PrologError::Failure);
                    }

                    let rdf_type_id = match instance_layer.predicate_id(RDF_TYPE) {
                        Some(id) => id,
                        None => return Err(PrologError::Failure),
                    };

                    // Filter changes by type.
                    let filtered: Vec<(u64, ChangeType)> = changes
                        .into_iter()
                        .filter(|(id, change_type)| {
                            let entry_type_id = match change_type {
                                ChangeType::Added(t) | ChangeType::Deleted(t) => *t,
                                ChangeType::Changed => {
                                    match instance_layer.single_triple_sp(*id, rdf_type_id) {
                                        Some(triple) => triple.object,
                                        None => return false,
                                    }
                                }
                            };
                            filter_type_ids.contains(&entry_type_id)
                        })
                        .collect();

                    Ok(Some(State { changes: filtered, layer: instance_layer }))
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

/// Embedding metadata change detection on a commit's schema-layer delta.
///
/// Schema `@metadata` is stored as content-addressed JSON:
/// `Type --sys:metadata--> JSON-root --json#embedding--> JSON-node`.
/// The embedding node is the config's signature: identical query/template
/// content yields the identical node, any change yields a new one.
///
/// Detection triggers on `sys:metadata` edges churning in the delta, then
/// compares the embedding-node sets reachable from each type's old roots
/// vs new roots. Roots are looked up in the merged layer (covering
/// subtrees that survive, including roots shared between types with
/// identical metadata) plus the delta's removed edges (covering subtrees
/// that went away entirely). Piecing chains inside the delta alone is NOT
/// sufficient: a shared root's subtree edges never enter the delta, so a
/// type dropping metadata it shared would go undetected.
///
/// Equal embedding-node sets — e.g. an unrelated metadata key edit that
/// retargets sys:metadata but keeps the same embedding subtree — produce
/// no report, so unrelated edits cannot false-trigger. Default-embedding
/// types (json2markdown) have no metadata edges and are excluded by
/// construction.
pub const SYS_METADATA: &str = "http://terminusdb.com/schema/sys#metadata";
pub const JSON_EMBEDDING: &str = "http://terminusdb.com/schema/json#embedding";
pub const JSON_QUERY: &str = "http://terminusdb.com/schema/json#query";
pub const JSON_TEMPLATE: &str = "http://terminusdb.com/schema/json#template";

/// Embedding nodes directly under the given metadata roots in the merged
/// layer view (state at this commit).
fn embedding_nodes(layer: &SyncStoreLayer, embedding_pred: u64, roots: &HashSet<u64>) -> HashSet<u64> {
    let mut out = HashSet::new();
    for r in roots {
        for t in layer.triples_sp(*r, embedding_pred) {
            out.insert(t.object);
        }
    }
    out
}

/// Returns the subject ids of types whose embedding config changed in this
/// schema layer's delta.
pub fn changed_embedding_type_ids(layer: &SyncStoreLayer) -> io::Result<HashSet<u64>> {
    let (Some(metadata_pred), Some(embedding_pred)) = (
        layer.predicate_id(SYS_METADATA),
        layer.predicate_id(JSON_EMBEDDING),
    ) else {
        // No metadata machinery in the dictionary → nothing can match.
        return Ok(HashSet::new());
    };

    // sys:metadata edges churned per type, and embedding edges removed —
    // the latter complete old subtrees that no merged-layer lookup can see.
    let mut removed_roots: HashMap<u64, HashSet<u64>> = HashMap::new();
    let mut added_roots: HashMap<u64, HashSet<u64>> = HashMap::new();
    let mut removed_embed: HashMap<u64, HashSet<u64>> = HashMap::new();
    for t in layer.triple_removals()? {
        if t.predicate == metadata_pred {
            removed_roots.entry(t.subject).or_default().insert(t.object);
        } else if t.predicate == embedding_pred {
            removed_embed.entry(t.subject).or_default().insert(t.object);
        }
    }
    for t in layer.triple_additions()? {
        if t.predicate == metadata_pred {
            added_roots.entry(t.subject).or_default().insert(t.object);
        }
    }

    let empty: HashSet<u64> = HashSet::new();
    let mut result = HashSet::new();
    for ty in removed_roots.keys().chain(added_roots.keys()) {
        // Old roots whose subtrees were deleted outright only show in the
        // delta's removed edges; surviving subtrees are in the merged view.
        let mut old = embedding_nodes(layer, embedding_pred,
                                      removed_roots.get(ty).unwrap_or(&empty));
        for r in removed_roots.get(ty).into_iter().flatten() {
            if let Some(objs) = removed_embed.get(r) {
                old.extend(objs.iter().copied());
            }
        }
        let new = embedding_nodes(layer, embedding_pred,
                                  added_roots.get(ty).unwrap_or(&empty));
        if old != new {
            result.insert(*ty);
        }
    }
    Ok(result)
}

predicates! {
    /// Nondet predicate that enumerates the types whose embedding
    /// query/template configuration changed in a commit's schema layer.
    ///
    /// Signature: collect_changed_embedding_types(+Transaction, -TypeIRI)
    ///
    /// Yields each changed type's IRI once. Fails (empty) when the commit
    /// has no schema layer or no embedding-config delta.
    #[module("$changes")]
    nondet fn collect_changed_embedding_types<State>(context, transaction_term, type_iri_term) {
        setup => {
            let schema_layer = transaction_schema_layer(context, transaction_term)?;
            match schema_layer {
                Some(schema_layer) => {
                    // ChangeType is unused here — ids only.
                    let types: Vec<(u64, ChangeType)> = context
                        .try_or_die(changed_embedding_type_ids(&schema_layer))?
                        .into_iter()
                        .map(|id| (id, ChangeType::Changed))
                        .collect();
                    if types.is_empty() {
                        return Err(PrologError::Failure);
                    }
                    Ok(Some(State { changes: types, layer: schema_layer }))
                },
                _ => Err(PrologError::Failure)
            }
        },
        call(state) => {
            let State{changes, layer} = state;
            if let Some((id, _)) = changes.pop() {
                let iri = layer.id_subject(id).expect("id was not in dictionary");
                type_iri_term.unify(iri)?;
                Ok(!changes.is_empty())
            } else {
                Err(PrologError::Failure)
            }
        }
    }
}

pub fn register() {
    register_changed_document_id();
    register_changed_document_id_filtered();
    register_collect_changed_documents();
    register_collect_changed_documents_filtered();
    register_collect_changed_embedding_types();
}

#[cfg(test)]
mod embedding_change_tests {
    use super::*;
    use terminusdb_store_prolog::terminus_store::layer::ValueTriple;
    use terminusdb_store_prolog::terminus_store::open_sync_memory_store;

    const TYPE_A: &str = "http://example.com/schema#A";
    const TYPE_B: &str = "http://example.com/schema#B";
    const META_A1: &str = "terminusdb:///json/JSON/SHA1/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa001";
    const EMB_A1: &str = "terminusdb:///json/JSON/SHA1/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa002";
    const META_A2: &str = "terminusdb:///json/JSON/SHA1/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb001";
    const EMB_A2: &str = "terminusdb:///json/JSON/SHA1/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb002";
    const META_B: &str = "terminusdb:///json/JSON/SHA1/ccccccccccccccccccccccccccccccccccc001";
    const EMB_B: &str = "terminusdb:///json/JSON/SHA1/ccccccccccccccccccccccccccccccccccc002";

    fn metadata_chain(b: &terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayerBuilder, ty: &str, root: &str, emb: &str, template: &str) {
        b.add_value_triple(ValueTriple::new_node(ty, SYS_METADATA, root)).unwrap();
        b.add_value_triple(ValueTriple::new_node(root, JSON_EMBEDDING, emb)).unwrap();
        b.add_value_triple(ValueTriple::new_string_value(emb, JSON_QUERY, "q")).unwrap();
        b.add_value_triple(ValueTriple::new_string_value(emb, JSON_TEMPLATE, template)).unwrap();
    }

    /// v1 layer: types A and B each with a metadata chain.
    fn base_layer() -> SyncStoreLayer {
        let store = open_sync_memory_store();
        let b = store.create_base_layer().unwrap();
        metadata_chain(&b, TYPE_A, META_A1, EMB_A1, "OLD {{t}}");
        metadata_chain(&b, TYPE_B, META_B, EMB_B, "SAME {{t}}");
        b.commit().unwrap()
    }

    #[test]
    fn template_change_reports_only_changed_type() {
        let base = base_layer();
        let b = base.open_write().unwrap();
        // Remove A's whole old chain, add new chain (template changed).
        for t in [
            ValueTriple::new_node(TYPE_A, SYS_METADATA, META_A1),
            ValueTriple::new_node(META_A1, JSON_EMBEDDING, EMB_A1),
            ValueTriple::new_string_value(EMB_A1, JSON_QUERY, "q"),
            ValueTriple::new_string_value(EMB_A1, JSON_TEMPLATE, "OLD {{t}}"),
        ] {
            b.remove_value_triple(t).unwrap();
        }
        metadata_chain(&b, TYPE_A, META_A2, EMB_A2, "NEW {{t}}");
        // B untouched — identical metadata produces no delta.
        let layer = b.commit().unwrap();

        let changed = changed_embedding_type_ids(&layer).unwrap();
        let a_id = layer.subject_id(TYPE_A).unwrap();
        assert!(changed.contains(&a_id));
        assert_eq!(changed.len(), 1);
    }

    #[test]
    fn unrelated_metadata_change_does_not_report() {
        let base = base_layer();
        let b = base.open_write().unwrap();
        // A's metadata root changes for an unrelated key — the embedding
        // subtree hash is unchanged, so only the sys:metadata edge and the
        // root's edge to the (same) embedding node are re-added. No leaf
        // edges churn → chain incomplete → no report.
        let other_old = "terminusdb:///json/JSON/SHA1/ddddddddddddddddddddddddddddddddddd001";
        let other_new = "terminusdb:///json/JSON/SHA1/ddddddddddddddddddddddddddddddddddd002";
        b.remove_value_triple(ValueTriple::new_node(TYPE_A, SYS_METADATA, META_A1)).unwrap();
        b.add_value_triple(ValueTriple::new_node(TYPE_A, SYS_METADATA, META_A2)).unwrap();
        b.add_value_triple(ValueTriple::new_node(META_A2, JSON_EMBEDDING, EMB_A1)).unwrap();
        b.add_value_triple(ValueTriple::new_string_value(META_A2, "http://terminusdb.com/schema/json#other", "x")).unwrap();
        let _ = (other_old, other_new);
        let layer = b.commit().unwrap();

        let changed = changed_embedding_type_ids(&layer).unwrap();
        assert!(changed.is_empty());
    }

    #[test]
    fn metadata_removal_reports_type() {
        let base = base_layer();
        let b = base.open_write().unwrap();
        for t in [
            ValueTriple::new_node(TYPE_B, SYS_METADATA, META_B),
            ValueTriple::new_node(META_B, JSON_EMBEDDING, EMB_B),
            ValueTriple::new_string_value(EMB_B, JSON_QUERY, "q"),
            ValueTriple::new_string_value(EMB_B, JSON_TEMPLATE, "SAME {{t}}"),
        ] {
            b.remove_value_triple(t).unwrap();
        }
        let layer = b.commit().unwrap();

        let changed = changed_embedding_type_ids(&layer).unwrap();
        let b_id = layer.subject_id(TYPE_B).unwrap();
        assert_eq!(changed.iter().copied().collect::<Vec<_>>(), vec![b_id]);
    }

    #[test]
    fn shared_metadata_root_removal_reports_type() {
        // A and B share one content-addressed metadata root. Removing A's
        // metadata emits only the A→root removal — the root's subtree edges
        // still exist via B, so they are absent from the delta.
        let store = open_sync_memory_store();
        let b = store.create_base_layer().unwrap();
        metadata_chain(&b, TYPE_A, META_A1, EMB_A1, "T {{t}}");
        b.add_value_triple(ValueTriple::new_node(TYPE_B, SYS_METADATA, META_A1)).unwrap();
        let base = b.commit().unwrap();

        let b = base.open_write().unwrap();
        b.remove_value_triple(ValueTriple::new_node(TYPE_A, SYS_METADATA, META_A1)).unwrap();
        let layer = b.commit().unwrap();

        let changed = changed_embedding_type_ids(&layer).unwrap();
        let a_id = layer.subject_id(TYPE_A).unwrap();
        assert_eq!(changed.iter().copied().collect::<Vec<_>>(), vec![a_id]);
    }

    #[test]
    fn shared_metadata_root_addition_reports_type() {
        // A gains metadata identical to B's existing config: only the
        // A→root edge is added; the root's subtree already existed.
        let store = open_sync_memory_store();
        let b = store.create_base_layer().unwrap();
        metadata_chain(&b, TYPE_B, META_B, EMB_B, "T {{t}}");
        let base = b.commit().unwrap();

        let b = base.open_write().unwrap();
        b.add_value_triple(ValueTriple::new_node(TYPE_A, SYS_METADATA, META_B)).unwrap();
        let layer = b.commit().unwrap();

        let changed = changed_embedding_type_ids(&layer).unwrap();
        let a_id = layer.subject_id(TYPE_A).unwrap();
        assert_eq!(changed.iter().copied().collect::<Vec<_>>(), vec![a_id]);
    }

    #[test]
    fn no_schema_delta_reports_nothing() {
        let base = base_layer();
        let b = base.open_write().unwrap();
        // Commit an empty child layer.
        let layer = b.commit().unwrap();
        let changed = changed_embedding_type_ids(&layer).unwrap();
        assert!(changed.is_empty());
    }
}
