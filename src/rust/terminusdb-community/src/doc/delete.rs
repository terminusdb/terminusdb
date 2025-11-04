use terminusdb_store_prolog::terminus_store::layer::LayerBuilder;

use super::*;
pub fn delete_id_document<L: Layer + Clone>(
    context: &DocumentContext<L>,
    builder: &mut dyn LayerBuilder,
    id: u64,
) {
    let mut visit_next = vec![id];
    let mut visited: HashSet<u64> = HashSet::new();
    let layer = context.layer();
    let rdf_type = context.rdf.type_();
    loop {
        let id = visit_next.pop();
        if id.is_none() {
            return;
        }

        let id = id.unwrap();
        visited.insert(id);

        for triple in layer.triples_s(id) {
            builder.remove_id_triple(triple);
            // Check if the object node should be recursively deleted
            if let Some(true) = layer.id_object_is_node(triple.object) {
                if rdf_type.is_some() && !visited.contains(&triple.object) {
                    let type_triple = layer.single_triple_sp(triple.object, rdf_type.unwrap());
                    if let Some(type_triple) = type_triple {
                        if !context.document_types.contains(&type_triple.object)
                            && !context.value_hashes.contains(&type_triple.object)
                        {
                            // Only apply reference counting for content-addressed JSON objects
                            // (shared Cons lists and JSON objects with SHA1 hashes)
                            let object_iri = layer.id_subject(triple.object).unwrap_or_default();
                            let is_content_addressed = object_iri.starts_with("terminusdb:///json/Cons/SHA1/") 
                                || object_iri.starts_with("terminusdb:///json/JSON/SHA1/");
                            let is_json_type = Some(type_triple.object) == context.sys.json() 
                                || Some(type_triple.object) == context.rdf.list();
                            
                            let should_recurse = if is_content_addressed && is_json_type {
                                // Content-addressed JSON objects: check for other references
                                !has_other_link(context, triple.object, id, &visited)
                            } else {
                                // Non-content-addressed objects: recurse unconditionally
                                true
                            };
                            
                            if should_recurse {
                                visit_next.push(triple.object);
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum DeleteError {
    #[error("document not found: {0}")]
    DocumentNotFound(String),
    #[error("stored document is not a json: {0}")]
    StoredDocumentIsNotAJson(String),
}

impl IntoPrologException for DeleteError {
    fn into_prolog_exception<'a, T: QueryableContextType>(
        self,
        context: &'a Context<'_, T>,
    ) -> PrologResult<Term<'a>> {
        let term = match self {
            DeleteError::DocumentNotFound(s) => term! {context: error(document_not_found(#s), _)}?,
            DeleteError::StoredDocumentIsNotAJson(s) => {
                term! {context: error(stored_document_is_not_a_json(#s), _)}?
            }
        };

        context.raise_exception(&term)
    }
}

pub fn delete_json_id_document<L: Layer + Clone>(
    context: &DocumentContext<L>,
    builder: &mut dyn LayerBuilder,
    id: u64,
) -> Result<(), DeleteError> {
    let mut visit_next = vec![id];
    let mut visited: HashSet<u64> = HashSet::new();
    let layer = context.layer();
    let rdf_type = context.rdf.type_();
    if rdf_type.is_none() {
        // no types means that the given id can't be a json
        return Err(DeleteError::DocumentNotFound(
            layer.id_subject(id).expect("id was not in dictionary"),
        ));
    }
    let rdf_type = rdf_type.unwrap();
    let rdf_list = context.rdf.list();
    let sys_json_document = context.sys.json_document();
    let sys_json = context.sys.json();
    if !json_document_exists(layer, id, rdf_type, rdf_list, sys_json_document, sys_json)? {
        return Err(DeleteError::DocumentNotFound(
            layer.id_subject(id).expect("id was not in dictionary"),
        ));
    }
    loop {
        let id = visit_next.pop();
        if id.is_none() {
            return Ok(());
        }
        let id = id.unwrap();
        visited.insert(id);

        for triple in layer.triples_s(id) {
            builder.remove_id_triple(triple);
            // Check if the object node should be recursively deleted
            if let Some(true) = layer.id_object_is_node(triple.object) {
                if !visited.contains(&triple.object) {
                    let json_exists = json_document_exists(
                        layer,
                        triple.object,
                        rdf_type,
                        rdf_list,
                        sys_json_document,
                        sys_json,
                    ).unwrap_or(false);
                    
                    if json_exists {
                        // Only apply reference counting for content-addressed JSON objects
                        // (shared Cons lists and JSON objects with SHA1 hashes)
                        let object_iri = layer.id_subject(triple.object).unwrap_or_default();
                        let is_content_addressed = object_iri.starts_with("terminusdb:///json/Cons/SHA1/") 
                            || object_iri.starts_with("terminusdb:///json/JSON/SHA1/");
                        
                        let should_recurse = if is_content_addressed {
                            // Content-addressed JSON objects: check for other references
                            !has_other_link(context, triple.object, id, &visited)
                        } else {
                            // Non-content-addressed JSON objects: recurse unconditionally
                            true
                        };
                        
                        if should_recurse {
                            visit_next.push(triple.object);
                        }
                    }
                }
            }
        }
    }
}

// does a bunch of checking in addition to returning whether the document exists.
fn json_document_exists<L: Layer>(
    layer: &L,
    id: u64,
    rdf_type: u64,
    rdf_list: Option<u64>,
    sys_json_document: Option<u64>,
    sys_json: Option<u64>,
) -> Result<bool, DeleteError> {
    let type_triple = layer.single_triple_sp(id, rdf_type);
    if type_triple.is_none() {
        return Ok(false);
    }
    let typ = Some(type_triple.unwrap().object);
    if typ != sys_json && typ != sys_json_document && typ != rdf_list {
        // none of the valid json types found. this must be an error.
        return Err(DeleteError::StoredDocumentIsNotAJson(
            layer.id_subject(id).expect("id was not in dictionary"),
        ));
    }

    Ok(true)
}

fn has_other_link<L: Layer + Clone>(
    context: &DocumentContext<L>,
    id: u64,
    expected_link: u64,
    visited: &HashSet<u64>,
) -> bool {
    let layer = context.layer();
    for triple in layer.triples_o(id) {
        // Ignore references from documents we're currently deleting
        if !visited.contains(&triple.subject) && triple.subject != expected_link {
            return true;
        }
    }
    false
}

pub fn unlink_id_document<L: Layer + Clone>(
    context: &DocumentContext<L>,
    builder: &mut dyn LayerBuilder,
    id: u64,
) {
    let layer = context.layer();
    let rdf_first = context.rdf.first();

    for triple in layer.triples_o(id) {
        // unlinking something that is pointed at from a list is currently not supported.
        if Some(triple.predicate) != rdf_first {
            builder.remove_id_triple(triple);
        }
    }
}

pub fn delete_all_triples<L: Layer + Clone>(layer: &L, builder: &mut dyn LayerBuilder) {
    // note: this could be faster by directly constructing the
    // negative layer, since we already have all triples in order
    // anyway.
    for triple in layer.triples() {
        builder.remove_id_triple(triple);
    }
}

/// Delete multiple JSON documents with pre-computed root IDs from Prolog.
/// This properly handles layer stacking by using root IDs that were found
/// using Prolog's id_triple/4 (which respects layer removals), rather than
/// Rust's triples_o() (which returns all triples from all layers).
pub fn delete_multiple_documents_with_roots<L: Layer + Clone>(
    context: &DocumentContext<L>,
    builder: &mut dyn LayerBuilder,
    document_ids: &HashSet<u64>,
    all_document_roots: &HashSet<u64>,
) {
    let layer = context.layer();
    let rdf_type = context.rdf.type_();
    if rdf_type.is_none() {
        return;
    }
    let rdf_type = rdf_type.unwrap();
    let rdf_list = context.rdf.list();
    let sys_json_document = context.sys.json_document();
    let sys_json = context.sys.json();

    // Pass 1: Traverse all document roots to build node_to_documents mapping
    let mut node_to_documents: HashMap<u64, HashSet<u64>> = HashMap::new();

    for &root_id in all_document_roots {
        let mut visit_next = vec![root_id];
        let mut visited: HashSet<u64> = HashSet::new();

        while let Some(current_id) = visit_next.pop() {
            if visited.contains(&current_id) {
                continue;
            }
            visited.insert(current_id);

            // Track that this root uses this node
            node_to_documents
                .entry(current_id)
                .or_insert_with(HashSet::new)
                .insert(root_id);

            // Traverse outgoing edges
            for triple in layer.triples_s(current_id) {
                if let Some(true) = layer.id_object_is_node(triple.object) {
                    if !visited.contains(&triple.object) {
                        let json_exists = json_document_exists(
                            layer,
                            triple.object,
                            rdf_type,
                            rdf_list,
                            sys_json_document,
                            sys_json,
                        )
                        .unwrap_or(false);

                        if json_exists {
                            visit_next.push(triple.object);
                        }
                    }
                }
            }
        }
    }

    // Pass 2: Delete documents and their content if no other live documents reference them
    for &doc_id in document_ids {
        let mut visit_next = vec![doc_id];
        let mut visited: HashSet<u64> = HashSet::new();

        while let Some(current_id) = visit_next.pop() {
            if visited.contains(&current_id) {
                continue;
            }
            visited.insert(current_id);

            // Remove all triples with this subject
            for triple in layer.triples_s(current_id) {
                builder.remove_id_triple(triple);

                // Check if the object node should be recursively deleted
                if let Some(true) = layer.id_object_is_node(triple.object) {
                    if !visited.contains(&triple.object) {
                        let json_exists = json_document_exists(
                            layer,
                            triple.object,
                            rdf_type,
                            rdf_list,
                            sys_json_document,
                            sys_json,
                        )
                        .unwrap_or(false);

                        if json_exists {
                            // Check if this node is content-addressed (shared)
                            let object_iri = layer.id_subject(triple.object).unwrap_or_default();
                            let is_content_addressed =
                                object_iri.starts_with("terminusdb:///json/Cons/SHA1/")
                                    || object_iri.starts_with("terminusdb:///json/JSON/SHA1/");

                            let should_recurse = if is_content_addressed {
                                // Content-addressed: check if any non-deleted document still uses it
                                if let Some(using_docs) = node_to_documents.get(&triple.object) {
                                    // Should recurse only if ALL documents using this node are being deleted
                                    using_docs.iter().all(|d| document_ids.contains(d))
                                } else {
                                    // No mapping means safe to delete
                                    true
                                }
                            } else {
                                // Non-content-addressed: always recurse
                                true
                            };

                            if should_recurse {
                                visit_next.push(triple.object);
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn delete_all_documents_by_type<L: Layer + Clone>(
    context: &DocumentContext<L>,
    builder: &mut dyn LayerBuilder,
    type_name: &str,
    unlink: bool,
) {
    let layer = context.layer();
    let rdf_type = context.rdf.type_();
    if rdf_type.is_none() {
        return;
    }
    let rdf_type = rdf_type.unwrap();

    for type_id in context.get_subtypes_for(type_name) {
        for triple in layer.triples_o(type_id) {
            if triple.predicate != rdf_type {
                continue;
            }

            delete_id_document(context, builder, triple.subject);
            if unlink {
                unlink_id_document(context, builder, triple.subject);
            }
        }
    }
}

pub fn delete_document_by_iri<L: Layer + Clone>(
    context: &DocumentContext<L>,
    builder: &mut dyn LayerBuilder,
    iri: &str,
    unlink: bool,
) -> Result<(), DeleteError> {
    let layer = context.layer();
    let id = layer.subject_id(iri);
    let rdf_type = context.rdf.type_();
    if id.is_none() || rdf_type.is_none() {
        return Err(DeleteError::DocumentNotFound(iri.to_owned()));
    }

    let id = id.unwrap();
    let rdf_type = rdf_type.unwrap();

    let document_type = layer.single_triple_sp(id, rdf_type).map(|t| t.object);
    if document_type.is_none() {
        return Err(DeleteError::DocumentNotFound(iri.to_owned()));
    }
    let json_type = context.sys.json_document();
    if document_type == json_type {
        delete_json_id_document(context, builder, id)?
    } else {
        delete_id_document(context, builder, id);
    }

    if unlink {
        unlink_id_document(context, builder, id);
    }

    Ok(())
}

pub fn delete_json_document_by_iri<L: Layer + Clone>(
    context: &DocumentContext<L>,
    builder: &mut dyn LayerBuilder,
    iri: &str,
    unlink: bool,
) -> Result<(), DeleteError> {
    let layer = context.layer();
    let id = layer.subject_id(iri);
    let rdf_type = context.rdf.type_();
    if id.is_none() || rdf_type.is_none() {
        return Err(DeleteError::DocumentNotFound(iri.to_owned()));
    }

    let id = id.unwrap();

    delete_json_id_document(context, builder, id)?;

    if unlink {
        unlink_id_document(context, builder, id);
    }

    Ok(())
}

predicates! {
    #[module("$doc")]
    semidet fn delete_document(context, document_context_term, transaction_term, iri_term, unlink_term) {
        let document_context: DocumentContextBlob = document_context_term.get_ex()?;
        let iri: PrologText = iri_term.get_ex()?;
        if document_context.layer.is_none() {
            // no layer means nothing to delete.
            return context.try_or_die(Err(DeleteError::DocumentNotFound(iri.to_string())));
        }
        let builder = transaction_instance_builder(context, transaction_term)?;
        if builder.is_none() {
            return context.raise_exception(&term! {context: error(builder_not_initialized, _)}?);
        }
        let builder = builder.unwrap();
        let unlink: bool = unlink_term.get_ex()?;
        context.try_or_die(
            context.try_or_die(builder.with_builder(|builder| {
                delete_document_by_iri(&document_context, &mut **builder, &iri, unlink)
        }))?)
    }
    #[module("$doc")]
    semidet fn delete_json_document(context, document_context_term, transaction_term, iri_term, unlink_term) {
        let document_context: DocumentContextBlob = document_context_term.get_ex()?;
        let iri: PrologText = iri_term.get_ex()?;
        if document_context.layer.is_none() {
            // no layer means nothing to delete.
            return context.try_or_die(Err(DeleteError::DocumentNotFound(iri.to_string())));
        }
        let builder = transaction_instance_builder(context, transaction_term)?;
        if builder.is_none() {
            return context.raise_exception(&term! {context: error(builder_not_initialized, _)}?);
        }
        let builder = builder.unwrap();
        let unlink: bool = unlink_term.get_ex()?;
        context.try_or_die(
            context.try_or_die(builder.with_builder(|builder| {
                delete_json_document_by_iri(&document_context, &mut **builder, &iri, unlink)
        }))?)
    }

    #[module("$doc")]
    semidet fn delete_documents_by_type(context, document_context_term, transaction_term, type_name_term, unlink_term) {
        let document_context: DocumentContextBlob = document_context_term.get_ex()?;
        if document_context.layer.is_none() {
            // no layer means nothing to delete.
            return Ok(())
        }
        let builder = transaction_instance_builder(context, transaction_term)?;
        if builder.is_none() {
            return context.raise_exception(&term! {context: error(builder_not_initialized, _)}?);
        }
        let builder = builder.unwrap();
        let type_name: PrologText = type_name_term.get_ex()?;
        let unlink: bool = unlink_term.get_ex()?;
        context.try_or_die(builder.with_builder(|builder| {
            delete_all_documents_by_type(&document_context, &mut **builder, &type_name, unlink)
        }))
    }

    #[module("$doc")]
    semidet fn delete_all(context, transaction_term) {
        let layer = transaction_instance_layer(context, transaction_term)?;
        if layer.is_none() {
            // no layer means nothing to delete.
            return Ok(())
        }
        let layer = layer.unwrap();
        let builder = transaction_instance_builder(context, transaction_term)?;
        if builder.is_none() {
            return context.raise_exception(&term! {context: error(builder_not_initialized, _)}?);
        }
        let builder = builder.unwrap();
            context.try_or_die(builder.with_builder(|builder| {
                delete_all_triples(&layer, &mut **builder)
        }))
    }

    /// Delete multiple JSON documents using pre-computed root IDs from Prolog.
    /// This properly handles layer stacking across multiple transactions.
    /// 
    /// Parameters:
    /// - document_context_term: Document context blob
    /// - transaction_term: Transaction object
    /// - iris_term: List of document IRIs to delete
    /// - all_root_ids_term: List of ALL document root IDs (from Prolog's id_triple/4)
    #[module("$doc")]
    semidet fn delete_documents_bulk_with_roots(
        context, 
        document_context_term, 
        transaction_term, 
        iris_term,
        all_root_ids_term
    ) {
        let document_context: DocumentContextBlob = document_context_term.get_ex()?;
        if document_context.layer.is_none() {
            // no layer means nothing to delete.
            return Ok(())
        }
        
        let builder = transaction_instance_builder(context, transaction_term)?;
        if builder.is_none() {
            return context.raise_exception(&term! {context: error(builder_not_initialized, _)}?);
        }
        let builder = builder.unwrap();
        
        // Parse list of IRIs to delete
        let iris_list: Vec<PrologText> = iris_term.get_ex()?;
        
        // Parse list of all root IDs (from Prolog)
        let all_root_ids_vec: Vec<u64> = all_root_ids_term.get_ex()?;
        let all_root_ids: HashSet<u64> = all_root_ids_vec.into_iter().collect();
        
        // Convert IRIs to internal IDs
        let layer = document_context.layer();
        let mut document_ids = HashSet::new();
        for iri in iris_list {
            if let Some(id) = layer.subject_id(&iri) {
                document_ids.insert(id);
            }
        }
        
        // Perform bulk deletion with reference counting
        context.try_or_die(builder.with_builder(|builder| {
            delete_multiple_documents_with_roots(
                &document_context, 
                &mut **builder, 
                &document_ids,
                &all_root_ids
            )
        }))
    }
}

pub fn register() {
    register_delete_document();
    register_delete_json_document();
    register_delete_documents_by_type();
    register_delete_all();
    register_delete_documents_bulk_with_roots();
}
