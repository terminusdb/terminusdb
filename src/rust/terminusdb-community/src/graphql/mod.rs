use juniper::{
    executor::{execute_validated_query, get_operation},
    http::{GraphQLRequest, GraphQLResponse},
    parser::parse_document_source,
    DefaultScalarValue, Definition, EmptyMutation, EmptySubscription, ExecutionError, GraphQLError,
    InputValue, RootNode, Value,
};
use terminusdb_store_prolog::terminus_store::Layer;

use lazy_static::lazy_static;
use lru::LruCache;
use regex::Regex;
use std::{collections::HashMap, io::Read};
use std::{
    num::NonZeroUsize,
    sync::{Arc, Mutex},
};

use swipl::prelude::*;

/// Post-processes GraphQL JSON or Handlebars text to convert high-precision
/// number markers to raw numbers.
/// Replaces "__TERMINUS_NUM__<digits>" markers with raw numbers to achieve
/// 20-digit precision. Handles both quoted markers (in JSON output, e.g.
/// `"__TERMINUS_NUM__100"`) and unquoted markers (in Handlebars template
/// output, e.g. `__TERMINUS_NUM__100`).
pub fn post_process_graphql_numbers(json_str: String) -> String {
    lazy_static! {
        static ref QUOTED_NUM_MARKER_RE: Regex =
            Regex::new(r#""__TERMINUS_NUM__([0-9.eE+-]+)""#).unwrap();
        static ref UNQUOTED_NUM_MARKER_RE: Regex =
            Regex::new(r"__TERMINUS_NUM__([0-9.eE+-]+)").unwrap();
    }
    // First replace quoted marker strings with unquoted numbers (JSON path)
    let intermediate = QUOTED_NUM_MARKER_RE.replace_all(&json_str, "$1").to_string();
    // Then replace any remaining unquoted markers (Handlebars text path)
    UNQUOTED_NUM_MARKER_RE.replace_all(&intermediate, "$1").to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quoted_marker_in_json() {
        let input = r#"{"category_code":"__TERMINUS_NUM__100","level":"__TERMINUS_NUM__3"}"#.to_string();
        let expected = r#"{"category_code":100,"level":3}"#;
        assert_eq!(post_process_graphql_numbers(input), expected);
    }

    #[test]
    fn test_unquoted_marker_in_handlebars_output() {
        let input = "Category code: __TERMINUS_NUM__100. Taxonomy level: __TERMINUS_NUM__3.".to_string();
        let expected = "Category code: 100. Taxonomy level: 3.";
        assert_eq!(post_process_graphql_numbers(input), expected);
    }

    #[test]
    fn test_mixed_quoted_and_unquoted_markers() {
        let input = r#"{"code":"__TERMINUS_NUM__42"} text: __TERMINUS_NUM__10"#.to_string();
        let expected = r#"{"code":42} text: 10"#;
        assert_eq!(post_process_graphql_numbers(input), expected);
    }

    #[test]
    fn test_decimal_marker() {
        let input = "Price: __TERMINUS_NUM__19.99.".to_string();
        let expected = "Price: 19.99.";
        assert_eq!(post_process_graphql_numbers(input), expected);
    }

    #[test]
    fn test_negative_marker() {
        let input = "Value: __TERMINUS_NUM__-42.".to_string();
        let expected = "Value: -42.";
        assert_eq!(post_process_graphql_numbers(input), expected);
    }

    #[test]
    fn test_no_markers_unchanged() {
        let input = "No markers here, just text.".to_string();
        assert_eq!(post_process_graphql_numbers(input), "No markers here, just text.");
    }

    #[test]
    fn test_handlebars_output_with_multiple_fields() {
        let input = "# Busbar Connectors.\n\nCategory code: __TERMINUS_NUM__100.\nTaxonomy level: __TERMINUS_NUM__3.\n\n  Sub-categories:\n  Industrial Busbar Connectors\n    (code __TERMINUS_NUM__386),\n  Commercial Busbar Connectors\n    (code __TERMINUS_NUM__387)\n  .".to_string();
        let expected = "# Busbar Connectors.\n\nCategory code: 100.\nTaxonomy level: 3.\n\n  Sub-categories:\n  Industrial Busbar Connectors\n    (code 386),\n  Commercial Busbar Connectors\n    (code 387)\n  .";
        assert_eq!(post_process_graphql_numbers(input), expected);
    }
}

mod filter;
pub mod frame;
mod main_graph;
mod mutation;
mod naming;
pub mod query;
mod sanitize;
pub mod schema;
mod subscription;
mod system;
mod top;

use crate::types::{transaction_instance_layer, transaction_schema_layer};

use self::{
    frame::{AllFrames, UncleanAllFrames},
    mutation::TerminusMutationRoot,
    schema::{
        DefaultTerminusTypeCollection, TerminusContext, TerminusTypeCollection,
        TerminusTypeCollectionInfo,
    },
    subscription::{SubscriptionResolveContext, TerminusSubscriptionRoot},
    system::{SystemData, SystemRoot},
};

type SubscriptionRootNode = RootNode<
    'static,
    TerminusTypeCollection<SubscriptionResolveContext>,
    EmptyMutation<SubscriptionResolveContext>,
    TerminusSubscriptionRoot<SubscriptionResolveContext>,
>;

pub fn type_collection_from_term<'a, C: QueryableContextType>(
    context: &Context<'a, C>,
    frame_term: &Term,
) -> PrologResult<TerminusTypeCollectionInfo> {
    // TODO this should probably do more proper erroring
    let pre_frames: UncleanAllFrames = context
        .deserialize_from_term(frame_term)
        .expect("Unable to parse frames into rust struct");
    let frames: AllFrames = pre_frames.finalize();

    Ok(TerminusTypeCollectionInfo {
        allframes: Arc::new(frames),
    })
}

pub struct GraphQLExecutionContext {
    pub(crate) root_node: RootNode<
        'static,
        DefaultTerminusTypeCollection,
        TerminusMutationRoot,
        TerminusSubscriptionRoot<TerminusContext<'static>>,
    >,
    pub(crate) context: TerminusContext<'static>,
}

impl GraphQLExecutionContext {
    pub fn new(
        type_collection: TerminusTypeCollectionInfo,
        context: TerminusContext<'static>,
    ) -> Self {
        let root_node = RootNode::new_with_info(
            DefaultTerminusTypeCollection::default(),
            TerminusMutationRoot,
            TerminusSubscriptionRoot::<TerminusContext<'static>>::new(),
            type_collection.clone(),
            (),
            type_collection,
        );

        Self { root_node, context }
    }

    pub unsafe fn new_from_context_terms<'a, C: QueryableContextType>(
        type_collection: TerminusTypeCollectionInfo,
        context: &'a Context<'_, C>,
        auth_term: &Term,
        system_term: &'a Term,
        meta_term: &Term,
        commit_term: &Term,
        transaction_term: &'a Term,
        author_term: &'a Term,
        message_term: &'a Term,
    ) -> PrologResult<Self> {
        let context: GenericQueryableContext<'a> = context.into_generic();
        let graphql_context: TerminusContext<'a> = TerminusContext::new(
            context,
            auth_term,
            system_term,
            meta_term,
            commit_term,
            transaction_term,
            author_term,
            message_term,
            type_collection.clone(),
        )?;
        let lifetime_erased_graphql_context: TerminusContext<'static> =
            unsafe { std::mem::transmute(graphql_context) };
        Ok(Self::new(type_collection, lifetime_erased_graphql_context))
    }

    pub fn prolog_context(&self) -> &GenericQueryableContext<'static> {
        &self.context.context
    }

    pub fn execute_query<T, F: Fn(&GraphQLResponse) -> T>(
        &self,
        request: GraphQLRequest,
        response_handler: F,
    ) -> T {
        let response = request.execute_sync(&self.root_node, &self.context);
        response_handler(&response)
    }

    pub fn execute_query_document<'b>(
        &self,
        request_document: &[Definition<'b, DefaultScalarValue>],
        variables: &HashMap<String, InputValue>,
    ) -> Result<
        (
            Value<DefaultScalarValue>,
            Vec<ExecutionError<DefaultScalarValue>>,
        ),
        GraphQLError<'b>,
    > {
        let operation = get_operation(request_document, None).unwrap();
        execute_validated_query(
            request_document,
            operation,
            &self.root_node,
            &variables,
            &self.context,
        )
    }
}

lazy_static! {
    static ref GRAPHQL_CONTEXT_CACHE: Arc<Mutex<LruCache<[u32; 5], TerminusTypeCollectionInfo>>> =
        Arc::new(Mutex::new(LruCache::new(NonZeroUsize::new(10).unwrap())));
}

lazy_static! {
    static ref SUBSCRIPTION_ROOT_NODE_CACHE: Arc<Mutex<LruCache<u64, Arc<SubscriptionRootNode>>>> =
        Arc::new(Mutex::new(LruCache::new(NonZeroUsize::new(10).unwrap())));
}

pub fn get_or_create_subscription_root_node(
    type_collection: &TerminusTypeCollectionInfo,
) -> Arc<SubscriptionRootNode> {
    let key = type_collection.allframes.cache_key();
    {
        let mut cache = SUBSCRIPTION_ROOT_NODE_CACHE.lock().unwrap();
        if let Some(node) = cache.get(&key) {
            return node.clone();
        }
    }
    let node = Arc::new(RootNode::new_with_info(
        TerminusTypeCollection::<SubscriptionResolveContext>::default(),
        EmptyMutation::<SubscriptionResolveContext>::new(),
        TerminusSubscriptionRoot::<SubscriptionResolveContext>::new(),
        type_collection.clone(),
        (),
        type_collection.clone(),
    ));
    let mut cache = SUBSCRIPTION_ROOT_NODE_CACHE.lock().unwrap();
    cache.put(key, node.clone());
    node
}

fn get_graphql_context_from_cache<C: QueryableContextType>(
    context: &Context<C>,
    transaction_term: &Term,
) -> PrologResult<Option<TerminusTypeCollectionInfo>> {
    if let Some(layer) = transaction_schema_layer(context, transaction_term)? {
        let mut cache = GRAPHQL_CONTEXT_CACHE.lock().unwrap();
        if let Some(context) = cache.get(&layer.name()) {
            return Ok(Some(context.clone()));
        }
    }

    Ok(None)
}

fn cache_graphql_context<C: QueryableContextType>(
    context: &Context<C>,
    transaction_term: &Term,
    graphql_context: TerminusTypeCollectionInfo,
) -> PrologResult<()> {
    if let Some(layer) = transaction_schema_layer(context, transaction_term)? {
        let mut cache = GRAPHQL_CONTEXT_CACHE.lock().unwrap();
        cache.put(layer.name(), graphql_context);
    }

    Ok(())
}

predicates! {
    #[module("$graphql")]
    semidet fn get_cached_graphql_context(context, transaction_term, graphql_context_term) {
        if let Some(graphql_context) = get_graphql_context_from_cache(context, transaction_term)? {
            graphql_context_term.unify(graphql_context)
        } else {
            fail()
        }
    }
    #[module("$graphql")]
    semidet fn get_graphql_context(context, transaction_term, frame_term, graphql_context_term) {
        let type_collection = type_collection_from_term(context, frame_term)?;
        cache_graphql_context(context, transaction_term, type_collection.clone())?;
        graphql_context_term.unify(type_collection)
    }
    #[module("$graphql")]
    semidet fn handle_request(context, method_term, graphql_context_term, system_term, meta_term, commit_term, transaction_term, auth_term, content_length_term, input_stream_term, response_term, is_error_term, author_term, message_term) {
        let mut input: ReadablePrologStream = input_stream_term.get_ex()?;
        let len = content_length_term.get_ex::<u64>()? as usize;
        let mut buf = vec![0;len];
        context.try_or_die_generic(input.read_exact(&mut buf))?;

        let request =
            match serde_json::from_slice::<GraphQLRequest>(&buf) {
                Ok(r) => r,
                Err(error) => return context.raise_exception(&term!{context: error(json_parse_error(#error.line() as u64, #error.column() as u64), _)}?)
            };

        let type_collection: TerminusTypeCollectionInfo = graphql_context_term.get_ex()?;

        // Defense in depth: if the method is get, reject mutation operations.
        // The Prolog layer should already enforce this via access control,
        // but this ensures mutations cannot bypass security at the engine level.
        let method: Atom = method_term.get_ex()?;
        if method.name() == "get" {
            if let Ok(json_val) = serde_json::from_slice::<serde_json::Value>(&buf) {
                if let Some(query_str) = json_val.get("query").and_then(|v| v.as_str()) {
                    let root_node = get_or_create_subscription_root_node(&type_collection);
                    let source = Box::new(query_str.to_string());
                    if let Ok(document) = parse_document_source::<DefaultScalarValue>(&source, &root_node.schema) {
                        for def in &document {
                            if let Definition::Operation(op) = def {
                                if op.item.operation_type == juniper::OperationType::Mutation {
                                    let error_response = r#"{"errors":[{"message":"Mutations are not allowed via GET. Use POST instead."}]}"#;
                                    is_error_term.unify(true)?;
                                    return response_term.unify(error_response.to_string());
                                }
                                break;
                            }
                        }
                    }
                }
            }
        }

        let execution_context = unsafe {GraphQLExecutionContext::new_from_context_terms(type_collection, context, auth_term, system_term, meta_term, commit_term, transaction_term, author_term, message_term)? };
        execution_context.execute_query(request,
                                        |response: &GraphQLResponse| {
                                            let errored = response.inner_ref().as_ref()
                                                .map(|(_, errors)|!errors.is_empty())
                                                .unwrap_or(false);
                                            is_error_term.unify(errored)?;
                                            match serde_json::to_string(&response){
                                                Ok(r) => {
                                                    let processed = post_process_graphql_numbers(r);
                                                    response_term.unify(processed)
                                                },
                                                Err(_) => return context.raise_exception(&term!{context: error(json_serialize_error, _)}?),
                                            }
                                        })
    }

    /// get_operation_type(+GraphqlContext, +QueryString, -OperationType)
    ///
    /// Parses a GraphQL query string using the juniper parser and returns
    /// the operation type as an atom: `query`, `mutation`, or `subscription`.
    /// If parsing fails or the operation type cannot be determined, defaults
    /// to `mutation` for safety (write access required).
    #[module("$graphql")]
    semidet fn get_operation_type(_context, graphql_context_term, query_string_term, operation_type_term) {
        let type_collection: TerminusTypeCollectionInfo = graphql_context_term.get_ex()?;
        let query: String = query_string_term.get_ex()?;

        let root_node = get_or_create_subscription_root_node(&type_collection);
        let source = Box::new(query);
        let document = match parse_document_source::<DefaultScalarValue>(&source, &root_node.schema) {
            Ok(doc) => doc,
            Err(_) => {
                // Parse failure: default to mutation for safety
                return operation_type_term.unify(Atom::new("mutation"));
            }
        };

        // Find the first operation definition and return its type.
        // If no operation is found, default to mutation.
        for def in &document {
            if let Definition::Operation(op) = def {
                let op_type = match op.item.operation_type {
                    juniper::OperationType::Query => "query",
                    juniper::OperationType::Mutation => "mutation",
                    juniper::OperationType::Subscription => "subscription",
                };
                return operation_type_term.unify(Atom::new(op_type));
            }
        }

        // No operation found: default to mutation
        operation_type_term.unify(Atom::new("mutation"))
    }

    /// get_operation_type_from_body(+GraphqlContext, +BodyString, -OperationType)
    ///
    /// Parses a GraphQL JSON request body using serde_json (fast) to extract
    /// the query string, then uses the juniper parser to determine the
    /// operation type. Returns `query`, `mutation`, or `subscription` as an
    /// atom. If JSON parsing fails, the query field is missing, or operation
    /// type cannot be determined, defaults to `mutation` for safety.
    #[module("$graphql")]
    semidet fn get_operation_type_from_body(_context, graphql_context_term, body_string_term, operation_type_term) {
        let type_collection: TerminusTypeCollectionInfo = graphql_context_term.get_ex()?;
        let body: String = body_string_term.get_ex()?;

        // Parse JSON body with serde_json — much faster than Prolog json_read_dict.
        let query_string: String = match serde_json::from_str::<serde_json::Value>(&body) {
            Ok(json) => {
                match json.get("query").and_then(|v| v.as_str()) {
                    Some(q) => q.to_string(),
                    None => return operation_type_term.unify(Atom::new("mutation")),
                }
            }
            Err(_) => return operation_type_term.unify(Atom::new("mutation")),
        };

        let root_node = get_or_create_subscription_root_node(&type_collection);
        let source = Box::new(query_string);
        let document = match parse_document_source::<DefaultScalarValue>(&source, &root_node.schema) {
            Ok(doc) => doc,
            Err(_) => return operation_type_term.unify(Atom::new("mutation")),
        };

        for def in &document {
            if let Definition::Operation(op) = def {
                let op_type = match op.item.operation_type {
                    juniper::OperationType::Query => "query",
                    juniper::OperationType::Mutation => "mutation",
                    juniper::OperationType::Subscription => "subscription",
                };
                return operation_type_term.unify(Atom::new(op_type));
            }
        }

        operation_type_term.unify(Atom::new("mutation"))
    }

    #[module("$graphql")]
    semidet fn handle_system_request(context, _method_term, system_term, auth_term, content_length_term, input_stream_term, response_term) {
        let mut input: ReadablePrologStream = input_stream_term.get_ex()?;
        let len = content_length_term.get_ex::<u64>()? as usize;
        let mut buf = vec![0;len];
        context.try_or_die_generic(input.read_exact(&mut buf))?;

        let request =
            match serde_json::from_slice::<GraphQLRequest>(&buf) {
                Ok(r) => r,
                Err(error) => return context.raise_exception(&term!{context: error(json_parse_error(#error.line() as u64, #error.column() as u64), _)}?)
            };

        let user: Atom = auth_term.get_ex()?;
        let system = transaction_instance_layer(context, system_term)?.unwrap();

        let root_node = RootNode::new_with_info(SystemRoot::default(),
                                                EmptyMutation::new(),
                                                EmptySubscription::new(),
                                                (),
                                                (),
                                                ());
        let system_data = SystemData { user, system };
        let response = request.execute_sync(&root_node, &system_data);
        match serde_json::to_string(&response){
            Ok(r) => {
                // Post-process to convert high-precision markers to JSON numbers
                let processed = post_process_graphql_numbers(r);
                response_term.unify(processed)
            },
            Err(_) => return context.raise_exception(&term!{context: error(json_serialize_error, _)}?),
        }
    }

    #[module("$graphql")]
    semidet fn parse_subscription_query(context, graphql_context_term, query_string_term, parsed_term) {
        let type_collection: TerminusTypeCollectionInfo = graphql_context_term.get_ex()?;
        let query: String = query_string_term.get_ex()?;
        match subscription::parse_subscription_query(&query, &type_collection) {
            Ok(parsed) => {
                let dict = DictBuilder::new()
                    .tag("parsed")
                    .entry("field_name", parsed.field_name.clone())
                    .entry("class_name", Atom::new(&parsed.class_name))
                    .entry("operation", Atom::new(&parsed.operation))
                    .entry("filter_canonical_json", parsed.filter_canonical_json.clone())
                    .entry("selection_set", parsed.selection_set.clone())
                    .entry("selection_set_hash", Atom::new(&parsed.selection_set_hash))
                    .entry("selection_set_graphql", parsed.selection_set_graphql.clone())
                    .entry("include_children", parsed.include_children);
                parsed_term.unify(dict)
            }
            Err(e) => context.raise_exception(&term!{context: error(graphql_subscription_parse_error(#e), _)}?)
        }
    }

    /// resolve_subscription_event(+Transaction, +GraphqlContext, +QueryString,
    ///                             +ChangeType, +CommitId, +Timestamp, +Datetime,
    ///                             -ResponseJson)
    ///
    /// Executes a GraphQL query (the Query-root resolution query built by
    /// resolve_event_through_juniper/11 in Prolog) against the transaction's
    /// instance and schema layers, returning the JSON response string.
    ///
    /// The transaction term is either a fresh open_descriptor transaction
    /// (for non-deleted events) or the Validation_Object (for deleted events,
    /// whose instance_objects.read layer contains the pre-commit state).
    /// The graphql_context term is a TerminusTypeCollectionInfo created by
    /// get_graphql_context (same as the regular GraphQL HTTP endpoint).
    ///
    /// ChangeType, CommitId, Timestamp, and Datetime carry subscription event
    /// metadata that is made available to the GraphQL resolver as the nested
    /// `_commit { _change_type _id _timestamp _datetime }` object. The fields
    /// are only included in the response if the user explicitly requests
    /// `_commit` in the selection set.
    #[module("$graphql")]
    semidet fn resolve_subscription_event(context, transaction_term, graphql_context_term, query_string_term, change_type_term, commit_id_term, timestamp_term, datetime_term, response_term) {
        let type_collection: TerminusTypeCollectionInfo = graphql_context_term.get_ex()?;
        let query: String = query_string_term.get_ex()?;
        let change_type: String = change_type_term.get_ex()?;
        let commit_id: String = commit_id_term.get_ex()?;
        let timestamp: f64 = timestamp_term.get_ex()?;
        let datetime: String = datetime_term.get_ex()?;

        // Extract schema and instance layers from the transaction term.
        // For deleted events, the transaction term is the Validation_Object,
        // whose instance_objects.read layer is the post-commit state.
        // The parent of that layer is the pre-commit state where the
        // document still exists.
        let schema_layer = match transaction_schema_layer(context, transaction_term)? {
            Some(layer) => layer,
            None => return context.raise_exception(&term!{context: error(no_schema_layer_in_transaction, _)}?),
        };
        let instance_layer = transaction_instance_layer(context, transaction_term)?;

        // For deleted documents, use the parent (pre-commit) layer as the
        // instance so that run_filter_query finds the document and
        // TerminusType::resolve_field resolves fields from pre-commit state.
        let (instance_layer, parent_layer) = if change_type == "deleted" {
            let parent = instance_layer.as_ref().and_then(|l| l.parent().ok().flatten());
            (parent.clone(), parent)
        } else {
            (instance_layer, None)
        };

        // Build the SubscriptionResolveContext with event metadata so the
        // resolver can return the _CommitMetadata nested object fields.
        let mut resolve_context = subscription::SubscriptionResolveContext::with_metadata(
            schema_layer,
            instance_layer,
            type_collection.clone(),
            change_type,
            commit_id,
            timestamp,
            datetime,
        );
        resolve_context.parent_instance = parent_layer;

        // Get the cached subscription root node (schema + subscription types).
        let root_node = get_or_create_subscription_root_node(&type_collection);

        // Parse the query string as a GraphQLRequest and execute it.
        let request: GraphQLRequest = match serde_json::from_str(&query) {
            Ok(r) => r,
            Err(error) => return context.raise_exception(&term!{context: error(graphql_resolve_parse_error(#error.line() as u64, #error.column() as u64), _)}?),
        };

        let response = request.execute_sync(&root_node, &resolve_context);
        match serde_json::to_string(&response) {
            Ok(r) => {
                let processed = post_process_graphql_numbers(r);
                response_term.unify(processed)
            }
            Err(_) => context.raise_exception(&term!{context: error(json_serialize_error, _)}?),
        }
    }

    /// resolve_change_set_event(+Transaction, +GraphqlContext, +SelectionSet,
    ///                           +CommitId, +Timestamp, +Datetime,
    ///                           +IncludeChildren, -ResponseJson)
    ///
    /// Executes a _ChangeSet GraphQL query entirely in Rust. This predicate:
    /// 1. Calls changed_document_ids() to get all changed documents
    /// 2. Groups them by class + operation (e.g. "Product_added")
    /// 3. Gets the parent layer for deleted document resolution
    /// 4. Builds SubscriptionResolveContext with change_set_ids
    /// 5. Wraps the selection set in { _ChangeSet { ... } }
    /// 6. Executes via Juniper and returns the JSON response
    ///
    /// Prolog only needs to call this and send the result via SSE.
    #[module("$graphql")]
    semidet fn resolve_change_set_event(context, transaction_term, graphql_context_term, selection_set_term, commit_id_term, timestamp_term, datetime_term, include_children_term, response_term) {
        let type_collection: TerminusTypeCollectionInfo = graphql_context_term.get_ex()?;
        let selection_set: String = selection_set_term.get_ex()?;
        let commit_id: String = commit_id_term.get_ex()?;
        let timestamp: f64 = timestamp_term.get_ex()?;
        let datetime: String = datetime_term.get_ex()?;
        let include_children: bool = include_children_term.get_ex()?;

        // Extract schema and instance layers from the transaction term.
        let schema_layer = match transaction_schema_layer(context, transaction_term)? {
            Some(layer) => layer,
            None => return context.raise_exception(&term!{context: error(no_schema_layer_in_transaction, _)}?),
        };
        let instance_layer = match transaction_instance_layer(context, transaction_term)? {
            Some(layer) => layer,
            None => return context.raise_exception(&term!{context: error(no_instance_layer_in_transaction, _)}?),
        };

        // Get parent layer for deleted document resolution.
        let parent_layer = instance_layer.parent().ok().flatten();

        // Collect all changed documents using the pure Rust function.
        let changes = context.try_or_die(
            crate::changes::changed_document_ids(&schema_layer, &instance_layer),
        )?;

        // Get rdf:type predicate ID for looking up class of Changed documents.
        let rdf_type_id = instance_layer.predicate_id(crate::consts::RDF_TYPE);

        // Group changes by "{Class}_{operation}" → Vec<document_iri>
        let mut change_set_ids: HashMap<String, Vec<String>> = HashMap::new();
        // Map document IRI → actual class GraphQL name for include_children filtering.
        let mut doc_class_map: HashMap<String, String> = HashMap::new();

        for (id, change_type) in changes {
            // Determine the type_id and which layer to use for IRI lookups
            let (type_id, op_name, lookup_layer) = match &change_type {
                crate::changes::ChangeType::Added(tid) => (*tid, "added", &instance_layer),
                crate::changes::ChangeType::Deleted(tid) => {
                    // For deleted docs, use parent layer for type IRI lookup
                    let layer = parent_layer.as_ref().unwrap_or(&instance_layer);
                    (*tid, "deleted", layer)
                }
                crate::changes::ChangeType::Changed => {
                    // For changed docs, look up rdf:type in the current instance
                    match rdf_type_id.and_then(|rt| instance_layer.single_triple_sp(id, rt)) {
                        Some(t) => (t.object, "changed", &instance_layer),
                        None => continue, // skip if no rdf:type found
                    }
                }
            };

            // Get the class IRI string from the lookup layer
            let class_iri = match lookup_layer.id_object_node(type_id) {
                Some(iri) => iri,
                None => continue,
            };

            // Convert class IRI to GraphQL class name using AllFrames
            let graphql_name = match type_collection.allframes.iri_to_graphql_name_opt(
                &self::frame::IriName(class_iri),
            ) {
                Some(name) => name,
                None => continue,
            };

            // Get the document IRI string
            let doc_iri = match lookup_layer.id_subject(id) {
                Some(iri) => iri,
                None => continue,
            };

            // Record the actual class name for this document IRI.
            // Used by include_children filtering in ChangeSet::resolve_field.
            doc_class_map.insert(doc_iri.clone(), graphql_name.0.to_string());

            // Group under the document's own class and all superclasses.
            // This ensures a Dog document appears in both Dog_added and
            // Animal_added when Dog inherits from Animal.
            for class_name in type_collection.allframes.superclasses_of(&graphql_name) {
                let key = format!("{}_{}", class_name.0, op_name);
                change_set_ids.entry(key).or_default().push(doc_iri.clone());
            }
        }

        // Build the SubscriptionResolveContext with change_set_ids and parent layer.
        let resolve_context = SubscriptionResolveContext::with_change_set_metadata(
            schema_layer,
            Some(instance_layer),
            type_collection.clone(),
            "commit".to_string(),
            commit_id,
            timestamp,
            datetime,
            change_set_ids,
            parent_layer,
            doc_class_map,
            include_children,
        );

        // Get the cached subscription root node (schema + subscription types).
        let root_node = get_or_create_subscription_root_node(&type_collection);

        // Wrap the selection set in a query: { _ChangeSet { <selection set> } }
        // Use serde_json to properly escape the selection set string.
        let query = format!("{{ _ChangeSet {{ {} }} }}", selection_set);
        let query_json = serde_json::json!({"query": query}).to_string();

        let request: GraphQLRequest = match serde_json::from_str(&query_json) {
            Ok(r) => r,
            Err(error) => return context.raise_exception(&term!{context: error(graphql_resolve_parse_error(#error.line() as u64, #error.column() as u64), _)}?),
        };

        let response = request.execute_sync(&root_node, &resolve_context);
        let response_str = match serde_json::to_string(&response) {
            Ok(r) => r,
            Err(_) => return context.raise_exception(&term!{context: error(json_serialize_error, _)}?),
        };
        let processed = post_process_graphql_numbers(response_str);
        response_term.unify(processed)
    }
}

pub fn register() {
    register_get_cached_graphql_context();
    register_get_graphql_context();
    register_handle_request();
    register_get_operation_type();
    register_get_operation_type_from_body();
    register_handle_system_request();
    register_parse_subscription_query();
    register_resolve_subscription_event();
    register_resolve_change_set_event();
}
