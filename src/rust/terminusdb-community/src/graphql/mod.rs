use juniper::{
    executor::{execute_validated_query, get_operation},
    http::{GraphQLRequest, GraphQLResponse},
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
mod system;
mod top;

use crate::types::{transaction_instance_layer, transaction_schema_layer};

use self::{
    frame::{AllFrames, UncleanAllFrames},
    mutation::TerminusMutationRoot,
    schema::{TerminusContext, TerminusTypeCollection, TerminusTypeCollectionInfo},
    system::{SystemData, SystemRoot},
};

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
        TerminusTypeCollection,
        TerminusMutationRoot,
        EmptySubscription<TerminusContext<'static>>,
    >,
    pub(crate) context: TerminusContext<'static>,
}

impl GraphQLExecutionContext {
    pub fn new(
        type_collection: TerminusTypeCollectionInfo,
        context: TerminusContext<'static>,
    ) -> Self {
        let root_node = RootNode::new_with_info(
            TerminusTypeCollection,
            TerminusMutationRoot,
            EmptySubscription::<TerminusContext<'static>>::new(),
            type_collection,
            (),
            (),
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
        compress_ids_term: &Term,
    ) -> PrologResult<Self> {
        let context: GenericQueryableContext<'a> = context.into_generic();
        let compress_ids: bool = compress_ids_term.get_ex::<Atom>()? == atom!("true");
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
            compress_ids,
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
    semidet fn handle_request(context, _method_term, graphql_context_term, system_term, meta_term, commit_term, transaction_term, auth_term, content_length_term, input_stream_term, response_term, is_error_term, author_term, message_term, compress_ids_term) {
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
        let execution_context = unsafe {GraphQLExecutionContext::new_from_context_terms(type_collection, context, auth_term, system_term, meta_term, commit_term, transaction_term, author_term, message_term, compress_ids_term)? };
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
}

pub fn register() {
    register_get_cached_graphql_context();
    register_get_graphql_context();
    register_handle_request();
    register_handle_system_request();
}
