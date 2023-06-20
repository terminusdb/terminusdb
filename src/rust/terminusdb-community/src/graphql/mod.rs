use juniper::{
    executor::{execute_validated_query, get_operation},
    http::{GraphQLRequest, GraphQLResponse},
    DefaultScalarValue, Definition, EmptyMutation, EmptySubscription, ExecutionError, GraphQLError,
    InputValue, RootNode, Value,
};

use std::sync::Arc;
use std::{collections::HashMap, io::Read};
use swipl::prelude::*;

mod filter;
pub mod frame;
pub mod query;
mod sanitize;
pub mod schema;
mod top;

use self::{
    frame::{AllFrames, PreAllFrames},
    schema::{TerminusContext, TerminusTypeCollection, TerminusTypeCollectionInfo},
};

pub fn type_collection_from_term<'a, C: QueryableContextType>(
    context: &Context<'a, C>,
    frame_term: &Term,
) -> PrologResult<TerminusTypeCollectionInfo> {
    // TODO this should probably do more proper erroring
    let pre_frames: PreAllFrames = context
        .deserialize_from_term(frame_term)
        .expect("Unable to parse frames into rust struct");
    let frames: AllFrames = pre_frames.finalize();

    Ok(TerminusTypeCollectionInfo {
        allframes: Arc::new(frames),
    })
}

pub struct GraphQLExecutionContext<'a, C: QueryableContextType> {
    pub(crate) root_node: RootNode<
        'a,
        TerminusTypeCollection<'a, C>,
        EmptyMutation<TerminusContext<'a, C>>,
        EmptySubscription<TerminusContext<'a, C>>,
    >,
    context: TerminusContext<'a, C>,
}

impl<'a, C: QueryableContextType> GraphQLExecutionContext<'a, C> {
    pub fn new(
        type_collection: TerminusTypeCollectionInfo,
        context: TerminusContext<'a, C>,
    ) -> Self {
        let root_node = RootNode::new_with_info(
            TerminusTypeCollection::new(),
            EmptyMutation::<TerminusContext<'a, C>>::new(),
            EmptySubscription::<TerminusContext<'a, C>>::new(),
            type_collection,
            (),
            (),
        );

        Self { root_node, context }
    }

    pub fn new_from_context_terms(
        type_collection: TerminusTypeCollectionInfo,
        context: &'a Context<'a, C>,
        auth_term: &Term,
        system_term: &Term,
        meta_term: &Term,
        commit_term: &Term,
        transaction_term: &Term,
    ) -> PrologResult<Self> {
        let graphql_context = TerminusContext::new(
            context,
            auth_term,
            system_term,
            meta_term,
            commit_term,
            transaction_term,
        )?;
        Ok(Self::new(type_collection, graphql_context))
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

predicates! {
    #[module("$graphql")]
    semidet fn handle_request(context, _method_term, frame_term, system_term, meta_term, commit_term, transaction_term, auth_term, content_length_term, input_stream_term, response_term) {
        let mut input: ReadablePrologStream = input_stream_term.get_ex()?;
        let len = content_length_term.get_ex::<u64>()? as usize;
        let mut buf = vec![0;len];
        context.try_or_die_generic(input.read_exact(&mut buf))?;

        let request =
            match serde_json::from_slice::<GraphQLRequest>(&buf) {
                Ok(r) => r,
                Err(error) => return context.raise_exception(&term!{context: error(json_parse_error(#error.line() as u64, #error.column() as u64), _)}?)
            };

        let type_collection = type_collection_from_term(context, frame_term)?;
        let execution_context = GraphQLExecutionContext::new_from_context_terms(type_collection, context, auth_term, system_term, meta_term, commit_term, transaction_term)?;
        execution_context.execute_query(request,
                                        |response| {
                                            match serde_json::to_string(&response){
                                                Ok(r) => response_term.unify(r),
                                                Err(_) => return context.raise_exception(&term!{context: error(json_serialize_error, _)}?),
                                            }
                                        })
    }
}

pub fn register() {
    register_handle_request();
}
