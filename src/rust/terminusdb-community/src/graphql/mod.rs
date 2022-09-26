use juniper::{
    tests::fixtures::starwars::schema::{Database, Query},
    http::GraphQLRequest,
    GraphQLType, RootNode, EmptyMutation, EmptySubscription};

use std::sync::Arc;
use swipl::prelude::*;
use std::io::{Read, Write};

predicates! {
    #[module("$graphql")]
    semidet fn handle_request(context, _method_term, _request_term, content_length_term, input_stream_term, output_stream_term) {
        let mut input: ReadablePrologStream = input_stream_term.get_ex()?;
        let len = content_length_term.get_ex::<u64>()? as usize;
        let mut buf = vec![0;len];
        context.try_or_die_generic(input.read_exact(&mut buf))?;

        let request;
        match serde_json::from_slice::<GraphQLRequest>(&buf) {
            Ok(r) => {
                log_info!(context, "request: {:?}", r)?;
                request = r;
            },
            Err(error) => return context.raise_exception(&term!{context: error(json_parse_error(#error.line() as u64, #error.column() as u64), _)}?)
        }

        let root_node = RootNode::new(Query, EmptyMutation::<Database>::new(), EmptySubscription::<Database>::new());
        let graphql_context = Arc::new(Database::new());


        let response = request.execute_sync(&root_node, &graphql_context);

        log_info!(context, "graphql response: {:?}", response)?;

        let mut s: WritablePrologStream = output_stream_term.get_ex()?;
        context.try_or_die_generic(write!(s, "Status: 200\n\n"))?;
        context.try_or_die_generic(serde_json::to_writer(s, &response))?;

        Ok(())
    }
}

pub fn register() {
    register_handle_request();
}
