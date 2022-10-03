use juniper::{
    http::{graphiql::graphiql_source, GraphQLRequest},
    EmptyMutation,
    EmptySubscription,
    RootNode,
};

use std::io::{Read, Write};
use std::sync::Arc;
use swipl::prelude::*;

mod top;
mod frame;
mod schema;

use top::*;

use self::{frame::AllFrames, schema::{TerminusType, TerminusTypeCollection, TerminusContext}};

predicates! {
    #[module("$graphql")]
    semidet fn handle_request(context, _method_term, frame_term, system_term, meta_term, commit_term, branch_term, transaction_term, auth_term, content_length_term, input_stream_term, output_stream_term) {
        let mut input: ReadablePrologStream = input_stream_term.get_ex()?;
        let len = content_length_term.get_ex::<u64>()? as usize;
        let mut buf = vec![0;len];
        context.try_or_die_generic(input.read_exact(&mut buf))?;

        let request;
        match serde_json::from_slice::<GraphQLRequest>(&buf) {
            Ok(r) => {
                log_debug!(context, "request: {:?}", r)?;
                request = r;
            },
            Err(error) => return context.raise_exception(&term!{context: error(json_parse_error(#error.line() as u64, #error.column() as u64), _)}?)
        }

        let frames: AllFrames = context.deserialize_from_term(&frame_term).expect("aaa");
        log_info!(context, "parsed frames: {:?}", frames)?;

        let root_node = RootNode::new_with_info(TerminusTypeCollection::new(), EmptyMutation::<TerminusContext<'a, C>>::new(), EmptySubscription::<TerminusContext<'a,C>>::new(), Arc::new(frames), (), ());

        let graphql_context = TerminusContext::new(context, transaction_term)?;
        //let graphql_context = Info::new(context, system_term, meta_term, commit_term, branch_term, transaction_term, auth_term)?;

        let response = request.execute_sync(&root_node, &graphql_context);

        log_debug!(context, "graphql response: {:?}", response)?;

        let mut s: WritablePrologStream = output_stream_term.get_ex()?;
        context.try_or_die_generic(write!(s, "Access-Control-Allow-Credentials: true\nAccess-Control-Allow-Origin: http://127.0.0.1:6363\nStatus: 200\n\n"))?;
        context.try_or_die_generic(serde_json::to_writer(s, &response))?;

        Ok(())
    }

    #[module("$graphql")]
    semidet fn graphiql(context, path_term, output_term, port_term) {
        let port: u64 = port_term.get_ex()?;
        let path: String = path_term.get_ex()?;
        let full_url: String = format!("http://localhost:{}/api/graphql/{}", port, path);
        let source = graphiql_source(&full_url, None);

        let mut stream: WritablePrologStream = output_term.get_ex()?;
        context.try_or_die_generic(write!(stream, "Status: 200\n\n"))?;
        context.try_or_die_generic(stream.write_all(source.as_bytes()))?;

        Ok(())
    }
}

pub fn register() {
    register_handle_request();
    register_graphiql();
}
