use juniper;
use swipl::prelude::*;
use std::io::Write;

predicates! {
    #[module("$graphql")]
    semidet fn handle_request(context, _method_term, _request_term, response_stream_term) {
        let mut s: WritablePrologStream = response_stream_term.get_ex()?;
        context.try_or_die_generic(write!(s, "Status: 400\n\n"))?;
        Ok(())
    }
}

pub fn register() {
    register_handle_request();
}
