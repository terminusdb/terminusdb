use std::io::{BufReader, Read};
use swipl::{prelude::*, term::ser::to_term_with_config};

predicates! {
    #[module("$util")]
    semidet fn json_read_dict_fast(context, stream_term, doc_term) {
        let stream: ReadablePrologStream = stream_term.get_ex()?;
        let mut json_deserializer = serde_json::Deserializer::from_reader(stream.decoding_reader()).into_iter::<serde_json::Value>();
        let json = match json_deserializer.next() {
            Some(result) => context.try_or_die_generic(result)?,
            None => return doc_term.unify(atom!("eof"))
        };
        let config = SerializerConfiguration::new().default_tag(atom!("json"));

        context.try_or_die_generic(to_term_with_config(context, doc_term, &json, config))
    }
}

pub fn register() {
    register_json_read_dict_fast();
}
