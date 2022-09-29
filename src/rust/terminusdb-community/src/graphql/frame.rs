use serde::{self, Deserialize};
use std::collections::BTreeMap;

#[derive(Deserialize, PartialEq, Debug)]
struct Prefixes {
    #[serde(rename = "@base")]
    base: String,
    #[serde(rename = "@schema")]
    schema: String,
    #[serde(flatten)]
    extra_prefixes: BTreeMap<String, String>
}

#[derive(Deserialize)]
struct TypeDefinition;

#[derive(Deserialize)]
struct Frame {
    context: Prefixes,
    #[serde(flatten)]
    stuff: BTreeMap<String, TypeDefinition>
}

#[cfg(test)]
mod tests {
    use super::*;
    use swipl::prelude::*;
    #[test]
    fn deserialize_context() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
_{'@base': "http://some_base/",
  '@schema': "http://some_schema#",
  a: "http://extra_prefix/a",
  b: "http://extra_prefix/b"
}
            "#;
        let term = context.term_from_string(term).unwrap();

        let prefixes: Prefixes = context.deserialize_from_term(&term).unwrap();

        assert_eq!(Prefixes {
            base: "http://some_base/".to_string(),
            schema: "http://some_schema#".to_string(),
            extra_prefixes: BTreeMap::from([("a".to_string(), "http://extra_prefix/a".to_string()),
                                            ("b".to_string(), "http://extra_prefix/b".to_string())])
        },
                   prefixes);

    }
}
