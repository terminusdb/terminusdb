use serde::{self, Deserialize, de::{Visitor, MapAccess}};
use swipl::prelude::Atom;
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

#[derive(Deserialize, PartialEq, Debug)]
struct ClassDocumentationDefinition {
}

/*
#[derive(PartialEq, Debug)]
enum FieldContentDefinition {
    Direct(String),
    Subdocument(String),
    Indirect(TypeDefinition)
}

#[derive(Deserialize, PartialEq, Debug)]
struct ArrayDefinition {
    field: FieldContentDefinition,
    dimensions: usize
}
*/

#[inline]
fn default_dimensionality() -> usize { 1 }

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
enum ComplexFieldDefinition {
    Optional{#[serde(rename = "@class")]class: InnerFieldDefinition},
    Set{#[serde(rename = "@class")]class: InnerFieldDefinition},
    Array{#[serde(rename = "@class")]class: InnerFieldDefinition, #[serde(default = "default_dimensionality")] dimensions: usize},
    List{#[serde(rename = "@class")]class: InnerFieldDefinition},
    Cardinality{#[serde(rename = "@class")]class: InnerFieldDefinition, min: Option<usize>, max: Option<usize>}
}

#[derive(Deserialize, PartialEq, Debug)]
struct SubdocumentFieldDefinition {
    #[serde(rename = "@class")]
    class: String
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(untagged)]
enum InnerFieldDefinition {
    SimpleField(String),
    SubdocumentField(SubdocumentFieldDefinition)
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(untagged)]
enum FieldDefinition {
    SimpleField(String),
    ContainerField(ComplexFieldDefinition),
    SubdocumentField(SubdocumentFieldDefinition)
}

/*
#[derive(PartialEq, Debug)]
enum FieldDefinition {
    Required(FieldContentDefinition),
    Optional(FieldContentDefinition),
    Set(FieldContentDefinition),
    Array{field: FieldContentDefinition, dimensions: usize},
    List(FieldContentDefinition),
    Cardinality{field: FieldContentDefinition, min: Option<usize>, max: Option<usize>}
}
*/

/*

impl<'de> Deserialize<'de> for FieldDefinition {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> {
        let visitor = FieldDefinitionVisitor;

        deserializer.deserialize_any(visitor)
    }
}

struct FieldDefinitionVisitor;

impl<'de> Visitor<'de> for FieldDefinitionVisitor {
    type Value = FieldDefinition;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "a field definition")
    }

    fn visit_string<E>(self, s: String) -> Result<FieldDefinition, E> {
        Ok(FieldDefinition::Required(FieldContentDefinition::Direct(s)))
    }
    fn visit_str<E>(self, s: &str) -> Result<FieldDefinition, E> {
        Ok(FieldDefinition::Required(FieldContentDefinition::Direct(s.to_string())))
    }

    fn visit_map<A:MapAccess<'de>>(self, mut map: A) -> Result<FieldDefinition, E> {
        while let Some(key) = map.next_key::<Atom>()? {

        }
    }
}
*/

#[derive(Deserialize, PartialEq, Debug)]
struct KeyDefinition;    

#[derive(Deserialize, PartialEq, Debug)]
struct ClassDefinition {
    documentation: Option<ClassDocumentationDefinition>,
    key: Option<KeyDefinition>,
    #[serde(flatten)]
    fields: BTreeMap<String, FieldDefinition>
}
#[derive(Deserialize, PartialEq, Debug)]
struct TaggedUnionDefinition;

#[derive(Deserialize, PartialEq, Debug)]
struct EnumDocumentationDefinition {
}


#[derive(Deserialize, PartialEq, Debug)]
struct EnumDefinition {
    documentation: Option<EnumDocumentationDefinition>,
    #[serde(rename = "@value")]
    value: Vec<String>
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag="@type")]
enum TypeDefinition {
    Class(ClassDefinition),
    TaggedUnion(TaggedUnionDefinition),
    Enum(EnumDefinition)
}

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

    #[test]
    fn deserialize_field_definition() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
_{'@type': "Array", '@class': _{'@class': 'asdfads', '@subdocument': []}}
"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let typedef: FieldDefinition = context.deserialize_from_term(&term).unwrap();

        panic!("{:?}", typedef);
    }
}
