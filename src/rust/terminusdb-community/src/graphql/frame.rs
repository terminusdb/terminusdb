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
enum StructuralComplexFieldDefinition {
    Optional{#[serde(rename = "@class")]class: StructuralInnerFieldDefinition},
    Set{#[serde(rename = "@class")]class: StructuralInnerFieldDefinition},
    Array{#[serde(rename = "@class")]class: StructuralInnerFieldDefinition, #[serde(default = "default_dimensionality")] dimensions: usize},
    List{#[serde(rename = "@class")]class: StructuralInnerFieldDefinition},
    Cardinality{#[serde(rename = "@class")]class: StructuralInnerFieldDefinition, min: Option<usize>, max: Option<usize>},
    Enum{#[serde(rename = "@id")] id: String, values: Vec<String>}

}

#[derive(Deserialize, PartialEq, Debug)]
struct StructuralSubdocumentFieldDefinition {
    #[serde(rename = "@class")]
    class: String
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(untagged)]
enum StructuralInnerFieldDefinition {
    SimpleField(String),
    Enum(StructuralEnumFieldDefinition),
    SubdocumentField(StructuralSubdocumentFieldDefinition)
}

#[derive(Deserialize, PartialEq, Debug)]
struct StructuralEnumFieldDefinition {
    #[serde(rename = "@type")]
    typ: String,
    id: String,
    values: Vec<String>
}

impl From<StructuralInnerFieldDefinition> for SimpleFieldDefinition {
    fn from(f: StructuralInnerFieldDefinition) -> Self {
        match f {
            StructuralInnerFieldDefinition::SimpleField(s) => {
                if is_base_type(&s) {
                    SimpleFieldDefinition::BaseType(s)
                }
                else {
                    SimpleFieldDefinition::Document{typ: s, is_subdocument: false}
                }
            },
            StructuralInnerFieldDefinition::Enum(StructuralEnumFieldDefinition {id, values, ..}) =>
                SimpleFieldDefinition::Enum{name: id, values},
            StructuralInnerFieldDefinition::SubdocumentField(s) =>
                    SimpleFieldDefinition::Document{typ: s.class, is_subdocument: true},
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(untagged)]
enum StructuralFieldDefinition {
    SimpleField(String),
    ContainerField(StructuralComplexFieldDefinition),
    SubdocumentField(StructuralSubdocumentFieldDefinition)
}

#[derive(PartialEq, Debug)]
enum SimpleFieldDefinition {
    BaseType(String),
    Document{typ: String, is_subdocument: bool},
    Enum{name: String, values: Vec<String>},
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(from = "StructuralFieldDefinition")]
enum FieldDefinition {
    Required(SimpleFieldDefinition),
    Optional(SimpleFieldDefinition),
    Set(SimpleFieldDefinition),
    List(SimpleFieldDefinition),

    Array{field: SimpleFieldDefinition, dimensions: usize},
    Cardinality{field: SimpleFieldDefinition, min: Option<usize>, max: Option<usize>},
}

fn is_base_type(_s: &str) -> bool { true }

impl From<StructuralFieldDefinition> for FieldDefinition {
    fn from(f: StructuralFieldDefinition) -> Self {
        match f {
            StructuralFieldDefinition::SimpleField(s) => {
                if is_base_type(&s) {
                    FieldDefinition::Required(SimpleFieldDefinition::BaseType(s))
                }
                else {
                    FieldDefinition::Required(SimpleFieldDefinition::Document{typ: s, is_subdocument: false})
                }
            },

            StructuralFieldDefinition::ContainerField(StructuralComplexFieldDefinition::Optional{class}) => FieldDefinition::Optional(class.into()),
            StructuralFieldDefinition::ContainerField(StructuralComplexFieldDefinition::Set{class}) => FieldDefinition::Set(class.into()),
            StructuralFieldDefinition::ContainerField(StructuralComplexFieldDefinition::List{class}) => FieldDefinition::List(class.into()),
            StructuralFieldDefinition::ContainerField(StructuralComplexFieldDefinition::Array{class, dimensions}) => FieldDefinition::Array{field: class.into(), dimensions},
            StructuralFieldDefinition::ContainerField(StructuralComplexFieldDefinition::Cardinality{class, min, max}) => FieldDefinition::Cardinality{field: class.into(), min, max},
            StructuralFieldDefinition::ContainerField(StructuralComplexFieldDefinition::Enum{id, values}) => FieldDefinition::Required(SimpleFieldDefinition::Enum{name: id, values}),

            StructuralFieldDefinition::SubdocumentField(StructuralSubdocumentFieldDefinition{class}) => FieldDefinition::Required(SimpleFieldDefinition::Document{typ: class, is_subdocument: true})
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
struct KeyDefinition;    

#[derive(Deserialize, PartialEq, Debug)]
struct ClassDefinition {
    documentation: Option<ClassDocumentationDefinition>,
    key: Option<KeyDefinition>,
    #[serde(flatten)]
    fields: BTreeMap<String, StructuralFieldDefinition>
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
