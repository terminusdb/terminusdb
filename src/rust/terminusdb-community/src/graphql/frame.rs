use bimap::BiMap;
use serde::{self, Deserialize};
use std::collections::{BTreeMap, HashMap};

use super::sanitize::graphql_sanitize;

#[derive(Deserialize, PartialEq, Debug, Clone)]
pub struct SchemaDocumentation {
    #[serde(rename = "@title")]
    pub title: Option<String>,
    #[serde(rename = "@description")]
    pub description: Option<String>,
    #[serde(rename = "@authors")]
    pub authors: Option<Vec<String>>,
}

#[derive(Deserialize, PartialEq, Debug, Clone)]
pub struct Prefixes {
    #[serde(rename = "@type")]
    pub kind: String,
    #[serde(rename = "@base")]
    pub base: String,
    #[serde(rename = "@schema")]
    pub schema: String,
    #[serde(rename = "@documentation")]
    pub documentation: Option<SchemaDocumentation>,
    #[serde(flatten)]
    pub extra_prefixes: BTreeMap<String, String>,
}

impl Prefixes {
    pub fn expand_schema(&self, s: &str) -> String {
        // this is dumb but will work for now
        format!("{}{}", self.schema, s)
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum StructuralPropertyDocumentationRecord {
    OnlyPropertyLabel(String),
    PropertyCommentLabel {
        #[serde(rename = "@label")]
        label: Option<String>,
        #[serde(rename = "@comment")]
        comment: Option<String>,
    },
}

#[derive(Deserialize, PartialEq, Debug, Clone)]
#[serde(from = "StructuralPropertyDocumentationRecord")]
pub struct PropertyDocumentationRecord {
    pub label: Option<String>,
    pub comment: Option<String>,
}

impl From<StructuralPropertyDocumentationRecord> for PropertyDocumentationRecord {
    fn from(f: StructuralPropertyDocumentationRecord) -> Self {
        match f {
            StructuralPropertyDocumentationRecord::OnlyPropertyLabel(s) => {
                PropertyDocumentationRecord {
                    label: Some(s),
                    comment: None,
                }
            }
            StructuralPropertyDocumentationRecord::PropertyCommentLabel { label, comment } => {
                PropertyDocumentationRecord { label, comment }
            }
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct PropertyDocumentation {
    #[serde(flatten)]
    pub records: BTreeMap<String, PropertyDocumentationRecord>,
}

impl PropertyDocumentation {
    pub fn sanitize(&self) -> PropertyDocumentation {
        let mut records = BTreeMap::new();
        for (s, pdr) in self.records.iter() {
            records.insert(graphql_sanitize(s), pdr.clone());
        }
        PropertyDocumentation { records }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct ClassDocumentationDefinition {
    #[serde(rename = "@label")]
    pub label: Option<String>,
    #[serde(rename = "@comment")]
    pub comment: Option<String>,
    #[serde(rename = "@properties")]
    pub properties: Option<PropertyDocumentation>,
}

impl ClassDocumentationDefinition {
    pub fn sanitize(&self) -> ClassDocumentationDefinition {
        ClassDocumentationDefinition {
            label: self.label.clone(),
            comment: self.comment.clone(),
            properties: self.properties.as_ref().map(|pd| pd.sanitize()),
        }
    }
}

#[inline]
fn default_dimensionality() -> usize {
    1
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
enum ComplexFieldDefinition {
    Optional {
        #[serde(rename = "@class")]
        class: String,
    },
    Set {
        #[serde(rename = "@class")]
        class: String,
    },
    Array {
        #[serde(rename = "@class")]
        class: String,
        #[serde(default = "default_dimensionality")]
        dimensions: usize,
    },
    List {
        #[serde(rename = "@class")]
        class: String,
    },
    Cardinality {
        #[serde(rename = "@class")]
        class: String,
        min: Option<usize>,
        max: Option<usize>,
    },
}

impl From<StructuralFieldDefinition> for FieldDefinition {
    fn from(f: StructuralFieldDefinition) -> Self {
        match f {
            StructuralFieldDefinition::SimpleField(s) => FieldDefinition::Required(s),
            StructuralFieldDefinition::ContainerField(c) => match c {
                ComplexFieldDefinition::Optional { class } => FieldDefinition::Optional(class),
                ComplexFieldDefinition::Set { class } => FieldDefinition::Set(class),
                ComplexFieldDefinition::List { class } => FieldDefinition::List(class),
                ComplexFieldDefinition::Array { class, dimensions } => {
                    FieldDefinition::Array { class, dimensions }
                }
                ComplexFieldDefinition::Cardinality { class, min, max } => {
                    FieldDefinition::Cardinality { class, min, max }
                }
            },
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(untagged)]
enum StructuralFieldDefinition {
    SimpleField(String),
    ContainerField(ComplexFieldDefinition),
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(from = "StructuralFieldDefinition")]
pub enum FieldDefinition {
    Required(String),
    Optional(String),
    Set(String),
    List(String),
    Array {
        class: String,
        dimensions: usize,
    },
    Cardinality {
        class: String,
        min: Option<usize>,
        max: Option<usize>,
    },
}

#[derive(PartialEq)]
pub enum FieldKind {
    Required,
    Optional,
    Set,
    List,
    Array,
    Cardinality,
}

pub fn is_base_type(s: &str) -> bool {
    // TODO this is not good enough
    s.starts_with("xsd:") || s.starts_with("sys:")
}

pub fn sanitize_class(s: &String) -> String {
    if is_base_type(s) {
        s.to_string()
    } else {
        graphql_sanitize(s)
    }
}

impl FieldDefinition {
    pub fn range(&self) -> &String {
        match self {
            Self::Required(c) => c,
            Self::Optional(c) => c,
            Self::Set(c) => c,
            Self::List(c) => c,
            Self::Array { class, .. } => class,
            Self::Cardinality { class, .. } => class,
        }
    }

    pub fn base_type(&self) -> Option<&str> {
        let range = self.range();
        if is_base_type(range) {
            Some(&range[4..])
        } else {
            None
        }
    }

    pub fn document_type<'a>(&'a self, allframes: &'a AllFrames) -> Option<&'a str> {
        if self.base_type().is_some() {
            None
        } else {
            let range = self.range();
            allframes.document_type(range)
        }
    }

    pub fn enum_type<'a>(&'a self, allframes: &'a AllFrames) -> Option<&'a str> {
        if self.base_type().is_some() {
            None
        } else {
            let range = self.range();
            allframes.enum_type(range)
        }
    }

    pub fn kind(&self) -> FieldKind {
        match self {
            Self::Required(_) => FieldKind::Required,
            Self::Optional(_) => FieldKind::Optional,
            Self::Set(_) => FieldKind::Set,
            Self::List(_) => FieldKind::List,
            Self::Array { .. } => FieldKind::Array,
            Self::Cardinality { .. } => FieldKind::Cardinality,
        }
    }

    pub fn sanitize(&self) -> FieldDefinition {
        match self {
            Self::Required(c) => Self::Required(sanitize_class(c)),
            Self::Optional(c) => Self::Optional(sanitize_class(c)),
            Self::Set(c) => Self::Set(sanitize_class(c)),
            Self::List(c) => Self::List(sanitize_class(c)),
            Self::Array { class, dimensions } => Self::Array {
                class: sanitize_class(class),
                dimensions: dimensions.clone(),
            },
            Self::Cardinality { class, min, max } => Self::Cardinality {
                class: sanitize_class(class),
                min: min.clone(),
                max: max.clone(),
            },
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
pub enum KeyDefinition {
    Random,
    Lexical {
        #[serde(rename = "@fields")]
        fields: Vec<String>,
    },
    Hash {
        #[serde(rename = "@fields")]
        fields: Vec<String>,
    },
    ValueHash,
}

impl KeyDefinition {
    pub fn sanitize(&self) -> KeyDefinition {
        match self {
            KeyDefinition::Random => KeyDefinition::Random,
            KeyDefinition::Lexical { fields } => {
                let fields = fields.iter().map(|f| graphql_sanitize(f)).collect();
                KeyDefinition::Lexical { fields }
            }
            KeyDefinition::Hash { fields } => {
                let fields = fields.iter().map(|f| graphql_sanitize(f)).collect();
                KeyDefinition::Hash { fields }
            }
            KeyDefinition::ValueHash => KeyDefinition::ValueHash,
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct OneOf {
    #[serde(flatten)]
    pub choices: BTreeMap<String, FieldDefinition>,
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct ClassDefinition {
    #[serde(rename = "@documentation")]
    pub documentation: Option<ClassDocumentationDefinition>,
    #[serde(rename = "@key")]
    pub key: Option<KeyDefinition>,
    #[serde(rename = "@subdocument")]
    pub is_subdocument: Option<Vec<()>>,
    #[serde(rename = "@abstract")]
    pub is_abstract: Option<Vec<()>>,
    #[serde(rename = "@oneOf")]
    pub one_of: Option<Vec<OneOf>>,
    #[serde(flatten)]
    pub fields: BTreeMap<String, FieldDefinition>,
    #[serde(skip_serializing)]
    pub field_renaming: Option<HashMap<String, String>>,
}

impl ClassDefinition {
    pub fn sanitize(&self) -> ClassDefinition {
        let mut field_map: HashMap<String, String> = HashMap::new();
        let mut fields: BTreeMap<String, _> = BTreeMap::new();
        for (field, fd) in self.fields.iter() {
            let sanitized_field = graphql_sanitize(field);
            field_map.insert(sanitized_field.clone(), field.clone())
                .and_then::<(), _>(|dup| panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires field names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]'. Please rename your fields to remove the following duplicate: {dup:?}"));
            fields.insert(sanitized_field, fd.sanitize());
        }
        let one_of = self.one_of.as_ref().map(|o| {
            o.into_iter()
                .map(|c| {
                    let mut choices = BTreeMap::new();
                    for (field,fd) in c.choices.iter() {
                        let sanitized_field = graphql_sanitize(field);
                        field_map.insert(sanitized_field.clone(), field.clone())
                            .and_then::<(), _>(|dup| panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires field names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]'. Please rename your fields to remove the following duplicate: {dup:?}"));
                        choices.insert(sanitized_field.clone(), fd.sanitize());
                    }
                    OneOf { choices }
                })
                .collect()
        });
        let documentation = self.documentation.as_ref().map(|d| d.sanitize());
        let key = self.key.as_ref().map(|k| k.sanitize());
        let mut field_renaming: HashMap<String, String> = HashMap::new();
        for (s, t) in field_map.iter() {
            field_renaming.insert(s.to_string(), t.to_string());
        }
        ClassDefinition {
            documentation,
            key,
            is_subdocument: self.is_subdocument.clone(),
            is_abstract: self.is_abstract.clone(),
            one_of,
            fields,
            field_renaming: Some(field_renaming),
        }
    }

    pub fn resolve_field(&self, field_name: &String) -> &FieldDefinition {
        let maybe_field: Option<&FieldDefinition> = self.one_of.as_ref().and_then(|oneofs| {
            oneofs.iter().find_map(|o| {
                o.choices
                    .iter()
                    .find_map(|(s, f)| if s == field_name { Some(f) } else { None })
            })
        });
        if maybe_field.is_some() {
            maybe_field.unwrap()
        } else {
            &self.fields[field_name]
        }
    }

    pub fn fields(&self) -> Vec<(&String, &FieldDefinition)> {
        let mut one_ofs: Vec<(&String, &FieldDefinition)> = self
            .one_of
            .as_ref()
            .map(|s| {
                s.into_iter()
                    .map(move |c| c.choices.iter().collect::<Vec<_>>())
                    .flatten()
            })
            .into_iter()
            .flatten()
            .collect();
        one_ofs.dedup();
        let mut fields: Vec<(&String, &FieldDefinition)> = self
            .fields
            .iter()
            .collect::<Vec<(&String, &FieldDefinition)>>();
        fields.append(&mut one_ofs);
        fields
    }

    pub fn fully_qualified_property_name(&self, prefixes: &Prefixes, property: &String) -> String {
        self.field_renaming
            .as_ref()
            .map(|map| {
                let db_name = map.get(property).expect(&format!(
                    "The fully qualified property name for {property:?} *should* exist"
                ));
                prefixes.expand_schema(db_name)
            })
            .expect("The field renaming was never built!")
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct EnumDocumentationDefinition {
    #[serde(rename = "@label")]
    pub label: Option<String>,
    #[serde(rename = "@comment")]
    pub comment: Option<String>,
    #[serde(rename = "@values")]
    pub values: Option<PropertyDocumentation>,
}

impl EnumDocumentationDefinition {
    pub fn sanitize(&self) -> EnumDocumentationDefinition {
        EnumDocumentationDefinition {
            label: self.label.clone(),
            comment: self.comment.clone(),
            values: self.values.as_ref().map(|pd| pd.sanitize()),
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct EnumDefinition {
    //#[serde(rename = "@type")]
    //pub kind: String,
    #[serde(rename = "@documentation")]
    pub documentation: Option<EnumDocumentationDefinition>,
    #[serde(rename = "@values")]
    pub values: Vec<String>,
    #[serde(skip_serializing, skip_deserializing)]
    pub values_renaming: Option<BiMap<String, String>>,
}

impl EnumDefinition {
    pub fn sanitize(&self) -> EnumDefinition {
        let mut values_renaming: BiMap<String, String> = BiMap::new();
        let values = self.values.iter().map(|v| {
            let sanitized = graphql_sanitize(v);
            let res = values_renaming.insert(sanitized.to_string(), v.to_string());
            let overwritten = res.did_overwrite();
            if overwritten {
                panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires enum value names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]'. Please rename your enum values to remove the following duplicate: {res:?}")
            };
            sanitized
        }).collect();
        let documentation = self.documentation.as_ref().map(|ed| ed.sanitize());
        EnumDefinition {
            documentation,
            values,
            values_renaming: Some(values_renaming),
        }
    }

    pub fn value_name(&self, value: &str) -> String {
        self.values_renaming
            .as_ref()
            .map(|map| {
                urlencoding::encode(
                    map.get_by_left(value)
                        .expect(&format!("The value name for {value:?} *should* exist")),
                )
                .into_owned()
            })
            .expect("No values renaming was generated")
    }

    pub fn name_value(&self, name: &str) -> &str {
        self.values_renaming
            .as_ref()
            .map(|map| {
                map.get_by_right(
                    &*urlencoding::decode(name)
                        .expect("Somehow encoding went terribly wrong in saving"),
                )
                .expect(&format!(
                    "The value for {name:?} *should* exist as it is in your database"
                ))
            })
            .expect("No values renaming was generated")
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
pub enum TypeDefinition {
    Class(ClassDefinition),
    Enum(EnumDefinition),
}

impl TypeDefinition {
    pub fn is_document_type(&self) -> bool {
        match self {
            Self::Class(_) => true,
            _ => false,
        }
    }
    pub fn is_enum_type(&self) -> bool {
        match self {
            Self::Enum(_) => true,
            _ => false,
        }
    }

    pub(crate) fn as_class_definition(&self) -> &ClassDefinition {
        match self {
            Self::Class(c) => &c,
            _ => panic!("tried to unwrap non-class definition as class definition"),
        }
    }

    pub(crate) fn as_enum_definition(&self) -> &EnumDefinition {
        match self {
            Self::Enum(e) => &e,
            _ => panic!("tried to unwrap non-enum definition as enum definition"),
        }
    }
}

impl FieldKind {
    pub fn is_collection(&self) -> bool {
        match self {
            Self::Set => true,
            Self::Array => true,
            Self::List => true,
            Self::Cardinality => true,
            _ => false,
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct AllFrames {
    #[serde(rename = "@context")]
    pub context: Prefixes,
    #[serde(flatten)]
    pub frames: BTreeMap<String, TypeDefinition>,
    #[serde(skip_serializing)]
    pub class_renaming: Option<HashMap<String, String>>,
}

impl AllFrames {
    pub fn sanitize(&self) -> AllFrames {
        let mut class_renaming: HashMap<String, String> = HashMap::new();
        let mut frames: BTreeMap<String, TypeDefinition> = BTreeMap::new();
        for (class_name, typedef) in self.frames.iter() {
            let sanitized_class = graphql_sanitize(class_name);
            class_renaming.insert(sanitized_class.clone(), class_name.clone())
                .and_then::<(),_>(|dup|
                                  panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires class names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]'. Please rename your classes to remove the following duplicate: {dup:?}"));
            let new_typedef = match typedef {
                TypeDefinition::Class(cd) => TypeDefinition::Class(cd.sanitize()),
                TypeDefinition::Enum(ed) => TypeDefinition::Enum(ed.sanitize()),
            };
            frames.insert(sanitized_class.clone(), new_typedef);
        }
        AllFrames {
            context: self.context.clone(),
            frames,
            class_renaming: Some(class_renaming),
        }
    }

    pub fn document_type<'a>(&self, s: &'a str) -> Option<&'a str> {
        if self.frames[s].is_document_type() {
            Some(s)
        } else {
            None
        }
    }

    pub fn enum_type<'a>(&self, s: &'a str) -> Option<&'a str> {
        if self.frames[s].is_enum_type() {
            Some(s)
        } else {
            None
        }
    }

    pub fn fully_qualified_class_name(&self, class_name: &str) -> String {
        self.class_renaming
            .as_ref()
            .map(|map| {
                let db_name = map
                    .get(class_name)
                    .expect("This fully qualified class name *should* exist");
                self.context.expand_schema(db_name)
            })
            .expect("No renaming was built")
    }

    pub fn fully_qualified_enum_value(&self, enum_type: &str, value: &str) -> String {
        let enum_type_expanded = self.fully_qualified_class_name(enum_type);
        let enum_type_definition = self.frames[enum_type].as_enum_definition();
        let value = enum_type_definition.value_name(value);
        let expanded_object_node = format!("{}/{}", enum_type_expanded, value);

        expanded_object_node
    }
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
  '@type' : 'Context',
  a: "http://extra_prefix/a",
  b: "http://extra_prefix/b"
}
            "#;
        let term = context.term_from_string(term).unwrap();

        let prefixes: Prefixes = context.deserialize_from_term(&term).unwrap();

        assert_eq!(
            Prefixes {
                kind: "Context".to_string(),
                documentation: None,
                base: "http://some_base/".to_string(),
                schema: "http://some_schema#".to_string(),
                extra_prefixes: BTreeMap::from([
                    ("a".to_string(), "http://extra_prefix/a".to_string()),
                    ("b".to_string(), "http://extra_prefix/b".to_string())
                ])
            },
            prefixes
        );
    }

    #[test]
    fn deserialize_field_definition() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
_{'@type': "Array", '@class': 'Something'}
"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let typedef: FieldDefinition = context.deserialize_from_term(&term).unwrap();

        assert_eq!(
            FieldDefinition::Array {
                class: "Something".to_string(),
                dimensions: 1
            },
            typedef
        );
    }

    #[test]
    fn deserialize_key_definition() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
_{'@type': "Lexical", '@fields': ["foo", "bar"]}
"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let typedef: KeyDefinition = context.deserialize_from_term(&term).unwrap();

        assert_eq!(
            KeyDefinition::Lexical {
                fields: vec!["foo".to_string(), "bar".to_string()]
            },
            typedef
        );
    }

    #[test]
    fn deserialize_oneof() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"json{'@oneOf':[json{a:'xsd:string',b:'xsd:integer'},
                           json{c:'xsd:string',d:'xsd:integer'}],
                           '@type':'Class'}"#;

        let term = unwrap_result(&context, context.term_from_string(term));
        let typedef: TypeDefinition = context.deserialize_from_term(&term).unwrap();
        assert_eq!(
            TypeDefinition::Class(ClassDefinition {
                field_renaming: None,
                documentation: None,
                key: None,
                is_subdocument: None,
                is_abstract: None,
                one_of: Some(vec![
                    OneOf {
                        choices: BTreeMap::from([
                            (
                                "a".to_string(),
                                FieldDefinition::Required("xsd:string".to_string())
                            ),
                            (
                                "b".to_string(),
                                FieldDefinition::Required("xsd:integer".to_string())
                            )
                        ])
                    },
                    OneOf {
                        choices: BTreeMap::from([
                            (
                                "c".to_string(),
                                FieldDefinition::Required("xsd:string".to_string())
                            ),
                            (
                                "d".to_string(),
                                FieldDefinition::Required("xsd:integer".to_string())
                            )
                        ])
                    }
                ]),
                fields: BTreeMap::from([]),
            }),
            typedef
        );
    }

    #[test]
    fn deserialize_all_frames() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
json{'@context':_27018{'@base':"terminusdb:///data/",
                       '@schema':"terminusdb:///schema#",
                       '@type':'Context'},
      'Test':json{'@type':'Class',bar:'xsd:string',foo:'xsd:integer'}}
"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let _frames: AllFrames = context.deserialize_from_term(&term).unwrap();

        // TODO actually test something here
    }

    #[test]
    fn deserialize_action_frame() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
json{ '@documentation':json{ '@comment':"The exhaustive list of actions which are available to roles."
					  },
		     '@type':'Enum',
		     '@values':[ create_database,
				 delete_database,
				 class_frame,
				 clone,
				 fetch,
				 push,
				 branch,
				 rebase,
				 instance_read_access,
				 instance_write_access,
				 schema_read_access,
				 schema_write_access,
				 meta_read_access,
				 meta_write_access,
				 commit_read_access,
				 commit_write_access,
				 manage_capabilities
			       ]
		   }

"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let typedef: TypeDefinition = dbg!(context.deserialize_from_term(&term)).unwrap();

        assert_eq!(
            TypeDefinition::Enum(EnumDefinition {
                values_renaming: None,
                documentation: Some(EnumDocumentationDefinition {
                    label: None,
                    values: None,
                    comment: Some(
                        "The exhaustive list of actions which are available to roles.".to_string()
                    ),
                }),
                values: vec![
                    "create_database".to_string(),
                    "delete_database".to_string(),
                    "class_frame".to_string(),
                    "clone".to_string(),
                    "fetch".to_string(),
                    "push".to_string(),
                    "branch".to_string(),
                    "rebase".to_string(),
                    "instance_read_access".to_string(),
                    "instance_write_access".to_string(),
                    "schema_read_access".to_string(),
                    "schema_write_access".to_string(),
                    "meta_read_access".to_string(),
                    "meta_write_access".to_string(),
                    "commit_read_access".to_string(),
                    "commit_write_access".to_string(),
                    "manage_capabilities".to_string()
                ]
            }),
            typedef
        );
    }

    #[test]
    fn deserialize_system_frame() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
json{ '@context':_{ '@base':"terminusdb://system/data/",
		    '@documentation':json{ '@authors':[ "Gavin Mendel-Gleason",
							"Matthijs van Otterdijk"
						      ],
					   '@description':"This is the System schema in which resides all information regarding capabilities, users, organizations, databases and available actions.",
					   '@title':"The System schema"
					 },
		    '@schema':"http://terminusdb.com/schema/system#",
		    '@type':'Context',
		    xsd:"http://www.w3.org/2001/XMLSchema#"
		  },
      'Action':json{ '@documentation':json{ '@comment':"The exhaustive list of actions which are available to roles."
					  },
		     '@type':'Enum',
		     '@values':[ create_database,
				 delete_database,
				 class_frame,
				 clone,
				 fetch,
				 push,
				 branch,
				 rebase,
				 instance_read_access,
				 instance_write_access,
				 schema_read_access,
				 schema_write_access,
				 meta_read_access,
				 meta_write_access,
				 commit_read_access,
				 commit_write_access,
				 manage_capabilities
			       ]
		   },
      'Capability':json{ '@documentation':json{ '@comment':"A capability is a set of roles combined with a rescource over which those roles hold.",
						'@properties':json{ role:"The set of roles the capability has access to.",
								    scope:"The resource over which the role holds."
								  }
					      },
			 '@key':json{'@type':"Random"},
			 '@type':'Class',
			 role:json{'@class':'Role','@type':'Set'},
			 scope:'Resource'
		       },
      'Database':json{ '@documentation':json{ '@comment':"A database.",
					      '@properties':json{ name:"The name of the resource."
								}
					    },
		       '@type':'Class',
		       name:'xsd:string'
		     },
      'DatabaseState':json{ '@documentation':json{ '@comment':"The current system transaction state of a database. Only the 'finalized' state is a consistent state, all others indicate that database construction failed."
						 },
			    '@type':'Enum',
			    '@values':[creating,deleting,finalized]
			  },
      'Organization':json{ '@documentation':json{ '@comment':"An organisation.",
						  '@properties':json{ child:"The set of organizations which are children of the current organization.",
								      database:"The set of databases controlled by the organization.",
								      name:"The name of the organization."
								    }
						},
			   '@key':json{'@fields':[name],'@type':"Lexical"},
			   '@type':'Class',
			   child:json{'@class':'Organization','@type':'Set'},
			   database:json{'@class':'Database','@type':'Set'},
			   name:'xsd:string'
			 },
      'Resource':json{ '@documentation':json{ '@comment':"A named resource.",
					      '@properties':json{ name:"The name of the resource."
								}
					    },
		       '@type':'Class',
		       name:'xsd:string'
		     },
      'Role':json{ '@documentation':json{ '@comment':"Roles are named collections of actions which can be provided to a capability.",
					  '@properties':json{ action:"The set of actions associated with the role.",
							      name:"The name of the role."
							    }
					},
		   '@type':'Class',
		   action:json{'@class':'Action','@type':'Set'},
		   name:'xsd:string'
		 },
      'SystemDatabase':json{ '@documentation':json{ '@comment':"The special system database.",
						    '@properties':json{ name:"The name of the resource."
								      }
						  },
			     '@type':'Class',
			     name:'xsd:string'
			   },
      'User':json{ '@documentation':json{ '@comment':"A database user.",
					  '@properties':json{ capability:"A set of capabilities which the user has access to.",
							      key_hash:"An optional key hash for authentication.",
							      name:"The users name."
							    }
					},
		   '@key':json{'@fields':[name],'@type':"Lexical"},
		   '@type':'Class',
		   capability:json{'@class':'Capability','@type':'Set'},
		   key_hash:json{'@class':'xsd:string','@type':'Optional'},
		   name:'xsd:string'
		 },
      'UserDatabase':json{ '@documentation':json{ '@comment':"A normal user database.",
						  '@properties':json{ comment:"A comment associated with the database.",
								      creation_date:"The time of creation of the database.",
								      label:"The label name of the database.",
								      name:"The name of the resource.",
								      state:"The system transaction state of the database."
								    }
						},
			   '@key':json{'@type':"Random"},
			   '@type':'Class',
			   comment:'xsd:string',
			   creation_date:'xsd:dateTime',
			   label:'xsd:string',
			   name:'xsd:string',
			   state:'DatabaseState'
			 }
    }
"#;

        let term = unwrap_result(&context, context.term_from_string(term));
        let _frames: AllFrames = context.deserialize_from_term(&term).unwrap();
        // at least it parses!
    }

    #[test]
    fn deserialize_woql_frames() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"json{'@context':_48112{'@base':"terminusdb://woql/data/",'@documentation':json{'@authors':["Gavin Mendel-Gleason"],'@description':"This is the WOQL schema. It gives a complete specification of the syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic correctness, helps to prevent errors and detect conflicts in merge of queries, and allows the storage and retrieval of queries so that queries can be associated with data products.",'@title':"WOQL schema"},'@schema':"http://terminusdb.com/schema/woql#",'@type':'Context',xsd:"http://www.w3.org/2001/XMLSchema#"},
'AddData':json{'@documentation':json{'@comment':"Add an edge with a data value.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A data value or variable which is the target or object of the graph edge. The variable must be bound.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge. The variable must be bound.",subject:"A URI or variable which is the source or subject of the graph edge. The variable must be bound."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'DataValue',predicate:'NodeValue',subject:'NodeValue'},
'AddLink':json{'@documentation':json{'@comment':"Add an edge which links between nodes in the graph.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A URI or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'NodeValue',predicate:'NodeValue',subject:'NodeValue'},
'AddTriple':json{'@documentation':json{'@comment':"Specify an edge to add to the graph.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A URI, datatype or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'Value',predicate:'NodeValue',subject:'NodeValue'},
'AddedData':json{'@documentation':json{'@comment':"Specify an edge pattern with data value which was added in *this* commit*.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A datatype or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'DataValue',predicate:'NodeValue',subject:'NodeValue'},
'AddedLink':json{'@documentation':json{'@comment':"Specify an edge pattern which links between nodes at *this* commit.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A URI or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'NodeValue',predicate:'NodeValue',subject:'NodeValue'},
'AddedTriple':json{'@documentation':json{'@comment':"Specify an edge pattern which was *added* at *this commit*.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A URI, datatype or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'Value',predicate:'NodeValue',subject:'NodeValue'},
'And':json{'@documentation':json{'@comment':"A conjunction of queries which must all have a solution.",'@properties':json{and:"List of queries which must hold."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',and:json{'@class':'Query','@type':'List'}},
'ArithmeticExpression':json{'@abstract':[],'@documentation':json{'@comment':"An abstract class specifying the AST super-class of all arithemtic expressions."},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class'}}"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let _frames: AllFrames = context.deserialize_from_term(&term).unwrap();
        // at least it parses!
        // TODO test something here
    }
}
