use bimap::BiMap;
use lazy_static::lazy_static;
use regex::Regex;
use serde::{self, Deserialize};
use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

use crate::value::type_is_json;

use super::{naming::inverse_field_name, sanitize::graphql_sanitize};

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Deserialize, Default, Hash)]
pub struct GraphQLName<'a>(pub Cow<'a, str>);
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Deserialize, Default, Hash)]
pub struct IriName(pub String);
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Deserialize, Default, Hash)]
#[repr(transparent)]
pub struct ShortName(pub String);

impl<'a> GraphQLName<'a> {
    pub fn as_static(&self) -> GraphQLName<'static> {
        match &self.0 {
            Cow::Borrowed(s) => GraphQLName(Cow::Owned(s.to_string())),
            Cow::Owned(s) => GraphQLName(Cow::Owned(s.clone())),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl<'a> PartialOrd<str> for GraphQLName<'a> {
    fn partial_cmp(&self, other: &str) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&<&str as Into<std::borrow::Cow<'_, str>>>::into(other))
    }
}

impl<'a> PartialEq<str> for GraphQLName<'a> {
    fn eq(&self, other: &str) -> bool {
        self.0.eq(other)
    }
}

impl ShortName {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn sanitize(&self) -> GraphQLName<'static> {
        graphql_sanitize(&self.0)
    }
}

impl Display for ShortName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a> Display for GraphQLName<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Display for IriName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl IriName {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

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
#[serde(untagged)]
pub enum OneOrMore<T> {
    One(T),
    More(Vec<T>),
}

impl<T> From<OneOrMore<T>> for Vec<T> {
    fn from(value: OneOrMore<T>) -> Self {
        match value {
            OneOrMore::One(o) => vec![o],
            OneOrMore::More(v) => v,
        }
    }
}

impl<T> OneOrMore<T> {
    fn into_vec(self) -> Vec<T> {
        self.into()
    }
}

fn empty<T>() -> OneOrMore<T> {
    OneOrMore::More(vec![])
}

#[derive(Deserialize, PartialEq, Debug, Clone)]
pub struct UncleanRestrictionDefinition {
    #[serde(rename = "_id")]
    pub id: ShortName,
    #[serde(rename = "@on")]
    pub on: ShortName,
    #[serde(flatten)]
    pub restriction_data: BTreeMap<String, serde_json::Value>,
}

#[derive(Deserialize, PartialEq, Debug, Clone)]
pub struct RestrictionDefinition<'a> {
    #[serde(rename = "_id")]
    pub id: GraphQLName<'a>,
    #[serde(rename = "@on")]
    pub on: GraphQLName<'a>,
    #[serde(skip)]
    pub original_id: ShortName,
    #[serde(flatten)]
    pub restriction_data: BTreeMap<String, serde_json::Value>,
}

impl UncleanRestrictionDefinition {
    fn sanitize(self) -> RestrictionDefinition<'static> {
        let id = self.id.sanitize().as_static();
        let on = self.on.sanitize().as_static();

        RestrictionDefinition {
            id,
            on,
            original_id: self.id.clone(),
            restriction_data: self.restriction_data,
        }
    }
}

#[derive(Deserialize, PartialEq, Debug, Clone)]
pub struct SchemaMetadata {
    #[serde(default)]
    pub restrictions: Vec<UncleanRestrictionDefinition>,
    #[serde(flatten)]
    pub extra_metadata: BTreeMap<String, serde_json::Value>,
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
    #[serde(default = "empty")]
    pub documentation: OneOrMore<SchemaDocumentation>,
    #[serde(rename = "@metadata")]
    pub metadata: Option<SchemaMetadata>,
    #[serde(flatten)]
    pub extra_prefixes: BTreeMap<String, String>,
}

fn has_protocol(s: &str) -> bool {
    lazy_static! {
        static ref HAS_PROTOCOL: Regex = Regex::new(r"^\p{L}(\p{L}|\p{N})*://.+").unwrap();
    }
    HAS_PROTOCOL.is_match(s)
}

fn has_prefix_if_no_protocol(s: &str) -> Option<(String, String)> {
    lazy_static! {
        static ref HAS_PREFIX: Regex = Regex::new(
            r"^(?<prefix>(\p{L}|@)((\p{L}|\p{N}|-|\.)*(\p{L}|\p{N}|_|-))?):(?<suffix>.*)$"
        )
        .unwrap();
    }
    let captures = HAS_PREFIX.captures(s);
    captures.map(|captures| {
        (
            captures
                .name("prefix")
                .expect("This must exist")
                .as_str()
                .to_string(),
            captures
                .name("suffix")
                .expect("This has a suffix")
                .as_str()
                .to_string(),
        )
    })
}

pub enum NodeVariety {
    Base(String),
    Prefixed(String, String),
    URL(String),
}

impl From<&ShortName> for NodeVariety {
    fn from(value: &ShortName) -> Self {
        node_variety(value.as_str())
    }
}

impl From<&IriName> for NodeVariety {
    fn from(value: &IriName) -> Self {
        NodeVariety::URL(value.0.to_string())
    }
}

pub fn node_variety(s: &str) -> NodeVariety {
    if has_protocol(s) {
        NodeVariety::URL(s.to_string())
    } else if let Some((prefix, suffix)) = has_prefix_if_no_protocol(s) {
        NodeVariety::Prefixed(prefix, suffix)
    } else {
        NodeVariety::Base(s.to_string())
    }
}

impl Prefixes {
    pub fn expand_schema(&self, nv: &NodeVariety) -> IriName {
        match nv {
            // this is dumb but will work for now
            NodeVariety::Base(s) => IriName(format!("{}{}", self.schema, s)),
            NodeVariety::Prefixed(prefix, suffix) => {
                if let Some(expanded) = self.extra_prefixes.get(prefix) {
                    IriName(format!("{}{}", expanded, suffix))
                } else {
                    panic!("Unable to find prefix {prefix}!")
                }
            }
            NodeVariety::URL(s) => IriName(s.to_string()),
        }
    }
    pub fn expand_instance(&self, nv: &NodeVariety) -> IriName {
        match nv {
            // this is dumb but will work for now
            NodeVariety::Base(s) => IriName(format!("{}{}", self.base, s)),
            NodeVariety::Prefixed(prefix, suffix) => {
                if let Some(expanded) = self.extra_prefixes.get(prefix) {
                    IriName(format!("{}{}", expanded, suffix))
                } else {
                    panic!("Unable to find prefix {prefix}!");
                }
            }
            NodeVariety::URL(s) => IriName(s.to_string()),
        }
    }

    // This may yet be necessary but *shouldn't* be

    // pub fn compress_schema(&self, s: &str) -> String {
    //     if s.starts_with(&self.schema) {
    //         s[self.schema.len()..].to_string()
    //     } else {
    //         s.to_string()
    //     }
    // }
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
pub struct UncleanPropertyDocumentationRecord {
    pub label: Option<String>,
    pub comment: Option<String>,
}

impl From<StructuralPropertyDocumentationRecord> for UncleanPropertyDocumentationRecord {
    fn from(f: StructuralPropertyDocumentationRecord) -> Self {
        match f {
            StructuralPropertyDocumentationRecord::OnlyPropertyLabel(s) => {
                UncleanPropertyDocumentationRecord {
                    label: Some(s),
                    comment: None,
                }
            }
            StructuralPropertyDocumentationRecord::PropertyCommentLabel { label, comment } => {
                UncleanPropertyDocumentationRecord { label, comment }
            }
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct UncleanPropertyDocumentation {
    #[serde(flatten)]
    pub records: BTreeMap<ShortName, UncleanPropertyDocumentationRecord>,
}

impl UncleanPropertyDocumentation {
    pub fn sanitize(self) -> PropertyDocumentation {
        let mut records = BTreeMap::new();
        for (s, pdr) in self.records.iter() {
            records.insert(s.sanitize(), pdr.clone());
        }
        PropertyDocumentation { records }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct PropertyDocumentation {
    #[serde(flatten)]
    pub records: BTreeMap<GraphQLName<'static>, UncleanPropertyDocumentationRecord>,
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct UncleanClassDocumentationDefinition {
    #[serde(rename = "@label")]
    pub label: Option<String>,
    #[serde(rename = "@comment")]
    pub comment: Option<String>,
    #[serde(rename = "@properties")]
    pub properties: Option<UncleanPropertyDocumentation>,
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

impl UncleanClassDocumentationDefinition {
    pub fn sanitize(self) -> ClassDocumentationDefinition {
        ClassDocumentationDefinition {
            label: self.label,
            comment: self.comment,
            properties: self.properties.map(move |pd| pd.sanitize()),
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
        class: StructuralFieldInnerDefinition,
    },
    Set {
        #[serde(rename = "@class")]
        class: StructuralFieldInnerDefinition,
    },
    Array {
        #[serde(rename = "@class")]
        class: StructuralFieldInnerDefinition,
        #[serde(default = "default_dimensionality")]
        dimensions: usize,
    },
    List {
        #[serde(rename = "@class")]
        class: StructuralFieldInnerDefinition,
    },
    Cardinality {
        #[serde(rename = "@class")]
        class: StructuralFieldInnerDefinition,
        min: Option<usize>,
        max: Option<usize>,
    },
    Foreign {
        #[serde(rename = "@class")]
        class: StructuralFieldInnerDefinition,
    },
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(untagged)]
enum StructuralFieldInnerDefinition {
    Class(String),
    Foreign(StructuralForeignDefinition),
}

impl StructuralFieldInnerDefinition {
    fn name(self) -> BaseOrDerived<ShortName> {
        match self {
            Self::Class(s) => base_or_derived(s),
            Self::Foreign(d) => BaseOrDerived::Derived(d.class),
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
struct StructuralForeignDefinition {
    #[serde(rename = "@class")]
    class: ShortName,
}

impl From<StructuralFieldDefinition> for UncleanFieldDefinition {
    fn from(f: StructuralFieldDefinition) -> Self {
        match f {
            StructuralFieldDefinition::SimpleField(s) => {
                UncleanFieldDefinition::Required(base_or_derived(s))
            }
            StructuralFieldDefinition::ContainerField(c) => match c {
                ComplexFieldDefinition::Optional { class } => {
                    UncleanFieldDefinition::Optional(class.name())
                }
                ComplexFieldDefinition::Set { class } => UncleanFieldDefinition::Set(class.name()),
                ComplexFieldDefinition::List { class } => {
                    UncleanFieldDefinition::List(class.name())
                }
                ComplexFieldDefinition::Array { class, dimensions } => {
                    UncleanFieldDefinition::Array {
                        class: class.name(),
                        dimensions,
                    }
                }
                ComplexFieldDefinition::Cardinality { class, min, max } => {
                    UncleanFieldDefinition::Cardinality {
                        class: class.name(),
                        min,
                        max,
                    }
                }
                ComplexFieldDefinition::Foreign { class } => {
                    UncleanFieldDefinition::Required(class.name())
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

#[derive(Deserialize, PartialEq, Debug, Clone)]
#[serde(from = "StructuralFieldDefinition")]
pub enum UncleanFieldDefinition {
    Required(BaseOrDerived<ShortName>),
    Optional(BaseOrDerived<ShortName>),
    Set(BaseOrDerived<ShortName>),
    List(BaseOrDerived<ShortName>),
    Array {
        class: BaseOrDerived<ShortName>,
        dimensions: usize,
    },
    Cardinality {
        class: BaseOrDerived<ShortName>,
        min: Option<usize>,
        max: Option<usize>,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub enum FieldDefinition {
    Required(BaseOrDerived<GraphQLName<'static>>),
    Optional(BaseOrDerived<GraphQLName<'static>>),
    Set(BaseOrDerived<GraphQLName<'static>>),
    List(BaseOrDerived<GraphQLName<'static>>),
    Array {
        class: BaseOrDerived<GraphQLName<'static>>,
        dimensions: usize,
    },
    Cardinality {
        class: BaseOrDerived<GraphQLName<'static>>,
        min: Option<usize>,
        max: Option<usize>,
    },
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum FieldKind {
    Required,
    Optional,
    Set,
    List,
    Array,
    Cardinality,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum CollectionKind {
    Property,
    List,
    Array,
}

impl TryFrom<FieldKind> for CollectionKind {
    type Error = ();

    fn try_from(value: FieldKind) -> Result<Self, Self::Error> {
        match value {
            FieldKind::Required | FieldKind::Optional => Err(()),
            FieldKind::Set => Ok(CollectionKind::Property),
            FieldKind::List => Ok(CollectionKind::List),
            FieldKind::Array => Ok(CollectionKind::Array),
            FieldKind::Cardinality => Ok(CollectionKind::Property),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BaseOrDerived<T: Clone + PartialEq + ToString + Eq> {
    Base(String),
    Derived(T),
}

impl BaseOrDerived<ShortName> {
    pub fn sanitize(&self) -> BaseOrDerived<GraphQLName<'static>> {
        match &self {
            BaseOrDerived::Base(s) => BaseOrDerived::Base(s.to_string()),
            BaseOrDerived::Derived(t) => BaseOrDerived::Derived(t.sanitize()),
        }
    }
}

pub fn is_base_type(s: &str) -> bool {
    // TODO this is not good enough
    s.starts_with("xsd:") || s.starts_with("sys:")
}

pub fn base_or_derived(c: String) -> BaseOrDerived<ShortName> {
    if is_base_type(&c) {
        BaseOrDerived::Base(c[4..].to_string())
    } else {
        BaseOrDerived::Derived(ShortName(c))
    }
}

pub fn sanitize_class(s: &BaseOrDerived<ShortName>) -> BaseOrDerived<GraphQLName<'static>> {
    s.sanitize()
}

impl UncleanFieldDefinition {
    pub fn sanitize(self) -> FieldDefinition {
        match self {
            Self::Required(c) => FieldDefinition::Required(sanitize_class(&c)),
            Self::Optional(c) => FieldDefinition::Optional(sanitize_class(&c)),
            Self::Set(c) => FieldDefinition::Set(sanitize_class(&c)),
            Self::List(c) => FieldDefinition::List(sanitize_class(&c)),
            Self::Array { class, dimensions } => FieldDefinition::Array {
                class: sanitize_class(&class),
                dimensions,
            },
            Self::Cardinality { class, min, max } => FieldDefinition::Cardinality {
                class: sanitize_class(&class),
                min,
                max,
            },
        }
    }
}

impl FieldDefinition {
    pub fn base_type(&self) -> Option<&str> {
        let range = self.range();
        match range {
            BaseOrDerived::Base(s) => Some(s),
            BaseOrDerived::Derived(_) => None,
        }
    }

    pub fn is_json_type(&self) -> bool {
        self.base_type().map(type_is_json).unwrap_or(false)
    }

    pub fn range(&self) -> &BaseOrDerived<GraphQLName> {
        match self {
            Self::Required(c) => c,
            Self::Optional(c) => c,
            Self::Set(c) => c,
            Self::List(c) => c,
            Self::Array { class, .. } => class,
            Self::Cardinality { class, .. } => class,
        }
    }

    pub fn document_type<'a>(&'a self, allframes: &'a AllFrames) -> Option<&'a GraphQLName> {
        let range = self.range();
        match range {
            BaseOrDerived::Base(_) => None,
            BaseOrDerived::Derived(s) => allframes.document_type(s),
        }
    }

    pub fn enum_type<'a>(&'a self, allframes: &'a AllFrames) -> Option<&'a GraphQLName> {
        let range = self.range();
        match range {
            BaseOrDerived::Base(_) => None,
            BaseOrDerived::Derived(s) => allframes.enum_type(s),
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
}

impl FieldDefinition {
    pub fn optionalize(self) -> Self {
        match self {
            FieldDefinition::Required(df) => FieldDefinition::Optional(df),
            res => res,
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
pub enum UncleanKeyDefinition {
    Random,
    Lexical {
        #[serde(rename = "@fields")]
        fields: Vec<ShortName>,
    },
    Hash {
        #[serde(rename = "@fields")]
        fields: Vec<ShortName>,
    },
    ValueHash,
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
pub enum KeyDefinition {
    Random,
    Lexical {
        #[serde(rename = "@fields")]
        fields: Vec<GraphQLName<'static>>,
    },
    Hash {
        #[serde(rename = "@fields")]
        fields: Vec<GraphQLName<'static>>,
    },
    ValueHash,
}

impl UncleanKeyDefinition {
    pub fn sanitize(self) -> KeyDefinition {
        match self {
            UncleanKeyDefinition::Random => KeyDefinition::Random,
            UncleanKeyDefinition::Lexical { fields } => {
                let fields = fields.iter().map(|f| f.sanitize()).collect();
                KeyDefinition::Lexical { fields }
            }
            UncleanKeyDefinition::Hash { fields } => {
                let fields = fields.iter().map(|f| f.sanitize()).collect();
                KeyDefinition::Hash { fields }
            }
            UncleanKeyDefinition::ValueHash => KeyDefinition::ValueHash,
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct UncleanOneOf {
    #[serde(flatten)]
    pub choices: BTreeMap<ShortName, UncleanFieldDefinition>,
}

#[derive(PartialEq, Debug)]
pub struct OneOf {
    pub choices: BTreeMap<GraphQLName<'static>, FieldDefinition>,
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct UncleanClassDefinition {
    #[serde(rename = "@documentation")]
    #[serde(default = "empty")]
    pub documentation: OneOrMore<UncleanClassDocumentationDefinition>,
    #[serde(rename = "@metadata")]
    pub metadata: Option<serde_json::Value>,
    #[serde(rename = "@key")]
    pub key: Option<UncleanKeyDefinition>,
    #[serde(rename = "@subdocument")]
    pub is_subdocument: Option<Vec<()>>,
    #[serde(rename = "@unfoldable")]
    pub is_unfoldable: Option<Vec<()>>,
    #[serde(rename = "@abstract")]
    pub is_abstract: Option<Vec<()>>,
    #[serde(rename = "@oneOf")]
    pub one_of: Option<Vec<UncleanOneOf>>,
    #[serde(rename = "@inherits")]
    pub inherits: Option<Vec<ShortName>>,
    #[serde(flatten)]
    pub fields: BTreeMap<ShortName, UncleanFieldDefinition>,
}

#[derive(PartialEq, Debug)]
pub struct ClassDefinition {
    pub documentation: OneOrMore<ClassDocumentationDefinition>,
    pub metadata: Option<serde_json::Value>,
    pub key: Option<KeyDefinition>,
    pub is_subdocument: Option<Vec<()>>,
    pub is_unfoldable: Option<Vec<()>>,
    pub is_abstract: Option<Vec<()>>,
    pub one_of: Option<Vec<OneOf>>,
    pub inherits: Option<Vec<GraphQLName<'static>>>,
    pub fields: BTreeMap<GraphQLName<'static>, FieldDefinition>,
    pub graphql_to_iri: BiMap<GraphQLName<'static>, IriName>,
    pub graphql_to_short_name: BiMap<GraphQLName<'static>, ShortName>,
}

static RESERVED_CLASSES: [&str; 4] = ["BigFloat", "DateTime", "BigInt", "JSON"];

impl UncleanClassDefinition {
    pub fn sanitize(self, prefixes: &Prefixes) -> ClassDefinition {
        let mut field_map: BiMap<GraphQLName, ShortName> = BiMap::new();
        let mut fields: BTreeMap<GraphQLName, _> = BTreeMap::new();
        for (field, fd) in self.fields.into_iter() {
            let sanitized_field = field.sanitize();
            let res = field_map.insert_no_overwrite(sanitized_field.clone(), field.clone());
            if let Err((left, right)) = res {
                if left != sanitized_field || right != field {
                    panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires enum value names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]'. Please rename your enum values to remove the following uninvertible pair: ({left},{right}) != ({sanitized_field},{field})")
                }
            }
            fields.insert(sanitized_field, fd.sanitize());
        }
        let one_of = self.one_of.map(|o| {
            o.into_iter()
                .map(|c| {
                    let mut choices = BTreeMap::new();
                    for (field,fd) in c.choices.into_iter() {
                        let sanitized_field = field.sanitize();
                        let res = field_map.insert_no_overwrite(sanitized_field.clone(), field.clone());
                        if let Err((left, right)) = res {
                            if left != sanitized_field || right != field {
                                panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires enum value names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]'. Please rename your enum values to remove the following uninvertible pair: ({left},{right}) != ({sanitized_field},{field})")
                            }
                        }
                        choices.insert(sanitized_field.clone(), fd.sanitize().optionalize());
                    }
                    OneOf { choices }
                })
                .collect()
        });
        let documentation: Vec<_> = self
            .documentation
            .into_vec()
            .into_iter()
            .map(move |d| d.sanitize())
            .collect();
        let key = self.key.map(|k| k.sanitize());
        let mut field_renaming: BiMap<GraphQLName, IriName> = BiMap::new();
        for (s, t) in field_map.iter() {
            let expanded = prefixes.expand_schema(&t.into());
            field_renaming.insert(s.clone(), expanded);
        }
        let inherits = if let Some(inherits_val) = &self.inherits {
            let mut result = vec![];
            for class_name in inherits_val {
                let sanitized = class_name.sanitize();
                result.push(sanitized)
            }
            Some(result)
        } else {
            None
        };
        ClassDefinition {
            documentation: OneOrMore::More(documentation),
            metadata: self.metadata.clone(),
            key,
            is_subdocument: self.is_subdocument.clone(),
            is_unfoldable: self.is_unfoldable.clone(),
            is_abstract: self.is_abstract,
            inherits,
            one_of,
            fields,
            graphql_to_iri: field_renaming,
            graphql_to_short_name: field_map,
        }
    }
}

impl ClassDefinition {
    pub fn resolve_field<'a>(&'a self, field_name: &'a GraphQLName<'a>) -> &'a FieldDefinition {
        let maybe_field: Option<&FieldDefinition> = self.one_of.as_ref().and_then(|oneofs| {
            oneofs.iter().find_map(|o| {
                o.choices
                    .iter()
                    .find_map(|(s, f)| if s == field_name { Some(f) } else { None })
            })
        });
        if let Some(field) = maybe_field {
            field
        } else {
            &self.fields[field_name]
        }
    }

    pub fn fields(&self) -> Vec<(&GraphQLName, &FieldDefinition)> {
        let mut one_ofs: Vec<(&GraphQLName, &FieldDefinition)> = self
            .one_of
            .as_ref()
            .map(|s| {
                s.iter()
                    .flat_map(move |c| c.choices.iter().collect::<Vec<_>>())
            })
            .into_iter()
            .flatten()
            .collect();
        one_ofs.dedup();
        let mut fields: Vec<(&GraphQLName, &FieldDefinition)> = self
            .fields
            .iter()
            .collect::<Vec<(&GraphQLName, &FieldDefinition)>>();
        fields.append(&mut one_ofs);
        fields
    }

    pub fn graphql_to_iri_name<'a>(
        &'a self,
        _prefixes: &Prefixes,
        property: &GraphQLName<'a>,
    ) -> &IriName {
        // is this really fully qualified, I don't see how????
        self.graphql_to_iri
            .get_by_left(property)
            .unwrap_or_else(|| {
                panic!("The fully qualified property name for {property:?} *should* exist")
            })
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct UncleanEnumDocumentationDefinition {
    #[serde(rename = "@label")]
    pub label: Option<String>,
    #[serde(rename = "@comment")]
    pub comment: Option<String>,
    #[serde(rename = "@values")]
    pub values: Option<UncleanPropertyDocumentation>,
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

impl UncleanEnumDocumentationDefinition {
    pub fn sanitize(self) -> EnumDocumentationDefinition {
        EnumDocumentationDefinition {
            label: self.label.clone(),
            comment: self.comment.clone(),
            values: self.values.map(|pd| pd.sanitize()),
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct UncleanEnumDefinition {
    //#[serde(rename = "@type")]
    //pub kind: String,
    #[serde(rename = "@documentation")]
    #[serde(default = "empty")]
    pub documentation: OneOrMore<UncleanEnumDocumentationDefinition>,
    #[serde(rename = "@metadata")]
    pub metadata: Option<serde_json::Value>,
    #[serde(rename = "@values")]
    pub values: Vec<ShortName>,
}

#[derive(PartialEq, Debug)]
pub struct EnumDefinition {
    pub documentation: OneOrMore<EnumDocumentationDefinition>,
    pub metadata: Option<serde_json::Value>,
    pub values: Vec<GraphQLName<'static>>,
    pub values_renaming: BiMap<GraphQLName<'static>, ShortName>,
}

impl UncleanEnumDefinition {
    pub fn sanitize(self) -> EnumDefinition {
        let mut values_renaming: BiMap<GraphQLName, ShortName> = BiMap::new();
        let values : Vec<GraphQLName> = self.values.iter().map(|v| {
            let sanitized = v.sanitize();
            let res = values_renaming.insert_no_overwrite(sanitized.clone(), v.clone());
            if let Err((left,right)) = res {
                if left != sanitized || right != *v {
                    panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires enum value names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]'. Please rename your enum values to remove the following uninvertible pair: ({left},{right}) != ({sanitized},{v})")
                }
            }
            sanitized
        }).collect();
        let documentation: Vec<_> = self
            .documentation
            .into_vec()
            .into_iter()
            .map(|ed| ed.sanitize())
            .collect();
        EnumDefinition {
            documentation: OneOrMore::More(documentation),
            metadata: self.metadata,
            values,
            values_renaming,
        }
    }
}

impl EnumDefinition {
    // TODO: These look really questionable - how are we encoding / decoding?
    pub fn value_name(&self, value: &GraphQLName) -> IriName {
        let res = self
            .values_renaming
            .get_by_left(value)
            .unwrap_or_else(|| panic!("The value name for {value:?} *should* exist"));
        IriName(urlencoding::encode(res.as_str()).into_owned())
    }

    pub fn name_value(&self, name: &IriName) -> &GraphQLName {
        let decoded = &*urlencoding::decode(name.as_str())
            .expect("Somehow encoding went terribly wrong in saving");
        self.values_renaming
            .get_by_right(&ShortName(decoded.to_string()))
            .unwrap_or_else(|| {
                panic!("The value for {name:?} *should* exist as it is in your database")
            })
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
pub enum UncleanTypeDefinition {
    Class(UncleanClassDefinition),
    Enum(UncleanEnumDefinition),
}

#[derive(PartialEq, Debug)]
pub enum TypeDefinition {
    Class(ClassDefinition),
    Enum(EnumDefinition),
}

impl TypeDefinition {
    pub fn is_document_type(&self) -> bool {
        matches!(self, Self::Class(_))
    }

    pub fn is_enum_type(&self) -> bool {
        matches!(self, Self::Enum(_))
    }

    pub(crate) fn as_class_definition(&self) -> &ClassDefinition {
        match self {
            Self::Class(c) => c,
            _ => panic!("tried to unwrap non-class definition as class definition: {self:?}"),
        }
    }

    pub(crate) fn as_enum_definition(&self) -> &EnumDefinition {
        match self {
            Self::Enum(e) => e,
            _ => panic!("tried to unwrap non-enum definition as enum definition: {self:?}"),
        }
    }
}

impl FieldKind {
    pub fn is_collection(&self) -> bool {
        matches!(
            self,
            Self::Set | Self::Array | Self::List | Self::Cardinality
        )
    }
}

#[derive(Deserialize, Debug)]
pub struct UncleanAllFrames {
    #[serde(rename = "@context")]
    pub context: Prefixes,
    #[serde(flatten)]
    pub frames: BTreeMap<ShortName, UncleanTypeDefinition>,
}

#[derive(Debug)]
pub struct AllFrames {
    pub context: Prefixes,
    pub frames: BTreeMap<GraphQLName<'static>, TypeDefinition>,
    pub class_renaming: BiMap<GraphQLName<'static>, ShortName>,
    pub graphql_to_iri_renaming: BiMap<GraphQLName<'static>, IriName>,
    pub inverted: AllInvertedFrames,
    pub subsumption: HashMap<GraphQLName<'static>, Vec<GraphQLName<'static>>>,
    pub restrictions: BTreeMap<GraphQLName<'static>, RestrictionDefinition<'static>>,
}

impl UncleanAllFrames {
    pub fn sanitize(
        self,
    ) -> (
        BTreeMap<GraphQLName<'static>, TypeDefinition>,
        BTreeMap<GraphQLName<'static>, RestrictionDefinition<'static>>,
        BiMap<GraphQLName<'static>, IriName>,
        BiMap<GraphQLName<'static>, ShortName>,
        Prefixes,
    ) {
        let mut class_renaming: BiMap<GraphQLName, ShortName> =
            BiMap::from_iter(RESERVED_CLASSES.iter().map(|x| {
                (
                    GraphQLName(Cow::Owned(x.to_string())),
                    ShortName(x.to_string()),
                )
            }));
        let mut frames: BTreeMap<GraphQLName, TypeDefinition> = BTreeMap::new();
        let mut graphql_to_iri_renaming: BiMap<GraphQLName, IriName> = BiMap::new();
        for (class_name, typedef) in self.frames.into_iter() {
            let class_name_ref = &class_name;
            let iri_name = self.context.expand_schema(&class_name_ref.into());
            let sanitized_class = class_name.sanitize();
            // GraphQL <> IRI
            let res =
                graphql_to_iri_renaming.insert_no_overwrite(sanitized_class.clone(), iri_name);
            if let Err((left, right)) = res {
                panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires class names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]' and which do not overlap with reserved type names. Please rename your classes to remove the following duplicate pair: ({left},{right}) == ({sanitized_class},{class_name})")
            }
            // GraphQL <> ShortName
            let res =
                class_renaming.insert_no_overwrite(sanitized_class.clone(), class_name.clone());
            if let Err((left, right)) = res {
                panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires class names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]' and which do not overlap with reserved type names. Please rename your classes to remove the following duplicate pair: ({left},{right}) == ({sanitized_class},{class_name})")
            }
            let new_typedef = match typedef {
                UncleanTypeDefinition::Class(cd) => {
                    TypeDefinition::Class(cd.sanitize(&self.context))
                }
                UncleanTypeDefinition::Enum(ed) => TypeDefinition::Enum(ed.sanitize()),
            };
            frames.insert(sanitized_class.clone(), new_typedef);
        }
        let mut sanitized_restrictions = BTreeMap::new();
        if let Some(restrictions) = self.context.metadata.as_ref().map(|m| &m.restrictions) {
            for restriction in restrictions.iter() {
                let restriction_name = &restriction.id;
                let sanitized_restriction_name = restriction_name.sanitize();
                let res = class_renaming.insert_no_overwrite(
                    sanitized_restriction_name.clone(),
                    restriction_name.clone(),
                );
                if let Err((left, right)) = res {
                    panic!("This schema has name collisions under TerminusDB's automatic GraphQL sanitation renaming. GraphQL requires class names match the following Regexp: '^[^_a-zA-Z][_a-zA-Z0-9]'. Please rename your classes to remove the following duplicate pair: ({left},{right}) != ({sanitized_restriction_name},{restriction_name})")
                }

                sanitized_restrictions
                    .insert(sanitized_restriction_name, restriction.clone().sanitize());
            }
        }
        (
            frames,
            sanitized_restrictions,
            graphql_to_iri_renaming,
            class_renaming,
            self.context,
        )
    }

    pub fn finalize(self) -> AllFrames {
        let (frames, restrictions, graphql_to_iri_renaming, class_renaming, context) =
            self.sanitize();
        let inverted = allframes_to_allinvertedframes(&frames);
        let subsumption = AllFrames::calculate_subsumption(&frames);

        AllFrames {
            context,
            frames,
            restrictions,
            graphql_to_iri_renaming,
            class_renaming,
            inverted,
            subsumption,
        }
    }
}

impl AllFrames {
    pub fn document_type<'a>(&self, s: &'a GraphQLName<'a>) -> Option<&'a GraphQLName<'a>> {
        if self.frames.contains_key(s) && self.frames[s].is_document_type() {
            Some(s)
        } else {
            None
        }
    }

    pub fn enum_type<'a>(&self, s: &'a GraphQLName<'a>) -> Option<&'a GraphQLName<'a>> {
        if self.frames.contains_key(s) && self.frames[s].is_enum_type() {
            Some(s)
        } else {
            None
        }
    }

    #[allow(dead_code)]
    pub fn short_to_graphql_name_opt<'a>(
        &'a self,
        short_name: &ShortName,
    ) -> Option<GraphQLName<'a>> {
        self.class_renaming.get_by_right(short_name).cloned()
    }

    #[allow(dead_code)]
    pub fn short_name_to_graphql_name<'a>(&'a self, db_name: &ShortName) -> GraphQLName<'a> {
        self.short_to_graphql_name_opt(db_name)
            .unwrap_or_else(|| panic!("This class name {db_name} *should* exist"))
    }

    pub fn iri_to_graphql_name_opt(&self, iri: &IriName) -> Option<GraphQLName> {
        self.graphql_to_iri_renaming.get_by_right(iri).cloned()
    }

    pub fn iri_to_graphql_name<'a>(&'a self, iri: &IriName) -> GraphQLName<'a> {
        self.iri_to_graphql_name_opt(iri)
            .unwrap_or_else(|| panic!("This class name {iri} *should* exist"))
    }

    pub fn graphql_to_short_name_opt<'a>(
        &'a self,
        class_name: &GraphQLName<'a>,
    ) -> Option<&ShortName> {
        self.class_renaming.get_by_left(class_name)
    }

    pub fn graphql_to_short_name<'a>(&'a self, class_name: &GraphQLName<'a>) -> &ShortName {
        self.graphql_to_short_name_opt(class_name)
            .unwrap_or_else(|| panic!("This class name {class_name} *should* exist"))
    }

    pub fn graphql_to_iri_name<'a>(&'a self, class_name: &GraphQLName<'a>) -> IriName {
        let db_name = self.graphql_to_short_name(class_name);
        self.context.expand_schema(&db_name.into())
    }

    pub fn graphql_property_to_iri<'a>(
        &'a self,
        class_name: &GraphQLName<'a>,
        property_name: &GraphQLName<'a>,
    ) -> Option<&IriName> {
        let class_record = self.frames.get(class_name)?;
        let class_definition = class_record.as_class_definition();
        Some(class_definition.graphql_to_iri_name(&self.context, property_name))
    }

    pub fn graphql_enum_value_to_iri_name<'a>(
        &self,
        enum_type: &GraphQLName<'a>,
        value: &GraphQLName<'a>,
    ) -> IriName {
        let enum_type_expanded = self.graphql_to_iri_name(enum_type);
        let enum_type_definition = self.frames[enum_type].as_enum_definition();
        let value = enum_type_definition.value_name(value);
        IriName(format!("{enum_type_expanded}/{value}"))
    }

    pub fn reverse_link<'a>(
        &'a self,
        class: &'a GraphQLName,
        field: &'a GraphQLName,
    ) -> Option<&'a InvertedFieldDefinition> {
        self.inverted
            .classes
            .get(class)
            .and_then(move |inverted| inverted.domain.get(field))
    }

    pub fn subsumed<'a>(&'a self, class: &GraphQLName<'a>) -> Vec<GraphQLName<'a>> {
        self.subsumption
            .get(class)
            .cloned()
            .unwrap_or_else(|| vec![class.clone()])
    }

    pub fn is_foreign<'a>(&'a self, class: &GraphQLName<'a>) -> bool {
        // This will seem a bit strange in isolation, but what we're trying to say here is that any class that is not appearing in the frames must be a foreign.
        !self.frames.contains_key(class)
    }

    fn calculate_subsumption(
        frames: &BTreeMap<GraphQLName<'static>, TypeDefinition>,
    ) -> HashMap<GraphQLName<'static>, Vec<GraphQLName<'static>>> {
        let mut subsumption_rel: HashMap<GraphQLName<'static>, Vec<GraphQLName<'static>>> =
            HashMap::new();
        for (class, typedef) in frames {
            if typedef.is_document_type() {
                let mut supers = typedef
                    .as_class_definition()
                    .inherits
                    .clone()
                    .unwrap_or(Vec::new());
                supers.push(class.as_static());
                for superclass in supers {
                    if let Some(v) = subsumption_rel.get_mut(&superclass) {
                        v.push(class.as_static());
                    } else {
                        subsumption_rel.insert(superclass.as_static(), vec![class.as_static()]);
                    }
                }
            }
        }
        subsumption_rel
    }
}

#[derive(Debug)]
pub struct AllInvertedFrames {
    pub classes: BTreeMap<GraphQLName<'static>, InvertedTypeDefinition>,
}

#[derive(Debug)]
pub struct InvertedFieldDefinition {
    pub property: GraphQLName<'static>,
    pub kind: FieldKind,
    pub class: GraphQLName<'static>,
}

#[derive(Debug)]
pub struct InvertedTypeDefinition {
    pub domain: BTreeMap<GraphQLName<'static>, InvertedFieldDefinition>,
}

pub fn allframes_to_allinvertedframes(
    frames: &BTreeMap<GraphQLName<'static>, TypeDefinition>,
) -> AllInvertedFrames {
    let mut classes: BTreeMap<GraphQLName<'static>, InvertedTypeDefinition> = BTreeMap::new();
    for (class, record) in frames.iter() {
        match record {
            TypeDefinition::Class(classdefinition) => {
                let fields = &classdefinition.fields;
                for (property, fieldrecord) in fields.iter() {
                    let kind = fieldrecord.kind();
                    let range = fieldrecord.range();
                    let field_name = inverse_field_name(property, class);
                    match range {
                        BaseOrDerived::Derived(range) => {
                            let range = range.as_static();
                            if let Some(inverted_type_definition) = classes.get_mut(&range) {
                                let inverted_type = &mut inverted_type_definition.domain;
                                inverted_type.insert(
                                    field_name.as_static(),
                                    InvertedFieldDefinition {
                                        property: property.as_static(),
                                        kind,
                                        class: class.as_static(),
                                    },
                                );
                            } else {
                                let mut range_properties: BTreeMap<
                                    GraphQLName<'static>,
                                    InvertedFieldDefinition,
                                > = BTreeMap::new();
                                range_properties.insert(
                                    field_name.as_static(),
                                    InvertedFieldDefinition {
                                        property: property.as_static(),
                                        kind,
                                        class: class.as_static(),
                                    },
                                );
                                classes.insert(
                                    range,
                                    InvertedTypeDefinition {
                                        domain: range_properties,
                                    },
                                );
                            }
                        }
                        BaseOrDerived::Base(_) => (),
                    }
                }
            }
            TypeDefinition::Enum(_) => (),
        };
    }
    AllInvertedFrames { classes }
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
                documentation: OneOrMore::More(vec![]),
                metadata: None,
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
        let typedef: UncleanFieldDefinition = context.deserialize_from_term(&term).unwrap();

        assert_eq!(
            UncleanFieldDefinition::Array {
                class: BaseOrDerived::Derived(ShortName("Something".to_string())),
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
        let typedef: UncleanKeyDefinition = context.deserialize_from_term(&term).unwrap();

        assert_eq!(
            UncleanKeyDefinition::Lexical {
                fields: vec![ShortName("foo".to_string()), ShortName("bar".to_string())]
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
        let typedef: UncleanTypeDefinition = context.deserialize_from_term(&term).unwrap();
        assert_eq!(
            UncleanTypeDefinition::Class(UncleanClassDefinition {
                documentation: OneOrMore::More(vec![]),
                metadata: None,
                key: None,
                is_subdocument: None,
                is_unfoldable: None,
                is_abstract: None,
                inherits: None,
                one_of: Some(vec![
                    UncleanOneOf {
                        choices: BTreeMap::from([
                            (
                                ShortName("a".to_string()),
                                UncleanFieldDefinition::Required(BaseOrDerived::Base(
                                    "string".to_string()
                                ))
                            ),
                            (
                                ShortName("b".to_string()),
                                UncleanFieldDefinition::Required(BaseOrDerived::Base(
                                    "integer".to_string()
                                ))
                            )
                        ])
                    },
                    UncleanOneOf {
                        choices: BTreeMap::from([
                            (
                                ShortName("c".to_string()),
                                UncleanFieldDefinition::Required(BaseOrDerived::Base(
                                    "string".to_string()
                                ))
                            ),
                            (
                                ShortName("d".to_string()),
                                UncleanFieldDefinition::Required(BaseOrDerived::Base(
                                    "integer".to_string()
                                ))
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
        let _frames: UncleanAllFrames = context.deserialize_from_term(&term).unwrap();

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
        let typedef: UncleanTypeDefinition = context.deserialize_from_term(&term).unwrap();

        assert_eq!(
            UncleanTypeDefinition::Enum(UncleanEnumDefinition {
                metadata: None,
                documentation: OneOrMore::One(UncleanEnumDocumentationDefinition {
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
                .iter()
                .map(|x| ShortName(x.to_string()))
                .collect()
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
        let _frames: UncleanAllFrames = context.deserialize_from_term(&term).unwrap();
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
        let _frames: UncleanAllFrames = context.deserialize_from_term(&term).unwrap();
        // at least it parses!
        // TODO test something here
    }

    #[test]
    fn invert_frames() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"json{
                   '@context': json{'@base':"terminusdb://woql/data/",'@schema':"http://terminusdb.com/schema/woql#",'@type':'Context'},
                   'Bar' : json{ '@type' : "Class", elt : "xsd:string" },
                   'Foo' : json{ '@type' : "Class", a : "Bar", b: json{ '@type' : "Optional", '@class' : "Bar"}, c: json{ '@type' : "Set", '@class' : "Bar"}}}"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let pre_allframes: UncleanAllFrames = context.deserialize_from_term(&term).unwrap();
        let allframes = pre_allframes.finalize();

        assert_eq!(
            allframes.inverted.classes[&GraphQLName(Cow::Owned("Bar".to_string()))].domain
                [&GraphQLName(Cow::Owned("_a_of_Foo".to_string()))]
                .class,
            GraphQLName(Cow::Owned("Foo".to_string()))
        );
        assert_eq!(
            allframes.inverted.classes[&GraphQLName(Cow::Owned("Bar".to_string()))].domain
                [&GraphQLName(Cow::Owned("_a_of_Foo".to_string()))]
                .kind,
            FieldKind::Required
        );
        assert_eq!(
            allframes.inverted.classes[&GraphQLName(Cow::Owned("Bar".to_string()))].domain
                [&GraphQLName(Cow::Owned("_b_of_Foo".to_string()))]
                .class,
            GraphQLName(Cow::Owned("Foo".to_string()))
        );
        assert_eq!(
            allframes.inverted.classes[&GraphQLName(Cow::Owned("Bar".to_string()))].domain
                [&GraphQLName(Cow::Owned("_b_of_Foo".to_string()))]
                .kind,
            FieldKind::Optional
        );
        assert_eq!(
            allframes.inverted.classes[&GraphQLName(Cow::Owned("Bar".to_string()))].domain
                [&GraphQLName(Cow::Owned("_c_of_Foo".to_string()))]
                .class,
            GraphQLName(Cow::Owned("Foo".to_string()))
        );
        assert_eq!(
            allframes.inverted.classes[&GraphQLName(Cow::Owned("Bar".to_string()))].domain
                [&GraphQLName(Cow::Owned("_c_of_Foo".to_string()))]
                .kind,
            FieldKind::Set
        )
    }

    #[test]
    fn complex_documentation() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"json{
                      '@context': json{
                         '@base': "terminusdb:///data/",
                         '@documentation': [
                              json{'@authors':["Gavin Mendel-Gleason"],
                                   '@description':"This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.",
                                   '@language':"en",
                                   '@title':"Example Schema"},
                              json{'@authors':["გავინ მენდელ-გლისონი"],
                                   '@description':"ეს არის მაგალითის სქემა. ჩვენ ვიყენებთ მას, რათა ვაჩვენოთ ინფორმაციის მრავალ ენაზე ჩვენების შესაძლებლობა ერთი და იმავე სემანტიკური შინაარსის შესახებ.",
                                   '@language':"ka",
                                   '@title':"მაგალითი სქემა"}
                         ],
                         '@schema': "terminusdb:///schema#",
                         '@type': "@context",
                         xsd: "http://www.w3.org/2001/XMLSchema#"
                      }}"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let pre_allframes: UncleanAllFrames = context.deserialize_from_term(&term).unwrap();
        let allframes = pre_allframes.finalize();
        assert_eq!(allframes.context.documentation,
                   OneOrMore::More(vec![
                       SchemaDocumentation {
                           title: Some("Example Schema".to_string()),
                           description: Some("This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.".to_string()),
                           authors: Some(vec!["Gavin Mendel-Gleason".to_string()]) },
                       SchemaDocumentation {
                           title: Some("მაგალითი სქემა".to_string()),
                           description: Some("ეს არის მაგალითის სქემა. ჩვენ ვიყენებთ მას, რათა ვაჩვენოთ ინფორმაციის მრავალ ენაზე ჩვენების შესაძლებლობა ერთი და იმავე სემანტიკური შინაარსის შესახებ.".to_string()),
                           authors: Some(vec!["გავინ მენდელ-გლისონი".to_string()])
                       }]))
    }
}
