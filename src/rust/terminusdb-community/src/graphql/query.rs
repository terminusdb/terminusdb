use itertools::Itertools;
use juniper::{self, FromInputValue, InputValue, ID};
use ordered_float::OrderedFloat;
use regex::{Regex, RegexSet};
use rug::Integer;
use terminusdb_store_prolog::terminus_store::structure::{Decimal, TdbDataType, TypedDictEntry};

use crate::consts::{RDF_FIRST, RDF_NIL, RDF_REST, SYS_VALUE};
use crate::path::iterator::{CachedClonableIterator, ClonableIterator};
use crate::path::{Path, Pred};
use crate::schema::RdfListIterator;
use crate::terminus_store::store::sync::SyncStoreLayer;

use crate::value::{base_type_kind, value_to_bigint, value_to_string, BaseTypeKind};
use crate::{consts::RDF_TYPE, terminus_store::*};

use super::filter::{
    BigFloatFilterInputObject, BigIntFilterInputObject, BooleanFilterInputObject,
    CollectionFilterInputObject, DateTimeFilterInputObject, EnumFilterInputObject,
    FilterInputObject, FloatFilterInputObject, IdFilterInputObject, IntFilterInputObject,
    StringFilterInputObject,
};
use super::frame::{
    is_base_type, AllFrames, ClassDefinition, CollectionKind, FieldKind, Prefixes, TypeDefinition,
};
use super::schema::{
    id_matches_restriction, BigFloat, BigInt, DateTime, GeneratedEnum, TerminusContext,
    TerminusOrderBy, TerminusOrdering,
};

use crate::path::compile::{compile_path, path_to_class};

use std::cmp::*;
use std::collections::{HashSet, VecDeque};
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum GenericOperation {
    Ne,
    Eq,
    Ge,
    Gt,
    Lt,
    Le,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum EnumOperation {
    Ne,
    Eq,
}

#[derive(Debug, Clone)]
enum TextOperation {
    Regex(Regex),
    StartsWith(String),
    AllOfTerms(Vec<String>),
    AnyOfTerms(Vec<String>),
}

#[derive(Debug, Clone, Copy)]
enum CollectionOperation {
    SomeHave,
    AllHave,
}

#[derive(Debug, Clone)]
enum IdOperation {
    Equals(String),
    OneOf(Vec<String>),
}

#[derive(Debug)]
struct FilterObject {
    restriction: Option<String>,
    edges: Vec<(String, FilterScope)>,
    ids: Vec<String>,
}

#[derive(Debug, Clone)]
enum FilterScope {
    Required(FilterObjectType),
    // Optional(OptionalOperation),
    Collection(CollectionKind, CollectionOperation, FilterObjectType),
    And(Vec<Rc<FilterObject>>),
    Or(Vec<Rc<FilterObject>>),
    Not(Rc<FilterObject>),
}

#[derive(Debug, Clone)]
enum FilterObjectType {
    Node(Rc<FilterObject>, String),
    Value(FilterValue),
}

#[derive(Debug, Clone)]
enum FilterValue {
    // op : TextOperation, type: String
    Text(TextOperation, String),
    // op : GenericOperation,  value : String, type: String
    SmallInt(GenericOperation, i32, String),
    Float(GenericOperation, f64, String),
    Boolean(GenericOperation, bool, String),
    BigInt(GenericOperation, BigInt, String),
    BigFloat(GenericOperation, BigFloat, String),
    DateTime(GenericOperation, DateTime, String),
    String(GenericOperation, String, String),
    Enum { op: EnumOperation, value: String },
    Foreign(IdOperation, String),
}

fn ordering_matches_op(ord: Ordering, op: GenericOperation) -> bool {
    match ord {
        Ordering::Less => {
            op == GenericOperation::Le || op == GenericOperation::Lt || op == GenericOperation::Ne
        }
        Ordering::Equal => {
            op == GenericOperation::Eq || op == GenericOperation::Le || op == GenericOperation::Ge
        }
        Ordering::Greater => {
            op == GenericOperation::Ge || op == GenericOperation::Gt || op == GenericOperation::Ne
        }
    }
}

/*

FilterObject{ edges:
  vec![
    ("friend", Collection(SomeHave,
                   FilterObject{ edges: vec![("name", Text(Eq,"Jim"))]}))
  ]
}
*/

fn compile_string_input_value(string_type: &str, value: StringFilterInputObject) -> FilterValue {
    if let Some(val) = value.eq {
        FilterValue::String(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.ne {
        FilterValue::String(GenericOperation::Ne, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterValue::String(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterValue::String(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterValue::String(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterValue::String(GenericOperation::Ge, val, string_type.to_string())
    } else if let Some(val) = value.regex {
        let regex = Regex::new(&val).expect("Could not compile regex");
        FilterValue::Text(TextOperation::Regex(regex), string_type.to_string())
    } else if let Some(val) = value.startsWith {
        FilterValue::Text(TextOperation::StartsWith(val), string_type.to_string())
    } else if let Some(val) = value.allOfTerms {
        FilterValue::Text(TextOperation::AllOfTerms(val), string_type.to_string())
    } else if let Some(val) = value.anyOfTerms {
        FilterValue::Text(TextOperation::AnyOfTerms(val), string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_big_int_input_value(string_type: &str, value: BigIntFilterInputObject) -> FilterValue {
    if let Some(val) = value.eq {
        FilterValue::BigInt(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.ne {
        FilterValue::BigInt(GenericOperation::Ne, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterValue::BigInt(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterValue::BigInt(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterValue::BigInt(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterValue::BigInt(GenericOperation::Ge, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_datetime_input_value(
    string_type: &str,
    value: DateTimeFilterInputObject,
) -> FilterValue {
    if let Some(val) = value.eq {
        FilterValue::DateTime(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.ne {
        FilterValue::DateTime(GenericOperation::Ne, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterValue::DateTime(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterValue::DateTime(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterValue::DateTime(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterValue::DateTime(GenericOperation::Ge, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_float_input_value(string_type: &str, value: FloatFilterInputObject) -> FilterValue {
    if let Some(val) = value.eq {
        FilterValue::Float(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.ne {
        FilterValue::Float(GenericOperation::Ne, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterValue::Float(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterValue::Float(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterValue::Float(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterValue::Float(GenericOperation::Ge, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_decimal_input_value(string_type: &str, value: BigFloatFilterInputObject) -> FilterValue {
    if let Some(val) = value.eq {
        FilterValue::BigFloat(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.ne {
        FilterValue::BigFloat(GenericOperation::Ne, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterValue::BigFloat(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterValue::BigFloat(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterValue::BigFloat(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterValue::BigFloat(GenericOperation::Ge, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_small_integer_input_value(
    string_type: &str,
    value: IntFilterInputObject,
) -> FilterValue {
    if let Some(val) = value.eq {
        FilterValue::SmallInt(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.ne {
        FilterValue::SmallInt(GenericOperation::Ne, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterValue::SmallInt(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterValue::SmallInt(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterValue::SmallInt(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterValue::SmallInt(GenericOperation::Ge, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_boolean_input_value(string_type: &str, value: BooleanFilterInputObject) -> FilterValue {
    if let Some(val) = value.eq {
        FilterValue::Boolean(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.ne {
        FilterValue::Boolean(GenericOperation::Ne, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_typed_filter(
    range: &str,
    all_frames: &AllFrames,
    spanning_input_value: &juniper::Spanning<InputValue>,
) -> FilterObjectType {
    if is_base_type(range) {
        match base_type_kind(&range[4..]) {
            BaseTypeKind::String => {
                let value = StringFilterInputObject::from_input_value(&spanning_input_value.item);
                FilterObjectType::Value(compile_string_input_value(range, value.unwrap()))
            }
            BaseTypeKind::SmallInteger => {
                let value = IntFilterInputObject::from_input_value(&spanning_input_value.item);
                FilterObjectType::Value(compile_small_integer_input_value(range, value.unwrap()))
            }
            BaseTypeKind::BigIntger => {
                let value = BigIntFilterInputObject::from_input_value(&spanning_input_value.item);
                FilterObjectType::Value(compile_big_int_input_value(range, value.unwrap()))
            }
            BaseTypeKind::Boolean => {
                let value = BooleanFilterInputObject::from_input_value(&spanning_input_value.item);
                FilterObjectType::Value(compile_boolean_input_value(range, value.unwrap()))
            }
            BaseTypeKind::DateTime => {
                let value = DateTimeFilterInputObject::from_input_value(&spanning_input_value.item);
                FilterObjectType::Value(compile_datetime_input_value(range, value.unwrap()))
            }
            BaseTypeKind::Float => {
                let value = FloatFilterInputObject::from_input_value(&spanning_input_value.item);
                FilterObjectType::Value(compile_float_input_value(range, value.unwrap()))
            }
            BaseTypeKind::Decimal => {
                let value = BigFloatFilterInputObject::from_input_value(&spanning_input_value.item);
                FilterObjectType::Value(compile_decimal_input_value(range, value.unwrap()))
            }
        }
    } else {
        match &all_frames.frames.get(range) {
            Some(TypeDefinition::Class(class_definition)) => {
                if let InputValue::Object(edges) = &spanning_input_value.item {
                    let inner = compile_edges_to_filter(range, all_frames, class_definition, edges);
                    FilterObjectType::Node(Rc::new(inner), range.to_string())
                } else {
                    panic!("object filter was not an object")
                }
            }
            Some(TypeDefinition::Enum(_)) => {
                let value =
                    EnumFilterInputObject::from_input_value(&spanning_input_value.item).unwrap();
                let encoded = all_frames.fully_qualified_enum_value(range, &value.enum_value.value);
                FilterObjectType::Value(FilterValue::Enum {
                    op: value.op,
                    value: encoded,
                })
            }
            None => {
                // it's a foreign
                let IdFilterInputObject { id, ids } =
                    IdFilterInputObject::from_input_value(&spanning_input_value.item).unwrap();
                let op = match (id, ids) {
                    (Some(id), _) => IdOperation::Equals(id.to_string()),
                    (_, Some(ids)) => {
                        IdOperation::OneOf(ids.into_iter().map(|id| id.to_string()).collect())
                    }
                    // TODO this should be a proper error
                    (None, None) => {
                        panic!("input document does not have either an id or a list of ids")
                    }
                };
                FilterObjectType::Value(FilterValue::Foreign(op, range.to_string()))
            }
        }
    }
}

fn compile_collection_filter(
    collection_filter: CollectionFilterInputObject,
    all_frames: &AllFrames,
    range: &str,
    kind: CollectionKind,
) -> FilterScope {
    let mut edges = collection_filter.edges;
    if let Some((op, next)) = edges.pop() {
        let operation = match op.item.as_str() {
            "someHave" => CollectionOperation::SomeHave,
            "allHave" => CollectionOperation::AllHave,
            _ => panic!("Unknown collection filter"),
        };
        let object_type = compile_typed_filter(range, all_frames, &next);
        FilterScope::Collection(kind, operation, object_type)
    } else {
        panic!("No operation for compiling collection filter")
    }
}

fn compile_edges_to_filter(
    class_name: &str,
    all_frames: &AllFrames,
    class_definition: &ClassDefinition,
    edges: &Vec<(juniper::Spanning<String>, juniper::Spanning<InputValue>)>,
) -> FilterObject {
    let mut result: Vec<(String, FilterScope)> = Vec::with_capacity(edges.len());
    let mut restriction = None;
    let mut ids = Vec::new();
    for (spanning_string, spanning_input_value) in edges.iter() {
        let field_name = &spanning_string.item;
        if field_name == "_and" {
            let input_value = &spanning_input_value.item;
            match input_value {
                InputValue::List(lst) => {
                    let mut result_vector = Vec::with_capacity(lst.len());
                    for spanning_elt in lst.iter() {
                        let elt = &spanning_elt.item;
                        match elt {
                            InputValue::Object(o) => {
                                result_vector.push(Rc::new(compile_edges_to_filter(
                                    class_name,
                                    all_frames,
                                    class_definition,
                                    o,
                                )))
                            }
                            _ => panic!("We should not have a non object in And-clause"),
                        };
                    }
                    result.push(("_and".to_string(), FilterScope::And(result_vector)))
                }
                _ => panic!("Invalid operand to and "),
            }
        } else if field_name == "_or" {
            let input_value = &spanning_input_value.item;
            match input_value {
                InputValue::List(lst) => {
                    let mut result_vector = Vec::with_capacity(lst.len());
                    for spanning_elt in lst.iter() {
                        let elt = &spanning_elt.item;
                        match elt {
                            InputValue::Object(o) => {
                                result_vector.push(Rc::new(compile_edges_to_filter(
                                    class_name,
                                    all_frames,
                                    class_definition,
                                    o,
                                )))
                            }
                            _ => panic!("We should not have a non object in And-clause"),
                        };
                    }
                    result.push(("_or".to_string(), FilterScope::Or(result_vector)))
                }
                _ => panic!("Invalid operand to and "),
            }
        } else if field_name == "_not" {
            let elt = &spanning_input_value.item;
            match elt {
                InputValue::Object(o) => result.push((
                    "_not".to_string(),
                    FilterScope::Not(Rc::new(compile_edges_to_filter(
                        class_name,
                        all_frames,
                        class_definition,
                        o,
                    ))),
                )),
                _ => panic!("We should not have a non object in And-clause"),
            }
        } else if field_name == "_restriction" {
            let input_value = &spanning_input_value.item;
            let expected: GeneratedEnum = GeneratedEnum::from_input_value(input_value)
                .expect("restriction value in filter was not a string");
            restriction = Some(expected.value);
        } else if field_name == "_ids" {
            if !ids.is_empty() {
                panic!("You must not specify '_id' and '_ids' simultaneously");
            }
            for id_span in spanning_input_value
                .item
                .to_list_value()
                .into_iter()
                .flatten()
            {
                let id = id_span
                    .as_string_value()
                    .expect("id to match on should have been a stringy value")
                    .to_owned();
                ids.push(id);
            }
        } else if field_name == "_id" {
            if !ids.is_empty() {
                panic!("You must not specify '_id' and '_ids' simultaneously");
            }
            let id = spanning_input_value
                .item
                .as_string_value()
                .expect("id to match on should have been a stringy value")
                .to_owned();
            ids.push(id);
        } else {
            let field = class_definition.resolve_field(field_name);
            let prefixes = &all_frames.context;
            let property = class_definition.fully_qualified_property_name(prefixes, field_name);
            let range = field.range();
            let kind = field.kind();
            match kind {
                FieldKind::Required | FieldKind::Optional => {
                    let res = compile_typed_filter(range, all_frames, spanning_input_value);
                    result.push((property.to_string(), FilterScope::Required(res)));
                }
                FieldKind::Set | FieldKind::List | FieldKind::Array | FieldKind::Cardinality => {
                    let value =
                        CollectionFilterInputObject::from_input_value(&spanning_input_value.item);
                    let kind = CollectionKind::try_from(kind).unwrap();
                    let filter_value =
                        compile_collection_filter(value.unwrap(), all_frames, range, kind);
                    result.push((property.to_string(), filter_value))
                }
            }
        }
    }
    FilterObject {
        restriction,
        edges: result,
        ids,
    }
}

fn compile_filter_object(
    class_name: &str,
    all_frames: &AllFrames,
    filter_input: &FilterInputObject,
) -> FilterObject {
    let class_definition: &ClassDefinition = all_frames.frames[class_name].as_class_definition();
    let edges = &filter_input.edges;
    compile_edges_to_filter(class_name, all_frames, class_definition, edges)
}

pub fn predicate_value_filter<'a, 'b>(
    g: &'a SyncStoreLayer,
    property: &'a str,
    object: ObjectType,
    iter: ClonableIterator<'a, u64>,
) -> ClonableIterator<'a, u64>
where
    'b: 'a,
{
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        let maybe_object_id = match object {
            ObjectType::Value(entry) => g.object_value_id(&entry),
            ObjectType::Node(node) => g.object_node_id(&node),
        };
        if let Some(object_id) = maybe_object_id {
            ClonableIterator::new(CachedClonableIterator::new(iter.filter(move |s| {
                g.triples_sp(*s, property_id).any(|t| t.object == object_id)
            })))
        } else {
            ClonableIterator::new(std::iter::empty())
        }
    } else {
        ClonableIterator::new(std::iter::empty())
    }
}

pub fn predicate_value_iter<'a>(
    g: &'a SyncStoreLayer,
    property: &'a str,
    object: &ObjectType,
) -> ClonableIterator<'a, u64> {
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        let maybe_object_id = match object {
            ObjectType::Value(entry) => g.object_value_id(entry),
            ObjectType::Node(node) => g.object_node_id(node),
        };
        if let Some(object_id) = maybe_object_id {
            ClonableIterator::new(CachedClonableIterator::new(
                g.triples_o(object_id)
                    .filter(move |t| t.predicate == property_id)
                    .map(|t| t.subject),
            ))
        } else {
            ClonableIterator::new(std::iter::empty())
        }
    } else {
        ClonableIterator::new(std::iter::empty())
    }
}

/*
Supposing we have a filter query of the following form:

{ ty1_edge1 : { startsWith : "blah"},
  ty1_edge2 : { allHave : { ty2_edge1 : { eq : "Test" }}}}
  ty1_edge3 : { ty3_edge1 : {le : 12 }}
  or : [{ ty1_edge4 : { eq : "left"}},
        { ty2_edge5 : { eq : "right"}}]}

 */

fn object_type_filter<'a>(
    g: &'a SyncStoreLayer,
    filter_type: &'a FilterValue,
    iter: ClonableIterator<'a, u64>,
) -> ClonableIterator<'a, u64> {
    match filter_type {
        FilterValue::Text(operation, _) => match operation {
            TextOperation::Regex(regex) => {
                let regex = regex.clone();
                let g = g.clone();
                ClonableIterator::new(iter.filter(move |object| {
                    let string = g
                        .id_object_value(*object)
                        .unwrap()
                        .as_val::<String, String>();
                    regex.is_match(&string)
                }))
            }
            TextOperation::StartsWith(string) => {
                let escaped = &regex::escape(string);
                let regex = Regex::new(&format!("^{escaped}"))
                    .expect("Regex should always be valid due to escaping");
                let g = g.clone();
                ClonableIterator::new(iter.filter(move |object| {
                    let string = g
                        .id_object_value(*object)
                        .unwrap()
                        .as_val::<String, String>();
                    regex.is_match(&string)
                }))
            }
            TextOperation::AllOfTerms(vec) => {
                let regexs: Vec<_> = vec.iter().map(|s| regex::escape(s)).collect();
                let regexset =
                    RegexSet::new(&regexs).expect("Regex should always be valid due to escaping");
                let length = regexs.len();
                let g = g.clone();
                ClonableIterator::new(iter.filter(move |object| {
                    let string = g
                        .id_object_value(*object)
                        .unwrap()
                        .as_val::<String, String>();
                    length == regexset.matches(&string).into_iter().count()
                }))
            }
            TextOperation::AnyOfTerms(vec) => {
                let regexs: Vec<_> = vec.iter().map(|s| regex::escape(s)).collect();
                let regexset =
                    RegexSet::new(regexs).expect("Regex should always be valid due to escaping");
                let g = g.clone();
                ClonableIterator::new(iter.filter(move |object| {
                    let string = g
                        .id_object_value(*object)
                        .unwrap()
                        .as_val::<String, String>();
                    regexset.is_match(&string)
                }))
            }
        },
        FilterValue::SmallInt(op, i, _) => {
            let op = *op;
            let i = *i;
            let h = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = h.id_object_value(*object).expect("Object value must exist");
                let object_int = object_value.as_i32().expect("not a numerical value");
                let cmp = object_int.cmp(&i);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterValue::Float(op, f, _) => {
            let op = *op;
            let f = *f;
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_float = object_value.as_val::<f64, f64>();
                let cmp = OrderedFloat(object_float).cmp(&OrderedFloat(f));
                ordering_matches_op(cmp, op)
            }))
        }
        FilterValue::Boolean(op, b, _) => {
            let op = *op;
            let b = *b;
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_bool = object_value.as_val::<bool, bool>();
                let cmp = object_bool.cmp(&b);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterValue::BigInt(op, BigInt(bigint), _) => {
            let op = *op;
            let val = bigint.parse::<Integer>().unwrap();
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_int = value_to_bigint(&object_value);
                let cmp = object_int.cmp(&val);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterValue::BigFloat(op, BigFloat(decimal), _) => {
            let op = *op;
            let val = Decimal::make_entry(&Decimal::new(decimal.to_string()).unwrap());
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let cmp = object_value.cmp(&val);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterValue::DateTime(op, DateTime(val), _) => {
            let op = *op;
            let val = val.clone();
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_string = value_to_string(&object_value).to_string();
                // Datetimes are reverse order: newer is bigger, but less!
                let cmp = val.cmp(&object_string);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterValue::String(op, val, _) => {
            let op = *op;
            let val = val.clone();
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_string = value_to_string(&object_value).to_string();
                let cmp = object_string.cmp(&val);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterValue::Enum { op, value } => {
            let op = *op;
            match g.object_node_id(value) {
                Some(object_id) => ClonableIterator::new(iter.filter(move |object| match op {
                    EnumOperation::Eq => *object == object_id,
                    EnumOperation::Ne => *object != object_id,
                })),
                None => match op {
                    EnumOperation::Eq => ClonableIterator::new(std::iter::empty()),
                    EnumOperation::Ne => iter,
                },
            }
        }
        FilterValue::Foreign(op, _) => match op {
            IdOperation::Equals(val) => match g.object_node_id(val) {
                Some(object_id) => {
                    ClonableIterator::new(iter.filter(move |object| *object == object_id))
                }
                None => ClonableIterator::new(std::iter::empty()),
            },
            IdOperation::OneOf(vals) => {
                let object_ids: Vec<_> = vals
                    .iter()
                    .filter_map(|val| g.object_node_id(val))
                    .collect();
                if object_ids.is_empty() {
                    ClonableIterator::new(std::iter::empty())
                } else {
                    ClonableIterator::new(iter.filter(move |object| object_ids.contains(object)))
                }
            }
        },
    }
}

fn compile_query<'a>(
    context: &'a TerminusContext<'static>,
    g: &'a SyncStoreLayer,
    filter: Rc<FilterObject>,
    iter: ClonableIterator<'a, u64>,
) -> ClonableIterator<'a, u64> {
    let mut iter = iter;
    if !filter.ids.is_empty() {
        let resolved_ids: Vec<_> = filter.ids.iter().flat_map(|s| g.subject_id(s)).collect();
        iter = ClonableIterator::new(iter.filter(move |id| resolved_ids.contains(id)));
    }
    if let Some(restriction_name) = filter.restriction.clone() {
        iter = ClonableIterator::new(iter.filter(move |id| {
            id_matches_restriction(context, &restriction_name, *id)
                .unwrap()
                .is_some()
        }));
    }
    for (predicate, filter) in filter.edges.iter() {
        match filter {
            FilterScope::And(vec) => {
                for filter in vec.iter() {
                    iter = compile_query(context, g, filter.clone(), iter)
                }
            }
            FilterScope::Or(vec) => {
                // iter - iter1 +
                //     \ - iter2 +
                //      \ - iter3
                //
                let initial_vector = iter.collect::<Vec<u64>>();
                //or_iter = ClonableIterator::new(std::iter::empty());
                iter = ClonableIterator::new(vec.clone().into_iter().flat_map(move |filter| {
                    let iter_copy = ClonableIterator::new(initial_vector.clone().into_iter());
                    compile_query(context, g, filter, iter_copy)
                }));
            }
            FilterScope::Not(filter) => {
                //  A = iter.collect()
                //  B = sub_iter
                //  C = A \ B
                let initial_set: HashSet<u64> = iter.collect();
                let initial_iter = ClonableIterator::new(CachedClonableIterator::new(
                    initial_set.clone().into_iter(),
                ));
                let sub_iter: HashSet<u64> =
                    compile_query(context, g, filter.clone(), initial_iter).collect();

                let result = &initial_set - &sub_iter;
                iter = ClonableIterator::new(CachedClonableIterator::new(result.into_iter()));
            }
            FilterScope::Required(object_type) => {
                let maybe_property_id = g.predicate_id(predicate);
                if let Some(property_id) = maybe_property_id {
                    match object_type {
                        FilterObjectType::Node(sub_filter, _) => {
                            let sub_filter = sub_filter.clone();
                            iter = ClonableIterator::new(iter.filter(move |subject| {
                                let objects = ClonableIterator::new(
                                    g.single_triple_sp(*subject, property_id)
                                        .map(|t| t.object)
                                        .into_iter(),
                                );
                                compile_query(context, g, sub_filter.clone(), objects)
                                    .next()
                                    .is_some()
                            }))
                        }
                        FilterObjectType::Value(filter_type) => {
                            let filter_type = filter_type.clone();
                            iter = ClonableIterator::new(iter.filter(move |subject| {
                                let object_iter = ClonableIterator::new(
                                    g.single_triple_sp(*subject, property_id)
                                        .map(|t| t.object)
                                        .into_iter(),
                                );
                                object_type_filter(g, &filter_type, object_iter)
                                    .next()
                                    .is_some()
                            }))
                        }
                    }
                } else {
                    return ClonableIterator::new(std::iter::empty());
                }
            }
            FilterScope::Collection(kind, op, o) => {
                // TODO: Filtering broken on lists and arrays
                let kind = kind.clone();
                let maybe_property_id = g.predicate_id(predicate);
                if let Some(property_id) = maybe_property_id {
                    match op {
                        CollectionOperation::SomeHave => match o {
                            FilterObjectType::Node(sub_filter, _) => {
                                let sub_filter = sub_filter.clone();
                                iter = ClonableIterator::new(iter.filter(move |subject| {
                                    let objects =
                                        collection_kind_iterator(g, kind, *subject, property_id);
                                    compile_query(context, g, sub_filter.clone(), objects)
                                        .next()
                                        .is_some()
                                }));
                            }
                            FilterObjectType::Value(filter_type) => {
                                let filter_type = filter_type.clone();
                                iter = ClonableIterator::new(iter.filter(move |subject| {
                                    let objects =
                                        collection_kind_iterator(g, kind, *subject, property_id);
                                    object_type_filter(g, &filter_type, objects)
                                        .next()
                                        .is_some()
                                }));
                            }
                        },
                        CollectionOperation::AllHave => match o {
                            FilterObjectType::Node(sub_filter, _) => {
                                let sub_filter = sub_filter.clone();
                                iter = ClonableIterator::new(iter.filter(move |subject| {
                                    let objects =
                                        collection_kind_iterator(g, kind, *subject, property_id);
                                    compile_query(context, g, sub_filter.clone(), objects)
                                        .all(|_| true)
                                }));
                            }
                            FilterObjectType::Value(filter_type) => {
                                let filter_type = filter_type.clone();
                                iter = ClonableIterator::new(iter.filter(move |subject| {
                                    let objects =
                                        collection_kind_iterator(g, kind, *subject, property_id);
                                    object_type_filter(g, &filter_type, objects).all(|_| true)
                                }));
                            }
                        },
                    }
                } else {
                    return ClonableIterator::new(std::iter::empty());
                }
            }
        }
    }
    iter
}

fn collection_kind_iterator(
    g: &SyncStoreLayer,
    kind: CollectionKind,
    subject: u64,
    property_id: u64,
) -> ClonableIterator<u64> {
    match kind {
        CollectionKind::Property => ClonableIterator::new(CachedClonableIterator::new(
            g.triples_sp(subject, property_id).map(|t| t.object),
        )),
        CollectionKind::List => {
            let opt_o = g.single_triple_sp(subject, property_id).map(|t| t.object);
            match opt_o {
                Some(list_id) => {
                    ClonableIterator::new(CachedClonableIterator::new(RdfListIterator {
                        layer: g,
                        cur: list_id,
                        rdf_first_id: g.predicate_id(RDF_FIRST),
                        rdf_rest_id: g.predicate_id(RDF_REST),
                        rdf_nil_id: g.subject_id(RDF_NIL),
                    }))
                }
                None => ClonableIterator::new(std::iter::empty()),
            }
        }
        CollectionKind::Array => match g.predicate_id(SYS_VALUE) {
            None => ClonableIterator::new(std::iter::empty()),
            Some(sys_value) => {
                let g = g.clone();
                ClonableIterator::new(CachedClonableIterator::new(
                    g.triples_sp(subject, property_id)
                        .filter_map(move |t| g.single_triple_sp(t.object, sys_value))
                        .map(|t| t.object),
                ))
            }
        },
    }
}

#[derive(Clone, Debug, Copy)]
enum PathEdgeType<'a> {
    Property(&'a str),
    List(&'a str),
    Array(&'a str),
}

impl<'a> std::ops::Deref for PathEdgeType<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            PathEdgeType::Property(s) => s,
            PathEdgeType::List(s) => s,
            PathEdgeType::Array(s) => s,
        }
    }
}

fn generate_iterator_from_filter<'a>(
    g: &'a SyncStoreLayer,
    class_name: &'a str,
    all_frames: &AllFrames,
    filter_opt: Option<&FilterObject>,
    includes_children: bool,
) -> Option<ClonableIterator<'a, u64>> {
    let filter = filter_opt?;
    let mut iter = None;
    let mut visit_next: VecDeque<(Vec<PathEdgeType>, &FilterObject)> = VecDeque::new();
    visit_next.push_back((vec![], filter));
    while let Some(next) = visit_next.pop_front() {
        if !next.1.ids.is_empty() {
            // amazing we found something.
            let ids: Vec<_> = next.1.ids.iter().flat_map(|id| g.subject_id(id)).collect();
            iter = Some(iterator_from_path_and_ids(
                g,
                &all_frames.context,
                next.0,
                ids.into_iter(),
            ));
            break;
        } else if let Some(it) = generate_iterator_from_edges(g, &all_frames.context, &next) {
            iter = Some(it);
            break;
        } else {
            // nothing here :( push children.
            for e in next.1.edges.iter() {
                let mut path = next.0.clone();
                match &e.1 {
                    FilterScope::Required(FilterObjectType::Node(next_f, _)) |
                    // We need to do something clever with collection type here.
                    FilterScope::Collection(
                        _,
                        CollectionOperation::SomeHave,
                        FilterObjectType::Node(next_f, _),
                    ) => {
                        let kind = e.1.kind().unwrap();
                        path.push(PathEdgeType::new(&e.0, kind));
                        visit_next.push_back((path, &next_f))
                    },
                    FilterScope::And(next_fs) => {
                        for next_f in next_fs.iter() {
                            visit_next.push_back((path.clone(), next_f));
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    if let Some(iter) = iter {
        // we have collected a path leading up to this point. Let's filter it down to the expected type.
        let expanded_type_name = all_frames.fully_qualified_class_name(class_name);
        if includes_children {
            let correct_types: Vec<_> = all_frames
                .subsumed(class_name)
                .into_iter()
                .map(|c| all_frames.fully_qualified_class_name(&c))
                .flat_map(|id| g.subject_id(&id))
                .collect();
            let rdf_type_id = g.predicate_id(RDF_TYPE)?;
            Some(ClonableIterator::new(iter.filter(move |id| {
                match g.single_triple_sp(*id, rdf_type_id) {
                    Some(t) => correct_types.contains(&t.object),
                    None => false,
                }
            })))
        } else {
            Some(predicate_value_filter(
                g,
                RDF_TYPE,
                ObjectType::Node(expanded_type_name),
                iter,
            ))
        }
    } else {
        // no path was found. we default to None
        None
    }
}

fn path_from_components(pet: PathEdgeType) -> Path {
    match pet {
        PathEdgeType::Property(p) => Path::Negative(Pred::Named(p.to_string())),
        PathEdgeType::List(p) => {
            let path = Path::Seq(vec![
                Path::Positive(Pred::Named(p.to_string())),
                Path::Star(Rc::new(Path::Positive(Pred::Named(RDF_REST.to_string())))),
                Path::Positive(Pred::Named(RDF_FIRST.to_string())),
            ]);
            path.reverse()
        }
        PathEdgeType::Array(p) => Path::Seq(vec![
            Path::Negative(Pred::Named(SYS_VALUE.to_string())),
            Path::Negative(Pred::Named(p.to_string())),
        ]),
    }
}

fn iterator_from_path_and_ids<'a>(
    g: &'a SyncStoreLayer,
    prefixes: &Prefixes,
    components: Vec<PathEdgeType>,
    ids: impl Iterator<Item = u64> + 'a + Clone,
) -> ClonableIterator<'a, u64> {
    let components: Vec<_> = components
        .into_iter()
        .rev()
        .map(path_from_components)
        .collect();
    let path = Path::Seq(components);
    eprintln!("path: {path:?}");
    ClonableIterator::new(
        compile_path(g, prefixes.clone(), path, ClonableIterator::new(ids)).unique(),
    )
}

fn filter_value_to_entry(value: &FilterValue) -> Option<TypedDictEntry> {
    match value {
        FilterValue::String(GenericOperation::Eq, val, _) => Some(String::make_entry(val)),
        FilterValue::SmallInt(GenericOperation::Eq, val, _) => Some(i32::make_entry(val)),
        FilterValue::Float(GenericOperation::Eq, val, _) => Some(f64::make_entry(val)),
        FilterValue::Boolean(GenericOperation::Eq, val, _) => Some(bool::make_entry(val)),
        FilterValue::BigInt(GenericOperation::Eq, val, _) => val
            .0
            .parse::<Integer>()
            .ok()
            .map(|parsed| Integer::make_entry(&parsed)),
        FilterValue::BigFloat(GenericOperation::Eq, val, _) => Decimal::new(val.0.clone())
            .ok()
            .map(|parsed| Decimal::make_entry(&parsed)),
        _ => None,
    }
}

impl FilterScope {
    fn kind(&self) -> Option<CollectionKind> {
        match &self {
            FilterScope::Required(_) => Some(CollectionKind::Property),
            FilterScope::Collection(kind, _, _) => Some(*kind),
            FilterScope::And(_) => None,
            FilterScope::Or(_) => None,
            FilterScope::Not(_) => None,
        }
    }
}

impl<'a> PathEdgeType<'a> {
    fn new(name: &'a str, ck: CollectionKind) -> Self {
        match ck {
            CollectionKind::Property => Self::Property(name),
            CollectionKind::List => Self::List(name),
            CollectionKind::Array => Self::Array(name),
        }
    }
}

fn generate_iterator_from_edges<'a, 'b>(
    g: &'a SyncStoreLayer,
    prefixes: &Prefixes,
    cur: &(Vec<PathEdgeType<'b>>, &FilterObject),
) -> Option<ClonableIterator<'a, u64>> {
    for (name, e) in cur.1.edges.iter() {
        match e {
            FilterScope::Required(FilterObjectType::Value(value))
            | FilterScope::Collection(_, _, FilterObjectType::Value(value)) => {
                let kind = e.kind().unwrap();
                if let Some(entry) = filter_value_to_entry(value) {
                    let id_opt = g.object_value_id(&entry);
                    if id_opt.is_none() {
                        continue;
                    }
                    let id = id_opt.unwrap();
                    let mut components = cur.0.clone();
                    components.push(PathEdgeType::new(name, kind));

                    return Some(iterator_from_path_and_ids(
                        g,
                        prefixes,
                        components,
                        [id].into_iter(),
                    ));
                }
            }
            _ => {}
        }
    }

    None
}

fn generate_initial_iterator<'a>(
    g: &'a SyncStoreLayer,
    class_name: &'a str,
    all_frames: &AllFrames,
    filter_opt: Option<FilterObject>,
    zero_iter: Option<ClonableIterator<'a, u64>>,
    includes_children: bool,
) -> (Option<FilterObject>, ClonableIterator<'a, u64>) {
    match zero_iter {
        None => {
            // If we have a filter here, we probably need to use it.
            match generate_iterator_from_filter(
                g,
                class_name,
                all_frames,
                filter_opt.as_ref(),
                includes_children,
            ) {
                Some(zi) => (filter_opt, zi),
                None => {
                    let subsuming = if includes_children {
                        all_frames.subsumed(class_name)
                    } else {
                        vec![class_name.to_string()]
                    };
                    let mut iter = ClonableIterator::new(std::iter::empty());
                    for sub_class in subsuming {
                        let sub_class_expanded = all_frames.fully_qualified_class_name(&sub_class);
                        let next = predicate_value_iter(
                            g,
                            RDF_TYPE,
                            &ObjectType::Node(sub_class_expanded),
                        );
                        iter = ClonableIterator::new(iter.chain(next))
                    }
                    (filter_opt, iter)
                }
            }
        }
        Some(zi) => (filter_opt, zi),
    }
}

fn lookup_by_filter<'a>(
    context: &'a TerminusContext<'static>,
    g: &'a SyncStoreLayer,
    class_name: &'a str,
    all_frames: &AllFrames,
    filter_opt: Option<FilterObject>,
    zero_iter: Option<ClonableIterator<'a, u64>>,
    includes_children: bool,
) -> ClonableIterator<'a, u64> {
    let (continuation_filter_opt, iterator) = generate_initial_iterator(
        g,
        class_name,
        all_frames,
        filter_opt,
        zero_iter,
        includes_children,
    );
    if let Some(continuation_filter) = continuation_filter_opt {
        let continuation_filter = Rc::new(continuation_filter);
        compile_query(context, g, continuation_filter, iterator)
    } else {
        iterator
    }
}

pub fn run_filter_query<'a>(
    context: &'a TerminusContext<'static>,
    g: &'a SyncStoreLayer,
    prefixes: &'a Prefixes,
    arguments: &'a juniper::Arguments,
    class_name: &'a str,
    all_frames: &'a AllFrames,
    zero_iter: Option<ClonableIterator<'a, u64>>,
) -> Vec<u64> {
    let new_zero_iter: Option<ClonableIterator<'a, u64>> =
        match (arguments.get::<ID>("id"), arguments.get::<Vec<ID>>("ids")) {
            (Some(id_string), None) => match zero_iter {
                None => {
                    let expanded_type_name = all_frames.fully_qualified_class_name(class_name);
                    Some(predicate_value_filter(
                        g,
                        RDF_TYPE,
                        ObjectType::Node(expanded_type_name),
                        ClonableIterator::new(g.subject_id(&id_string).into_iter()),
                    ))
                }
                Some(zi) => Some(ClonableIterator::new(zi.filter(move |i| {
                    g.subject_id(&id_string).into_iter().any(|id| *i == id)
                }))),
            },
            (None, Some(id_vec)) => match zero_iter {
                None => {
                    let expanded_type_name = all_frames.fully_qualified_class_name(class_name);
                    Some(predicate_value_filter(
                        g,
                        RDF_TYPE,
                        ObjectType::Node(expanded_type_name),
                        ClonableIterator::new(id_vec.into_iter().flat_map(|id| g.subject_id(&id))),
                    ))
                }
                Some(zi) => {
                    let id_vec: Vec<String> = id_vec.into_iter().map(|id| id.to_string()).collect();
                    Some(ClonableIterator::new(zi.filter(move |i| {
                        id_vec
                            .clone()
                            .into_iter()
                            .flat_map(|id| g.subject_id(&id))
                            .any(|id| *i == id)
                    })))
                }
            },
            (Some(_), Some(_)) => panic!("You must not specify 'id' and 'ids' simultaneously"),
            (None, None) => zero_iter,
        };

    let new_zero_iter: Option<ClonableIterator<'a, u64>> =
        if let Some(path_string) = arguments.get::<String>("path") {
            if let Some(zi) = new_zero_iter {
                Some(path_to_class(
                    &path_string,
                    g,
                    class_name,
                    all_frames,
                    ClonableIterator::new(zi),
                ))
            } else {
                panic!("We need some starting id for our path");
            }
        } else {
            new_zero_iter
        };
    let offset: i32 = arguments.get("offset").unwrap_or(0);
    let limit: Option<i32> = arguments.get("limit");
    let filter_arg_opt: Option<FilterInputObject> = arguments.get("filter");
    let filter = filter_arg_opt
        .map(|filter_input| compile_filter_object(class_name, all_frames, &filter_input));
    let includes_children = include_children(arguments);
    let it: ClonableIterator<'a, u64> =
        if let Some(TerminusOrderBy { fields }) = arguments.get::<TerminusOrderBy>("orderBy") {
            let mut results: Vec<u64> = lookup_by_filter(
                context,
                g,
                class_name,
                all_frames,
                filter,
                new_zero_iter,
                includes_children,
            )
            .unique()
            .collect();
            results.sort_by_cached_key(|id| create_query_order_key(g, prefixes, *id, &fields));
            // Probs should not be into_iter(), done to satisfy both arms of let symmetry
            // better to borrow in the other branch?
            ClonableIterator::new(
                results
                    .into_iter()
                    .skip(usize::try_from(offset).unwrap_or(0)),
            )
        } else {
            ClonableIterator::new(
                lookup_by_filter(
                    context,
                    g,
                    class_name,
                    all_frames,
                    filter,
                    new_zero_iter,
                    includes_children,
                )
                .skip(usize::try_from(offset).unwrap_or(0)),
            )
        };

    if let Some(limit) = limit {
        it.take(usize::try_from(limit).unwrap_or(0)).collect()
    } else {
        it.collect()
    }
}

fn include_children(arguments: &juniper::Arguments) -> bool {
    arguments.get("include_children").unwrap_or(true)
}

fn create_query_order_key(
    g: &SyncStoreLayer,
    p: &Prefixes,
    id: u64,
    order_desc: &[(String, TerminusOrdering)],
) -> QueryOrderKey {
    let vec: Vec<_> = order_desc
        .iter()
        .filter_map(|(property, ordering)| {
            let predicate = p.expand_schema(property);
            let predicate_id = g.predicate_id(&predicate)?;
            let res = g.single_triple_sp(id, predicate_id).map(move |t| {
                g.id_object(t.object)
                    .expect("This object must exist")
                    .value()
                    .unwrap()
            });
            Some((res, *ordering))
        })
        .collect();

    QueryOrderKey { vec }
}

struct QueryOrderKey {
    vec: Vec<(Option<TypedDictEntry>, TerminusOrdering)>,
}

impl PartialEq for QueryOrderKey {
    fn eq(&self, other: &QueryOrderKey) -> bool {
        for i in 0..self.vec.len() {
            let (option_value, _) = &self.vec[i];
            let (other_value, _) = &other.vec[i];
            let res = match option_value {
                Some(tde) => {
                    Ordering::Equal
                        == (match other_value {
                            None => Ordering::Greater,
                            Some(value) => tde.cmp(value),
                        })
                }
                x => x == other_value,
            };
            if !res {
                return false;
            }
        }
        true
    }
}

impl Eq for QueryOrderKey {}

impl Ord for QueryOrderKey {
    fn cmp(&self, other: &QueryOrderKey) -> Ordering {
        for i in 0..self.vec.len() {
            let (option_value, order) = &self.vec[i];
            let (other_value, _) = &other.vec[i];
            let res = match option_value {
                Some(tde) => match other_value {
                    None => Ordering::Greater,
                    Some(value) => tde.cmp(value),
                },
                None => match other_value {
                    Some(_) => Ordering::Less,
                    None => Ordering::Equal,
                },
            };

            let final_order = match order {
                TerminusOrdering::Desc => res.reverse(),
                TerminusOrdering::Asc => res,
            };

            if !final_order.is_eq() {
                return final_order;
            }
        }
        Ordering::Equal
    }
}

impl PartialOrd for QueryOrderKey {
    fn partial_cmp(&self, other: &QueryOrderKey) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
