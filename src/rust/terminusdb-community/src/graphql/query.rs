use itertools::Itertools;
use juniper::{self, DefaultScalarValue, FromInputValue, InputValue, ScalarValue, ID};
use ordered_float::OrderedFloat;
use regex::{Regex, RegexSet};
use rug::Integer;

use crate::path::iterator::{CachedClonableIterator, ClonableIterator};
use crate::terminus_store::store::sync::SyncStoreLayer;

use crate::value::{
    base_type_kind, value_string_to_bigint, value_string_to_bool, value_string_to_number,
    value_string_to_string, BaseTypeKind,
};
use crate::{consts::RDF_TYPE, terminus_store::*, value::value_string_to_graphql};

use super::filter::{
    BigIntFilterInputObject, BooleanFilterInputObject, CollectionFilterInputObject,
    DateTimeFilterInputObject, EnumFilterInputObject, FilterInputObject, FloatFilterInputObject,
    IntFilterInputObject, StringFilterInputObject,
};
use super::frame::{is_base_type, AllFrames, ClassDefinition, FieldKind, Prefixes, TypeDefinition};
use super::schema::{BigInt, DateTime, TerminusOrderBy, TerminusOrdering};

use crate::path::compile::path_to_class;

use float_ord::FloatOrd;

use std::cmp::*;
use std::collections::HashSet;
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

#[derive(Debug)]
enum CollectionOperation {
    SomeHave,
    AllHave,
}

#[derive(Debug, Clone)]
enum ObjectType {
    Node(Rc<FilterObject>, String),
    Value(FilterType),
}

#[derive(Debug, Clone)]
enum FilterType {
    // op : TextOperation, type: String
    Text(TextOperation, String),
    // op : GenericOperation,  value : String, type: String
    SmallInt(GenericOperation, i32, String),
    Float(GenericOperation, f64, String),
    Boolean(GenericOperation, bool, String),
    BigInt(GenericOperation, BigInt, String),
    DateTime(GenericOperation, DateTime, String),
    String(GenericOperation, String, String),
    Enum { op: EnumOperation, value: String },
}

#[derive(Debug)]
enum FilterValue {
    Required(ObjectType),
    // Optional(OptionalOperation),
    Collection(CollectionOperation, ObjectType),
    And(Vec<Rc<FilterObject>>),
    Or(Vec<Rc<FilterObject>>),
    Not(Rc<FilterObject>),
}

#[derive(Debug)]
struct FilterObject {
    edges: Vec<(String, FilterValue)>,
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

fn compile_string_input_value(string_type: &str, value: StringFilterInputObject) -> FilterType {
    if let Some(val) = value.eq {
        FilterType::String(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterType::String(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterType::String(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterType::String(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterType::String(GenericOperation::Ge, val, string_type.to_string())
    } else if let Some(val) = value.regex {
        let regex = Regex::new(&val).expect("Could not compile regex");
        FilterType::Text(TextOperation::Regex(regex), string_type.to_string())
    } else if let Some(val) = value.startsWith {
        FilterType::Text(TextOperation::StartsWith(val), string_type.to_string())
    } else if let Some(val) = value.allOfTerms {
        FilterType::Text(TextOperation::AllOfTerms(val), string_type.to_string())
    } else if let Some(val) = value.anyOfTerms {
        FilterType::Text(TextOperation::AnyOfTerms(val), string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_big_int_input_value(string_type: &str, value: BigIntFilterInputObject) -> FilterType {
    if let Some(val) = value.eq {
        FilterType::BigInt(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterType::BigInt(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterType::BigInt(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterType::BigInt(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterType::BigInt(GenericOperation::Ge, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_datetime_input_value(string_type: &str, value: DateTimeFilterInputObject) -> FilterType {
    if let Some(val) = value.eq {
        FilterType::DateTime(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterType::DateTime(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterType::DateTime(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterType::DateTime(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterType::DateTime(GenericOperation::Ge, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_float_input_value(string_type: &str, value: FloatFilterInputObject) -> FilterType {
    if let Some(val) = value.eq {
        FilterType::Float(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterType::Float(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterType::Float(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterType::Float(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterType::Float(GenericOperation::Ge, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_small_integer_input_value(string_type: &str, value: IntFilterInputObject) -> FilterType {
    if let Some(val) = value.eq {
        FilterType::SmallInt(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterType::SmallInt(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterType::SmallInt(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterType::SmallInt(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterType::SmallInt(GenericOperation::Ge, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_boolean_input_value(string_type: &str, value: BooleanFilterInputObject) -> FilterType {
    if let Some(val) = value.eq {
        FilterType::Boolean(GenericOperation::Eq, val, string_type.to_string())
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_typed_filter(
    range: &str,
    all_frames: &AllFrames,
    spanning_input_value: &juniper::Spanning<InputValue>,
) -> ObjectType {
    if is_base_type(range) {
        match base_type_kind(&range[4..]) {
            BaseTypeKind::String => {
                let value = StringFilterInputObject::from_input_value(&spanning_input_value.item);
                ObjectType::Value(compile_string_input_value(range, value.unwrap()))
            }
            BaseTypeKind::SmallInteger => {
                let value = IntFilterInputObject::from_input_value(&spanning_input_value.item);
                ObjectType::Value(compile_small_integer_input_value(range, value.unwrap()))
            }
            BaseTypeKind::BigIntger => {
                let value = BigIntFilterInputObject::from_input_value(&spanning_input_value.item);
                ObjectType::Value(compile_big_int_input_value(range, value.unwrap()))
            }
            BaseTypeKind::Boolean => {
                let value = BooleanFilterInputObject::from_input_value(&spanning_input_value.item);
                ObjectType::Value(compile_boolean_input_value(range, value.unwrap()))
            }
            BaseTypeKind::DateTime => {
                let value = DateTimeFilterInputObject::from_input_value(&spanning_input_value.item);
                ObjectType::Value(compile_datetime_input_value(range, value.unwrap()))
            }
            BaseTypeKind::Float => {
                let value = FloatFilterInputObject::from_input_value(&spanning_input_value.item);
                ObjectType::Value(compile_float_input_value(range, value.unwrap()))
            }
        }
    } else {
        match &all_frames.frames[range] {
            TypeDefinition::Class(class_definition) => {
                if let InputValue::Object(edges) = &spanning_input_value.item {
                    let inner =
                        compile_edges_to_filter(range, all_frames, class_definition, &edges);
                    ObjectType::Node(Rc::new(inner), range.to_string())
                } else {
                    panic!()
                }
            }
            TypeDefinition::Enum(_) => {
                let value =
                    EnumFilterInputObject::from_input_value(&spanning_input_value.item).unwrap();
                let encoded = all_frames.fully_qualified_enum_value(range, &value.enum_value.value);
                ObjectType::Value(FilterType::Enum {
                    op: value.op,
                    value: encoded,
                })
            }
        }
    }
}

fn compile_collection_filter(
    collection_filter: CollectionFilterInputObject,
    all_frames: &AllFrames,
    range: &str,
) -> FilterValue {
    let mut edges = collection_filter.edges;
    if let Some((op, next)) = edges.pop() {
        let operation = match op.item.as_str() {
            "someHave" => CollectionOperation::SomeHave,
            "allHave" => CollectionOperation::AllHave,
            _ => panic!("Unknown collection filter"),
        };
        let object_type = compile_typed_filter(range, all_frames, &next);
        FilterValue::Collection(operation, object_type)
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
    let mut result: Vec<(String, FilterValue)> = Vec::with_capacity(edges.len());
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
                                    &o,
                                )))
                            }
                            _ => panic!("We should not have a non object in And-clause"),
                        };
                    }
                    result.push(("_and".to_string(), FilterValue::And(result_vector)))
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
                                    &o,
                                )))
                            }
                            _ => panic!("We should not have a non object in And-clause"),
                        };
                    }
                    result.push(("_or".to_string(), FilterValue::Or(result_vector)))
                }
                _ => panic!("Invalid operand to and "),
            }
        } else if field_name == "_not" {
            let elt = &spanning_input_value.item;
            match elt {
                InputValue::Object(o) => result.push((
                    "_not".to_string(),
                    FilterValue::Not(Rc::new(compile_edges_to_filter(
                        class_name,
                        all_frames,
                        class_definition,
                        &o,
                    ))),
                )),
                _ => panic!("We should not have a non object in And-clause"),
            }
        } else {
            let field = class_definition.resolve_field(&field_name);
            let prefixes = &all_frames.context;
            let property = class_definition.fully_qualified_property_name(&prefixes, field_name);
            let range = field.range();
            let kind = field.kind();
            match kind {
                FieldKind::Required | FieldKind::Optional => {
                    let res = compile_typed_filter(range, all_frames, &spanning_input_value);
                    result.push((property.to_string(), FilterValue::Required(res)));
                }
                FieldKind::Set | FieldKind::List | FieldKind::Array | FieldKind::Cardinality => {
                    let value =
                        CollectionFilterInputObject::from_input_value(&spanning_input_value.item);
                    let filter_value = compile_collection_filter(value.unwrap(), all_frames, range);
                    result.push((property.to_string(), filter_value))
                }
            }
        }
    }
    FilterObject { edges: result }
}

fn compile_filter_object(
    class_name: &str,
    all_frames: &AllFrames,
    filter_input: &FilterInputObject,
) -> FilterObject {
    let class_definition: &ClassDefinition = all_frames.frames[class_name].as_class_definition();
    let edges = &filter_input.edges;
    compile_edges_to_filter(class_name, all_frames, class_definition, &edges)
}

pub fn predicate_value_filter<'a>(
    g: &'a SyncStoreLayer,
    property: &'a str,
    object_type: &NodeOrValue,
    object: String,
    iter: ClonableIterator<'a, u64>,
) -> ClonableIterator<'a, u64> {
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        let maybe_object_id = match object_type {
            NodeOrValue::Value => g.object_value_id(&object),
            NodeOrValue::Node => g.object_node_id(&object),
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
    object_type: &NodeOrValue,
    object: String,
) -> ClonableIterator<'a, u64> {
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        let maybe_object_id = match object_type {
            NodeOrValue::Value => g.object_value_id(&object),
            NodeOrValue::Node => g.object_node_id(&object),
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

pub enum NodeOrValue {
    Node,
    #[allow(unused)]
    Value,
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
    filter_type: &'a FilterType,
    iter: ClonableIterator<'a, u64>,
) -> ClonableIterator<'a, u64> {
    match filter_type {
        FilterType::Text(operation, _) => match operation {
            TextOperation::Regex(regex) => {
                let regex = regex.clone();
                let g = g.clone();
                ClonableIterator::new(iter.filter(move |object| {
                    let string = g
                        .id_object(*object)
                        .unwrap()
                        .value()
                        .expect("This object should be a value");
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
                        .id_object(*object)
                        .unwrap()
                        .value()
                        .expect("This object should be a value");
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
                        .id_object(*object)
                        .unwrap()
                        .value()
                        .expect("This object should be a value");
                    length == regexset.matches(&string).into_iter().count()
                }))
            }
            TextOperation::AnyOfTerms(vec) => {
                let regexs: Vec<_> = vec.iter().map(|s| regex::escape(s)).collect();
                let regexset =
                    RegexSet::new(&regexs).expect("Regex should always be valid due to escaping");
                let g = g.clone();
                ClonableIterator::new(iter.filter(move |object| {
                    let string = g
                        .id_object(*object)
                        .unwrap()
                        .value()
                        .expect("This object should be a value");
                    regexset.is_match(&string)
                }))
            }
        },
        FilterType::SmallInt(op, i, _) => {
            let op = *op;
            let i = i.clone();
            let h = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = h.id_object_value(*object).expect("Object value must exist");
                let object_int = value_string_to_number(&object_value)
                    .as_i64()
                    .expect("How did it even get in to the database as a small int")
                    as i32;
                let cmp = object_int.cmp(&i);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterType::Float(op, f, _) => {
            let op = *op;
            let f = f.clone();
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_float = value_string_to_number(&object_value)
                    .as_f64()
                    .expect("How did this get into the database bigger than f64?");
                let cmp = OrderedFloat(object_float).cmp(&OrderedFloat(f));
                ordering_matches_op(cmp, op)
            }))
        }
        FilterType::Boolean(op, b, _) => {
            let op = *op;
            let b = b.clone();
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_bool = value_string_to_bool(&object_value);
                let cmp = object_bool.cmp(&b);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterType::BigInt(op, BigInt(bigint), _) => {
            let op = *op;
            let val = bigint.parse::<Integer>().unwrap();
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_int = value_string_to_bigint(&object_value);
                let cmp = object_int.cmp(&val);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterType::DateTime(op, DateTime(val), _) => {
            let op = *op;
            let val = val.clone();
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_string = value_string_to_string(&object_value).to_string();
                // Datetimes are reverse order: newer is bigger, but less!
                let cmp = val.cmp(&object_string);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterType::String(op, val, _) => {
            let op = *op;
            let val = val.clone();
            let g = g.clone();
            ClonableIterator::new(iter.filter(move |object| {
                let object_value = g.id_object_value(*object).expect("Object value must exist");
                let object_string = value_string_to_string(&object_value).to_string();
                let cmp = object_string.cmp(&val);
                ordering_matches_op(cmp, op)
            }))
        }
        FilterType::Enum { op, value } => {
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
    }
}

fn compile_query<'a>(
    g: &'a SyncStoreLayer,
    filter: Rc<FilterObject>,
    iter: ClonableIterator<'a, u64>,
) -> ClonableIterator<'a, u64> {
    let mut iter = iter;
    for (predicate, filter) in filter.edges.iter() {
        match filter {
            FilterValue::And(vec) => {
                for filter in vec.iter() {
                    iter = compile_query(g, filter.clone(), iter)
                }
            }
            FilterValue::Or(vec) => {
                // iter - iter1 +
                //     \ - iter2 +
                //      \ - iter3
                //
                let initial_vector = iter.collect::<Vec<u64>>();
                //or_iter = ClonableIterator::new(std::iter::empty());
                iter = ClonableIterator::new(vec.clone().into_iter().flat_map(move |filter| {
                    let iter_copy = ClonableIterator::new(initial_vector.clone().into_iter());
                    compile_query(&g, filter.clone(), iter_copy)
                }));
            }
            FilterValue::Not(filter) => {
                //  A = iter.collect()
                //  B = sub_iter
                //  C = A \ B
                let initial_set: HashSet<u64> = iter.collect();
                let initial_iter = ClonableIterator::new(CachedClonableIterator::new(
                    initial_set.clone().into_iter(),
                ));
                let sub_iter: HashSet<u64> =
                    compile_query(&g, filter.clone(), initial_iter).collect();

                let result = &initial_set - &sub_iter;
                iter = ClonableIterator::new(CachedClonableIterator::new(result.into_iter()));
            }
            FilterValue::Required(object_type) => {
                let maybe_property_id = g.predicate_id(&predicate);
                if let Some(property_id) = maybe_property_id {
                    match object_type {
                        ObjectType::Node(sub_filter, _) => {
                            let sub_filter = sub_filter.clone();
                            iter = ClonableIterator::new(iter.filter(move |subject| {
                                let objects = ClonableIterator::new(
                                    g.single_triple_sp(*subject, property_id)
                                        .map(|t| t.object)
                                        .into_iter(),
                                );
                                compile_query(&g, sub_filter.clone(), objects)
                                    .next()
                                    .is_some()
                            }))
                        }
                        ObjectType::Value(filter_type) => {
                            let filter_type = filter_type.clone();
                            iter = ClonableIterator::new(iter.filter(move |subject| {
                                let object_iter = ClonableIterator::new(
                                    g.single_triple_sp(*subject, property_id)
                                        .map(|t| t.object)
                                        .into_iter(),
                                );
                                object_type_filter(&g, &filter_type, object_iter)
                                    .next()
                                    .is_some()
                            }))
                        }
                    }
                } else {
                    return ClonableIterator::new(std::iter::empty());
                }
            }
            FilterValue::Collection(op, o) => {
                let maybe_property_id = g.predicate_id(&predicate);
                if let Some(property_id) = maybe_property_id {
                    match op {
                        CollectionOperation::SomeHave => match o {
                            ObjectType::Node(sub_filter, _) => {
                                let sub_filter = sub_filter.clone();
                                iter = ClonableIterator::new(iter.filter(move |subject| {
                                    let objects =
                                        ClonableIterator::new(CachedClonableIterator::new(
                                            g.triples_sp(*subject, property_id).map(|t| t.object),
                                        ));
                                    compile_query(&g, sub_filter.clone(), objects)
                                        .next()
                                        .is_some()
                                }));
                            }
                            ObjectType::Value(filter_type) => {
                                let filter_type = filter_type.clone();
                                iter = ClonableIterator::new(iter.filter(move |subject| {
                                    let objects =
                                        ClonableIterator::new(CachedClonableIterator::new(
                                            g.triples_sp(*subject, property_id).map(|t| t.object),
                                        ));
                                    object_type_filter(&g, &filter_type, objects)
                                        .next()
                                        .is_some()
                                }));
                            }
                        },
                        CollectionOperation::AllHave => match o {
                            ObjectType::Node(sub_filter, _) => {
                                let sub_filter = sub_filter.clone();
                                iter = ClonableIterator::new(iter.filter(move |subject| {
                                    let objects =
                                        ClonableIterator::new(CachedClonableIterator::new(
                                            g.triples_sp(*subject, property_id).map(|t| t.object),
                                        ));
                                    compile_query(&g, sub_filter.clone(), objects).all(|_| true)
                                }));
                            }
                            ObjectType::Value(filter_type) => {
                                let filter_type = filter_type.clone();
                                iter = ClonableIterator::new(iter.filter(move |subject| {
                                    let objects =
                                        ClonableIterator::new(CachedClonableIterator::new(
                                            g.triples_sp(*subject, property_id).map(|t| t.object),
                                        ));
                                    object_type_filter(&g, &filter_type, objects).all(|_| true)
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

fn generate_initial_iterator<'a>(
    g: &'a SyncStoreLayer,
    class_name: &'a str,
    all_frames: &AllFrames,
    filter_opt: Option<FilterObject>,
    zero_iter: Option<ClonableIterator<'a, u64>>,
) -> (Option<FilterObject>, ClonableIterator<'a, u64>) {
    match zero_iter {
        None => {
            // If we have a filter here, we probably need to use it.
            let expanded_type_name = all_frames.fully_qualified_class_name(&class_name.to_string());
            (
                filter_opt,
                predicate_value_iter(g, &RDF_TYPE, &NodeOrValue::Node, expanded_type_name),
            )
        }
        Some(zi) => (filter_opt, zi),
    }
}

fn lookup_by_filter<'a>(
    g: &'a SyncStoreLayer,
    class_name: &'a str,
    all_frames: &AllFrames,
    filter_opt: Option<FilterObject>,
    zero_iter: Option<ClonableIterator<'a, u64>>,
) -> ClonableIterator<'a, u64> {
    let (continuation_filter_opt, iterator) =
        generate_initial_iterator(g, class_name, all_frames, filter_opt, zero_iter);
    if let Some(continuation_filter) = continuation_filter_opt {
        let continuation_filter = Rc::new(continuation_filter);
        compile_query(g, continuation_filter, iterator)
    } else {
        iterator
    }
}

pub fn run_filter_query<'a>(
    g: &'a SyncStoreLayer,
    prefixes: &'a Prefixes,
    arguments: &'a juniper::Arguments,
    class_name: &'a str,
    all_frames: &'a AllFrames,
    zero_iter: Option<ClonableIterator<'a, u64>>,
) -> Vec<u64> {
    let new_zero_iter: Option<ClonableIterator<'a, u64>> =
        if let Some(id_string) = arguments.get::<ID>("id") {
            Some({
                if let Some(id) = g.subject_id(&*id_string) {
                    match zero_iter {
                        None => {
                            let zi = ClonableIterator::new(vec![id].into_iter());
                            let expanded_type_name =
                                all_frames.fully_qualified_class_name(&class_name.to_string());
                            predicate_value_filter(
                                g,
                                &RDF_TYPE,
                                &NodeOrValue::Node,
                                expanded_type_name,
                                zi,
                            )
                        }
                        Some(zi) => ClonableIterator::new(zi.filter(move |i| *i == id)),
                    }
                } else {
                    ClonableIterator::new(std::iter::empty())
                }
            })
        } else {
            zero_iter
        };
    eprintln!("Arguments: {arguments:?}");
    let new_zero_iter: Option<ClonableIterator<'a, u64>> =
        if let Some(path_string) = arguments.get::<String>("path") {
            eprintln!("Found a path argument");
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
    let it: ClonableIterator<'a, u64> =
        if let Some(TerminusOrderBy { fields }) = arguments.get::<TerminusOrderBy>("orderBy") {
            let mut results: Vec<u64> =
                lookup_by_filter(g, class_name, all_frames, filter, new_zero_iter)
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
                lookup_by_filter(g, class_name, all_frames, filter, new_zero_iter)
                    .skip(usize::try_from(offset).unwrap_or(0)),
            )
        };

    if let Some(limit) = limit {
        it.take(usize::try_from(limit).unwrap_or(0)).collect()
    } else {
        it.collect()
    }
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
            let res = g.single_triple_sp(id, predicate_id).and_then(move |t| {
                let db_value = g
                    .id_object(t.object)
                    .expect("This object must exist")
                    .value()
                    .unwrap();
                match value_string_to_graphql(&db_value) {
                    juniper::Value::Scalar(s) => Some(s),
                    juniper::Value::Null => None,
                    _ => panic!("Scalar value or null expected"),
                }
            });
            Some((res, *ordering))
        })
        .collect();

    QueryOrderKey { vec }
}

struct QueryOrderKey {
    vec: Vec<(Option<DefaultScalarValue>, TerminusOrdering)>,
}

fn compare_int(i: i32, other_value: &Option<DefaultScalarValue>) -> Ordering {
    match other_value {
        None => Ordering::Greater,
        Some(value) => {
            let j = value.as_int().expect("This should have been an int");
            i.cmp(&j)
        }
    }
}

fn compare_float(f: f64, other_value: &Option<DefaultScalarValue>) -> Ordering {
    match other_value {
        None => Ordering::Greater,
        Some(value) => {
            let g = value.as_float().expect("This should have been a float");
            let fp = FloatOrd(f);
            let gp = FloatOrd(g);
            fp.cmp(&gp)
        }
    }
}

fn compare_string(s: &str, other_value: &Option<DefaultScalarValue>) -> Ordering {
    match other_value {
        None => Ordering::Greater,
        Some(value) => {
            let t = value.as_string().expect("This should have been a string");
            s.cmp(&t)
        }
    }
}

fn compare_bool(b: bool, other_value: &Option<DefaultScalarValue>) -> Ordering {
    match other_value {
        None => Ordering::Greater,
        Some(value) => {
            let c = value.as_boolean().expect("This should have been a bool");
            b.cmp(&c)
        }
    }
}

impl PartialOrd for QueryOrderKey {
    fn partial_cmp(&self, other: &QueryOrderKey) -> Option<Ordering> {
        for i in 0..self.vec.len() {
            let (option_value, order) = &self.vec[i];
            let (other_value, _) = &other.vec[i];
            let res = match option_value {
                Some(DefaultScalarValue::Int(i)) => compare_int(*i, other_value),
                Some(DefaultScalarValue::Float(f)) => compare_float(*f, other_value),
                Some(DefaultScalarValue::String(s)) => compare_string(s, other_value),
                Some(DefaultScalarValue::Boolean(b)) => compare_bool(*b, other_value),
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
                return Some(final_order);
            }
        }
        Some(Ordering::Equal)
    }
}

impl PartialEq for QueryOrderKey {
    fn eq(&self, other: &QueryOrderKey) -> bool {
        for i in 0..self.vec.len() {
            let (option_value, _) = &self.vec[i];
            let (other_value, _) = &other.vec[i];
            let res = match option_value {
                Some(DefaultScalarValue::Float(f)) => {
                    Ordering::Equal == compare_float(*f, other_value)
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
        self.partial_cmp(other)
            .expect("OrderKey should never return None")
    }
}
