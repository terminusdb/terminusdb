use juniper::{
    self, parser::Spanning, DefaultScalarValue, FromInputValue, InputValue, ScalarValue, ID,
};

use crate::terminus_store::store::sync::SyncStoreLayer;

use crate::value::{base_type_kind, value_string_to_string, BaseTypeKind};
use crate::{
    consts::RDF_TYPE,
    terminus_store::*,
    value::{graphql_scalar_to_value_string, value_string_to_graphql},
};

use super::filter::{
    BigIntFilterInputObject, BooleanFilterInputObject, CollectionFilterInputObject,
    DateTimeFilterInputObject, FilterInputObject, FloatFilterInputObject,
    SmallIntegerFilterInputObject, StringFilterInputObject,
};
use super::frame::{is_base_type, AllFrames, ClassDefinition, FieldKind, Prefixes};
use super::schema::{
    is_reserved_argument_name, BigInt, DateTime, TerminusEnum, TerminusOrderBy, TerminusOrdering,
};

use float_ord::FloatOrd;

use std::cmp::*;
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

#[derive(Debug)]
enum TextOperation {
    StartsWith(String),
    AllOfTerms(Vec<String>),
    AnyOfTerms(Vec<String>),
}

#[derive(Debug)]
enum CollectionOperation {
    SomeHave,
    AllHave,
}

#[derive(Debug)]
enum FilterValue {
    // op : TextOperation, type: String
    Text(TextOperation, String),
    // op : GenericOperation,  value : String, type: String
    SmallInt(GenericOperation, i32, String),
    Float(GenericOperation, f64, String),
    Boolean(GenericOperation, bool, String),
    BigInt(GenericOperation, BigInt, String),
    DateTime(GenericOperation, DateTime, String),
    String(GenericOperation, String, String),
    // filter: FilterObject, type: String
    Node(Rc<FilterObject>, String),
    // op : CollectionOperation,  value : String, type: String
    Collection(CollectionOperation, Rc<FilterObject>, String),
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

fn compile_string_input_value(string_type: &str, value: StringFilterInputObject) -> FilterValue {
    if let Some(val) = value.eq {
        FilterValue::String(GenericOperation::Eq, val, string_type.to_string())
    } else if let Some(val) = value.lt {
        FilterValue::String(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.le {
        FilterValue::String(GenericOperation::Lt, val, string_type.to_string())
    } else if let Some(val) = value.gt {
        FilterValue::String(GenericOperation::Gt, val, string_type.to_string())
    } else if let Some(val) = value.ge {
        FilterValue::String(GenericOperation::Ge, val, string_type.to_string())
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

fn compile_small_integer_input_value(
    string_type: &str,
    value: SmallIntegerFilterInputObject,
) -> FilterValue {
    if let Some(val) = value.eq {
        FilterValue::SmallInt(GenericOperation::Eq, val, string_type.to_string())
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
    } else {
        panic!("Unable to compile string input value to a filter")
    }
}

fn compile_typed_filter(
    range: &str,
    all_frames: &AllFrames,
    spanning_input_value: &juniper::Spanning<InputValue>,
) -> FilterValue {
    if is_base_type(range) {
        match base_type_kind(range) {
            BaseTypeKind::String => {
                let value = StringFilterInputObject::from_input_value(&spanning_input_value.item);
                compile_string_input_value(range, value.unwrap())
            }
            BaseTypeKind::SmallInteger => {
                let value =
                    SmallIntegerFilterInputObject::from_input_value(&spanning_input_value.item);
                compile_small_integer_input_value(range, value.unwrap())
            }
            BaseTypeKind::BigIntger => {
                let value = BigIntFilterInputObject::from_input_value(&spanning_input_value.item);
                compile_big_int_input_value(range, value.unwrap())
            }
            BaseTypeKind::Boolean => {
                let value = BooleanFilterInputObject::from_input_value(&spanning_input_value.item);
                compile_boolean_input_value(range, value.unwrap())
            }
            BaseTypeKind::DateTime => {
                let value = DateTimeFilterInputObject::from_input_value(&spanning_input_value.item);
                compile_datetime_input_value(range, value.unwrap())
            }
            BaseTypeKind::Float => {
                let value = FloatFilterInputObject::from_input_value(&spanning_input_value.item);
                compile_float_input_value(range, value.unwrap())
            }
        }
    } else {
        let class_definition: &ClassDefinition = all_frames.frames[range].as_class_definition();
        if let InputValue::Object(edges) = &spanning_input_value.item {
            let inner = compile_edges_to_filter(range, all_frames, class_definition, &edges);
            FilterValue::Node(Rc::new(inner), range.to_string())
        } else {
            panic!()
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
        if let InputValue::Object(edges) = next.item {
            let class_definition: &ClassDefinition = all_frames.frames[range].as_class_definition();
            let class_filter = compile_edges_to_filter(range, all_frames, class_definition, &edges);
            FilterValue::Collection(operation, Rc::new(class_filter), range.to_string())
        } else {
            panic!("No input object for value collection filter")
        }
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
        let field = class_definition.resolve_field(&field_name);
        let prefixes = &all_frames.context;
        let property = class_definition.fully_qualified_property_name(&prefixes, field_name);
        let range = field.range();
        let kind = field.kind();
        match kind {
            FieldKind::Required | FieldKind::Optional => {
                let res = compile_typed_filter(range, all_frames, &spanning_input_value);
                result.push((property.to_string(), res));
            }
            FieldKind::Set | FieldKind::List | FieldKind::Array | FieldKind::Cardinality => {
                let value =
                    CollectionFilterInputObject::from_input_value(&spanning_input_value.item);
                let filter_value = compile_collection_filter(value.unwrap(), all_frames, range);
                result.push((property.to_string(), filter_value))
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

fn predicate_value_iter<'a>(
    g: &'a SyncStoreLayer,
    property: &'a str,
    object_type: &NodeOrValue,
    object: String,
) -> Box<dyn Iterator<Item = u64> + 'a> {
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        let maybe_object_id = match object_type {
            NodeOrValue::Value => g.object_value_id(&object),
            NodeOrValue::Node => g.object_node_id(&object),
        };
        if let Some(object_id) = maybe_object_id {
            Box::new(
                g.triples_o(object_id)
                    .filter(move |t| t.predicate == property_id)
                    .map(|t| t.subject),
            )
        } else {
            Box::new(std::iter::empty())
        }
    } else {
        Box::new(std::iter::empty())
    }
}

pub enum NodeOrValue {
    Node,
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

fn compile_query<'a>(
    g: &'a SyncStoreLayer,
    filter: Rc<FilterObject>,
    iter: Box<dyn Iterator<Item = u64> + 'a>,
) -> Box<dyn Iterator<Item = u64> + 'a> {
    let mut iter = iter;
    for (predicate, filter) in filter.edges.iter() {
        let maybe_property_id = g.predicate_id(&predicate);
        if let Some(property_id) = maybe_property_id {
            match filter {
                FilterValue::String(op, val, _) => {
                    let op = *op;
                    let val = val.clone();
                    iter = Box::new(iter.filter(move |subject| {
                        let mut triples = g.triples_sp(*subject, property_id);
                        triples.any(|t| {
                            let object_value = g
                                .id_object_value(t.object)
                                .expect("Object value must exist");
                            let object_string = value_string_to_string(&object_value).to_string();
                            println!("op: {op:?}");
                            println!("object: {object_string}");
                            println!("value: {val}");
                            let cmp = object_string.cmp(&val);
                            ordering_matches_op(cmp, op)
                        })
                    }))
                }
                FilterValue::Text(_, _) => todo!(),
                FilterValue::BigInt(_, _, _) => todo!(),
                FilterValue::Float(_, _, _) => todo!(),
                FilterValue::DateTime(_, _, _) => todo!(),
                FilterValue::Collection(_, _, _) => todo!(),
                FilterValue::SmallInt(_, _, _) => todo!(),
                FilterValue::Boolean(_, _, _) => todo!(),
                FilterValue::Node(sub_filter, _obj_type) => {
                    let sub_filter = sub_filter.clone();
                    iter = Box::new(iter.filter(move |subject| {
                        let objects =
                            Box::new(g.triples_sp(*subject, property_id).map(|t| t.object));
                        compile_query(g, sub_filter.clone(), objects)
                            .next()
                            .is_some()
                    }))
                }
            }
        } else {
            return Box::new(std::iter::empty());
        }
    }
    iter
}

fn generate_initial_iterator<'a>(
    g: &'a SyncStoreLayer,
    class_name: &'a str,
    class_definition: &ClassDefinition,
    all_frames: &AllFrames,
    filter_opt: Option<FilterObject>,
    zero_iter: Option<Box<dyn Iterator<Item = u64> + 'a>>,
) -> (Option<FilterObject>, Box<dyn Iterator<Item = u64> + 'a>) {
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
    class_definition: &ClassDefinition,
    all_frames: &AllFrames,
    filter_opt: Option<FilterObject>,
    zero_iter: Option<Box<dyn Iterator<Item = u64> + 'a>>,
) -> impl Iterator<Item = u64> + 'a {
    let (continuation_filter_opt, iterator) = generate_initial_iterator(
        g,
        class_name,
        class_definition,
        all_frames,
        filter_opt,
        zero_iter,
    );
    println!("Generated initial iterator with filter: {continuation_filter_opt:?}");
    if let Some(continuation_filter) = continuation_filter_opt {
        let continuation_filter = Rc::new(continuation_filter);
        compile_query(g, continuation_filter, iterator)
    } else {
        iterator
    }
}

pub fn run_filter_query<'a>(
    g: &'a SyncStoreLayer,
    prefixes: &Prefixes,
    arguments: &juniper::Arguments,
    class_name: &str,
    all_frames: &AllFrames,
    zero_iter: Option<Box<dyn Iterator<Item = u64> + 'a>>,
) -> Vec<u64> {
    let class_definition: &ClassDefinition = all_frames.frames[class_name].as_class_definition();
    let new_zero_iter: Option<Box<dyn Iterator<Item = u64> + 'a>> = match arguments.get::<ID>("id")
    {
        Some(id_string) => Some({
            if let Some(id) = g.subject_id(&*id_string) {
                match zero_iter {
                    None => Box::new(vec![id].into_iter()),
                    Some(zi) => Box::new(zi.filter(move |i| *i == id)),
                }
            } else {
                Box::new(std::iter::empty())
            }
        }),
        None => zero_iter,
    };
    let offset: i32 = arguments.get("offset").unwrap_or(0);
    let limit: Option<i32> = arguments.get("limit");
    let filter_arg_opt: Option<FilterInputObject> = arguments.get("filter");
    let filter = filter_arg_opt
        .map(|filter_input| compile_filter_object(class_name, all_frames, &filter_input));
    let it: Box<dyn Iterator<Item = u64>> =
        if let Some(TerminusOrderBy { fields }) = arguments.get::<TerminusOrderBy>("orderBy") {
            let mut results: Vec<u64> = lookup_by_filter(
                g,
                class_name,
                class_definition,
                all_frames,
                filter,
                new_zero_iter,
            )
            .collect();
            results.sort_by_cached_key(|id| create_query_order_key(g, prefixes, *id, &fields));
            // Probs should not be into_iter(), done to satisfy both arms of let symmetry
            // better to borrow in the other branch?
            Box::new(
                results
                    .into_iter()
                    .skip(usize::try_from(offset).unwrap_or(0)),
            )
        } else {
            Box::new(
                lookup_by_filter(
                    g,
                    class_name,
                    class_definition,
                    all_frames,
                    filter,
                    new_zero_iter,
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

fn constraint_builder_example(
    class_definition: &ClassDefinition,
    arguments: &juniper::Arguments,
    prefixes: &Prefixes,
    all_frames: &AllFrames,
    class_name: &str,
) -> Vec<(String, NodeOrValue, String)> {
    let mut constraints: Vec<(String, NodeOrValue, String)> = class_definition
        .fields
        .iter()
        .filter_map(|(field_name, field_definition)| {
            if is_reserved_argument_name(field_name) {
                return None;
            }

            if let Some(base_type) = field_definition.base_type() {
                if let Some(value) = arguments.get(field_name) {
                    let field_name_expanded =
                        class_definition.fully_qualified_property_name(prefixes, field_name);
                    let expanded_object_value = graphql_scalar_to_value_string(value, base_type);
                    Some((
                        field_name_expanded,
                        NodeOrValue::Value,
                        expanded_object_value,
                    ))
                } else {
                    None
                }
            } else if let Some(enum_type) = field_definition.enum_type(all_frames) {
                if let Some(terminus_enum) = arguments.get::<TerminusEnum>(field_name) {
                    let field_name_expanded =
                        class_definition.fully_qualified_property_name(prefixes, field_name);
                    let enum_type_expanded = all_frames.fully_qualified_class_name(enum_type);
                    let enum_type_definition = all_frames.frames[enum_type].as_enum_definition();
                    let value = enum_type_definition.value_name(&terminus_enum.value);
                    let expanded_object_node = format!("{}/{}", enum_type_expanded, value);
                    Some((field_name_expanded, NodeOrValue::Node, expanded_object_node))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();
    let expanded_class_name = all_frames.fully_qualified_class_name(&class_name.to_string());
    constraints.push((RDF_TYPE.to_owned(), NodeOrValue::Node, expanded_class_name));
    constraints
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
