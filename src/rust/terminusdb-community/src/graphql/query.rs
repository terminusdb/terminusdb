use juniper::{
    self, parser::Spanning, DefaultScalarValue, FromInputValue, InputValue, ScalarValue, ID,
};

use crate::terminus_store::store::sync::SyncStoreLayer;

use crate::value::{base_type_kind, value_string_to_untyped_value, BaseTypeKind};
use crate::{
    consts::RDF_TYPE,
    terminus_store::*,
    value::{graphql_scalar_to_value_string, value_string_to_graphql},
};

use super::filter::FilterInputObject;
use super::frame::{AllFrames, ClassDefinition, FieldKind, Prefixes};
use super::schema::{is_reserved_argument_name, TerminusEnum, TerminusOrderBy, TerminusOrdering};

use float_ord::FloatOrd;

use std::cmp::*;

#[derive(Debug)]
enum GenericOperation {
    Eq,
    Ge,
    Gt,
    Lt,
    Le,
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
    TextFilter(TextOperation),
    StringFilter(GenericOperation, String),
    BigIntFilter(GenericOperation, String),
    DateTimeFilter(GenericOperation, String),
    Collection(CollectionOperation, FilterObject),
}

#[derive(Debug)]
struct FilterObject {
    pairs: Vec<(String, FilterValue)>,
}

impl From<StringFieldInputObject> for FilterValue {
    fn from(value: StringFieldInputObject) -> Self {
        if let Some(val) = value.eq {
            FilterValue::StringFilter(GenericOperation::Eq, val)
        } else if let Some(val) = value.lt {
            FilterValue::StringFilter(GenericOperation::Lt, val)
        } else if let Some(val) = value.le {
            FilterValue::StringFilter(GenericOperation::Lt, val)
        } else if let Some(val) = value.gt {
            FilterValue::StringFilter(GenericOperation::Gt, val)
        } else if let Some(val) = value.ge {
            FilterValue::StringFilter(GenericOperation::Ge, val)
        } else if let Some(val) = value.startsWith {
            FilterValue::TextFilter(TextOperation::StartsWith(val))
        } else if let some(val) = value.allOfTerms {
            FilterValue::TextFilter(TextOperation::AllOfTerms(val))
        } else if let some(val) = value.anyOfTerms {
            FilterValue::TextFilter(TextOperation::AnyOfTerms(val))
        }
    }
}

fn compile_input_object(
    class_name: &str,
    all_frames: &AllFrames,
    class_definition: &ClassDefinition,
    filter_input: &[(juniper::Spanning<String>, juniper::spanning<InputValue>)],
) -> FilterObject {
    let mut result: Vec<(String, FilterValue)> = Vec::with_capacity(filter_input.len());
    for (spanning_string, spanning_input_value) in filter_input.iter() {
        let field_name = spanning_string.item;
        let field = class_definition.resolve_field(field_name);
        let kind = field.kind();
        match kind {
            FieldKind::Required | FieldKind::Optional => {
                let ty = field.range();
                if is_base_type(ty) {
                    match base_type_kind(ty) {
                        BaseTypeKind::String => {
                            let value: Option<StringFilterInputValue> =
                                spanning_input_value.item.from_input_value();
                            let (operation, value) = compile_string_input_operation(value);
                            result.push((field_name, FilterValue::StringFilter(operation, value)))
                        }
                        BaseTypeKind::SmallInteger => todo!(),
                        BaseTypeKind::BigIntger => todo!(),
                        BaseTypeKind::Boolean => todo!(),
                        BaseTypeKind::DateTime => todo!(),
                        BaseTypeKind::Float => todo!(),
                    }
                } else {
                }
            }
            FieldKind::Set | FieldKind::List | FieldKind::Array | FieldKind::Cardinality => todo!(),
        }
    }
}

fn compile_filter_object(
    class_name: &str,
    all_frames: &AllFrames,
    filter_input: &FilterInputObject,
) -> FilterObject {
    let class_definition: &ClassDefinition = all_frames.frames[class_name].as_class_definition();
    let FilterInputObject { pairs } = &filter_input;
    compile_input_object(class_name, all_frames, class_definition, pairs)
}

fn predicate_value_filter<'a>(
    g: &'a SyncStoreLayer,
    iter: Box<dyn Iterator<Item = u64> + 'a>,
    property: &str,
    base_type: &str,
    filter: InputValue,
) -> Box<dyn Iterator<Item = u64> + 'a> {
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        match filter {
            InputValue::Object(o) => {
                if let Some((spanning_opname, spanning_value)) = o.pop() {
                    let op = spanning_opname.item;
                    if let InputValue::Scalar(scalar) = spanning_value.item {
                        Box::new(iter.filter(|subject| {
                            let objects = g.triples_sp(*subject, property_id);
                            objects.any(|t| {
                                let object =
                                    g.id_object_value(t.object).expect("should have existed");
                                let ord = match base_type_kind(base_type) {
                                    crate::value::BaseTypeKind::String => {
                                        if let DefaultScalarValue::String(scalar) = scalar {
                                            let object_string =
                                                value_string_to_untyped_value(&object);
                                            object_string.cmp(&scalar)
                                        } else {
                                            panic!("asdfasdf");
                                        }
                                    }
                                    crate::value::BaseTypeKind::SmallInteger => todo!(),
                                    crate::value::BaseTypeKind::BigIntger => todo!(),
                                    crate::value::BaseTypeKind::Boolean => todo!(),
                                    crate::value::BaseTypeKind::DateTime => todo!(),
                                    crate::value::BaseTypeKind::Float => todo!(),
                                };

                                match op.as_str() {
                                    "eq" => ord == Ordering::Equal,
                                    "ne" => ord != Ordering::Equal,
                                    "lt" => ord == Ordering::Less,
                                    "gt" => ord == Ordering::Greater,
                                    "le" => ord == Ordering::Equal || ord == Ordering::Less,
                                    "ge" => ord == Ordering::Equal || ord == Ordering::Greater,
                                }
                            })
                        }))
                    } else {
                        panic!();
                    }
                } else {
                    panic!("There is no comparison to the value at this point");
                }
            }
            _ => panic!("We should have a valid GraphQL input object here"),
        }
    } else {
        Box::new(std::iter::empty())
    }
}

fn predicate_value_iter<'a>(
    g: &'a SyncStoreLayer,
    property: &'a str,
    object_type: &NodeOrValue,
    object: &'a str,
) -> Box<dyn Iterator<Item = u64> + 'a> {
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        let maybe_object_id = match object_type {
            NodeOrValue::Value => g.object_value_id(object),
            NodeOrValue::Node => g.object_node_id(object),
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


let iter = zi;
let stack = Vec<FilterInputObject>
for edge in ty1_edge1.

 */

fn compile_query_step<'a>(
    g: &'a SyncStoreLayer,
    class_name: &str,
    class_definition: &ClassDefinition,
    all_frames: &AllFrames,
    filter_opt: Option<FilterInputObject>,
    iter: Box<dyn Iterator<Item = u64> + 'a>,
) -> (
    Option<FilterInputObject>,
    Box<dyn Iterator<Item = u64> + 'a>,
) {
    let filter_opt = if let Some(FilterInputObject { edges }) = filter_opt {
        if edges.is_empty() {
            None
        } else {
            filter_opt
        }
    } else {
        filter_opt
    };
    match filter_opt {
        Some(FilterInputObject { edges }) => {
            let (spanning_string, spanning_input_value) = edges.pop().unwrap();
            let field_name = spanning_string.item;
            let field = class_definition.resolve_field(&field_name.to_string());
            let kind = field.kind();
            let new_iterator = match kind {
                super::frame::FieldKind::Required => {
                    let range = field.range();
                    if range.is_base_type() {
                        predicate_value_filter(
                            g,
                            zero_iter,
                            &field_name,
                            range,
                            spanning_input_value.item,
                        )
                    } else {
                        todo!();
                    }
                }
                super::frame::FieldKind::Optional => todo!(),
                super::frame::FieldKind::Set => todo!(),
                super::frame::FieldKind::List => todo!(),
                super::frame::FieldKind::Array => todo!(),
                super::frame::FieldKind::Cardinality => todo!(),
            };
            let new_filter_object = if edges.is_empty() {
                None
            } else {
                Some(FilterInputObject { edges })
            };
            (new_filter_object, new_iterator)
        }
        None => (None, Box::new(zi)),
    }
}

fn compile_query_iter<'a>(
    g: &'a SyncStoreLayer,
    class_name: &str,
    class_definition: &ClassDefinition,
    all_frames: &AllFrames,
    filter_opt: Option<FilterInputObject>,
    zero_iter: Option<Box<dyn Iterator<Item = u64> + 'a>>,
) -> (
    Option<FilterInputObject>,
    Box<dyn Iterator<Item = u64> + 'a>,
) {
    match zero_iter {
        None => {
            let expanded_type_name = all_frames.fully_qualified_class_name(class_name);
            (
                None,
                predicate_value_iter(
                    g,
                    RDF_TYPE.to_owned(),
                    NodeOrValue::Node,
                    expanded_type_name,
                ),
            )
        }
        Some(zi) => (filter_opt, zi),
    }
}

fn lookup_by_filter<'a>(
    g: &'a SyncStoreLayer,
    class_name: &str,
    class_definition: &ClassDefinition,
    all_frames: &AllFrames,
    filter_opt: Option<FilterInputObject>,
    zero_iter: Option<Box<dyn Iterator<Item = u64> + 'a>>,
) -> impl Iterator<Item = u64> + 'a {
    let (mut continuation_filter_opt, mut iterator) = compile_query_iter(
        g,
        class_name,
        class_definition,
        all_frames,
        filter_opt,
        zero_iter,
    );
    while continuation_filter_opt.is_some() {
        (continuation_filter_opt, iterator) = compile_query_step(
            g,
            class_name,
            class_definition,
            all_frames,
            continuation_filter_opt,
            iterator,
        );
    }
    iterator
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
    let filter: Option<FilterInputObject> = arguments.get("filter");

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
