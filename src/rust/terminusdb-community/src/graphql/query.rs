use juniper::{
    self, parser::Spanning, DefaultScalarValue, FromInputValue, InputValue, ScalarValue, ID,
};

use crate::terminus_store::store::sync::SyncStoreLayer;

use crate::{
    consts::RDF_TYPE,
    terminus_store::*,
    value::{graphql_scalar_to_value_string, value_string_to_graphql},
};

use super::frame::{AllFrames, ClassDefinition, Prefixes};
use super::schema::{is_reserved_argument_name, TerminusEnum, TerminusOrdering};

use float_ord::FloatOrd;

use std::cmp::*;

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

fn predicate_value_filter<'a>(
    g: &'a SyncStoreLayer,
    iter: Box<dyn Iterator<Item = u64> + 'a>,
    property: &str,
    object_type: &NodeOrValue,
    object: &str,
) -> Box<dyn Iterator<Item = u64> + 'a> {
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        let maybe_object_id = match object_type {
            NodeOrValue::Value => g.object_value_id(object),
            NodeOrValue::Node => g.object_node_id(object),
        };
        if let Some(object_id) = maybe_object_id {
            Box::new(iter.filter(move |s| g.triple_exists(*s, property_id, object_id)))
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

pub fn lookup_field_requests<'a>(
    g: &'a SyncStoreLayer,
    constraints: &'a [(String, NodeOrValue, String)],
    zero_iter: Option<Box<dyn Iterator<Item = u64> + 'a>>,
) -> impl Iterator<Item = u64> + 'a {
    if constraints.len() == 0 {
        panic!("Somehow there are no constraints on the triples")
    } else {
        let start;
        let zi;
        if let Some(zero_iter) = zero_iter {
            start = 0;
            zi = zero_iter;
        } else {
            let (p, oty, o) = &constraints[0];
            zi = predicate_value_iter(g, &p, &oty, &o);
            start = 1;
        }
        constraints[start..].iter().fold(zi, |iter, (p, oty, o)| {
            predicate_value_filter(g, iter, &p, &oty, &o)
        })
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
                    let expanded_object_node =
                        format!("{}/{}", enum_type_expanded, terminus_enum.value);
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

    let it: Box<dyn Iterator<Item = u64>> = if let Some(QueryOrderByDesc(vec)) =
        arguments.get::<QueryOrderByDesc>("orderBy")
    {
        let mut results: Vec<u64> = lookup_field_requests(g, &constraints, new_zero_iter).collect();
        results.sort_by_cached_key(|id| create_query_order_key(g, prefixes, *id, &vec));
        // Probs should not be into_iter(), done to satisfy both arms of let symmetry
        // better to borrow in the other branch?
        Box::new(
            results
                .into_iter()
                .skip(usize::try_from(offset).unwrap_or(0)),
        )
    } else {
        Box::new(
            lookup_field_requests(g, &constraints, new_zero_iter)
                .skip(usize::try_from(offset).unwrap_or(0)),
        )
    };

    if let Some(limit) = limit {
        it.take(usize::try_from(limit).unwrap_or(0)).collect()
    } else {
        it.collect()
    }
}

struct QueryOrderByDesc(Vec<(Spanning<String>, Spanning<InputValue<DefaultScalarValue>>)>);

impl FromInputValue for QueryOrderByDesc {
    fn from_input_value(input_value: &InputValue<DefaultScalarValue>) -> Option<Self> {
        match input_value {
            InputValue::Object(vec) => Some(QueryOrderByDesc(vec.clone())),
            InputValue::Null => None,
            _ => panic!("Order by descriptor can not have a non orderby object"),
        }
    }
}

fn create_query_order_key(
    g: &SyncStoreLayer,
    p: &Prefixes,
    id: u64,
    order_desc: &[(Spanning<String>, Spanning<InputValue<DefaultScalarValue>>)],
) -> QueryOrderKey {
    let vec: Vec<_> = order_desc
        .iter()
        .filter_map(|(property, value)| {
            let property_string = &property.item;
            let value_input = &value.item;
            let ordering = match value_input {
                InputValue::Enum(s) => {
                    if s == "ASC" {
                        TerminusOrdering::Asc
                    } else {
                        TerminusOrdering::Desc
                    }
                }
                _ => panic!("This ordering parameter must be an enum!"),
            };
            let predicate = p.expand_schema(property_string);
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
            Some((res, ordering))
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
