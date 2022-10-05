use juniper::ID;

use crate::terminus_store::store::sync::SyncStoreLayer;

use crate::{terminus_store::*, value::graphql_scalar_to_value_string, consts::RDF_TYPE};

use super::frame::{Prefixes, ClassDefinition};
use super::schema::is_reserved_argument_name;

use std::convert::TryInto;

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
                    .map(|t|t.subject),
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
    zero_iter: Option<Box<dyn Iterator<Item=u64>+'a>>
) -> impl Iterator<Item=u64>+'a {
    if constraints.len() == 0 {
        panic!("Somehow there are no constraints on the triples")
    } else {
        let start;
        let zi;
        if let Some(zero_iter) = zero_iter {
            start = 0;
            zi = zero_iter;
        }
        else {
            let (p, oty, o) = &constraints[0];
            zi = predicate_value_iter(g, &p, &oty, &o);
            start = 1;
        }
        constraints[start..]
            .iter()
            .fold(zi, |iter, (p, oty, o)| {
                predicate_value_filter(g, iter, &p, &oty, &o)
            })
    }
}

pub fn run_filter_query<'a>(
    g: &'a SyncStoreLayer,
    prefixes: &Prefixes,
    arguments: &juniper::Arguments,
    class_name: &str,
    class_definition: &ClassDefinition,
    zero_iter: Option<Box<dyn Iterator<Item=u64>+'a>>) -> Vec<u64> {
    let new_zero_iter: Option<Box<dyn Iterator<Item=u64>+'a>> =
        match arguments.get::<ID>("id") {
            Some(id_string) => Some({
                if let Some(id) = g.subject_id(&*id_string) {
                    match zero_iter {
                        None => Box::new(vec![id].into_iter()),
                        Some(zi) => Box::new(zi.filter(move |i| *i == id))
                    }
                }
                else {
                    Box::new(std::iter::empty())
                }
            }),
            None => zero_iter
        };
    let offset: i32 = arguments.get("offset").unwrap_or(0);
    let limit: Option<i32> = arguments.get("limit");
    let mut constraints : Vec<(String,NodeOrValue,String)> = class_definition.fields.iter()
        .filter_map(|(field_name,field_definition)| {
            if is_reserved_argument_name(field_name) {
                return None;
            }

            if let Some(base_type) = field_definition.base_type() {
                if let Some(value) = arguments.get(field_name) {
                    let field_name_expanded =
                        prefixes.expand_schema(field_name);
                    let expanded_object_value =
                        graphql_scalar_to_value_string(value,base_type);
                    Some((field_name_expanded,
                          NodeOrValue::Value,
                          expanded_object_value))
                }else{
                    None
                }
            }else{
                None
            }
        })
        .collect();
    let expanded_class_name = prefixes.expand_schema(class_name);
    constraints.push((RDF_TYPE.to_owned(), NodeOrValue::Node, expanded_class_name));
    let it = lookup_field_requests(g, &constraints, new_zero_iter)
        .skip(usize::try_from(offset).unwrap_or(0));

    if let Some(limit) = limit {
        it.take(usize::try_from(limit).unwrap_or(0)).collect()
    }
    else {
        it.collect()
    }
}
