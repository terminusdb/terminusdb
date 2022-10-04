use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;

use crate::{terminus_store::*, value::graphql_scalar_to_value_string, consts::RDF_TYPE};

use super::frame::{Prefixes, ClassDefinition};

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

pub fn lookup_field_requests(
    g: &SyncStoreLayer,
    constraints: Vec<(String, NodeOrValue, String)>,
) -> Vec<u64> {
    if constraints.len() == 0 {
        panic!("Somehow there are no constraints on the triples")
    } else {
        let (p, oty, o) = &constraints[0];
        let zero_iter = predicate_value_iter(g, &p, &oty, &o);
        constraints[1..]
            .iter()
            .fold(zero_iter, |iter, (p, oty, o)| {
                predicate_value_filter(g, iter, &p, &oty, &o)
            })
            .collect()
    }
}

pub fn run_filter_query(
    g: &SyncStoreLayer,
    prefixes: &Prefixes,
    arguments: &juniper::Arguments,
    class_name: &str,
    class_definition: &ClassDefinition) -> Vec<u64> {

    let mut constraints : Vec<(String,NodeOrValue,String)> = class_definition.fields.iter()
        .filter_map(|(field_name,field_definition)| {
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
    lookup_field_requests(g, constraints)
}
