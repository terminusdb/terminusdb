use terminus_store::layer::*;
use terminusdb_store_prolog::layer::*;
use swipl::prelude::*;

use std::collections::{HashMap,HashSet};

use super::types::*;
use itertools;

const SYS_CLASS: &str = "http://terminusdb.com/schema/sys#Class";
const SYS_TAGGED_UNION: &str = "http://terminusdb.com/schema/sys#TaggedUnion";
const SYS_SUBDOCUMENT: &str = "http://terminusdb.com/schema/sys#subdocument";
const SYS_INHERITS: &str = "http://terminusdb.com/schema/sys#inherits";
const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

fn get_direct_subdocument_ids_from_schema<L:Layer>(layer: &L) -> impl Iterator<Item=u64> {
    if let Some(subdocument_id) = layer.predicate_id(SYS_SUBDOCUMENT) {
        itertools::Either::Left(
            layer.triples_p(subdocument_id)
                .map(|t|t.subject))
    }
    else {
        itertools::Either::Right(std::iter::empty())
    }
}

fn get_subdocument_ids_from_schema<L:Layer>(layer: &L) -> HashSet<u64> {
    let mut result = HashSet::new();
    let inheritance = get_reverse_inheritance_graph(layer);
    let mut work: Vec<_> = get_direct_subdocument_ids_from_schema(layer).collect();
    loop {
        if let Some(cur) = work.pop() {
            if !result.insert(cur) {
                // we already found this type.
                continue;
            }

            if let Some(children) = inheritance.get(&cur) {
                work.extend(children);
            }
        }
        else {
            break;
        }
    }

    result
}

fn get_inheritance_graph<L:Layer>(layer: &L) -> HashMap<u64, HashSet<u64>> {
    if let Some(inherits_id) = layer.predicate_id(SYS_INHERITS) {
        let mut result = HashMap::new();
        for triple in layer.triples_p(inherits_id) {
            let entry = result.entry(triple.subject).or_insert_with(||HashSet::new());
            entry.insert(triple.object);
        }

        result
    }
    else {
        HashMap::with_capacity(0)
    }
}

fn get_reverse_inheritance_graph<L:Layer>(layer: &L) -> HashMap<u64, HashSet<u64>> {
    if let Some(inherits_id) = layer.predicate_id(SYS_INHERITS) {
        let mut result = HashMap::new();
        for triple in layer.triples_p(inherits_id) {
            let entry = result.entry(triple.object).or_insert_with(||HashSet::new());
            entry.insert(triple.subject);
        }

        result
    }
    else {
        HashMap::with_capacity(0)
    }
}

pub fn get_type_ids_from_schema<L:Layer>(layer: &L) -> impl Iterator<Item=u64> {
    let type_id_opt = layer.predicate_id(RDF_TYPE);
    if type_id_opt.is_none() {
        // no rdf:type? then there cannot be any type definitions.
        return itertools::Either::Left(std::iter::empty());
    }

    let type_id = type_id_opt.unwrap();
    let class_id = layer.object_node_id(SYS_CLASS);
    let tagged_union_id = layer.object_node_id(SYS_TAGGED_UNION);


    itertools::Either::Right(
        layer.triples_p(type_id)
            .filter(move |t| Some(t.object) == class_id || Some(t.object) == tagged_union_id)
            .map(|t|t.subject))
}

pub fn get_document_type_ids_from_schema<L:Layer>(layer: &L) -> impl Iterator<Item=u64> {
    let subdocument_ids = get_subdocument_ids_from_schema(layer);

    get_type_ids_from_schema(layer)
        .filter(move |t| !subdocument_ids.contains(t))
}

pub fn get_types_from_schema<L:Layer>(layer: &L) -> Vec<String> {
    get_type_ids_from_schema(layer)
        .map(|t|layer.id_subject(t).unwrap())
        .collect()
}

pub fn get_document_types_from_schema<L:Layer>(layer: &L) -> Vec<String> {
    get_document_type_ids_from_schema(layer)
        .map(|t|layer.id_subject(t).unwrap())
        .collect()
}

pub fn translate_subject_id<L1:Layer,L2:Layer>(layer1: &L1, layer2: &L2, id: u64) -> Option<u64> {
    let subject = layer1.id_subject(id).unwrap();
    layer2.subject_id(&subject)
}

pub fn translate_predicate_id<L1:Layer,L2:Layer>(layer1: &L1, layer2: &L2, id: u64) -> Option<u64> {
    let predicate = layer1.id_predicate(id).unwrap();
    layer2.subject_id(&predicate)
}

pub fn translate_object_id<L1:Layer,L2:Layer>(layer1: &L1, layer2: &L2, id: u64) -> Option<u64> {
    let object = layer1.id_object(id).unwrap();
    match object {
        ObjectType::Node(n) => layer2.object_node_id(&n),
        ObjectType::Value(v) => layer2.object_value_id(&v)
    }
}

pub fn schema_to_instance_types<'a, L1:'a+Layer, L2:'a+Layer, I:'a+IntoIterator<Item=u64>>(schema_layer: &'a L1, instance_layer: &'a L2, type_iter: I) -> impl Iterator<Item=u64>+'a {
    type_iter.into_iter()
        .filter_map(move |t| translate_subject_id(schema_layer, instance_layer, t))
}

predicates! {
    #[module("$moo")]
    semidet fn transaction_types(context, transaction_term, types_term) {
        if let Some(layer) = transaction_schema_layer(context, transaction_term)? {
            let types = get_types_from_schema(&layer);
            types_term.unify(types.as_slice())
        }
        else {
            fail()
        }
    }
    #[module("$moo")]
    semidet fn transaction_doctypes(context, transaction_term, types_term) {
        if let Some(layer) = transaction_schema_layer(context, transaction_term)? {
            let types = get_document_types_from_schema(&layer);
            types_term.unify(types.as_slice())
        }
        else {
            fail()
        }
    }

    #[module("$moo")]
    semidet fn layer_types(_context, layer_term, types_term) {
        let layer: WrappedLayer = layer_term.get()?;
        let types = get_types_from_schema(&*layer);
        types_term.unify(types.as_slice())
    }

    #[module("$moo")]
    semidet fn layer_doctypes(_context, layer_term, types_term) {
        let layer: WrappedLayer = layer_term.get()?;
        let types = get_document_types_from_schema(&*layer);
        types_term.unify(types.as_slice())
    }
}

pub fn register() {
    register_transaction_types();
    register_transaction_doctypes();
    register_layer_types();
    register_layer_doctypes();
}
