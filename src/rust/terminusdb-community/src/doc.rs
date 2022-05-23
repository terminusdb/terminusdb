use std::collections::HashSet;
use std::sync::Arc;

use terminus_store::*;
use serde_json::{Map, Value};

use super::prefix::PrefixContracter;
use super::schema::*;
use super::consts::*;

pub struct GetDocumentContext<L:Layer> {
    layer: Arc<L>,
    prefixes: PrefixContracter,
    terminators: HashSet<u64>,
    rdf_type_id: Option<u64>
}

impl<L:Layer> GetDocumentContext<L> {
    pub fn new<SL:Layer>(schema: &SL, instance: Arc<L>) -> GetDocumentContext<L> {
        let schema_type_ids = get_document_type_ids_from_schema(schema);
        let terminators: HashSet<u64> = schema_to_instance_types(schema, &*instance, schema_type_ids).collect();

        let prefixes = prefix_contracter_from_schema_layer(schema);

        let rdf_type_id = instance.predicate_id(RDF_TYPE);

        GetDocumentContext {
            layer: instance.clone(),
            prefixes,
            terminators,
            rdf_type_id
        }
    }

    pub fn get_document(&self, iri: &str) -> Option<Map<String, Value>> {
        if let Some(id) = self.layer.subject_id(iri) {
            self.get_id_document(id)
        }
        else {
            None
        }
    }

    fn get_id_document_shallow(&self, id: u64) -> Option<(Map<String, Value>, Vec<u64>)> {
        let mut result = Map::new();
        for t in self.layer.triples_s(id) {
            if Some(t.predicate) == self.rdf_type_id {
                result.append("@type", Value::String(
            }
        }

        result
    }

    pub fn get_id_document(&self, id: u64) -> Option<Map<String, Value>> {
        let mut stack: Vec<Map<String, Value>> = Vec::new();
        
        unimplemented!();
    }
}
