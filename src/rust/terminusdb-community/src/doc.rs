use std::collections::HashSet;
use std::sync::Arc;
use std::iter::Peekable;

use crate::terminus_store::*;
use crate::terminus_store::store::sync::*;
use serde_json::{Map, Value};
use serde_json::map;

use super::prefix::PrefixContracter;
use super::schema::*;
use super::consts::*;

use swipl::prelude::*;

pub struct GetDocumentContext<L:Layer> {
    layer: L,
    prefixes: PrefixContracter,
    terminators: HashSet<u64>,
    rdf_type_id: Option<u64>
}

impl<L:Layer> GetDocumentContext<L> {
    pub fn new<SL:Layer>(schema: &SL, instance: L) -> GetDocumentContext<L> {
        let schema_type_ids = get_document_type_ids_from_schema(schema);
        let terminators: HashSet<u64> = schema_to_instance_types(schema, &instance, schema_type_ids).collect();

        let prefixes = prefix_contracter_from_schema_layer(schema);

        let rdf_type_id = instance.predicate_id(RDF_TYPE);

        GetDocumentContext {
            layer: instance,
            prefixes,
            terminators,
            rdf_type_id
        }
    }

    pub fn get_document(&self, iri: &str) -> Option<Map<String, Value>> {
        if let Some(id) = self.layer.subject_id(iri) {
            Some(self.get_id_document(id))
        }
        else {
            None
        }
    }

    fn get_field(&self, object: u64) -> Result<Value, (Map<String, Value>, Peekable<Box<dyn Iterator<Item=IdTriple>+Send>>)> {
        if self.layer.id_object_is_node(object).unwrap() {
            // this might not be a terminator, but we won't know until trying to get a doc stub
            match self.get_doc_stub(object, true) {
                // it's not a terminator so we will need to descend into it. That is, we would need to descend into it if there were any children, so let's check.
                Ok(x) => Err(x),
                // it turns out it is a terminator after all, so we just use the id as a direct value
                Err(s) => Ok(Value::String(s))
            }
        }
        else {
            // This is a terminator, so we're gonna directly get the field
            match self.layer.id_object(object).unwrap() {
                ObjectType::Node(n) => {
                    let n_contracted = self.prefixes.instance_contract(&n).to_string();
                    Ok(Value::String(n_contracted))
                },
                ObjectType::Value(v) => {
                    // TODO obviously we gotta actually parse this value
                    Ok(Value::String(v))
                },
            }
        }
    }

    fn add_field(&self, obj: &mut Map<String, Value>, key: &str, value: Value) {
        // add a field, but if the field is already there, make it a collection
        match obj.entry(key) {
            map::Entry::Vacant(e) => {
                e.insert(value);
            },
            map::Entry::Occupied(mut e) => {
                let mut v = e.get_mut();
                match v {
                    Value::Array(a) => {
                        a.push(value);
                    },
                    _ => {
                        let mut a = Vec::new();
                        let mut old_v = Value::Null;
                        std::mem::swap(&mut old_v, &mut v);
                        a.push(old_v);
                        a.push(value);
                        *v = Value::Array(a);
                    }
                }
            }
        }
    }

    fn get_doc_stub(&self, id: u64, terminate: bool) -> Result<(Map<String, Value>, Peekable<Box<dyn Iterator<Item=IdTriple>+Send>>), String> {
        let id_name = self.layer.id_subject(id).unwrap();
        let id_name_contracted = self.prefixes.instance_contract(&id_name).to_string();

        let rdf_type_id = self.rdf_type_id;
        let mut fields = (Box::new(self.layer.triples_s(id)
                                   .filter(move |f| Some(f.predicate) != rdf_type_id)
        ) as Box<dyn Iterator<Item=IdTriple>+Send>).peekable();

        if fields.peek().is_none() {
            // we're actually dealing with a raw id here
            Err(id_name_contracted)
        }
        else {
            let mut type_name_contracted: Option<String> = None;
            if let Some(rdf_type_id) = self.rdf_type_id {
                if let Some(t) = self.layer.triples_sp(id, rdf_type_id).next() {
                    if terminate && self.terminators.contains(&t.object) {
                        return Err(id_name_contracted);
                    }
                    let type_name = self.layer.id_object_node(t.object).unwrap();
                    type_name_contracted = Some(self.prefixes.schema_contract(&type_name).to_string());
                }
            }

            let mut result = Map::new();
            result.insert("@id".to_string(), Value::String(id_name_contracted));
            if let Some(tn) = type_name_contracted {
                result.insert("@type".to_string(), Value::String(tn));
            }

            Ok((result, fields))
        }
    }

    pub fn get_id_document(&self, id: u64) -> Map<String, Value> {
        let mut stack = Vec::new();

        let (stub, fields) = self.get_doc_stub(id, false).unwrap();
        stack.push((stub, fields));

        loop {
            let (cur, fields) = stack.last_mut().unwrap();
            if let Some(next_field) = fields.peek() {
                match self.get_field(next_field.object) {
                    Ok(val) => {
                        // iterate past this field
                        let next_field = fields.next().unwrap();
                        let p_name = self.layer.id_predicate(next_field.predicate).unwrap();
                        let p_name_contracted = self.prefixes.schema_contract(&p_name).to_string();
                        self.add_field(cur, &p_name_contracted, val);
                    },
                    Err((next_stub, next_fields)) => {
                        // it's a subdocument, we need to iterate deeper, so add it to the stack without iterating past the field.
                        stack.push((next_stub, next_fields));
                    }
                }
            }
            else {
                // done!
                let (cur, _) = stack.pop().unwrap();
                if let Some((parent, parent_fields)) = stack.last_mut() {
                    // we previously peeked a field and decided we needed to recurse deeper.
                    // this is the time to pop it.
                    let t = parent_fields.next().unwrap();
                    let p_name = self.layer.id_predicate(t.predicate).unwrap();
                    let p_name_contracted = self.prefixes.schema_contract(&p_name).to_string();
                    self.add_field(parent, &p_name_contracted, Value::Object(cur));
                }
                else {
                    // we're done, this was the root, time to return!
                    return cur;
                }
            }
        }
    }
}

wrapped_arc_blob!("GetDocumentContext", GetDocumentContextBlob, GetDocumentContext<SyncStoreLayer>, defaults);

use super::types::*;
predicates! {
    #[module("$moo")]
    semidet fn get_document_context(context, transaction_term, context_term) {
        let schema_layer = transaction_schema_layer(context, transaction_term)?.unwrap();
        let instance_layer = transaction_instance_layer(context, transaction_term)?.unwrap();

        let get_context = GetDocumentContext::new(&schema_layer, instance_layer);

        context_term.unify(GetDocumentContextBlob(Arc::new(get_context)))
    }

    #[module("$moo")]
    semidet fn get_document_json_string(_context, get_context_term, doc_name_term, doc_json_string_term) {
        if !doc_name_term.is_string() && !doc_name_term.is_atom() {
            return fail();
        }

        let context: GetDocumentContextBlob = get_context_term.get()?;
        if let Some(result) = doc_name_term.get_str(|s| context.get_document(s.unwrap()))? {
            doc_json_string_term.unify(Value::Object(result).to_string())
        }
        else {
            fail()
        }
    }

    #[module("$moo")]
    semidet fn print_prefix_tree(_context, get_context_term) {
        let get_context: GetDocumentContextBlob = get_context_term.get()?;

        println!("{:?}", get_context.prefixes);

        Ok(())
    }

    #[module("$moo")]
    semidet fn prefix_schema_contract(_context, get_context_term, expanded_term, contracted_term) {
        let expanded: String = expanded_term.get()?;
        let get_context: GetDocumentContextBlob = get_context_term.get()?;

        let contracted = get_context.prefixes.schema_contract(&expanded);

        contracted_term.unify(&*contracted)
    }

    #[module("$moo")]
    semidet fn prefix_instance_contract(_context, get_context_term, expanded_term, contracted_term) {
        let expanded: String = expanded_term.get()?;
        let get_context: GetDocumentContextBlob = get_context_term.get()?;

        let contracted = get_context.prefixes.instance_contract(&expanded);

        contracted_term.unify(&*contracted)
    }
}

pub fn register() {
    register_get_document_context();
    register_get_document_json_string();
    register_prefix_schema_contract();
    register_prefix_instance_contract();
    register_print_prefix_tree();
}
