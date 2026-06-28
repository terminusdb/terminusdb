use rand::{thread_rng, Rng};
use serde_json::{Map, Value};
use std::collections::HashMap;

use crate::consts::*;
use super::DocumentContext;
use super::DocumentContextBlob;
use super::prefix::{expand_prefixed_name, expand_prefixed_name_data};

use crate::terminus_store::layer::*;
use swipl::prelude::*;

#[allow(dead_code)]
pub enum ElaborationError {
    Ineligible(String),
    SchemaError(String),
    ValueError(String),
}

/// Range types supported by the simple-document fast path.
const SIMPLE_RANGE_TYPES: &[&str] = &[
    "http://www.w3.org/2001/XMLSchema#string",
    "http://www.w3.org/2001/XMLSchema#integer",
    "http://www.w3.org/2001/XMLSchema#decimal",
    "http://www.w3.org/2001/XMLSchema#double",
    "http://www.w3.org/2001/XMLSchema#float",
    "http://www.w3.org/2001/XMLSchema#boolean",
];

/// Convert a Prolog term to a serde_json::Value.
///
/// This is a minimal converter that handles the shapes expected for simple
/// input documents: json{} dicts, lists, strings (both Prolog strings and
/// atoms), integers, floats, and the atoms true/false/null.
fn prolog_term_to_json_value<'a, C: QueryableContextType>(
    context: &'a Context<'a, C>,
    term: &Term<'a>,
) -> PrologResult<Value> {
    match term.term_type() {
        TermType::Dict => {
            let mut map = Map::new();
            for (key, val_term) in context.dict_entries(term) {
                let key_str = match key {
                    Key::Atom(a) => a.to_string(),
                    Key::Int(i) => i.to_string(),
                };
                let val = prolog_term_to_json_value(context, &val_term)?;
                map.insert(key_str, val);
            }
            Ok(Value::Object(map))
        }
        TermType::ListPair | TermType::Nil => {
            let mut arr = Vec::new();
            for elt_term in context.term_list_iter(term) {
                arr.push(prolog_term_to_json_value(context, &elt_term)?);
            }
            Ok(Value::Array(arr))
        }
        TermType::String => {
            let s: String = term.get()?;
            Ok(Value::String(s))
        }
        TermType::Atom => {
            let a: Atom = term.get()?;
            let s = a.to_string();
            if s == "true" {
                Ok(Value::Bool(true))
            } else if s == "false" {
                Ok(Value::Bool(false))
            } else if s == "null" {
                Ok(Value::Null)
            } else {
                Ok(Value::String(s))
            }
        }
        TermType::Integer => {
            let i: i64 = term.get()?;
            Ok(Value::Number(i.into()))
        }
        TermType::Float => {
            let f: f64 = term.get()?;
            match serde_json::Number::from_f64(f) {
                Some(n) => Ok(Value::Number(n)),
                None => Ok(Value::String(f.to_string())),
            }
        }
        _ => Err(PrologError::Failure),
    }
}

/// Convert a serde_json::Value to a Prolog term.
///
/// Numbers are kept as integers when possible, floats otherwise, strings as
/// Prolog strings, booleans as true/false atoms, null as the null atom, and
/// objects/arrays are recursively converted.
fn json_value_to_prolog_term<'a, C: QueryableContextType>(
    context: &'a Context<'a, C>,
    value: &Value,
) -> PrologResult<Term<'a>> {
    match value {
        Value::Null => {
            let term = context.new_term_ref();
            term.unify(Atom::new("null"))?;
            Ok(term)
        }
        Value::Bool(b) => {
            let term = context.new_term_ref();
            let atom = if *b { Atom::new("true") } else { Atom::new("false") };
            term.unify(atom)?;
            Ok(term)
        }
        Value::Number(n) => {
            let term = context.new_term_ref();
            if let Some(i) = n.as_i64() {
                term.unify(i)?;
            } else if let Some(f) = n.as_f64() {
                term.unify(f)?;
            } else {
                return Err(PrologError::Failure);
            }
            Ok(term)
        }
        Value::String(s) => {
            let term = context.new_term_ref();
            term.unify(Atom::new(s.as_str()))?;
            Ok(term)
        }
        Value::Array(arr) => {
            let mut terms = Vec::with_capacity(arr.len());
            for item in arr {
                terms.push(json_value_to_prolog_term(context, item)?);
            }
            let term = context.new_term_ref();
            term.unify(terms.as_slice())?;
            Ok(term)
        }
        Value::Object(map) => {
            let mut builder = DictBuilder::new().tag("json");
            for (key, val) in map.iter() {
                let val_term = json_value_to_prolog_term(context, val)?;
                builder = builder.entry(Atom::new(key.as_str()), val_term);
            }
            let term = context.new_term_ref();
            term.put(&builder)?;
            Ok(term)
        }
    }
}

/// Generate a random id suffix for a Random key.
fn random_id_suffix(length: usize) -> String {
    let mut rng = thread_rng();
    let mut buf = Vec::with_capacity(length);
    for _ in 0..length {
        let r = rng.gen_range(0..64);
        let c = if r < 26 {
            b'A' + r
        } else if r < 52 {
            b'a' + (r - 26)
        } else if r < 62 {
            b'0' + (r - 52)
        } else if r == 62 {
            b'-'
        } else {
            b'_'
        };
        buf.push(c);
    }
    unsafe { String::from_utf8_unchecked(buf) }
}

/// Elaborate a list of simple flat documents using the schema layer.
///
/// Falls back with an error if any document is not eligible for the fast path.
pub fn elaborate_simple_documents<L: Layer + Clone>(
    doc_context: &DocumentContext<L>,
    docs: &[Value],
) -> Result<Vec<(Option<String>, Value)>, ElaborationError> {
    let schema = doc_context
        .schema
        .as_ref()
        .ok_or_else(|| ElaborationError::SchemaError("no schema layer".to_string()))?;

    let mut result = Vec::with_capacity(docs.len());
    for doc in docs {
        let pair = elaborate_simple_document(doc_context, schema, doc)?;
        result.push(pair);
    }

    Ok(result)
}

fn elaborate_simple_document<L: Layer + Clone>(
    doc_context: &DocumentContext<L>,
    schema: &L,
    doc: &Value,
) -> Result<(Option<String>, Value), ElaborationError> {
    let obj = doc
        .as_object()
        .ok_or_else(|| ElaborationError::Ineligible("document is not a json object".to_string()))?;

    // Must have @type and no @capture.
    if obj.get("@capture").is_some() {
        return Err(ElaborationError::Ineligible(
            "@capture not supported".to_string(),
        ));
    }

    let type_short = obj
        .get("@type")
        .and_then(Value::as_str)
        .ok_or_else(|| ElaborationError::Ineligible("missing @type".to_string()))?;

    let type_full = expand_prefixed_name(doc_context, type_short)
        .map_err(|e| ElaborationError::SchemaError(format!("unknown prefix in type {}: {}", type_short, e)))?;

    let class_id = schema
        .subject_id(&type_full)
        .ok_or_else(|| ElaborationError::SchemaError(format!("type {} not found", type_full)))?;

    // Verify it is a Class.
    let rdf_type_id = schema
        .predicate_id(RDF_TYPE)
        .ok_or_else(|| ElaborationError::SchemaError("rdf:type not found".to_string()))?;
    let sys_class_id = doc_context
        .schema_sys
        .class()
        .ok_or_else(|| ElaborationError::SchemaError("sys:Class not found".to_string()))?;
    let is_class = schema
        .triples_sp(class_id, rdf_type_id)
        .any(|t| t.object == sys_class_id);
    if !is_class {
        return Err(ElaborationError::Ineligible(
            "type is not a simple Class".to_string(),
        ));
    }

    // Reject subdocuments: the fast path only handles top-level documents.
    if let Some(subdocument_id) = doc_context.schema_sys.subdocument() {
        if schema
            .triples_p(subdocument_id)
            .any(|t| t.subject == class_id)
        {
            return Err(ElaborationError::Ineligible(
                "subdocument class".to_string(),
            ));
        }
    }

    // Verify the key is Random and obtain the base.
    let sys_key_id = doc_context
        .schema_sys
        .key()
        .ok_or_else(|| ElaborationError::SchemaError("sys:key not found".to_string()))?;
    let sys_random_id = doc_context
        .schema_sys
        .random()
        .ok_or_else(|| ElaborationError::SchemaError("sys:Random not found".to_string()))?;
    let sys_base_id = doc_context
        .schema_sys
        .base()
        .ok_or_else(|| ElaborationError::SchemaError("sys:base not found".to_string()))?;

    let key_obj = schema
        .single_triple_sp(class_id, sys_key_id)
        .ok_or_else(|| ElaborationError::Ineligible("no key descriptor".to_string()))?
        .object;
    let is_random = schema
        .triples_sp(key_obj, rdf_type_id)
        .any(|t| t.object == sys_random_id);
    if !is_random {
        return Err(ElaborationError::Ineligible(
            "key is not Random".to_string(),
        ));
    }
    // Determine the ID base for this Random key.
    // First, check if the class has an explicit sys:base.
    let base = if let Some(base_triple) = schema.single_triple_sp(class_id, sys_base_id) {
        match schema.id_object(base_triple.object) {
            Some(ObjectType::Value(v)) => v.as_val::<String, String>(),
            _ => {
                return Err(ElaborationError::SchemaError(
                    "key base is not a string value".to_string(),
                ))
            }
        }
    } else {
        // Compute the default base: <context_base><type_local_name>/
        let context_id = doc_context
            .schema_sys
            .tdb_context()
            .ok_or_else(|| ElaborationError::SchemaError("no context node".to_string()))?;
        let base_pred_id = doc_context
            .schema_sys
            .base()
            .ok_or_else(|| ElaborationError::SchemaError("sys:base not found".to_string()))?;
        let context_base_id = schema
            .single_triple_sp(context_id, base_pred_id)
            .ok_or_else(|| ElaborationError::SchemaError("no context base".to_string()))?
            .object;
        let context_base = match schema.id_object(context_base_id) {
            Some(ObjectType::Value(v)) => v.as_val::<String, String>(),
            _ => {
                return Err(ElaborationError::SchemaError(
                    "context base is not a string value".to_string(),
                ))
            }
        };
        let type_local = type_full
            .rsplit_once('#')
            .map(|(_, local)| local)
            .or_else(|| type_full.rsplit_once('/').map(|(_, local)| local))
            .unwrap_or(&type_full);
        format!("{}{}/", context_base, type_local)
    };

    // Capture an explicit submitted @id (if any) for the contract.
    let submitted_id = obj.get("@id").and_then(Value::as_str).map(|s| s.to_string());

    // Determine the document ID. Use an explicit @id if present,
    // otherwise generate a random suffix.
    let id = if let Some(id_value) = obj.get("@id") {
        let id_short = id_value.as_str().ok_or_else(|| {
            ElaborationError::Ineligible("@id must be a string".to_string())
        })?;
        expand_prefixed_name_data(doc_context, id_short).map_err(|e| {
            ElaborationError::SchemaError(format!(
                "unknown prefix in @id {}: {}",
                id_short, e
            ))
        })?
    } else {
        format!("{}{}", base, random_id_suffix(16))
    };

    // Read property descriptors from the schema layer.
    let mut properties: HashMap<String, String> = HashMap::new();
    for triple in schema.triples_s(class_id) {
        let predicate_id = triple.predicate;
        if predicate_id == rdf_type_id || predicate_id == sys_key_id {
            continue;
        }
        let predicate = schema
            .id_predicate(predicate_id)
            .ok_or_else(|| ElaborationError::SchemaError("invalid predicate id".to_string()))?;

        let range_type_short = match schema.id_object(triple.object) {
            Some(ObjectType::Value(v)) => v.as_val::<String, String>(),
            Some(ObjectType::Node(_)) => {
                // Range types such as xsd:string are often stored as nodes
                // in the schema layer.
                schema
                    .id_object_node(triple.object)
                    .ok_or_else(|| ElaborationError::SchemaError(
                        "invalid range node object".to_string(),
                    ))?
            }
            None => {
                return Err(ElaborationError::SchemaError(
                    "invalid range object".to_string(),
                ))
            }
        };
        let range_type = expand_prefixed_name(doc_context, &range_type_short)
            .map_err(|e| ElaborationError::SchemaError(format!(
                "unknown prefix in range type {}: {}", range_type_short, e
            )))?;

        properties.insert(predicate, range_type);
    }

    // Build the elaborated document.
    let mut elaborated = Map::new();
    elaborated.insert("@id".to_string(), Value::String(id));
    elaborated.insert("@type".to_string(), Value::String(type_full));

    for (key, value) in obj.iter() {
        if key == "@type" || key == "@id" {
            continue;
        }
        let expanded_key = expand_prefixed_name(doc_context, key)
            .map_err(|e| ElaborationError::SchemaError(format!("unknown prefix in property key {}: {}", key, e)))?;
        let range_type = properties.get(&expanded_key).ok_or_else(|| {
            ElaborationError::Ineligible(format!("unknown property {}", key))
        })?;
        let converted = convert_value(value, range_type)?;
        let wrapped = {
            let mut m = Map::new();
            m.insert("@type".to_string(), Value::String(range_type.clone()));
            m.insert("@value".to_string(), converted);
            Value::Object(m)
        };
        elaborated.insert(expanded_key, wrapped);
    }

    Ok((submitted_id, Value::Object(elaborated)))
}

fn convert_value(value: &Value, range_type: &str) -> Result<Value, ElaborationError> {
    match range_type {
        "http://www.w3.org/2001/XMLSchema#string" => match value {
            Value::String(s) => Ok(Value::String(s.clone())),
            Value::Number(n) => Ok(Value::String(n.to_string())),
            Value::Bool(b) => Ok(Value::String(b.to_string())),
            _ => Err(ElaborationError::ValueError(format!(
                "cannot convert to xsd:string: {}",
                value
            ))),
        },
        "http://www.w3.org/2001/XMLSchema#integer" => match value {
            Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Ok(Value::Number(i.into()))
                } else {
                    Err(ElaborationError::ValueError(format!(
                        "expected integer for xsd:integer: {}",
                        value
                    )))
                }
            }
            _ => Err(ElaborationError::ValueError(format!(
                "expected integer for xsd:integer: {}",
                value
            ))),
        },
        "http://www.w3.org/2001/XMLSchema#boolean" => match value {
            Value::Bool(b) => Ok(Value::Bool(*b)),
            Value::String(s) => match s.as_str() {
                "true" => Ok(Value::Bool(true)),
                "false" => Ok(Value::Bool(false)),
                _ => Err(ElaborationError::ValueError(format!(
                    "expected boolean for xsd:boolean: {}",
                    value
                ))),
            },
            _ => Err(ElaborationError::ValueError(format!(
                "expected boolean for xsd:boolean: {}",
                value
            ))),
        },
        "http://www.w3.org/2001/XMLSchema#decimal"
        | "http://www.w3.org/2001/XMLSchema#double"
        | "http://www.w3.org/2001/XMLSchema#float" => match value {
            Value::Number(n) => Ok(Value::Number(n.clone())),
            _ => Err(ElaborationError::ValueError(format!(
                "expected number for {}: {}",
                range_type, value
            ))),
        },
        _ => Err(ElaborationError::Ineligible(format!(
            "unsupported range type {}",
            range_type
        ))),
    }
}

pub fn register() {
    register_rust_elaborate_simple_documents();
    super::prefix::register();
}

/// Build a Prolog verification_contract{} dict for an elaborated document.
fn build_simple_contract<'a, C: QueryableContextType>(
    context: &'a Context<'a, C>,
    submitted_id: &Option<String>,
    elaborated: &Value,
    pre_branch_commit_id_term: Term<'a>,
    pre_schema_layer_id_term: Term<'a>,
) -> PrologResult<Term<'a>> {
    let generated_id = elaborated
        .get("@id")
        .and_then(Value::as_str)
        .ok_or_else(|| PrologError::Failure)?;

    let submitted_id_term = context.new_term_ref();
    match submitted_id {
        Some(id) => submitted_id_term.unify(Atom::new(id.as_str()))?,
        None => submitted_id_term.unify(atom!("none"))?,
    }

    let generated_id_term = context.new_term_ref();
    generated_id_term.unify(Atom::new(generated_id))?;

    let normal_term = context.new_term_ref();
    normal_term.unify(Atom::new("normal"))?;

    let id_pair_term = context.new_term_ref();
    let pair_functor = Functor::new(Atom::new("-"), 2);
    id_pair_term.unify(pair_functor)?;
    id_pair_term.unify_arg(1, &generated_id_term)?;
    id_pair_term.unify_arg(2, &normal_term)?;

    let id_pairs_term = context.new_term_ref();
    id_pairs_term.unify([id_pair_term].as_slice())?;

    let dependencies_term = context.new_term_ref();
    dependencies_term.unify(&[] as &[Term])?;
    let backlinks_term = context.new_term_ref();
    backlinks_term.unify(&[] as &[Term])?;

    let captures_out_term = context.new_term_ref();
    captures_out_term.unify(Atom::new("t"))?;

    let contract = DictBuilder::new()
        .tag("verification_contract")
        .entry(Atom::new("submitted_id"), submitted_id_term)
        .entry(Atom::new("generated_id"), generated_id_term)
        .entry(Atom::new("id_pairs"), id_pairs_term)
        .entry(Atom::new("dependencies"), dependencies_term)
        .entry(Atom::new("backlinks"), backlinks_term)
        .entry(Atom::new("captures_out"), captures_out_term)
        .entry(Atom::new("pre_branch_commit_id"), pre_branch_commit_id_term)
        .entry(Atom::new("pre_schema_layer_id"), pre_schema_layer_id_term);

    let term = context.new_term_ref();
    term.unify(contract)?;
    Ok(term)
}

predicates! {
    #[module("$doc")]
    pub semidet fn rust_elaborate_simple_documents(context, document_context_term, docs_term, pre_branch_commit_id_term, pre_schema_layer_id_term, pairs_term) {
        let doc_context: DocumentContextBlob = document_context_term.get()?;

        // Read the input list of json{} documents into serde_json::Value.
        let mut docs = Vec::new();
        for doc_term in context.term_list_iter(docs_term) {
            docs.push(prolog_term_to_json_value(context, &doc_term)?);
        }

        let results = elaborate_simple_documents(&doc_context, &docs)
            .map_err(|_| PrologError::Failure)?;

        // Build the output list of Elaborated-Contract pairs.
        let mut pair_terms = Vec::with_capacity(results.len());
        for (submitted_id, elaborated) in &results {
            let elaborated_term = json_value_to_prolog_term(context, elaborated)?;
            let contract_term = build_simple_contract(
                context,
                submitted_id,
                elaborated,
                pre_branch_commit_id_term.clone(),
                pre_schema_layer_id_term.clone(),
            )?;
            let pair_term = context.new_term_ref();
            let pair_functor = Functor::new(Atom::new("-"), 2);
            pair_term.unify(pair_functor)?;
            pair_term.unify_arg(1, &elaborated_term)?;
            pair_term.unify_arg(2, &contract_term)?;
            pair_terms.push(pair_term);
        }

        let term = context.new_term_ref();
        term.unify(pair_terms.as_slice())?;
        pairs_term.unify(&term)
    }
}
